(void)

;; Enable server certificate validation for https URIs.
(define ((make-ssl-server-connector ctx) uri proxy)
  (let ((remote-end (or proxy uri)))
    (if (eq? 'https (uri-scheme remote-end))
        (ssl-connect (uri-host remote-end)
                     (uri-port remote-end)
                     ctx)
        (http:default-server-connector uri proxy))))
(http:server-connector
  (make-ssl-server-connector
    (ssl-make-client-context* verify?: (not (member "--no-ssl-verify" (command-line-arguments))))))



;; Configuration
;; =============

(let ((uri (config-ref 'server-uri)))
  (when uri (init! uri))
  (access-token (config-ref 'access-token))
  (mxid (config-ref 'mxid)))



;; Utilities
;; =========

(define (mark-last-message-as-read room-id)
  (let* ((last-evt (room-timeline room-id limit: 1))
         (evt-id (mref '(event_id) last-evt)))
    (when evt-id
      (room-mark-read room-id evt-id))))

(define (room-display-name id)
  (let ((ctx (room-context id)))
    (or (room-name ctx)
        (json-true? (mref '(("" . m.room.canonical_alias) alias) ctx))
        (and-let* ((v (json-true? (mref '(("" . m.room.aliases) aliases) ctx))))
             (vector-ref v 0))
        (and-let* ((members (room-members ctx))
                   (check (= (length members) 2))
                   (others (remove (lambda (p) (equal? (caar p) (string-downcase (mxid)))) members)))
             (or (member-displayname (caaar others) ctx)
                 (caaar others)))
        (symbol->string id))))



;; DB replacement
;; ==============

(define (room-exists? id)
  (pair? (symbol-plist id)))

(define (room-context id)
  (get id 'bottom-state))

(define (branch-insert! room-id event)
  (let ((tl (get room-id 'timeline)))
    (put! room-id 'timeline
          (cons event tl))))

(define (last-state-set! room-id ctx)
  (put! room-id 'bottom-state ctx))

(define (read-marker-ref room-id)
  (get room-id 'read-marker))

(define (read-marker-set! room-id event-id)
  (put! room-id 'read-marker event-id))

(define (room-timeline room-id #!key (limit #f) (offset #f))
  (let ((tl (get room-id 'timeline)))
    (if (and limit (<= limit (length tl)))
        (take tl limit)
        tl)))

(define *rooms* '())

(define (any-room)
  (and (pair? *rooms*) (car *rooms*)))

(define (joined-rooms)
  *rooms*)



;; Events contexts
;; ===============


(define (ignored-state-event? evt)
  (and (equal? "m.room.member" (mref '(type) evt))
       (equal? "leave" (mref '(content membership) evt))))

(define (update-context ctx evt #!optional (reverse #f))
  (if (mref '(state_key) evt)
      (let ((key (cons (state-key evt)
                       (string->symbol (mref '(type) evt))))
            (content (mref (if reverse '(prev_content) '(content))
                           evt)))
        (if (or (ignored-state-event? evt)
                (json-false? content))
            (alist-delete key ctx equal?)
            (alist-update key content ctx equal?)))
      ctx))

(define (initial-context state)
  (vector-fold (lambda (i ctx evt)
                 (if (ignored-state-event? evt)
                     ctx
                     (cons (cons (cons (state-key evt)
                                       (string->symbol (mref '(type) evt)))
                                 (mref '(content) evt))
                           ctx)))
               '() state))

(define (state-key evt)
  (string-downcase (mref '(state_key) evt)))

(define (member-displayname who ctx)
  (json-true? (mref `((,(string-downcase who) . m.room.member) displayname) ctx)))

(define (member-avatar who ctx)
  (json-true? (mref `((,(string-downcase who) . m.room.member) avatar_url) ctx)))

(define (room-name ctx)
  (json-true? (mref `(("" . m.room.name) name) ctx)))

(define (room-members ctx)
  (filter (lambda (p) (and (equal? (cdar p) 'm.room.member)
                           (equal? "join" (alist-ref 'membership (cdr p)))))
            ctx))



;; Events printers
;; ===============

(define (mxc->url mxc)
  (let ((mxc-uri (uri-reference mxc)))
    (if (eq? (uri-scheme mxc-uri) 'mxc)
        (uri->string
          (update-uri (server-uri)
                      path: `(/ "_matrix" "media" "r0" "download" 
                                ,(uri-host mxc-uri) ,(cadr (uri-path mxc-uri)))))
        #f)))


(define (m.room.message-printer evt ctx)
  (let* ((sender (mref '(sender) evt))
         (name (or (member-displayname sender ctx)
                   sender))
         (type (mref '(content msgtype) evt))
         (body (mref '(content body) evt)))
    (if body
        (case (string->symbol type)
          ((m.emote) (sprintf "* ~a ~a" name body))
          ((m.image m.file m.video m.audio)
           (sprintf "*** ~a uploaded ~a: ~a" name body
                    (or (mxc->url (mref '(content url) evt))
                        "[invalid uri]")))
          (else (sprintf "<~a> ~a" name body)))
        (sprintf "<~a> [redacted]" name))
        ))

;; TODO fix this mess up
;; TODO membership may be ban, leave… in the context
(define (m.room.member-printer evt ctx)
  (let* ((who (state-key evt))
         (membership (string->symbol (mref '(content membership) evt)))
         (maybe-name (json-true? (mref '(content displayname) evt)))
         (maybe-avatar (json-true? (mref '(content avatar_url) evt)))
         (displayed-name (or maybe-name
                             (member-displayname who ctx)
                             who)))
    (case membership
      ((invite)
       (sprintf "*** ~A was invited to the room" displayed-name))
      ((leave)
       (sprintf "*** ~A left the room" displayed-name))
      ((ban)
       (sprintf "*** ~A was banned from the room" displayed-name))
      ((knock)
       (sprintf "*** ~A knocked" displayed-name))
      ((join)
       (if (equal? "join" (mref `((,who . m.room.member) membership) ctx))
           (let* ((old-name (member-displayname who ctx))
                  (old-avatar (member-avatar who ctx))
                  (same-name (equal? old-name maybe-name))
                  (same-avatar (equal? old-avatar maybe-avatar)))
             (cond ((and (not same-name) (not same-avatar))
                    (sprintf "*** ~A changed its name to ~A and avatar to ~A"
                             (or old-name who) displayed-name (if maybe-avatar
                                                                  (mxc->url maybe-avatar)
                                                                  "nothing")))
                   ((not same-name)
                    (sprintf "*** ~A changed its name to ~A"
                             (or old-name who) displayed-name))
                   ((not same-avatar)
                    (sprintf "*** ~A changed its avatar to ~A"
                             displayed-name (if maybe-avatar
                                                (mxc->url maybe-avatar)
                                                "nothing")))
                   (else "")))
           (sprintf "*** ~A joined the room" displayed-name))))))

(define (com.upyum.ensemble.hole-printer evt ctx)
  (sprintf "### Some history excluded..."))

(define event-printers
  `((m.room.message . ,m.room.message-printer)
    (m.room.member . ,m.room.member-printer)
    (com.upyum.ensemble.hole . ,com.upyum.ensemble.hole-printer)))


;; Takes a contextualized event and gives a string representation of it
(define (print-event evt ctx)
  (let* ((type (string->symbol (mref '(type) evt)))
         (content (mref '(content) evt))
         (printer (alist-ref type event-printers))
         (str (and printer (printer evt ctx)))
         (timestamp (mref '(origin_server_ts) evt))
         (time (if timestamp
                   (time->string
                     (seconds->local-time (/ timestamp 1000))
                     "%d/%m %H:%M")
                   "unknown")))
    (sprintf "[~a] ~a~%" time
             (if str
                 (if (eq? (void) str)
                     (sprintf "### BUG in printer for ~a~%EVT: ~s~%CTX: ~s" type evt ctx)
                     str)
                 (sprintf "No event printer for ~a: ~s" type content)))))



;; Room management
;; ===============

(define (initialize-room! room-id state-events)
  (let* ((state (initial-context state-events)))
    (put! room-id 'top-state state)
    (put! room-id 'bottom-state state)
    (put! room-id 'timeline '())
    (push! room-id *rooms*)
    state))

(define ((advance-timeline events) timeline state)
  (let loop ((i 0)
             (timeline timeline)
             (state state))
    (if (= i (vector-length events))
        (values timeline state)
        (let* ((evt (vector-ref events i))
               (new-state (update-context state evt))
               (evt-id (mref '(event_id) evt))
               (fmt-evt `((event_id . ,evt-id)
                          (formated . ,(print-event evt state)))))
          (loop (add1 i)
                (cons fmt-evt timeline)
                new-state)))))

(define ((punch-hole prev-batch state-events) timeline state)
  (info "======= LIMITED in ~A~%" room-id)
  (values (cons (make-hole-event prev-batch)
                timeline)
          (vector-fold (lambda (i ctx evt)
                         (update-context ctx evt))
                       state
                       state-events)))

;; TODO
#;(define (manage-ephemerals ephemerals)
  (vector-for-each (lambda (i evt)
                     (when (equal? (mref '(type) evt) "m.receipt")
                       (let ((datum (mref '(content) evt)))
                         (for-each (lambda (id+reads)
                                     (when (member (string-downcase (mxid))
                                                   (map (o string-downcase symbol->string car)
                                                     (mref '(m.read) (cdr id+reads))))
                                       (info "[marker] id+reads: ~s~%" id+reads)
                                       (read-marker-set! room-id (car id+reads))))
                           datum)
                         )))
                   ephemerals))

(define (handle-sync batch)
  (let ((next (mref '(next_batch) batch)))
    (info "[~A] update: ~a~%" (seconds->string) next)
    (for-each advance-room (mref '(rooms join) batch))
    #;(config-set! 'next-batch next)
    next))

(define (advance-room id+data)
  (let* ((room-id (car id+data))
         (room-data (cdr id+data))
         (limited (mref '(timeline limited) room-data))
         (events (mref '(timeline events) room-data))
         (ephemerals (mref '(ephemeral events) room-data))
         (prev-batch (mref '(timeline prev_batch) room-data))
         (state* (mref '(state events) room-data))
         (state (if (vector? state*) state* #()))
         (notifs (mref '(unread_notifications notification_count) room-data))
         (highlights (mref '(unread_notifications highlight_count) room-data)))
    (unless (room-exists? room-id)
      (initialize-room! room-id state))
    (receive (new-timeline new-state)
      ((compose ;; Timeline events
                (advance-timeline events)
                ;; Timeline Hole
                (if limited
                    (punch-hole prev-batch state)
                    values))
       (room-timeline room-id) (room-context room-id))
      (put! room-id 'timeline new-timeline)
      (put! room-id 'bottom-state new-state))

    ;; TODO Ephemerals
    #;(manage-ephemerals ephemerals)
    ;; TODO notifications from backend to frontend
    #;(when (and update-ui window-dirty (eq? (current-room) room-id))
      (refresh-messageswin))
    #;(when (and highlights (> highlights 0) (not (eq? (current-room) room-id)))
      (set! *highlights* (lset-adjoin eq? *highlights* room-id))
      (when update-ui (refresh-statuswin) (beep)))
    #;(when (and notifs (> notifs 0) (not (eq? (current-room) room-id)))
      (set! *notifications* (lset-adjoin eq? *notifications* room-id))
      (when update-ui (refresh-statuswin)))
  ))



;; Holes management
;; ================

(define *requested-holes* '())

(define (make-hole-event from)
  (let ((evt-id (sprintf "hole-~A" from)))
    `((event_id . ,evt-id)
      (type . "com.upyum.ensemble.hole")
      (content (from . ,from))
      (formated . "… some history missing …"))))

#;(define (fill-hole room-id hole-evt-id hole-id msgs hole)
  (let* ((incoming-events (mref '(chunk) msgs))
         (new-events (filter-out-known-events! (vector->list incoming-events)))
         (number (+ (length new-events) 2))
         (neighs (event-neighbors room-id hole-id))
         (incr (if (car neighs) (/ (- (caddr neighs) (car neighs)) number) 1))
         (start (if (car neighs) (+ (car neighs) incr) (- (caddr neighs) number)))
         (ctx-id (cadr (event-ref hole-evt-id)))
         (ctx (state-by-id ctx-id))
         )
    (info "[fill-hole] ~a ~a ~s number: ~a start: ~a incr: ~a end: ~a~%"
          room-id hole-id neighs number start incr (+ start -1 (* number incr)))
    (with-transaction db
      (lambda ()
        (branch-remove! hole-id)
        (for-each
          (lambda (i evt)
            (let* ((evt-id (mref '(event_id) evt))
                   (new-ctx (update-context ctx evt #t)))
              (unless (equal? ctx new-ctx)
                (state-set! evt-id new-ctx)
                (set! ctx-id evt-id)
                (set! ctx new-ctx)) ;; FIXME this is wrong
              (event-set! evt-id
                          `((event_id . ,evt-id)
                            (formated . ,(print-event evt ctx)))
                          ctx-id)
              (branch-insert! room-id
                              (+ start (* i incr))
                              evt-id)))
          (iota (length new-events)
                (length new-events)
                -1)
          new-events)
        ;; New hole
        (unless (null? new-events)
          (let ((evt-id (sprintf "hole-~A-~A" room-id start)))
            (event-set! evt-id
                        `((type . "com.upyum.ensemble.hole")
                          (content (from . ,(mref '(end) msgs)))
                          (formated . "… some history missing …"))
                        ctx-id)
            (info "[new-hole] room: ~A seq: ~A evt: ~A~%" room-id start evt-id)
            (branch-insert! room-id start evt-id)))))
    (set! *requested-holes* (delete! hole *requested-holes*))
    ;; TODO send messages to frontend
    #;(refresh-messageswin)))

(define fill-hole void)

(define (filter-out-known-events! evts)
  (take-while! (lambda (evt) (null? (event-ref (mref '(event_id) evt))))
               evts))

(define (request-hole-messages room-id hole-evt-id hole-evt hole-id limit hole)
  (let* ((msgs (room-messages room-id
                              from: (mref '(content from) hole-evt)
                              limit: limit
                              dir: 'b)))
    (list room-id hole-evt-id hole-id msgs hole)))
