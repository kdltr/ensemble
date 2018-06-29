(void)

(cond-expand
      (csi (define (info fmt . args)
               (apply fprintf (current-error-port) fmt args)))
      (else (define-syntax info
              (syntax-rules ()
                ((info . rest) (void))))))

(include "matrix.scm")

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

(define (mark-last-message-as-read)
  (let* ((last-evt (caar (room-timeline (current-room) limit: 1)))
         (id (mref '(event_id) last-evt)))
    (when id
      (room-mark-read (current-room) id))))

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

(define (initialize-room! room-data)
  (let* ((room-id (car room-data))
         (events (mref '(state events) (cdr room-data)))
         (state (initial-context events))
         (state-id (if (null? state)
                       "empty-state"
                       (mref '(event_id)
                             (vector-ref events (sub1 (vector-length events)))))))
    (state-set! state-id state)
    (last-state-set! room-id state-id)
    (list state-id state)))

(define (advance-room room-data #!optional (update-ui #t))
  (let* ((room-id (car room-data))
         (window-dirty #f)
         (limited (mref '(timeline limited) (cdr room-data)))
         (events (mref '(timeline events) (cdr room-data)))
         (ephemerals (mref '(ephemeral events) (cdr room-data)))
         (state (mref '(state events) (cdr room-data)))
         (notifs (mref '(unread_notifications notification_count) (cdr room-data)))
         (highlights (mref '(unread_notifications highlight_count) (cdr room-data)))
         (base-sequence (add1 (branch-last-sequence-number room-id)))
         (init-ctx (cond ((and (room-exists? room-id) (vector? state) (not (vector-empty? state)))
                          (let ((id (mref '(event_id)
                                          (vector-ref state (sub1 (vector-length state)))))
                                (ctx (vector-fold (lambda (i ctx evt) (update-context ctx evt))
                                                  (room-context room-id)
                                                  state)))
                            (state-set! id ctx)
                            (list id ctx)))
                         ((room-exists? room-id)
                          (room-last-state-id-and-state room-id))
                         (else
                           (initialize-room! room-data)))))
    ;; Timeline Hole
    (when limited
      (info "======= LIMITED~%")
      (let ((evt-id (sprintf "hole-~A-~A" room-id base-sequence)))
        (event-set! evt-id
                    `((type . "com.upyum.ensemble.hole")
                      (content (from . ,(mref '(timeline prev_batch) (cdr room-data)))))
                    (car init-ctx))
        (info "room: ~A seq: ~A evt: ~A~%" room-id base-sequence evt-id)
        (branch-insert! room-id base-sequence evt-id)
        (set! base-sequence (add1 base-sequence))))
    ;; Timeline
    (vector-for-each (lambda (i evt)
                       (let* ((id+old-ctx init-ctx)
                              (prev-ctx-id (car id+old-ctx))
                              (old-ctx (cadr id+old-ctx))
                              (new-ctx (update-context old-ctx evt))
                              (evt-id (mref '(event_id) evt)))
                         (event-set! evt-id evt prev-ctx-id)
                         (when (not (eq? old-ctx new-ctx))
                           (state-set! evt-id new-ctx)
                           (set! init-ctx (list evt-id new-ctx)))
                         (last-state-set! room-id (car init-ctx))
                         (branch-insert! room-id
                                         (+ base-sequence i)
                                         evt-id)
                         (set! window-dirty #t)
                         ))
                     events)
    ;; Ephemerals
    (vector-for-each (lambda (i evt)
                       (when (equal? (mref '(type) evt) "m.receipt")
                         (let ((datum (mref '(content) evt)))
                           (for-each (lambda (id+reads)
                                       (when (member (string-downcase (mxid))
                                                     (map (o string-downcase symbol->string car)
                                                       (mref '(m.read) (cdr id+reads))))
                                         (info "[marker] id+reads: ~s~%" id+reads)
                                         (read-marker-set! room-id (car id+reads))
                                         (set! window-dirty #t)))
                             datum)
                           )))
                     ephemerals)
    (when (and update-ui window-dirty (eq? (current-room) room-id))
      (refresh-messageswin))
    (when (and highlights (> highlights 0) (not (eq? (current-room) room-id)))
      (set! *highlights* (lset-adjoin eq? *highlights* room-id))
      (when update-ui (refresh-statuswin) (beep)))
    (when (and notifs (> notifs 0) (not (eq? (current-room) room-id)))
      (set! *notifications* (lset-adjoin eq? *notifications* room-id))
      (when update-ui (refresh-statuswin)))
  ))



;; Holes management
;; ================

(define *requested-holes* '())

(define (fill-hole room-id hole-evt-id hole-id msgs hole)
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
                (set! ctx new-ctx))
              (event-set! evt-id evt ctx-id)
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
                          (content (from . ,(mref '(end) msgs))))
                        ctx-id)
            (info "[new-hole] room: ~A seq: ~A evt: ~A~%" room-id start evt-id)
            (branch-insert! room-id start evt-id)))))
    (set! *requested-holes* (delete! hole *requested-holes*))
    (refresh-messageswin)))

(define (filter-out-known-events! evts)
  (take-while! (lambda (evt) (null? (event-ref (mref '(event_id) evt))))
               evts))

(define (request-hole-messages room-id hole-evt-id hole-evt hole-id limit hole)
  (let* ((msgs (room-messages room-id
                              from: (mref '(content from) hole-evt)
                              limit: limit
                              dir: 'b)))
    (list room-id hole-evt-id hole-id msgs hole)))

(include "tui.scm")
