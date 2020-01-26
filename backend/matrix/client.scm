;; Utilities
;; =========

;; TODO update m.read when messages are shown on the screen, and
;;      m.fully_read on user request
(define (room-mark-read room evt)
  (defer 'receipt
    room-read-markers room
    `((m.read . ,evt)
      (m.fully_read . ,evt))))

(define (mark-last-message-as-read room-id)
  (let* ((tl (room-timeline room-id limit: 1))
         (last-evt (if (pair? tl) (car tl) '()))
         (evt-id (mref '(event_id) last-evt)))
    (when evt-id
      (room-mark-read room-id evt-id)
      evt-id)))

(define (room-display-name room-id ctx)
  (or (and (not ctx) "")
      (room-name ctx)
      (json-true? (mref '(("" . m.room.canonical_alias) alias) ctx))
      (and-let* ((v (json-true? (mref '(("" . m.room.aliases) aliases) ctx))))
           (vector-ref v 0))
      (and-let* ((members (room-members ctx))
                 (check (= (length members) 2))
                 (others (remove (lambda (p) (equal? (caar p) (string-downcase (mxid)))) members)))
           (or (member-displayname (caaar others) ctx)
               (caaar others)))
      (symbol->string room-id)))

(define new-transaction-id
  (let ((cnt 0))
    (lambda ()
      (set! cnt (add1 cnt))
      (sprintf "~a-~a-~a" (current-process-id) (current-seconds) cnt))))

(define (password-login user password)
  (let ((res (login `((type . "m.login.password")
                      (user . ,user)
                      (password . ,password)))))
    (mxid (alist-ref 'user_id res))
    (access-token (alist-ref 'access_token res))))


;; DB replacement
;; ==============

(define (room-exists? id)
  (pair? (symbol-plist id)))

(define (room-context id)
  (or (get id 'bottom-state) '()))

(define (last-state-set! room-id ctx)
  (put! room-id 'bottom-state ctx))

(define (read-marker-ref room-id)
  (get room-id 'read-marker))

(define (read-marker-set! room-id event-id)
  (put! room-id 'read-marker event-id))

(define (room-timeline room-id #!key (limit #f) (offset 0))
  (let* ((tl (or (get room-id 'timeline) '()))
         (offseted-tl (if (<= offset (length tl)) (drop tl offset) '())))
    (if (and limit (<= limit (length offseted-tl)))
        (take offseted-tl limit)
        offseted-tl)))

(define *rooms* '())
(define *next-batch* #f)

(define (joined-rooms)
  *rooms*)

(define (save-state)
  (info "State saving started")
  (with-output-to-file *state-file*
    (lambda ()
      (for-each
        (lambda (r)
          (info "Saving state for: ~a" r)
          (write `(room ,r ,(room-data r)))
          (newline))
        *rooms*)
      (for-each
        (lambda (inv)
          (info "Saving invitation: ~s" inv)
          (write `(invitation ,(car inv) ,(cdr inv)))
          (newline))
        *rooms-invited*)
      (write `(next-batch ,*next-batch*))
      (write `(state-version ,+state-cache-version+))))
  (info "Done saving state"))

(define +properties-saved+
  '(notifications highlights read-marker timeline bottom-state))

(define (room-data r)
  (define (extract! sym)
    (let ((prop value rest (get-properties sym +properties-saved+)))
      (set! (symbol-plist sym) (or rest '()))
      (if rest
          (cons* prop value (extract! sym))
          '())))

  (let ((tmp-sym (gensym))
        (plist (symbol-plist r)))
    (set! (symbol-plist tmp-sym) plist)
    (extract! tmp-sym)))

(define (load-state)
  (ipc-info "Loading profile cache")
  (define found-state-version #f)
  (define (load exp)
    (case (car exp)
      ((state-version)
       (set! found-state-version #t)
       (unless (equal? (cadr exp) +state-cache-version+)
         (error "state cache is out of date")))
      ((room)
       (set! (symbol-plist (cadr exp))
         (caddr exp))
       (push! (cadr exp) *rooms*))
      ((next-batch) (set! *next-batch* (cadr exp)))
      ((invitation) (send-invitation-notice (cadr exp) (caddr exp)))
      (else (error "Unknown state element" exp))))

  (handle-exceptions exn
    (begin
      (ipc-info "Profile cache is corrupted, restarting")
      (delete-file *state-file*)
      (restart))
    (when (file-exists? *state-file*)
      (with-input-from-file *state-file*
        (lambda ()
          (port-for-each load read)))
      (unless found-state-version
        (error "state cache is out of date")))))



;; Events contexts
;; ===============


(define (ignored-state-event? evt)
  (and (equal? "m.room.member" (mref '(type) evt))
       (equal? "leave" (mref '(content membership) evt))))

(define (update-context ctx evt #!optional (reverse #f))
  (if (mref '(state_key) evt)
      (let ((key (cons (state-key evt)
                       (string->symbol (mref '(type) evt))))
            (content (mref (if reverse '(unsigned prev_content) '(content))
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

(define (room-member-names ctx)
  (let ((members (room-members ctx)))
    (map
      (lambda (m)
        (or (json-true? (mref '(displayname) m))
            (caar m)))
      members)))


;; Events printers
;; ===============

(define (mxc->url mxc)
  (or
      (and-let* ((_ (string? mxc))
                 (mxc-uri (uri-reference mxc))
                 (_ (eq? (uri-scheme mxc-uri) 'mxc)))
        (uri->string
          (update-uri (server-uri)
                      path: `(/ "_matrix" "media" "r0" "download"
                                ,(uri-host mxc-uri) ,(cadr (uri-path mxc-uri))))))
      "[invalid uri]"))

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
           (let ((url (mref '(content url) evt)))
             (sprintf "*** ~a uploaded ~a: ~a" name body (mxc->url url))))
          (else (sprintf "<~a> ~a" name body)))
        (sprintf "<~a> [redacted]" name))
        ))

(define (m.sticker-printer evt ctx)
  (let* ((sender (mref '(sender) evt))
         (name (or (member-displayname sender ctx)
                   sender))
         (body (mref '(content body) evt))
         (url (mref '(content url) evt)))
    (sprintf "*** ~a sent a sticker: ~a ~a" name body (mxc->url url))))

(define (m.room.member-printer evt ctx)
  (let* ((sender (mref '(sender) evt))
         (sender-name (or (member-displayname sender ctx)
                          sender))
         (who (state-key evt))
         (membership (string->symbol (mref '(content membership) evt)))
         (maybe-name (json-true? (mref '(content displayname) evt)))
         (maybe-avatar (json-true? (mref '(content avatar_url) evt)))
         (displayed-name (or maybe-name
                             (member-displayname who ctx)
                             who)))
    (case membership
      ((invite)
       (sprintf "*** ~A invited ~A to the room" sender-name displayed-name))
      ((leave)
       (sprintf "*** ~A left the room" displayed-name))
      ((ban)
       (sprintf "*** ~A banned ~A from the room" sender-name displayed-name))
      ((knock)
       (sprintf "*** ~A knocked" displayed-name))
      ((join)
       (if (equal? "join" (mref '(unsigned prev_content membership) evt))
           (let* ((old-name (json-true? (mref '(unsigned prev_content displayname)
                                              evt)))
                  (old-avatar (json-true? (mref '(unsigned prev_content avatar_url)
                                                evt)))
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
                   ;; TODO just discard the event
                   (else
                     (sprintf "*** ~A joined but was already there"
                              displayed-name))))
           (sprintf "*** ~A joined the room" displayed-name)))
      (else
        (sprintf "*** ~A membership changed to ~A" displayed-name membership)))))

(define (com.upyum.ensemble.hole-printer evt ctx)
  (sprintf "### Some history excluded..."))

(define event-printers
  `((m.room.message . ,m.room.message-printer)
    (m.room.member . ,m.room.member-printer)
    (m.sticker . ,m.sticker-printer)
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
                     (seconds->local-time
                       (inexact->exact (round (/ timestamp 1000))))
                     "%d/%m %H:%M")
                   "unknown")))
    (sprintf "[~a] ~a~%" time
             (if str
                 str
                 (sprintf "No event printer for ~a: ~s" type content)))))

(define (cleanup-event evt)
  (alist-delete 'content evt))


;; Room management
;; ===============

(define (initialize-room! room-id context)
  (put! room-id 'bottom-state context)
  (put! room-id 'timeline '())
  (push! room-id *rooms*))

(define ((advance-timeline events) timeline state)
  (let loop ((i 0)
             (timeline timeline)
             (state state))
    (if (= i (vector-length events))
        (values timeline state)
        (let* ((evt (vector-ref events i))
               (new-state (update-context state evt))
               (evt-id (mref '(event_id) evt))
               (evt-type (alist-ref 'type evt))
               (formated (print-event evt state))
               (my-name (or (member-displayname (mxid) state)
                            (mxid)))
               (highlight? (and (not (equal? (mref '(sender) evt)
                                             (mxid)))
                                (irregex-search (list 'w/nocase my-name)
                                                formated)))
               (fmt-evt `((event_id . ,evt-id)
                          (type . ,evt-type)
                          (formated . ,formated)
                          ,@(if highlight? '((highlight . #t)) '()))))
          ;; TODO do not crash on this
          (assert (not (string-prefix? "com.upyum.ensemble" evt-type)))
          (loop (add1 i)
                (timeline-cons fmt-evt timeline)
                new-state)))))

(define ((punch-hole prev-batch hole-context) timeline context-now)
  (info "Punching hole: ~s" prev-batch)
  (values (timeline-cons (make-hole-event prev-batch hole-context)
                         timeline)
          context-now))

(define ((punch-checkpoint next-batch checkpoint-context) timeline context-now)
  (values (timeline-cons (make-checkpoint-event next-batch checkpoint-context)
                         timeline)
          context-now))

(define (manage-account-data room-id events)
  (vector-for-each (lambda (i evt)
                     (when (equal? (mref '(type) evt) "m.fully_read")
                       (let ((id (string->symbol (mref '(content event_id) evt))))
                         (read-marker-set! room-id id)
                         (ipc:read-marker room-id id))))
                   events))

(define (handle-sync batch)
  (let ((next (mref '(next_batch) batch)))
    (info "[~A] update: ~a~%" (seconds->string) next)
    (for-each advance-room (or (mref '(rooms join) batch) '()))
    (for-each register-invitation (or (mref '(rooms invite) batch) '()))
    (set! *next-batch* next)
    next))

(define (advance-room id+data)
  (let* ((room-id (car id+data))
         (room-data (cdr id+data))
         (limited (mref '(timeline limited) room-data))
         (events (mref '(timeline events) room-data))
         (account-data (mref '(account_data events) room-data))
         (prev-batch (mref '(timeline prev_batch) room-data))
         (state-events (mref '(state events) room-data))
         (state-events (if (vector? state-events) state-events #()))
         (context-before (room-context room-id))
         (context-now (vector-fold (lambda (i ctx evt)
                                     (update-context ctx evt))
                                   context-before
                                   state-events))
         (notifs (json-true? (mref '(unread_notifications notification_count)
                                   room-data)))
         (highlights (json-true? (mref '(unread_notifications highlight_count)
                                       room-data)))
         (old-timeline (room-timeline room-id)))
    (unless (room-exists? room-id)
      (initialize-room! room-id context-now))
    (set! *rooms-invited*
      (alist-delete! room-id *rooms-invited*))
    (receive (timeline-additions context-after)
      ((compose ;; Timeline events
                (advance-timeline events)
                ;; Timeline Hole
                (if limited
                    (punch-hole prev-batch context-now)
                    values)
                (if limited
                    (punch-checkpoint *next-batch* context-before)
                    values))
       '() context-now)
      (unless (equal? (room-display-name room-id context-before)
                      (room-display-name room-id context-after))
        (ipc:room-name room-id (room-display-name room-id context-after)))
      (unless (equal? (room-member-names context-before)
                      (room-member-names context-after))
        (ipc:room-members room-id (room-member-names context-after)))
      (put! room-id 'timeline
            (timeline-append timeline-additions old-timeline))
      (put! room-id 'bottom-state context-after)
      (remove-temporary-messages! room-id (vector->list events))
      (send-timeline-events room-id timeline-additions))

    (manage-account-data room-id account-data)
    (when highlights
      (put! room-id 'highlights highlights))
    (when notifs
      (put! room-id 'notifications notifs))
    (send-notifications room-id)))

(define (send-notifications room-id)
  (ipc:notifications room-id
                     (inexact->exact (round (or (get room-id 'highlights) 0)))
                     (inexact->exact (round (or (get room-id 'notifications) 0)))))

(define (send-timeline-events room-id tl)
  (for-each
    (lambda (m)
      (ipc:message room-id (cleanup-event m)))
    (reverse tl)))



(define *rooms-invited* '())
(define (register-invitation id+invited-room)
  (let* ((room-id (car id+invited-room))
         (state-events (or (mref '(invite_state events) (cdr id+invited-room))
                           '()))
         (state (initial-context state-events))
         (name (room-display-name room-id state)))
    (send-invitation-notice room-id name)))

(define (send-invitation-notice room-id name)
  (push! (cons room-id name) *rooms-invited*)
  (ipc-info "You have been invited to: ~a" name)
  (ipc-info "You can accept the invitation by typing `/join ~a` or reject it by typing `/leave ~a" room-id room-id))

(define (send-chained-messages room-id evts last-event-id)
  (unless (null? evts)
    (ipc:message-before room-id last-event-id (car evts))
    (send-chained-messages room-id
                           (cdr evts)
                           (string->symbol (mref '(event_id) (car evts))))))

;; Holes management
;; ================

(define *requested-holes* '())

(define (fill-hole room-id hole-evt msgs)
  (info "[fill-hole] ~a ~s~%" room-id hole-evt)
  (let* ((timeline (room-timeline room-id))
         (before-hole after-hole (timeline-split timeline hole-evt))
         (context-at-hole (mref '(content context) hole-evt))
         (events-reversed (mref '(chunk) msgs))
         (context-before-hole (vector-fold (lambda (i ctx evt)
                                             (update-context ctx evt #t))
                                           context-at-hole
                                           events-reversed))
         (events (vector-reverse-copy events-reversed))
         (new-timeline new-context
          ((compose ;; New checkpoint that replaces the previous hole
                    (punch-checkpoint (mref '(content from) hole-evt)
                                      context-at-hole)
                    ;; Timeline events
                    (advance-timeline events)
                    ;; Timeline Hole
                    (if (> (vector-length events) 0)
                        (punch-hole (mref '(end) msgs) context-before-hole)
                        values))
           '() context-before-hole)))
    (put! room-id 'timeline (timeline-append after-hole new-timeline before-hole))
    (send-chained-messages room-id
                           new-timeline
                           (string->symbol (mref '(event_id) hole-evt)))
    (ipc:remove room-id (string->symbol (mref '(event_id) hole-evt))))
  (set! *requested-holes* (delete! hole-evt *requested-holes*)))

(define (request-hole-messages room-id hole-evt checkpoint-evt limit)
  (assert (hole-event? hole-evt))
  (assert (if checkpoint-evt (checkpoint-event? checkpoint-evt) #t))
  (define (defered)
    (let* ((msgs (room-messages room-id
                                from: (mref '(content from) hole-evt)
                                to: (and checkpoint-evt
                                         (mref '(content to) checkpoint-evt))
                                limit: limit
                                dir: 'b)))
      (list room-id hole-evt msgs)))
  (unless (member hole-evt *requested-holes*)
    (push! hole-evt *requested-holes*)
    (defer 'hole-messages defered)))
