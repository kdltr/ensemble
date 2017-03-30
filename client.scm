(use vector-lib clojurian-syntax uri-common)

(load "matrix")
 
;; Config file
;; ===========

(define (reload)
  (save-config)
  (load "client"))

(define (save-config)
  (with-output-to-file "config.scm"
    (lambda ()
      (for-each write `((init! ,(uri->string (server-uri)))
                        (access-token ,(access-token))
                        (transaction-id ,(transaction-id)))))))

(when (file-exists? "config.scm")
  (for-each eval (read-file "config.scm")))


;; Events contexts
;; ===============

(define context-updaters
  `((m.room.create . ,(lambda (ctx evt)
                        (mupdate '(creator)
                                (mref '(content creator) evt)
                                ctx)))
    ;; TODO: Take care of the `ban` membership
    (m.room.member . ,(lambda (ctx evt)
                        (let ((membership (string->symbol (mref '(content membership) evt)))
                              (who (mref '(state_key) evt))
                              (display-name (mref '(content displayname) evt))
                              (avatar-url (mref '(content avatar_url) evt)))
                          (as-> ctx ctx
                                (case membership
                                  ((invite)
                                   (mupdate '(invited) (lset-adjoin string=? (or (mref '(invited) ctx) '()) who) ctx))
                                  ((join)
                                   (mupdate '(members) (lset-adjoin string=? (or (mref '(members) ctx) '()) who) ctx))
                                  ((leave)
                                   (mupdate '(members) (delete who (or (mref '(members) ctx) '())) ctx))
                                  (else
                                    (warning "Unknown membership from m.room.member event" membership)
                                    ctx))
                                (if display-name
                                    (if (eq? display-name 'null)
                                        (mdelete `(member-names ,who) ctx)
                                        (mupdate `(member-names ,who) display-name ctx))
                                    ctx)
                                (if avatar-url
                                    (if (eq? avatar-url 'null)
                                        (mdelete `(member-avatars ,who) ctx)
                                        (mupdate `(member-avatars ,who) avatar-url ctx))
                                    ctx)))))
    (m.room.name . ,(lambda (ctx evt)
                      (let ((name (mref '(content name) evt)))
                        (if (member name '(#f "" null))
                            (mdelete '(name) ctx)
                            (mupdate '(name) name ctx)))))
    (m.room.topic . ,(lambda (ctx evt)
                       (let ((topic (mref '(content topic) evt)))
                         (if (member topic '(#f "" null))
                             (mdelete '(topic) ctx)
                             (mupdate '(topic) topic ctx)))))
    (m.room.avatar . ,(lambda (ctx evt)
                        (let ((url (mref '(content url) evt)))
                          (mupdate '(avatar) url ctx))))
    ))

(define (update-context ctx evt)
  (let* ((evt-type (string->symbol (mref '(type) evt)))
         (updater (alist-ref evt-type context-updaters)))
    (if updater
        (updater ctx evt)
        (begin 
          (warning "Event of unknown type" evt-type)
          ctx))))

(define (initial-context state)
  (vector-fold (lambda (i ctx evt) (update-context ctx evt)) '() state))

;; Updates a timeline from a vector of events, attaching context to them
(define (advance-timeline timeline ctx evts)
  (let loop ((ctx ctx)
             (i 0)
             (timeline timeline))
    (if (= i (vector-length evts))
        timeline
        (let* ((evt (vector-ref evts i))
               (new-ctx (update-context ctx evt)))
          (loop new-ctx
                (add1 i)
                (cons (mupdate '(_context) new-ctx evt)
                      timeline))))))


;; Events printers
;; ===============

(define event-printers
  `((m.room.message . ,(lambda (evt)
                         (let* ((sender (mref '(sender) evt))
                                (name (or (mref `(_context member-names ,sender) evt)
                                          sender)))
                           (format #f "<~a> ~a" name (mref '(content body) evt)))))
    ))


;; Takes a contextualized event and gives a string representation of it
(define (print-event evt)
  (let* ((type (string->symbol (mref '(type) evt)))
         (content (mref '(content) evt))
         (printer (alist-ref type event-printers)))
    (if printer
        (printer evt)
        (format #f "unknown event ~a: ~s" type content))))


;; Stuff
;; =====

(define (initial-timelines batch)
  (let* ((rooms (mref '(rooms join) batch))
         (contexts (map (o initial-context (cut mref '(state events) <>) cdr) rooms))
         (timelines (map
                      (lambda (p ctx) (cons (car p)
                                            (advance-timeline '() ctx (mref '(timeline events) (cdr p)))))
                      rooms contexts)))
    timelines))

(define (advance-timelines timelines batch)
  (if (null? timelines)
      '()
      (let* ((room (car timelines))
             (room-id (car room))
             (timeline (cdr room))
             (batch-timeline (mref `(rooms join ,room-id timeline events) batch)))
        (cons (cons room-id
                    (advance-timeline timeline
                                      (mref '(_context) (car timeline))
                                      (or batch-timeline #())))
              (advance-timelines (cdr timelines) batch)))))

(define (rooms-contexts tls)
  (map
    (lambda (p)
      (cons (car p)
            (mref '(_context) (cadr p))))
    tls))


;; TUI connection
;; ==============

(load "tui")
(use mailbox utf8-srfi-13)

(define current-room (make-parameter #f))

(define *timelines* '())

(define ui-events (make-mailbox))

(define (sync-loop batch)
  (let ((new-batch (sync timeout: 30000 since: (mref '(next_batch) batch))))
    (mailbox-send! ui-events (cons 'batch new-batch))
    (sync-loop new-batch)))

(define (input-loop)
  (mailbox-send! ui-events
                 (process-input (cut cons 'char <>) (cut cons 'key <>)))
  (input-loop))

(define (main-loop)
  (fill! central-frame #\space)
  (draw-messages! (alist-ref (current-room) *timelines*))
  (refresh!)
  (let* ((ui-evt (mailbox-receive! ui-events))
         (type (car ui-evt))
         (content (cdr ui-evt)))
    (case type
      ((char)
       (register-char content))
      ((key)
       (register-key content))
      ((batch)
       (set! *timelines* (advance-timelines *timelines* content)))
      (else
        (error "unknown ui event" ui-evt)))
    (main-loop)))

(define commands
  `((me . ,(lambda (args) (message:emote (current-room) (string-join args " "))))
    (room . ,(lambda (args)
               (if (null? args)
                   (status-message (format #f "Current room: ~a" (current-room)))
                   (let ((room (string->symbol (car args))))
                     (when (alist-ref room *timelines*)
                       (status-message (format #f "Room changed from ~a to ~a" (current-room) room))
                       (current-room room)
                       (refresh!))))))
    (rooms . ,(lambda (args)
                (status-message (format #f "Rooms joined: ~a" (map car *timelines*)))))
    (exit . ,(lambda (args)
               (save-config) (exit 0)))
    ))

(define (handle-command str)
  (let* ((cmdline (string-split (string-drop str 1) " "))
         (cmd (string->symbol (car cmdline)))
         (args (cdr cmdline))
         (proc (alist-ref cmd commands)))
    (if proc
        (proc args)
        (status-message (format #f "Unknown command: ~a" cmd)))))

(define (handle-input str)
  (set! *text* "")
  (set! *cursor-pos* 0)
  (refresh-input-bar!)
  (unless (equal? str "")
    (if (char=? (string-ref str 0) #\/)
        (handle-command str)
        (message:text (current-room) str))))

(define (startup)
  (let ((batch0 (sync)))
    (set! *timelines* (initial-timelines batch0))
    (current-room (caar *timelines*))
    (thread-start! (lambda () (sync-loop batch0)))
    (thread-start! (make-thread input-loop))
    (main-loop)))
