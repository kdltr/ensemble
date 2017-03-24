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
                                   (mupdate '(members) (delete who (or (mref '(members) ctx) '())) ctx)))
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
                           (format #f "<~a>: ~a" name (mref '(content body) evt)))))
    ))


;; Takes a contextualized event and gives a string representation of it
(define (print-event evt)
  (let* ((type (string->symbol (mref '(type) evt)))
         (content (mref '(content) evt))
         (printer (alist-ref type event-printers)))
    (if printer
        (printer evt)
        (format #f "unknown event ~a: ~s" type content))))

