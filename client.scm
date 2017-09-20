;; Make ncurses wait less time when receiving an ESC character
(setenv "ESCDELAY" "20")

(cond-expand
      (csi (define (info fmt . args)
               (apply fprintf (current-error-port) fmt args))
           (load "locale.so"))
      (else (define-syntax info
              (syntax-rules ()
                ((info . rest) (void))))
            (include "locale.scm")))

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

(define sync-filter (make-parameter #f))



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
                        (account-name ,(account-name))
                        (mxid ,(mxid))
                        (sync-filter ,(sync-filter)))))))


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
                                (if (json-true? display-name)
                                    (mupdate `(member-names ,who) display-name ctx)
                                    (mdelete `(member-names ,who) ctx))
                                (if (json-true? avatar-url)
                                    (mupdate `(member-avatars ,who) avatar-url ctx)
                                    (mdelete `(member-avatars ,who) ctx))))))
    (m.room.name . ,(lambda (ctx evt)
                      (let ((name (mref '(content name) evt)))
                        (if (json-false? name)
                            (mdelete '(name) ctx)
                            (mupdate '(name) name ctx)))))
    (m.room.topic . ,(lambda (ctx evt)
                       (let ((topic (mref '(content topic) evt)))
                         (if (json-false? topic)
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
          (warning "No context updater for event" evt-type)
          ctx))))

(define (initial-context state)
  (vector-fold (lambda (i ctx evt) (update-context ctx evt)) '() state))



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
         (name (or (mref `(member-names ,sender) ctx)
                   sender))
         (type (mref '(content msgtype) evt))
         (body (mref '(content body) evt)))
    (if body
        (case (string->symbol type)
          ((m.emote) (sprintf "* ~a ~a" name body))
          ((m.image m.file m.video m.audio)
           (sprintf "*** ~a uploaded ~a: ~a" name body (or (mxc->url (mref '(content url) evt))
                                                           "[invalid uri]")))
          (else (sprintf "<~a> ~a" name body)))
        (sprintf "<~a> [redacted]" name))
        ))

;; TODO fix this mess up
(define (m.room.member-printer evt ctx)
  (let* ((who (mref '(state_key) evt))
         (membership (string->symbol (mref '(content membership) evt)))
         (maybe-name (json-true? (mref '(content displayname) evt)))
         (maybe-avatar (json-true? (mref '(content avatar_url) evt)))
         (displayed-name (or maybe-name (mref `(member-names ,who) ctx) who)))
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
       (if (member who (or (mref '(members) ctx) '()))
           (let* ((old-name (mref `(member-names ,who) ctx))
                  (old-avatar (mref `(member-avatars, who) ctx))
                  (same-name (equal? old-name maybe-name))
                  (same-avatar (equal? old-avatar maybe-avatar)))
             (cond ((and (not same-name) (not same-avatar))
                    (sprintf "*** ~A changed its name to ~A and avatar to ~A"
                             old-name displayed-name (if maybe-avatar
                                                         (mxc->url maybe-avatar)
                                                         "nothing")))
                   ((not same-name)
                    (sprintf "*** ~A changed its name to ~A"
                             old-name displayed-name))
                   ((not same-avatar)
                    (sprintf "*** ~A changed its avatar to ~A"
                             displayed-name (if maybe-avatar
                                                (mxc->url maybe-avatar)
                                                "nothing")))))
           (sprintf "*** ~A joined the room" displayed-name))))))

(define event-printers
  `((m.room.message . ,m.room.message-printer)
    (m.room.member . ,m.room.member-printer)
    ))


;; Takes a contextualized event and gives a string representation of it
(define (print-event evt ctx)
  (let* ((type (string->symbol (mref '(type) evt)))
         (content (mref '(content) evt))
         (printer (alist-ref type event-printers)))
    (if printer
        (printer evt ctx)
        (sprintf "No event printer for ~a: ~s" type content))))



(define tty-fileno 0)
(define rows)
(define cols)
(define inputwin)
(define statuswin)

(define (start-interface)
  (initscr)
  (noecho)
  (cbreak)
  (start_color)
  (set!-values (rows cols) (getmaxyx (stdscr)))

  (set! inputwin (newwin 1 cols (- rows 1) 0))
  (keypad inputwin #t)
  (set! statuswin (newwin 1 cols (- rows 2) 0))
  (init_pair 1 COLOR_BLACK COLOR_WHITE)
  (wbkgdset statuswin (COLOR_PAIR 1))
  )

(define current-room (make-parameter #f))

(define ui-chan (gochan 0))

(define-record room window context notification)
(define *rooms* '())

(include "tui.scm")
