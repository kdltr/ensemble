;; Make ncurses wait less time when receiving an ESC character
(setenv "ESCDELAY" "20")

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



(define tty-fileno 0)
(define rows)
(define cols)
(define inputwin)
(define statuswin)
(define messageswin)

(define (start-interface)
  (initscr)
  (noecho)
  (cbreak)
  (start_color)
  (set!-values (rows cols) (getmaxyx (stdscr)))

  (set! messageswin (newwin (- rows 2) cols 0 0))
  (scrollok messageswin #t)
  (idlok messageswin #t)

  (set! inputwin (newwin 1 cols (- rows 1) 0))
  (keypad inputwin #t)
  (set! statuswin (newwin 1 cols (- rows 2) 0))
  (init_pair 1 COLOR_BLACK COLOR_WHITE)
  (init_pair 2 COLOR_RED COLOR_WHITE)
  (wbkgdset statuswin (COLOR_PAIR 1))
  (wprintw messageswin "Loading…")
  (wrefresh messageswin)
  )

(define current-room (make-parameter #f))

(define ui-chan (gochan 0))

(define *notifications* '())
(define *highlights* '())

(include "tui.scm")
