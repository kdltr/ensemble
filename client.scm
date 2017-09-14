;; Make ncurses wait less time when receiving an ESC character
(setenv "ESCDELAY" "20")
;; Make ncurses behave correctly with utf-8
(include "locale.scm")

(include "matrix.scm")

(cond-expand
      (debug (define (info fmt . args)
               (apply fprintf (current-error-port) fmt args)))
      (else (define info void)))

;; Enable server certificate validation for https URIs.
(define ((make-ssl-server-connector ctx) uri proxy)
  (let ((remote-end (or proxy uri)))
    (if (eq? 'https (uri-scheme remote-end))
        (ssl-connect (uri-host remote-end)
                     (uri-port remote-end)
                     ctx)
        (http:default-server-connector uri proxy))))
(http:server-connector (make-ssl-server-connector (ssl-make-client-context*)))


(define sync-filter (make-parameter #f))

(define (mxid)
  (string-append "@" (account-name) ":" (uri-host (server-uri))))
 
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
          (warning "No context updater for event" evt-type)
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

(define (mxc->url mxc)
  (let ((mxc-uri (uri-reference mxc)))
    (if (eq? (uri-scheme mxc-uri) 'mxc)
        (uri->string
          (update-uri (server-uri)
                      path: `(/ "_matrix" "media" "r0" "download" 
                                ,(uri-host mxc-uri) ,(cadr (uri-path mxc-uri)))))
        "[invalid uri]")))


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
           (sprintf "*** ~a uploaded ~a: ~a" name body (mxc->url (mref '(content url) evt))))
          (else (sprintf "<~a> ~a" name body)))
        (sprintf "<~a> [redacted]" name))
        ))

(define event-printers
  `((m.room.message . ,m.room.message-printer)
    ))


;; Takes a contextualized event and gives a string representation of it
(define (print-event evt ctx)
  (let* ((type (string->symbol (mref '(type) evt)))
         (content (mref '(content) evt))
         (printer (alist-ref type event-printers)))
    (if printer
        (printer evt ctx)
        (sprintf "No event printer for ~a: ~s" type content))))


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

;; When debugging, use a different terminal
#|
(define ttyname (get-environment-variable "TTY"))
(unless ttyname
  (error "Please define the TTY environment variable"))
(define tty-fileno (newterm ttyname)))
|#


(define tty-fileno 0)
(define rows)
(define cols)
(define inputwin)
(define statuswin)

(define (start-interface)
  (initscr)
  (refresh)
  (noecho)
  (cbreak)
  (start_color)
  (set!-values (rows cols) (getmaxyx (stdscr)))

  (set! inputwin (newwin 1 cols (- rows 1) 0))
  (keypad inputwin #t)
  (set! statuswin (newwin 1 cols (- rows 2) 0)))

(define current-room (make-parameter #f))

(define ui-chan (gochan 0))

(define-record room window context)
(define *rooms* '())

(include "tui.scm")