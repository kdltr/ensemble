;; TODO “markup” for events
;; TODO better history navigation
;; TODO support for multiple profiles/backends
;; TODO (code quality) exceptions that write messages to the “ensemble” windows
;; TODO title bar for room name and topic
;; TODO dynamic status bar (scrolling, outside highlight indicators)
;; TODO remove windows for left rooms

(module (ensemble interface console) (run)
(import
  scheme
  (chicken base)
  (chicken condition)
  (chicken file)
  (chicken format)
  (chicken irregex)
  (chicken pathname)
  (chicken plist)
  (chicken port)
  (chicken process-context)
  (chicken process)
  (chicken process signal)
  (chicken repl)
  (chicken sort)
  (chicken syntax)
  (chicken type)
  srfi-1
  srfi-18
  srfi-69
  srfi-71
  gochan
  utf8
  utf8-srfi-13
  utf8-srfi-14
  unicode-char-sets
  ioctl
  ncurses
  miscmacros
  (ensemble libs bindings)
  (ensemble libs concurrency)
  (ensemble libs debug)
  (ensemble libs locations))

(include-relative "input.scm")

(define +backend-executable+ "ensemble.backend.matrix")

(define tty-fileno 0)
(define *locale* "")
(define rows)
(define cols)
(define inputwin)
(define statuswin)
(define messageswin)

(define worker)
(define *user-channel* (gochan 0))
(define *resize-channel* (gochan 0))

(define (user-read-loop)
  (gochan-send *user-channel* (get-input))
  (user-read-loop))

(begin-for-syntax
  (define *ipc-receiver* 'frontend)
  (define *ipc-send-procedure* 'ipc-send)
  (define *ipc-hash-table* '*ipc-procedures*))
(include-relative "../../ipc.scm")

(define *ipc-procedures* (make-hash-table))

(define (ipc-send exp)
  (info "Sending IPC: ~s" exp)
  (gochan-send (worker-outgoing worker) exp))

(define *read-marker* #f)

(define *rooms-offset* '())


;; Windows
;; =======

;; each window must have:
;; - an associated room id
;; - an associated worker/backend/profile
;; - a text / ID
;; - a notification and highlight count

;; except for a special frontend window and special backend/profile windows

(define *current-window* 'ensemble)
(define *special-windows* '(ensemble backend))
(define *room-windows* '())
(define *free-window-number* 0)


(define *window-associations* '())

(define (set-window-association! profile room window)
  (let ((profile-assoc (alist-ref profile *window-associations* equal? '())))
    (set! *window-associations*
      (alist-update! profile
                     (alist-update! room window profile-assoc equal?)
                     *window-associations*
                     equal?))))

(define (window-association profile room)
  (alist-ref room
             (alist-ref profile *window-associations* equal? '())
             equal?
             #f))


;; TODO remove
(define (current-room)
  (window-room *current-window*))

(define (window<? w1 w2)
  (let* ((str1 (symbol->string w1))
         (str2 (symbol->string w2))
         (num1 (string->number str1))
         (num2 (string->number str2)))
    (cond ((and num1 num2)  (< num1 num2))
          (num1 #f)
          (num2 #t)
          (else  (string<? str1 str2)))))

(define (current-window-name)
  (if (special-window? *current-window*)
      (symbol->string *current-window*)
      (get (current-room) 'name)))

(define (special-window-log win)
  (or (get win 'log) '()))

(define (special-window-write id fmt . args)
  (assert (special-window? id))
  (let* ((str (sprintf "~?" fmt args)))
    (put! id 'log (cons str (special-window-log id)))
    (put! id 'notifications (add1 (or (get id 'notifications) 0)))
    (when (eqv? id *current-window*)
      (maybe-newline)
      (wprintw messageswin "~a" str)))
  (refresh-statuswin))

(define (special-window? id)
  (memv id *special-windows*))

(define (room-window? id)
  (memv id *room-windows*))

(define (window-has-highlight? win)
  (let ((hls notifs (window-notifications win)))
    (not (zero? hls))))

(define (window-has-notification? win)
  (let ((hls notifs (window-notifications win)))
    (not (zero? notifs))))

(define (window-room win)
  (get win 'room-id))

(define (window-for-room room-id)
  (find (lambda (w) (equal? room-id (window-room w)))
        *room-windows*))

(define (window-profile win)
  (get win 'profile))

(define (window-notifications win)
  (values (or (get win 'highlights) 0)
          (or (get win 'notifications) 0)))

(define (add-room-window room-id)
  (define (do-add)
    (let* ((num-id (string->symbol (->string (add1 *free-window-number*))))
           (window (or (window-association "default" room-id)
                       num-id)))
      (when (eqv? window num-id)
        (set! *free-window-number* (add1 *free-window-number*)))
      (set-window-association! "default" room-id window)
      (put! window 'room-id room-id)
      (put! window 'profile "default") ;; TODO change that when multiple profiles are there
      (set! *room-windows*
        (merge! *room-windows* (list window) window<?))
      window))
  (or (window-for-room room-id) (do-add)))

(define (rename-window from to)
  (let ((plist (symbol-plist from)))
    (cond ((special-window? from)
           (special-window-write 'ensemble
                                 "You can’t rename the special window: ~a"
                                 from))
          (else
            (let ((room-id (window-room from)))
              (set-window-association! "default" room-id to))
            (set! (symbol-plist to) plist)
            (set! (symbol-plist from) '())
            (set! *room-windows*
              (merge! (delete! from *room-windows*)
                      (list to)
                      window<?))
            (let ((maybe-num (string->number (symbol->string to))))
              (when maybe-num
                (set! *free-window-number*
                  (max *free-window-number* (add1 maybe-num)))))
            (switch-window to)))))

(define (switch-window id)
  (cond ((special-window? id)
         (switch-special-window id))
        (else
          (switch-room-window id))))

(define (switch-special-window id)
  (set! *current-window* id)
  (refresh-statuswin)
  (refresh-current-window))

(define (switch-room-window id)
  (let ((current-room-id (window-room *current-window*))
        (room-id (window-room id)))
    (cond (room-id
           (when current-room-id
             (ipc:unsubscribe current-room-id))
           (set! *current-window* id)
           (ipc:subscribe room-id)
           (refresh-statuswin)
           (refresh-current-window))
          (else
            (special-window-write 'ensemble "No such room exist: ~a" id)))))

(define (refresh-current-window)
  (cond ((special-window? *current-window*)
         (refresh-special-window *current-window*))
        (else
          (refresh-room-window *current-window*))))

(define (refresh-room-window id)
  (let ((room-id (window-room id)))
    (ipc:fetch-events room-id rows
            (room-offset room-id))))

(define (refresh-special-window id)
  (wclear messageswin)
  (for-each
    (lambda (str)
      (maybe-newline)
      (wprintw messageswin "~a" str))
    (reverse (special-window-log id))))

(define (load-windows-associations)
  (let ((path (make-pathname (config-home) "window-associations")))
    (when (file-exists? path)
      (with-input-from-file path
        (lambda ()
          (port-for-each
            (lambda (exp)
              (case (car exp)
                ((associations)
                 (set! *window-associations* (cadr exp)))
                ((free-window-number)
                 (set! *free-window-number* (cadr exp)))
                (else
                  (special-window-write 'ensemble "Unknown config expression: ~s"
                                        exp))))
             read))))))

(define (save-windows-associations)
  (with-output-to-file (make-pathname (config-home) "window-associations")
    (lambda ()
      (write (list 'associations *window-associations*))
      (write (list 'free-window-number *free-window-number*)))))



;; TUI
;; ===

(define STATUS_HIGHLIGHT_PAIR 1)
(define MESSAGE_HIGHLIGHT_PAIR 2)

(define MESSAGE_LOWLIGHT_ATTRS)

(define STATUS_NORMAL_ATTRS)
(define STATUS_HIGHLIGHT_ATTRS)
(define STATUS_LOWLIGHT_ATTRS)

(define (initialize-attributes)
  (start_color)
  (use_default_colors)
  (init_pair STATUS_HIGHLIGHT_PAIR -1 COLOR_BLUE)
  (init_pair MESSAGE_HIGHLIGHT_PAIR COLOR_CYAN -1)
  (set! MESSAGE_LOWLIGHT_ATTRS A_UNDERLINE)
  (set! STATUS_NORMAL_ATTRS 0)
  (set! STATUS_HIGHLIGHT_ATTRS
    (COLOR_PAIR STATUS_HIGHLIGHT_PAIR))
  (set! STATUS_LOWLIGHT_ATTRS A_DIM))

(define (start-interface)
  ;; Make ncurses wait less time when receiving an ESC character
  (set! *locale* (setlocale ""))
  (set-environment-variable! "ESCDELAY" "20")
  (initscr)
  (noecho)
  (cbreak)
  (initialize-attributes)
  (set!-values (rows cols) (getmaxyx (stdscr)))
  (set! messageswin (newwin (- rows 2) cols 0 0))
  (scrollok messageswin #t)
  (idlok messageswin #t)
  (set! inputwin (newwin 1 cols (- rows 1) 0))
  (keypad inputwin #t)
  (set! statuswin (newwin 1 cols (- rows 2) 0))
  (wbkgdset statuswin A_REVERSE)
  (special-window-write 'ensemble "Loading…"))

;; FIXME less hacky solution
(define (run-login-prompt)
  (define (safe-key? p)
    (member (car p)
            (list KEY_BACKSPACE
                  KEY_LEFT #\x02 ;; C-b
                  KEY_RIGHT #\x06 ;; C-f
                  KEY_HOME #\x01 ;; C-a
                  #\x04 ;; C-d
                  KEY_END #\x05 ;; c-e
                  #\x0B ;; C-k
                  #\x0C ;; C-l
                  #\escape
                  #(#\escape #\b)
                  #(#\escape #\f)
                  #(#\escape #\d)
                  #\x17 ;; C-w
                  )))
  (define (fake-statuswin msg)
    (lambda ()
      (werase statuswin)
      (wattron statuswin STATUS_HIGHLIGHT_ATTRS)
      (waddstr* statuswin msg)
      (wattroff statuswin STATUS_HIGHLIGHT_ATTRS)))
  (define (refresh-hidden-inputwin)
    (werase inputwin))
  (define ((newline-input cont))
    (let ((input input-string))
      (set! input-string  "")
      (move-cursor 'left)
      (cont input)))
  (define (prompt msg #!key (password #f))
    (set! refresh-statuswin (fake-statuswin msg))
    (when password (set! refresh-inputwin refresh-hidden-inputwin))
    (refresh-statuswin)
    (refresh-inputwin)
    (call/cc
      (lambda (k)
        (push! (cons #\newline (newline-input k)) keys)
        (main-loop))))

  (set! input-string "")
  (move-cursor 'left)
  (let* ((safe-keys (filter safe-key? keys))
         (old-keys keys)
         (old-refresh-inputwin refresh-inputwin)
         (old-refresh-statuswin refresh-statuswin)
         ((values) (set! keys safe-keys))
         (server (prompt "Enter server URL (example: https://matrix.org)"))
         (username (prompt "Enter username"))
         (password (prompt "Enter password (not shown)" password: #t)))
    (set! keys old-keys)
    (set! refresh-inputwin old-refresh-inputwin)
    (set! refresh-statuswin old-refresh-statuswin)
    (refresh-statuswin)
    (refresh-inputwin)
    (values server username password)))

(define (waddstr* win str)
  (handle-exceptions exn #t
    (waddstr win str)))

(define (refresh-statuswin)
  (werase statuswin)
  (waddstr* statuswin (sprintf "Room: ~a | " (current-window-name)))
  (for-each
    (lambda (win)
      (let* ((hls notifs (window-notifications win))
             (fmt (if (and (zero? hls) (zero? notifs))
                      "~a"
                      "~a(~a:~a)"))
             (args (if (and (zero? hls) (zero? notifs))
                       (list win)
                       (list win
                             (if (zero? hls) "" hls)
                             (if (zero? notifs) "" notifs))))
             (attrs (cond ((> hls 0) STATUS_HIGHLIGHT_ATTRS)
                          ((> notifs 0) STATUS_NORMAL_ATTRS)
                          (else STATUS_LOWLIGHT_ATTRS))))
        (when (or (> hls 0)
                  (> notifs 0)
                  (special-window? win)
                  (eqv? win *current-window*))
          (wattrset statuswin attrs)
          (waddstr* statuswin
                    (if (eqv? win *current-window*)
                        (sprintf "[~?] " fmt args)
                        (sprintf "~? " fmt args)))
          (wattrset statuswin 0))))
    (append *special-windows* *room-windows*)))

(define (room-offset room-id)
  (alist-ref room-id *rooms-offset* equal? 0))

(define (room-offset-set! room-id offset)
  (set! *rooms-offset*
    (alist-update! room-id offset *rooms-offset* equal?)))

(define (room-offset-delete! room-id)
  (set! *rooms-offset*
    (alist-delete! room-id *rooms-offset* equal?)))

(define (maybe-newline)
  (let ((l c (getyx messageswin)))
    (unless (zero? c) (wprintw messageswin "~%"))))



(define (run)
  (set! (signal-handler signal/winch)
    (lambda (_)
      (thread-start!
        (lambda ()
          (gochan-send *user-channel* 'resize)))))
  (set! (signal-handler signal/int)
    (lambda (_) (reset)))
  (cond-expand (debug (info-port (open-output-file "frontend.log"))) (else))
  (start-interface)
  (unless (irregex-search '(w/nocase "utf" (? "-") "8") *locale*)
    (special-window-write 'ensemble
      "Warning: you don't seem to be using an UTF-8 locale"))
  (special-window-write 'ensemble "Loading config…")
  (load-config)
  (on-exit save-config)
  (special-window-write 'ensemble "Starting backend…")
  (set! worker
    (start-worker 'default
      (lambda ()
        (or (process-execute* (make-pathname (current-directory)
                                             +backend-executable+)
                              (cons "default" (cdr (argv))))
            (process-execute* +backend-executable+
                              (cons "default" (cdr (argv))))))))
  (thread-start! user-read-loop)
  (main-loop))

(define (load-config)
  ;; TODO validate each config file
  (load-windows-associations))

(define (save-config)
  (save-windows-associations))

(define (process-execute* exec args)
  (handle-exceptions exn #f
    (process-execute exec args)))

(define (main-loop)
  (info "INTERFACE REFRESH")
  (wnoutrefresh messageswin)
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
  (let ((msg fail meta
         (gochan-select*
           `((,(worker-incomming worker) worker)
             (,*user-channel* user)
             (,*resize-channel* resize)))))
    (if fail
        (error "select failed" (list msg fail meta))
    (case meta
      ((worker) (handle-backend-response msg))
      ((user) (if (eqv? msg 'resize)
                  (set! *resize-channel* (gochan-after 25))
                  (handle-input msg)))
      ((resize) (resize-terminal))
      (else (error "unknown channel sent a message" (list msg fail meta))))))
  (main-loop))

(define (get-input)
  (thread-wait-for-i/o! tty-fileno #:input)
  (wget_wch inputwin))

(define (resize-terminal)
  (let ((rows+cols (ioctl-winsize tty-fileno)))
    (set! rows (car rows+cols))
    (set! cols (cadr rows+cols))
    (move-extent cursor-pos) ;; input line cursor
    (resizeterm rows cols)
    (wresize messageswin (- rows 2) cols)
    (mvwin messageswin 0 0)
    (wresize statuswin 1 cols)
    (mvwin statuswin (- rows 2) 0)
    (wresize inputwin 1 cols)
    (mvwin inputwin (- rows 1) 0)
    (refresh-current-window)
    (refresh-statuswin)
    (refresh-inputwin)))

(define (handle-backend-response msg)
  (info "Recvd from backend: ~s" msg)
  (if (eof-object? msg)
      (handle-backend-disconnection worker)
      (let ((procedure (hash-table-ref/default *ipc-procedures* (car msg) #f)))
        (if procedure
            (apply procedure (cdr msg))
            (info "unknown message from backend: ~a" msg)))))

(define-ipc-implementation (bundle-start)
  (for-each
    handle-backend-response
    (collect-bundle-messages)))

(define-ipc-implementation (notifications room-id new-hls new-notifs)
  (let* ((window (add-room-window room-id))
         (old-hls _ (window-notifications window)))
    (when (> new-hls old-hls)
      (beep))
    (put! window 'highlights new-hls)
    (put! window 'notifications new-notifs)
    (refresh-statuswin)))

(define-ipc-implementation (clear room-id)
  (when (equal? room-id (current-room))
    (werase messageswin)))

(define-ipc-implementation (refresh room-id)
  (when (equal? room-id (window-room *current-window*))
    (refresh-current-window)))

(define-ipc-implementation (read-marker room-id event-id)
  (when (equal? room-id (current-room))
    (set! *read-marker* (symbol->string event-id))
    (refresh-current-window)))

(define-ipc-implementation (room-name room-id room-name)
  (put! room-id 'name room-name)
  (refresh-statuswin))

(define-ipc-implementation (room-members room-id members)
  (put! room-id 'members members))

(define-ipc-implementation (message room-id message)
  (when (equal? room-id (current-room))
    (maybe-newline)
    (when (alist-ref 'highlight message)
      (wcolor_set messageswin MESSAGE_HIGHLIGHT_PAIR #f))
    (when (alist-ref 'lowlight message)
      (wattrset messageswin MESSAGE_LOWLIGHT_ATTRS))
    (wprintw messageswin "~A"
             (alist-ref 'formated message))
    (wcolor_set messageswin 0 #f)
    (wattrset messageswin 0)
    (when (equal? (alist-ref 'event_id message)
                  *read-marker*)
      (wprintw messageswin "~A" (make-string cols #\-)))))

(define-ipc-implementation (info message)
  (special-window-write 'backend "~a" message))


(define (handle-backend-disconnection worker)
  (error "Backend disconnected"))

(define (collect-bundle-messages)
  (let ((msg (gochan-recv (worker-incomming worker))))
    (if (equal? msg '(bundle-end))
        '()
        (cons msg (collect-bundle-messages)))))

) ;; tui module

