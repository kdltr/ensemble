(module tui (run)
(import
  (except scheme
          string-length string-ref string-set! make-string string substring
          string->list list->string string-fill! write-char read-char display)
  (except chicken
          reverse-list->string print print*)
  (except data-structures
          ->string conc string-chop string-split string-translate
          substring=? substring-ci=? substring-index substring-index-ci)
  posix data-structures irregex srfi-18 miscmacros extras
  concurrency debug locations)
(use srfi-1 ioctl ncurses utf8 utf8-srfi-13 utf8-srfi-14 unicode-char-sets files
     srfi-71 gochan)

;; TODO “markup” for events
;; TODO persistent room numbering (irssi-like)
;; TODO special “log” room for backend informations / errors
;; TODO history navigation
;; TODO support for multiple profiles/backends

(include "tui/input.scm")

(define tty-fileno 0)
(define rows)
(define cols)
(define inputwin)
(define statuswin)
(define messageswin)

(define worker)
(define *user-channel* (gochan 0))
(define *worker-channel* (gochan 0))

(define (worker-read-loop wrk)
  (let ((exp (worker-receive wrk)))
    (gochan-send *worker-channel* exp)
    (unless (eof-object? exp) (worker-read-loop wrk))))

(define (user-read-loop)
  (gochan-send *user-channel* (get-input))
  (user-read-loop))

(define (ipc-send . args)
  (info "Sending IPC: ~s" args)
  (worker-send worker args))


(define *query-number* 0)
(define *queries* '())

(define (ipc-query . args)
  (apply ipc-send 'query (inc! *query-number*) args)
  (call/cc
    (lambda (k)
      (push! (cons *query-number* k)
             *queries*)
      (main-loop))))

(define (handle-query-response id datum)
  (info "Queries waiting: ~a" (length *queries*))
  (and-let* ((pair (assoc id *queries*))
             (task (cdr pair)))
       (set! *queries* (delete! pair *queries*))
       (task datum)))

(define current-room (make-parameter #f))

(define *notifications* '())
(define *highlights* '())

(define *rooms-offset* '())


;; DB Replacement
;; ==============

(define (room-display-name id)
  (ipc-query 'room-display-name id))


;; TUI
;; ===

(define (start-interface)
  ;; Make ncurses wait less time when receiving an ESC character
  (setenv "ESCDELAY" "20")

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
  (wprintw messageswin "Loading…~%")
  (wrefresh messageswin)
  )

(define (waddstr* win str)
  (handle-exceptions exn #t
    (waddstr win str)))

(define (refresh-statuswin)
  (let* ((regular-notifs (lset-difference eqv? *notifications* *highlights*))
         (highlights-names (map room-display-name *highlights*))
         (notifs-names (map room-display-name regular-notifs))
         (room-name (room-display-name (current-room))))
    (werase statuswin)
    (waddstr* statuswin (sprintf "Room: ~a | " room-name))
    (wcolor_set statuswin 2 #f) ;; highlight foreground
    (waddstr* statuswin (sprintf "~a" (string-join highlights-names " " 'suffix)))
    (wcolor_set statuswin 1 #f) ;; regular foreground
    (waddstr* statuswin (sprintf "~a" (string-join notifs-names " ")))))

#;(define (room-offset room-id)
  (alist-ref room-id *rooms-offset* equal? (branch-last-sequence-number room-id)))

#;(define (room-offset-set! room-id offset)
  (set! *rooms-offset*
    (alist-update! room-id offset *rooms-offset* equal?)))

#;(define (room-offset-delete! room-id)
  (set! *rooms-offset*
    (alist-delete! room-id *rooms-offset* equal?)))

(define (refresh-messageswin)
  (ipc-send 'fetch-events (current-room) rows))

(define (maybe-newline)
  (let ((l c (getyx messageswin)))
    (unless (zero? c) (wprintw messageswin "~%"))))

(define (switch-room room-id)
  (if room-id
      (begin
        (when (current-room)
          (ipc-send 'unsubscribe (current-room)))
        (current-room room-id)
        (ipc-send 'subscribe (current-room))
        (refresh-statuswin)
        (refresh-messageswin)
        )
      #f))


(define (run)
  (set! (signal-handler signal/winch)
    (lambda (_)
      (thread-start!
        (lambda ()
          (gochan-send *user-channel* 'resize)))))
  (set! (signal-handler signal/int)
    (lambda (_) (reset)))
  (info-port (open-output-file "frontend.log"))
  (start-interface)
  (wprintw messageswin "Starting backend…~%")
  (wrefresh messageswin)
  (set! worker
    (start-worker 'default
      (lambda ()
        (let ((profile-dir (make-pathname (config-home) "default"))
              (src-dir (current-directory)))
          (create-directory profile-dir #t)
          (change-directory profile-dir)
          (process-execute (make-pathname src-dir "backend"))))))
  (thread-start! user-read-loop)
  (thread-start! (lambda () (worker-read-loop worker)))
  (wprintw messageswin "Connecting…~%")
  (wrefresh messageswin)
  (ipc-send 'connect)
  (switch-room (ipc-query 'any-room))
  (main-loop))

(define (main-loop)
  (info "INTERFACE REFRESH")
  (wnoutrefresh messageswin)
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
  (gochan-select
    ((*worker-channel* -> msg fail)
     (handle-backend-response msg))
    ((*user-channel* -> msg fail)
     (if (eqv? msg 'resize)
         (resize-terminal)
         (handle-input msg))))
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
    (refresh-messageswin)
    (refresh-statuswin)
    (refresh-inputwin)))

(define (handle-backend-response msg)
  (info "Recvd from backend: ~s" msg)
  (if (eof-object? msg)
      (handle-backend-disconnection worker)
      (case (car msg)
        ((bundle-start)
         (for-each
           handle-backend-response
           (collect-bundle-messages)))
        ((notifications)
         (set! *highlights* (cadr msg))
         (set! *notifications* (caddr msg))
         (refresh-statuswin))
        ((clear)
         (when (equal? (cadr msg) (current-room))
           (werase messageswin)))
        ((refresh)
         (when (equal? (cadr msg) (current-room))
           (refresh-messageswin)))
        ((response)
         (apply handle-query-response (cdr msg)))
        ((message)
         (when (equal? (cadr msg) (current-room))
           (maybe-newline)
           (wprintw messageswin "~A"
                    (alist-ref 'formated (caddr msg)))))
        (else (info "Unknown message from backend: ~a" msg))
        )))

(define (handle-backend-disconnection worker)
  (error "Backend disconnected"))

(define (collect-bundle-messages)
  (let ((msg (gochan-recv *worker-channel*)))
    (if (equal? msg '(bundle-end))
        '()
        (cons msg (collect-bundle-messages)))))

) ;; tui module
