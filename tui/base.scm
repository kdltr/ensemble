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

;; TODO Separate info log and errors in two different ports
;; TODO “markup” for events
;; TODO persistent room numbering (irssi-like)
;; TODO special “log” room for backend informations / errors
;; TODO history navigation
;; TODO names tab-completion
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
(define *idling* #f)

(define (idle)
  (info "STARTING IDLE")
  (set! *idling* #t)
  (worker-send worker 'idle))

(define (stop-idle)
  (define (collect resps)
    (let ((exp (gochan-recv *worker-channel*)))
      (if (eqv? exp 'stopped)
          (begin
            (set! *idling* #f)
            (for-each handle-idle-response resps))
          (collect (cons exp resps)))))

  (when *idling*
    (info "STOP IDLE")
    (worker-send worker 'stop)
    (collect '())))

(define (with-idle-stopped thunk)
  (let ((was-idling *idling*))
    (stop-idle)
    (receive vals (thunk)
      (when was-idling (idle))
      (apply values vals))))


(define (worker-read-loop wrk)
  (let ((exp (worker-receive wrk)))
    (gochan-send *worker-channel* exp)
    (unless (eof-object? exp) (worker-read-loop wrk))))

(define (user-read-loop)
  (gochan-send *user-channel* (get-input))
  (user-read-loop))


(define (rpc . args)
  (with-idle-stopped
    (lambda ()
      (info "Sending RPC: ~s" args)
      (apply worker-send worker args)
      (let ((res (gochan-recv *worker-channel*)))
        (info "RECEIVING ~s" res)
        (cond ((eof-object? res)
               (handle-backend-disconnection worker))
              ((not (pair? res))
               (error "Strange RPC response" res))
              ((eqv? (car res) 'error)
               (error "Error from backend" (cadr res)))
              ((eqv? (car res) 'values)
               (info "Got response from RPC: ~s" res)
               (apply values (cdr res)))
              (else
                (error "Strange RPC response" res)))))))

(define current-room (make-parameter #f))

(define *notifications* '())
(define *highlights* '())

(define *rooms-offset* '())


;; DB Replacement
;; ==============

(define (room-display-name id)
  (rpc 'room-display-name id))


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
         (notifs-names (map room-display-name regular-notifs)))
    (werase statuswin)
    (waddstr* statuswin (sprintf "Room: ~a | " (room-display-name (current-room))))
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
  (let ((timeline (rpc 'fetch-events (current-room) rows))
        (read-marker (rpc 'read-marker (current-room))))
    (werase messageswin)
    (for-each
      (lambda (evt)
        (maybe-newline)
        (wprintw messageswin "~A" (alist-ref 'formated evt))
        (when (and read-marker (equal? read-marker (alist-ref 'event_id evt)))
          (maybe-newline)
          (wprintw messageswin "~A" (make-string cols #\-))))
      (reverse timeline))))

(define (maybe-newline)
  (let ((l c (getyx messageswin)))
    (unless (zero? c) (wprintw messageswin "~%"))))

(define (switch-room room-id)
  (if room-id
      (begin
        (current-room room-id)
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
  (current-error-port (open-output-file "frontend.log"))
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
  (unless (rpc 'connect)
    (error "could not connect"))
  (current-room (rpc 'any-room))
  (wprintw messageswin "Starting main loop…~%")
  (wrefresh messageswin)
  (refresh-messageswin)
  (refresh-statuswin)
  (idle)
  (main-loop))

(define (main-loop)
  (wnoutrefresh messageswin)
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
  (gochan-select
    ((*worker-channel* -> msg fail)
     (set! *idling* #f)
     (handle-idle-response msg)
     (idle))
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

(define (handle-defered who datum)
  (info "Running defered: ~s ~s" who datum)
  (case who
    ((input) (handle-input datum))
    ((resize) (resize-terminal))
    (else  (info "Unknown defered procedure: ~a ~s~%" who datum))))

(define (handle-idle-response type)
  (info "Got an idle notification: ~s" type)
  (if (eof-object? exp)
      (handle-backend-disconnection worker)
      (case type
        ((message) (refresh-messageswin))
        ((notifications)
         (set! *notifications* (rpc 'fetch-notifications))
         (refresh-statuswin))
        ((highlights)
         (set! *highlights* (rpc 'fetch-highlights))
         (refresh-statuswin))
        (else (info "unknown idle response: ~a" type))
        )))

(define (handle-backend-disconnection worker)
  (error "Backend disconnected"))


) ;; tui module