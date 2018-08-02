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
     srfi-71)

;; TODO Separate info log and errors in two different ports
;; FIXME Input get scrambled when typing fast / pasting text

(include "tui/input.scm")

(define tty-fileno 0)
(define rows)
(define cols)
(define inputwin)
(define statuswin)
(define messageswin)

(define worker)
(define *stored-defered* '())

(define (rpc . args)
  (info "Sending RPC: ~s" args)
  (apply worker-send worker args)
  (let lp ((res (worker-receive worker)))
    (info "RECEIVING ~s" res)
    (cond ((eof-object? res)
           (error "backend stopped"))
          ((eqv? res 'stopped)
           (lp (worker-receive worker)))
          ((not (pair? res))
           (error "Strange RPC response" res))
          ((eqv? (car res) 'error)
           (error "Error from backend" (cadr res)))
          ((eqv? (car res) 'values)
            (info "Got response from RPC: ~s" res)
            (apply values (cdr res)))
          (else
            (error "Strange RPC response" res))
    )))

(define current-room (make-parameter #f))

(define *notifications* '())
(define *highlights* '())

(define *rooms-offset* '())


;; DB Replacement
;; ==============

(define (room-display-name id)
  (rpc 'room-display-name id))

;; Helper procedures
;; =================

(define (json-false? o)
  (or (equal? o #f)
      (equal? o 'null)
      (equal? o "")
      (equal? o 0)))

(define (json-true? o)
  (and (not (json-false? o)) o))

(define (mref keys alist)
  (if (null? keys)
      alist
      (and-let* ((o (alist-ref (car keys) alist equal?)))
           (mref (cdr keys) o))))

(define (mupdate keys val alist)
  (if (null? (cdr keys))
      (alist-update (car keys) val alist equal?)
      (alist-update (car keys)
                    (mupdate (cdr keys) val (or (alist-ref (car keys) alist) '()))
                    alist
                    equal?)))

(define (mdelete keys alist)
  (if (null? (cdr keys))
      (alist-delete (car keys) alist equal?)
      (alist-update (car keys)
                    (mdelete (cdr keys) (or (alist-ref (car keys) alist) '()))
                    alist
                    equal?)))




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
        (wprintw messageswin "~A" (mref '(formated) evt))
        (when (and read-marker (equal? read-marker (mref '(event_id) evt)))
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
    (lambda (_) (defer 'resize (lambda () #t))))
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
  (wprintw messageswin "Connecting…~%")
  (wrefresh messageswin)
  (unless (rpc 'connect)
    (error "could not connect"))
  (current-room (rpc 'any-room))
  (wprintw messageswin "Starting main loop…~%")
  (wrefresh messageswin)
  (refresh-messageswin)
  (refresh-statuswin)
  (defer 'input get-input)
  (defer 'idle (lambda () (worker-receive worker)))
  (main-loop))

(define (main-loop)
  (info "MAIN LOOP")
  (wnoutrefresh messageswin)
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
  (let* ((th (receive-defered))
         (who datum (thread-join-protected! th)))
    (info "DEFERED: ~s ~s" who datum)
    (case who
      ((idle)
       (handle-idle-response datum)
       (worker-send worker 'idle)
       (defer 'idle (lambda () (worker-receive worker))))
      ((input)
       (worker-send worker 'stop)
       (push! (list who datum) *stored-defered*)
       (defer 'input get-input))
      (else
        (worker-send worker 'stop)
        (push! (list who datum) *stored-defered*))
      ))
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
    ;; FIXME input freezes at some point
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
        ((stopped)
         (for-each
           (lambda (args) (apply handle-defered args))
           *stored-defered*)
         (set! *stored-defered* '()))
        (else (info "unknown idle response: ~a" type))
        )))

(define (handle-backend-disconnection worker)
  (exit))


) ;; tui module