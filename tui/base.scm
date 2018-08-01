(module tui *
(import
  (except scheme
          string-length string-ref string-set! make-string string substring
          string->list list->string string-fill! write-char read-char display)
  (except chicken
          reverse-list->string print print*)
  (except data-structures
          ->string conc string-chop string-split string-translate
          substring=? substring-ci=? substring-index substring-index-ci)
  srfi-1 posix data-structures irregex srfi-18 miscmacros extras
  concurrency debug backend)
(use ioctl ncurses utf8 utf8-srfi-13 utf8-srfi-14 unicode-char-sets
     sandbox)

(include "tui/input.scm")

(define tty-fileno 0)
(define rows)
(define cols)
(define inputwin)
(define statuswin)
(define messageswin)

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
  (wprintw messageswin "Loading…")
  (wrefresh messageswin)
  )

(define current-room (make-parameter #f))

(define *notifications* '())
(define *highlights* '())

(define (waddstr* win str)
  (handle-exceptions exn #t
    (waddstr win str)))

(define (refresh-statuswin)
  (let* ((regular-notifs (lset-difference eq? *notifications* *highlights*))
         (highlights-names (map room-display-name *highlights*))
         (notifs-names (map room-display-name regular-notifs)))
    (werase statuswin)
    (waddstr* statuswin (sprintf "Room: ~a | " (room-display-name (current-room))))
    (wcolor_set statuswin 2 #f) ;; highlight foreground
    (waddstr* statuswin (sprintf "~a" (string-join highlights-names " " 'suffix)))
    (wcolor_set statuswin 1 #f) ;; regular foreground
    (waddstr* statuswin (sprintf "~a" (string-join notifs-names " ")))))

(define *rooms-offset* '())

#;(define (room-offset room-id)
  (alist-ref room-id *rooms-offset* equal? (branch-last-sequence-number room-id)))

(define (room-offset-set! room-id offset)
  (set! *rooms-offset*
    (alist-update! room-id offset *rooms-offset* equal?)))

(define (room-offset-delete! room-id)
  (set! *rooms-offset*
    (alist-delete! room-id *rooms-offset* equal?)))

(define (refresh-messageswin)
  (let ((timeline (room-timeline (current-room)
                                 limit: rows))
        (read-marker (read-marker-ref (current-room))))
    (werase messageswin)
    (for-each
      (lambda (evt)
        ;; Visible holes are dynamically loaded
        (when (equal? (mref '(type) evt)
                      "com.upyum.ensemble.hole")
          (info "[hole-detected] ~a ~s~%" (current-room) evt)
          (when (not (member evt *requested-holes*))
            (set! *requested-holes* (cons evt *requested-holes*))
            (defer 'hole-messages request-hole-messages
                   (current-room) evt rows)))
        (maybe-newline)
        (wprintw messageswin "~A" (mref '(formated) evt))
        (when (and read-marker (equal? read-marker (mref '(event_id) evt)))
          (maybe-newline)
          (wprintw messageswin "~A" (make-string cols #\-))))
      (reverse timeline))))

(define (maybe-newline)
  (let-values (((l c) (getyx messageswin)))
    (unless (zero? c) (wprintw messageswin "~%"))))

(define (switch-room room-id)
  (let ((room (room-exists? room-id)))
    (if room
        (begin
          (current-room room-id)
          (set! *notifications* (delete! room-id *notifications* eq?))
          (set! *highlights* (delete! room-id *highlights* eq?))
          (refresh-statuswin)
          (refresh-messageswin)
          )
        #f)))

(define (find-room regex)
  (define (searched-string ctx)
    (or (room-name ctx)
        (json-true? (mref '(("" . m.room.canonical_alias) alias) ctx))
        (and-let* ((v (json-true? (mref '(("" . m.room.aliases) aliases) ctx))))
             (vector-ref v 0))
        (string-join
         (filter-map (lambda (p)
                       (and (equal? (cdar p) 'm.room.member)
                            (or (member-displayname (caar p) ctx)
                                (caar p))))
                     ctx))
        ""))
  (find (lambda (room-id)
          (irregex-search (irregex regex 'i)
                          (searched-string (room-context room-id))))
        (joined-rooms)))

(define (startup)
  (set! (signal-handler signal/winch)
    (lambda (_) (defer 'resize (lambda () #t))))
  (set! (signal-handler signal/int)
    (lambda (_) (reset)))
  (start-interface)
  (let* ((first-batch (sync since: (config-ref 'next-batch)))
         (next (handle-sync first-batch)))
    (switch-room (any-room))
    (defer 'sync sync timeout: 30000 since: next)
    (defer 'input get-input)
    (refresh-statuswin)
    (main-loop)))

(define (main-loop)
  (wnoutrefresh messageswin)
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
  (let ((th (receive-defered)))
    (receive (who datum) (thread-join-protected! th)
      (case who
        ((input) (handle-input datum) (defer 'input get-input))
        ((rpc) (handle-rpc (car datum)) (defer 'rpc worker-receive (cadr datum)))
        ((resize) (resize-terminal))
        (else  (info "Unknown defered procedure: ~a ~s~%" who datum))
         ))
      )
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

(define (handle-rpc exp)
  (safe-eval exp))

) ;; tui module