(define (room-command args)
  (cond ((null? args)
         (void))
        ((char=? #\! (string-ref (car args) 0))
         (switch-room (string->symbol (car args))))
        (else
          (switch-room (find-room (string-join args))))))

(define commands
  `((me . ,(lambda (args) (message:emote (current-room) (string-join args " "))))
    (room . ,room-command)
    (r . ,room-command)
    #;(rooms . ,(lambda (args)
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
        #;(status-message (format #f "Unknown command: ~a" cmd)))))

(define (handle-input str)
  (unless (equal? str "")
    (if (char=? (string-ref str 0) #\/)
        (handle-command str)
        (message:text (current-room) str))))

(define input-string "")

(define (refresh-inputwin)
  (wclear inputwin)
  (wprintw inputwin
           (if (>= (string-length input-string) cols)
               (substring input-string (- (string-length input-string) cols -1))
               input-string))
  (wrefresh inputwin))

(define (register-input in)
  (cond ((or (number? in) (and (char? in) (char<? in #\space)))
         (handle-key in))
        ((char? in)
         (set! input-string (string-append input-string (string in)))
         ))
  (refresh-inputwin))

(define (handle-key k)
  (select k
    ((KEY_BACKSPACE)
     (unless (string=? input-string "")
       (set! input-string (substring input-string 0 (sub1 (string-length input-string))))))
    ((KEY_RESIZE)
     (set!-values (rows cols) (getmaxyx (stdscr)))
     (for-each redrawwin (list statuswin inputwin (room-window (alist-ref (current-room) *rooms*)))))
    ((#\newline)
     (handle-input input-string)
     (set! input-string ""))))