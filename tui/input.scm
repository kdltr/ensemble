;; Entry point procedure
(define (handle-input in)
  (cond ((or (number? in) (control-char? in))
         (handle-key in))
        ((char? in)
         (handle-char in)))
  (refresh-inputwin))

(define (control-char? c)
  (or (char<? c #\space)
      (char=? c #\delete)))

(define (refresh-inputwin)
  (wclear inputwin)
  (wprintw inputwin "~A" (buffer-window))
  (wmove inputwin 0 (cursor-position))
  (wrefresh inputwin))



;; Buffer for user input
;; =====================

(define input-string "")
(define cursor-pos 0)
(define extent-start 0)

(define (move-cursor #!optional (n 1))
  (let ((mvmt (case n
                ((left) 0)
                ((right) (string-length input-string))
                (else (max 0 (min (+ cursor-pos n)
                                  (string-length input-string)))))))
    (set! cursor-pos mvmt)
    (move-extent cursor-pos)))

(define (move-extent pos)
  (let ((ok-left (<= extent-start pos))
        (ok-right (> (+ extent-start (sub1 cols)) pos)))
    (cond ((not ok-left)
           (set! extent-start (max (- pos cols) 0)))
          ((not ok-right)
           (set! extent-start
             (- pos (sub1 cols)))))))

(define (buffer-window)
  (substring input-string
             extent-start
             (min (+ extent-start (sub1 cols))
                  (string-length input-string))))

(define (cursor-position)
  (- cursor-pos extent-start))

(define (buffer-insert! c)
  (set! input-string
    (string-append (substring input-string 0 cursor-pos)
                   (string c)
                   (substring input-string cursor-pos))))

(define (buffer-kill!)
  (set! input-string
    (substring input-string 0 cursor-pos)))

(define (handle-char c)
  (buffer-insert! c)
  (move-cursor))



;; Key bindings
;; ============

(define (handle-key k)
  (and-let* ((proc (alist-ref k keys))) (proc)))

(define keys '())
(define-syntax define-key (syntax-rules () ((_ k . body) (push! (cons k (lambda () . body)) keys))))

(define-key KEY_BACKSPACE
  (unless (string=? input-string "")
    (set! input-string (substring input-string 0 (sub1 (string-length input-string))))
    (move-cursor -1)))

(define-key KEY_RESIZE
  (set!-values (rows cols) (getmaxyx (stdscr)))
  (for-each redrawwin (list statuswin inputwin (room-window (alist-ref (current-room) *rooms*)))))

(define-key KEY_LEFT
  (move-cursor -1))

(define-key KEY_RIGHT
  (move-cursor 1))

(define-key #\x01 ;; C-a
  (move-cursor 'left))

(define-key #\x05 ;; C-e
  (move-cursor 'right))

(define-key #\x0B ;; C-k
  (buffer-kill!))

(define-key #\newline
  (unless (equal? input-string "")
    (if (char=? (string-ref input-string 0) #\/)
        (handle-command input-string)
        (defer 'message message:text (current-room) input-string))
    (set! input-string "")
    (move-cursor 'left)))



;; Commands
;; ========

(define (handle-command str)
  (let* ((cmdline (string-split (string-drop str 1) " "))
         (cmd (string->symbol (car cmdline)))
         (args (cdr cmdline))
         (proc (alist-ref cmd commands)))
    (if proc
        (proc args)
        #;(status-message (format #f "Unknown command: ~a" cmd)))))

(define commands '())
(define-syntax define-command
  (syntax-rules ()
    ((_ (sym ...) arg . body) (let ((proc (lambda (arg) . body)))
                            (push! (cons 'sym proc) commands)
                            ...))
    ((_ sym arg . body) (define-command (sym) arg . body))))


(define-command (room r) args
  (cond ((null? args)
         (void))
        ((char=? #\! (string-ref (car args) 0))
         (switch-room (string->symbol (car args))))
        (else
          (switch-room (find-room (string-join args))))))

(define-command me args
  (defer 'message message:emote (current-room) (string-join args " ")))

(define-command exit args
  (save-config)
  (exit))
