(define input-string "") ;; buffer for user input


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
  (wprintw inputwin
           (if (>= (string-length input-string) cols)
               (substring input-string (- (string-length input-string) cols -1))
               input-string))
  (wrefresh inputwin))


(define (handle-char c)
  (set! input-string (string-append input-string (string c))))



;; Key bindings
;; ============

(define (handle-key k)
  (and-let* ((proc (alist-ref k keys))) (proc)))

(define keys '())
(define-syntax define-key (syntax-rules () ((_ k . body) (push! (cons k (lambda () . body)) keys))))

(define-key KEY_BACKSPACE
  (unless (string=? input-string "")
    (set! input-string (substring input-string 0 (sub1 (string-length input-string))))))

(define-key KEY_RESIZE
  (set!-values (rows cols) (getmaxyx (stdscr)))
  (for-each redrawwin (list statuswin inputwin (room-window (alist-ref (current-room) *rooms*)))))

(define-key #\newline
  (unless (equal? input-string "")
    (if (char=? (string-ref input-string 0) #\/)
        (handle-command input-string)
        (message:text (current-room) input-string))
    (set! input-string "")))



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
  (message:emote (current-room) (string-join args " ")))

(define-command exit args
  (save-config)
  (exit))
