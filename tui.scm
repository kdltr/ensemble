(use ansi-escape-sequences posix parley nrepl utf8 fmt miscmacros)

(thread-start! (cut nrepl 6666))

(add-key-binding! #\x3 identity) ;; C-c
(add-key-binding! #\x4 identity) ;; C-d
(add-key-binding! #\xc identity) ;; C-l

(define messages '())

(define (print-message msg)
  (let* ((sender (car msg))
         (name-width (string-length sender))
         (padding (string-append (make-string name-width #\space) " | "))
         (text-width (- columns (+ name-width 3)))
         (text (cdr msg))
         (formated (fmt #f (wrap-lines text)))
         (lines (butlast (string-split formated "\n" #t))))
    (display sender)
    (display " | ")
    (display (car lines))
    (newline)
    (for-each (lambda (l) (display padding) (display l) (newline)) (cdr lines))))

(define-values (rows columns) (terminal-size (current-output-port)))

(print `(columns: ,columns rows: ,rows))

(let loop ()
  (set!-values (rows columns) (terminal-size (current-output-port)))
  (display (erase-display))
  (display (cursor-position 1 1))
  (for-each print-message (reverse messages))
  (display (cursor-position (sub1 rows) 1))
  (display (set-text '(bg-white) (make-string columns #\space)))
  (display (cursor-position rows 1))
  (flush-output)
  (let ((str (parley "> ")))
    (unless (string=? str "/exit")
      (unless (string=? str "")
        (push! `("Kooda" . ,str) messages))
      (loop))))
