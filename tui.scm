(use ansi-escape-sequences posix parley nrepl utf8 fmt miscmacros)

(thread-start! (cut nrepl 6666))

(add-key-binding! #\x3 identity) ;; C-c
(add-key-binding! #\x4 identity) ;; C-d
(add-key-binding! #\xc identity) ;; C-l

;; TODO this is absolutely wrong
(define (take-most-events rows events)
  (if (or (null? events) (<= rows 0))
      '()
      (let* ((evt (car events))
             (str (print-event evt))
             (lines (add1 (/ (string-length str) columns))))
        (cons str (take-most-events (- rows lines) (cdr events))))))

(define (print-events events)
  (for-each print (reverse (take-most-events (- rows 3) events))))

(define-values (rows columns) (terminal-size (current-output-port)))

(set! (signal-handler signal/winch)
  (lambda (_)
    (set!-values (rows columns) (terminal-size (current-output-port)))
    (refresh!)))

(define status-message (make-parameter ""))

(define (refresh!)
  (display (erase-display))
  (display (cursor-position 1 1))
  (print-events (alist-ref (current-room) *timelines*))
  (display (cursor-position (sub1 rows) 1))
  (display (set-text '(bg-white fg-black) (string-pad-right (status-message) columns)))
  (display (cursor-position rows 1))
  (flush-output))
