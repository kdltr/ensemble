(use posix stty ansi-escape-sequences utf8 utf8-srfi-13 unicode-char-sets matchable)

(load "tui-lib")

(define tty (get-environment-variable "TTY"))

(unless tty (error "please define the TTY environment variable"))

(define termin (open-input-file tty))
(define termout (open-output-file tty))

(unless (terminal-port? termout)
  (error "specified file is not a terminal" tty))

(define-values (rows columns) (terminal-size termout))

(set-buffering-mode! termin #:none)
(set-buffering-mode! termout #:full 4096)

;; WHOLE TERMINAL
(define buffer (make-grid columns rows))

(load "tui/input")
(load "tui/title")
(load "tui/status")
(load "tui/central-frame")

(define (blit-all!)
  (blit! buffer 0 0 title-bar)
  (blit! buffer 0 1 central-frame)
  (blit! buffer 0 (- rows 2) status-bar)
  (blit! buffer 0 (sub1 rows) input-bar))

(define (refresh!)
  (blit-all!)
  (with-output-to-port termout
    (lambda ()
      (display (hide-cursor))
      (draw-grid! buffer)
      (display (cursor-position rows (add1 *cursor-pos*)))
      (display (show-cursor))
      (flush-output))))

(define (register-key key)
  (match key
    (#\delete  (delete-prev-char))
    (#\x1  (cursor-first-char))
    (#\x5  (cursor-last-char))
    (#\x4  (delete-next-char))
    ((or (#\escape #\[ #\C)
         #\x6)
     (cursor-next-char))
    ((or (#\escape #\[ #\D)
         #\x2)
     (cursor-prev-char))
    ((#\escape #\f)
     (cursor-next-word))
    ((#\escape #\b)
     (cursor-prev-word))
    (#\vtab  (delete-from-cursor))
    (#\newline (handle-input *text*)
               (set! *text* "")
               (set! *cursor-pos* 0))
    (else  (format #t "unknown key: ~s~%" key))))

(define status-message void)