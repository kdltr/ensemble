;; WHOLE TERMINAL

(define-values (rows columns) (terminal-size termout))
(define buffer (make-grid columns rows))

(load "tui/input")
(load "tui/title")
(load "tui/status")
(load "tui/central-frame")
