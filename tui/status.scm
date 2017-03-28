;; STATUS
(define status-bar (make-grid columns 1))
(fill! status-bar #\space bg: 'red)
(gprint! status-bar 0 0 "status bar" bg: 'red)
