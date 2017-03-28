;; TITLE

(define title-bar (make-grid columns 1))
(fill! title-bar #\space bg: 'magenta)
(gprint! title-bar 0 0 "title bar" bg: 'magenta)
