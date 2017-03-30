;; TITLE

(define title-bar-grid (make-grid columns 1))
(fill! title-bar-grid #\space bg: 'magenta)
(gprint! title-bar-grid 0 0 "title bar" bg: 'magenta)

(define (title-bar str . args)
  (fill! title-bar-grid #\space bg: 'magenta)
  (gprint! title-bar-grid 0 0
           (apply format #f str args)
           bg: 'magenta))
