;; STATUS
(define status-bar-grid (make-grid columns 1))
(fill! status-bar-grid #\space bg: 'red)
(gprint! status-bar-grid 0 0 "status bar" bg: 'red)

(define (status-bar str . args)
  (fill! status-bar-grid #\space bg: 'red)
  (gprint! status-bar-grid 0 0
           (apply format #f str args)
           bg: 'red))
