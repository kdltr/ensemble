;; CENTRAL FRAME

(define messages-height (- (grid-height buffer)
                           (grid-height title-bar)
                           (grid-height status-bar)
                           (grid-height input-bar)))

(define central-frame (make-grid columns messages-height))
(fill! central-frame #\space bg: 'white)
(gprint! central-frame 0 0 "central frame" fg: 'black bg: 'white)

(define (new-message text)
  (let* ((lines (string-split text "\n" #t))
         (height (+ (length lines)
                    (fold + 0 (map (lambda (s) (quotient (string-length s) (add1 (grid-width central-frame))))
                                   lines))))
         (grid (make-grid (grid-width central-frame) height)))
    (gprint! grid 0 0 text)
    grid))

(define (draw-messages! msgs)
  (let loop ((last-row (grid-height central-frame))
             (rest msgs))
    (unless (or (<= last-row 0)
                (null? rest))
      (let* ((msg (car rest))
             (msg (if (string? msg) msg (print-event msg)))
             (msg-grid (new-message msg))
             (row (- last-row (grid-height msg-grid))))
        (if (< row 0)
            (loop 1 (list (xsubstring msg (* row (grid-width central-frame)) 0)))
            (begin
              (blit! central-frame 0 row msg-grid)
              (loop row (cdr rest))))))))
