;; TUI library
;; ===========

(use utf8 vector-lib ansi-escape-sequences miscmacros)

;; TODO more compact representation for grids
(define-record cell attrs fg bg char)
(define-record grid width height cells)

(define +valid-attributes+ '(bold underscore blink reverse-video concealed))
(define +valid-colors+ '(black red green yellow blue magenta cyan white))

(define empty-cell (make-cell '() #f #f #\space))

(define (cell=? c1 c2)
  (and (char=? (cell-char c1) (cell-char c2))
       (eq? (cell-fg c1) (cell-fg c2))
       (eq? (cell-bg c2) (cell-bg c2))
       (lset= eqv? (cell-attrs c1) (cell-attrs c2))))

(define %make-grid make-grid)
(define (make-grid width height)
  (%make-grid width height
              (make-vector (* width height) empty-cell)))

(define (check-bounds grid col row #!optional (loc #f))
  (unless (and (<= 0 col (sub1 (grid-width grid)))
               (<= 0 row (sub1 (grid-height grid))))
    (error loc "out-of-bounds access" col row)))

(define (cell-index grid col row)
  (+ col (* row (grid-width grid))))

(define (grid-ref grid col row)
  (check-bounds grid col row 'grid-ref)
  (vector-ref (grid-cells grid) (cell-index grid col row)))

(define (grid-put-cell! grid col row cell)
  (check-bounds grid col row 'grid-put-cell!)
  (vector-set! (grid-cells grid)
               (cell-index grid col row)
               cell))

(define (grid-put-char! grid col row char #!key (attrs '()) (fg #f) (bg #f))
  (grid-put-cell! grid col row
                  (make-cell attrs fg bg char)))

(define (fill! grid char #!key (attrs '()) (fg #f) (bg #f))
  (vector-fill! (grid-cells grid)
                (make-cell attrs fg bg char)))

(define (blit! dst col row src)
  (check-bounds dst col row 'blit!)
  (check-bounds dst (+ col (sub1 (grid-width src))) (+ row (sub1 (grid-height src))) 'blit!)
  (dotimes (i (grid-height src))
    (vector-copy! (grid-cells dst)
                  (cell-index dst col (+ row i))
                  (grid-cells src)
                  (cell-index src 0 i)
                  (cell-index src (grid-width src) i))))

(define (grid-put-string! grid col row str #!key (attrs '()) (fg #f) (bg #f))
  (let loop ((i 0))
    (unless (= i (min (string-length str)
                      (* (grid-width grid) (grid-height grid))))
      (grid-put-char! grid
                      (+ col (modulo i (grid-width grid)))
                      (+ row (quotient i (grid-width grid)))
                      (string-ref str i)
                      attrs: attrs fg: fg bg: bg)
      (loop (add1 i)))))

(define (gprint! grid col row str #!key (attrs '()) (fg #f) (bg #f))
  (cond ((null? str)
         'done)
        ((string? str)
         (gprint! grid col row (string-split str "\n" #t) attrs: attrs fg: fg bg: bg))
        ((pair? str)
         (grid-put-string! grid col row (car str) attrs: attrs fg: fg bg: bg)
         (gprint! grid col (add1 row) (cdr str) attrs: attrs fg: fg bg: bg))))

(define (color->ansi-attribute ground color)
  (unless (member ground '(fg bg))
    (error 'color->ansi-attribute "invalid ground" ground))
  (unless (member color +valid-colors+)
    (error 'color->ansi-attribute "invalid color" color))
  (symbol-append ground '- color))

(define *current-attrs* '(reset))

(define (draw-cell! cell)
  (let ((attrs (append '(reset)
                       (if (cell-fg cell)
                           (list (color->ansi-attribute 'fg (cell-fg cell)))
                           '())
                       (if (cell-bg cell)
                           (list (color->ansi-attribute 'bg (cell-bg cell)))
                           '())
                       (cell-attrs cell))))
    (write-string (if (lset= eqv? attrs *current-attrs*)
                      (string (cell-char cell))
                      (begin
                        (set! *current-attrs* attrs)
                        (set-text attrs (string (cell-char cell)) #f))))))

(define *previous-grid* (make-grid 0 0))

(define (draw-grid! grid)
  (if (and (= (grid-width grid) (grid-width *previous-grid*))
           (= (grid-height grid) (grid-height *previous-grid*)))
      (draw-diff-grid! *previous-grid* grid 0 #f)
      (draw-whole-grid! grid))
  (set! *previous-grid* (object-copy grid)))

(define (draw-whole-grid! grid)
  (write-string (cursor-position 1 1))
  (vector-for-each (lambda (i cell) (draw-cell! cell))
                   (grid-cells grid)))

(define (draw-diff-grid! from to index cursor-behind)
  (unless (= index (vector-length (grid-cells from)))
    (let* ((x (modulo index (grid-width from)))
           (y (quotient index (grid-width from)))
           (fc (grid-ref from x y))
           (tc (grid-ref to x y)))
      (if (cell=? fc tc)
          (draw-diff-grid! from to (add1 index) #f)
          (begin
            (unless cursor-behind
              (write-string (cursor-position (add1 y) (add1 x))))
            (draw-cell! tc)
            (draw-diff-grid! from to (add1 index) #t))))))
