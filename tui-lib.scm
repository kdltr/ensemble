;; TUI library
;; ===========

(use vector-lib ansi-escape-sequences miscmacros)

;; TODO more compact representation for grids
(define-record cell attrs fg bg char)
(define-record grid width height cells)

(define +valid-attributes+ '(bold underscore blink reverse-video concealed))
(define +valid-colors+ '(black red green yellow blue magenta cyan white))

(define empty-cell (make-cell '() #f #f #\space))

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

(define (gprint! grid col row str #!key (attrs '()) (fg #f) (bg #f))
  (let loop ((i 0))
    (unless (= i (min (string-length str)
                      (* (grid-width grid) (grid-height grid))))
      (grid-put-char! grid
                      (+ col (modulo i (grid-width grid)))
                      (+ row (quotient i (grid-width grid)))
                      (string-ref str i)
                      attrs: attrs fg: fg bg: bg)
      (loop (add1 i)))))

(define (color->ansi-attribute ground color)
  (unless (member ground '(fg bg))
    (error 'color->ansi-attribute "invalid ground" ground))
  (unless (member color +valid-colors+)
    (error 'color->ansi-attribute "invalid color" color))
  (symbol-append ground '- color))

(define (draw-cell! cell)
  (write-string (set-text (append '(reset)
                                  (if (cell-fg cell)
                                      (list (color->ansi-attribute 'fg (cell-fg cell)))
                                      '())
                                  (if (cell-bg cell)
                                      (list (color->ansi-attribute 'bg (cell-bg cell)))
                                      '())
                                  (cell-attrs cell))
                          (string (cell-char cell)))))

(define (draw-grid! grid)
  (write-string (cursor-position 1 1))
  (vector-for-each (lambda (i cell) (draw-cell! cell))
                   (grid-cells grid)))
