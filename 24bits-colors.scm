(use posix color)

(define-values (rows columns) (terminal-size (current-output-port)))

(define (set-color ground r g b text #!optional (reset #t))
  (format #f "\x1b[~a;2;~a;~a;~am~a~a"
          (case ground ((fg) 38) ((bg) 48))
          r g b
          text
          (if reset "\x1b[0m" "")))

(let l1 ((r 1))
  (newline)
  (let l2 ((c 1))
    (display (apply (cut set-color 'bg <> <> <> " ")
                    (color->sRGB (color:L*C*h (* (/ r rows) 100)
                                              50
                                              (* (/ c columns) 360)))))
    (unless (= c columns) (l2 (add1 c))))
  (unless (= r rows) (l1 (add1 r))))
