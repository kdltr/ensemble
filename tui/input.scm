(stty termin '((not echo)
               (not isig)
               (not icanon)
               (not clocal)
               (not ixoff)
               (stop #f)
               (start #f)
               ))

;; String procedures

(define (string-insert str i char)
  (string-replace str (string char) i i))

(define (string-remove str i)
  (string-append (substring str 0 i)
                 (substring str (add1 i))))

;; Input handling procedures

(define (read-escape-sequence)
  (if (char-ready? termin)
      (cons (read-char termin)
            (read-escape-sequence))
      '()))

;; TODO maybe add C1 control characters (https://en.wikipedia.org/wiki/C0_and_C1_control_codes)
(define (control-char? c)
  (or (and (char>=? c #\x00)
           (char<=? c #\x1F))
      (char=? c #\delete)))

(define (process-input char-proc keybinding-proc)
  (let ((char (read-char termin)))
    (cond ((char=? char #\escape)
           (keybinding-proc (cons char (read-escape-sequence))))
          ((control-char? char)
            (keybinding-proc char))
          (else
            (char-proc char)))))

(define text "")
(define cursor-pos 0)

;; INPUT bar
(define input-bar (make-grid columns 1))

(define (advance-input!)
  (process-input (lambda (c)
                   (set! text (string-insert text cursor-pos c))
                   (set! cursor-pos (add1 cursor-pos)))
                 (lambda (key)
                   (case key
                     ((#\backspace #\delete)
                      (key-backspace!))
                     )))
  (fill! input-bar #\space)
  (gprint! input-bar 0 0 text)
  )

(define (register-char c)
  (void))

(define (delete-last-char)
  (unless (or (string-null? text) (= cursor-pos 0))
    (set! text (string-remove text (sub1 cursor-pos)))
    (set! cursor-pos (sub1 cursor-pos))))
