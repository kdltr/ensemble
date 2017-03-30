(use stty)

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
  (thread-wait-for-i/o! (port->fileno termin) #:input)
  (let ((char (read-char termin)))
    (cond ((char=? char #\escape)
           (keybinding-proc (cons char (read-escape-sequence))))
          ((control-char? char)
            (keybinding-proc char))
          (else
            (char-proc char)))))

(define *text* "")
(define *cursor-pos* 0)

;; INPUT bar
(define input-bar-grid (make-grid columns 1))

(define (refresh-input-bar!)
  (fill! input-bar-grid #\space)
  (gprint! input-bar-grid 0 0 *text*))


;; Procedures for key bindings

(define (register-char c)
  (set! *text* (string-insert *text* *cursor-pos* c))
  (set! *cursor-pos* (add1 *cursor-pos*))
  (refresh-input-bar!))

(define (delete-prev-char)
  (unless (or (string-null? *text*) (= *cursor-pos* 0))
    (set! *cursor-pos* (sub1 *cursor-pos*))
    (set! *text* (string-remove *text* *cursor-pos*))
    (refresh-input-bar!)))

(define (delete-next-char)
  (unless (or (string-null? *text*) (= *cursor-pos* (string-length *text*)))
    (set! *text* (string-remove *text* *cursor-pos*))
    (refresh-input-bar!)))

(define (delete-from-cursor)
  (set! *text* (substring *text* 0 *cursor-pos*))
  (refresh-input-bar!))

(define (cursor-prev-char)
  (unless (= *cursor-pos* 0)
    (set! *cursor-pos* (sub1 *cursor-pos*))))

(define (cursor-next-char)
  (unless (= *cursor-pos* (string-length *text*))
    (set! *cursor-pos* (add1 *cursor-pos*))))

(define (cursor-next-word)
  (let ((idx (string-index *text* char-set:white-space (min (string-length *text*)
                                                            (add1 *cursor-pos*)))))
    (if idx
        (set! *cursor-pos* idx)
        (set! *cursor-pos* (string-length *text*)))))

(define (cursor-prev-word)
  (let ((idx (string-index-right *text* char-set:white-space 0 *cursor-pos*)))
    (if idx
        (set! *cursor-pos* idx)
        (set! *cursor-pos* 0))))

(define (cursor-first-char)
  (set! *cursor-pos* 0))

(define (cursor-last-char)
  (set! *cursor-pos* (string-length *text*)))
