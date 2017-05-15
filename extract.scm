(import scheme)

(define vec
  (make-u8vector (add1 (quotient #xFFFF 8)) 0))

(define (bit-set! vec index bit)
  (u8vector-set! vec (quotient index 8)
                 (bitwise-ior (* (if bit 1 0) (expt 2 (modulo index 8)))
                              (u8vector-ref vec (quotient index 8)))))

(define (bit-ref vec index)
  (< 0 (bitwise-and (u8vector-ref vec (quotient index 8))
                    (expt 2 (modulo index 8)))))

(define (extract line)
  (let* ((split (string-split line ":"))
         (charnum (string->number (car split) 16))
         (charwidth (/ (string-length (cadr split)) 32)))
    (assert (<= 1 charwidth 2))
    (bit-set! vec charnum (= charwidth 2))))

(define truc
(call-with-input-file "unifont.hex"
  (lambda (p)
    (port-for-each extract (lambda () (read-line p))))))
