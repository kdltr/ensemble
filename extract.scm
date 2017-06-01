(import scheme)
(use srfi-13 srfi-1)

(define port (open-input-file "EastAsianWidth.txt"))

(define (translate-width sym)
  (case sym
    ((F) 'full-width)
    ((H) 'half-width)
    ((W) 'wide)
    ((Na) 'narrow)
    ((A) 'ambiguous)
    ((N) 'neutral)
    (else (error "unknown width" sym))))

(define (extract)
  (let ((line (read-line port)))
    (if (eof-object? line)
        '()
        (extract-line line))))

(define (extract-line line)
  (let* ((uncomment (string-split line "#" #t))
         (data (string-split (string-trim-both (car uncomment)) ";")))
    (if (null? data)
        (extract)
        (let ((range (map (cut string->number <> 16) (string-split (car data) ".")))
              (width (translate-width (string->symbol (cadr data)))))
          (if (= 1 (length range))
              (cons (list (car range) (car range) width) (extract))
              (cons (list (car range) (cadr range) width) (extract)))))))

(define l (extract))

;; using vectors because they have a litteral representation

;; node vector:
;; 0 minimum
;; 1 left
;; 2 maximum
;; 3 right
;; 4 value

(define (make-tree l)
  (if (null? l) '()
  (let*-values (((len) (length l))
                ((left right) (split-at l (quotient len 2)))
                ((data) (car right)))
    (vector (car data)
            (make-tree left)
            (cadr data)
            (make-tree (cdr right))
            (caddr data)))))

(define (search n t)
  (let ((minimum (vector-ref t 0))
        (maximum (vector-ref t 2)))
    (cond ((>= maximum n minimum)
           (vector-ref t 4))
          ((< n minimum)
           (search n (vector-ref t 1)))
          ((> n maximum)
           (search n (vector-ref t 3))))))

(define t (make-tree l))

(define (check node)
  (let ((minimum (car node))
        (maximum (cadr node))
        (value (caddr node)))
    (every (lambda (n) (eq? (search n t) value))
           (iota (add1 (- maximum minimum)) minimum))))

(unless (every check l)
  (error "The tree doesn’t contain the right stuff"))

(with-output-to-file "ucd.dat.scm"
  (lambda () (write t)))
