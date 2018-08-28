(module (ensemble libs json-help) (json-false? json-true? mref)
(import scheme (chicken base) srfi-1)

(define (json-false? o)
  (or (equal? o #f)
      (equal? o 'null)
      (equal? o "")
      (equal? o 0)))

(define (json-true? o)
  (and (not (json-false? o)) o))

(define (mref keys alist)
  (if (null? keys)
      alist
      (and-let* ((o (alist-ref (car keys) alist equal?)))
           (mref (cdr keys) o))))

(define (mupdate keys val alist)
  (if (null? (cdr keys))
      (alist-update (car keys) val alist equal?)
      (alist-update (car keys)
                    (mupdate (cdr keys) val (or (alist-ref (car keys) alist) '()))
                    alist
                    equal?)))

(define (mdelete keys alist)
  (if (null? (cdr keys))
      (alist-delete (car keys) alist equal?)
      (alist-update (car keys)
                    (mdelete (cdr keys) (or (alist-ref (car keys) alist) '()))
                    alist
                    equal?)))
) ; module (ensemble libs json-help)