(module (ensemble libs history)
        (history-data history-previous history-next
         history-insert-before! history-insert-after! history-remove!
         history-fold-next history-fold-previous)

(import scheme (chicken base))

(define-record history data previous next)

(define (history-insert-before! data hn)
  (let ((new-node (make-history data '() '())))
    (if (null? hn)
        new-node
        (let ((prev-node (history-previous hn)))
          (unless (null? prev-node)
            (history-next-set! prev-node new-node)
            (history-previous-set! new-node prev-node))
          (history-previous-set! hn new-node)
          (history-next-set! new-node hn)
          new-node))))

(define (history-insert-after! data hn)
  (let ((new-node (make-history data '() '())))
    (if (null? hn)
        new-node
        (let ((next-node (history-next hn)))
          (unless (null? next-node)
            (history-previous-set! next-node new-node)
            (history-next-set! new-node next-node))
          (history-next-set! hn new-node)
          (history-previous-set! new-node hn)
          new-node))))

(define (history-remove! hn)
  (let ((previous-node (history-previous hn))
        (next-node (history-next hn)))
    (when previous-node
      (history-next-set! previous-node next-node))
    (when next-node
      (history-previous-set! next-node previous-node))))

(define (history-fold-next kons nil hn)
  (if (null? hn)
      nil
      (kons (history-data hn)
            (history-fold-next kons nil (history-next hn)))))

(define (history-fold-previous kons nil hn)
  (if (null? hn)
      nil
      (kons (history-data hn)
            (history-fold-previous kons nil (history-previous hn)))))

) ; module (ensemble libs history)
