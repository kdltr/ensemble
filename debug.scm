(module debug (info)
(import scheme chicken extras)

(cond-expand
      (debug (define (info fmt . args)
               (apply fprintf (current-error-port) fmt args)))
      (else (define-syntax info
              (syntax-rules ()
                ((info . rest) (void))))))
) ;; debug module
