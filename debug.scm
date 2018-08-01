(module debug (info)
(import scheme chicken extras)

(cond-expand
      (debug (define (info fmt . args)
               (fprintf (current-error-port) "~?~%" fmt args)
               (flush-output (current-error-port))))
      (else (define-syntax info
              (syntax-rules ()
                ((info . rest) (void))))))
) ;; debug module
