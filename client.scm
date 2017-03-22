(load "matrix")
 
;; Config file
;; ===========

(define (reload)
  (save-config)
  (load "client"))

(define (save-config)
  (with-output-to-file "config.scm"
    (lambda ()
      (for-each write `((init! ,(uri->string (server-uri)))
                        (access-token ,(access-token))
                        (transaction-id ,(transaction-id)))))))

(when (file-exists? "config.scm")
  (for-each eval (read-file "config.scm")))
