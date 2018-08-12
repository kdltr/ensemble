(module locations (config-home cache-home)
(import scheme
        (chicken base)
        (chicken pathname)
        (chicken process-context))

(define (xdg-config-home)
  (cond ((get-environment-variable "XDG_CONFIG_HOME")
         => values)
        ((get-environment-variable "HOME")
         => (lambda (d) (make-pathname d ".config")))
        (else
          (error "Either the XDG_CONFIG_HOME or the HOME environment variables must be defined"))))

(define (xdg-cache-home)
  (cond ((get-environment-variable "XDG_CACHE_HOME")
         => values)
        ((get-environment-variable "HOME")
         => (lambda (d) (make-pathname d ".cache")))
        (else
          (error "Either the XDG_CACHE_HOME or the HOME environment variable must be defined"))))

(define (config-home)
  (make-pathname (xdg-config-home) "ensemble"))

(define (cache-home)
  (make-pathname (xdg-cache-home) "ensemble"))
) ;; locations module