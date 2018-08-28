(module (ensemble libs bindings) (setlocale flock)
(import scheme (chicken base) (chicken foreign))

(foreign-declare "#include <locale.h>")
(foreign-declare "#include <sys/file.h>")

(define (flock fd)
  (let ((lock (foreign-lambda* int ((int fd))
                               "C_return(flock(fd, LOCK_EX|LOCK_NB));")))
    (if (zero? (lock fd))
        #t
        (error "error while locking file" fd))))

(define (setlocale str)
  ((foreign-lambda* c-string ((c-string str))
                    "C_return(setlocale(LC_ALL, str));")
   str))

)
