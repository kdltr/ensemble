(module (ensemble libs bindings) (flock)
(import scheme (chicken base) (chicken foreign))

(foreign-declare "#include <sys/file.h>")

(define (flock fd)
  (let ((lock (foreign-lambda* int ((int fd))
                               "C_return(flock(fd, LOCK_EX|LOCK_NB));")))
    (if (zero? (lock fd))
        #t
        (error "error while locking file" fd))))
)
