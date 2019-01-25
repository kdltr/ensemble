(module (ensemble libs bindings) (setlocale flock)
(import scheme (chicken base) (chicken foreign))

(foreign-declare "#include <locale.h>")
(foreign-declare "#include <sys/file.h>")

(define (flock fd #!optional (lock? #t))
  (let ((lock (foreign-lambda* int ((int fd) (bool lock))
                               "int operation;"
                               ;; LOCK_NB so that flock returns immediatly when file is already locked
                               "if (lock) { operation = LOCK_EX | LOCK_NB; } else { operation = LOCK_UN; }"
                               "C_return(flock(fd, operation));")))
    (if (zero? (lock fd lock?))
        #t
        (error "error while locking/unlocking file" fd))))

(define (setlocale str)
  ((foreign-lambda* c-string ((c-string str))
                    "C_return(setlocale(LC_ALL, str));")
   str))

)
