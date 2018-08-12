(declare (disable-interrupts))

(module nonblocking-ports (open-input-file*/nonblocking
                           open-output-file*/nonblocking)
(import scheme
        (chicken bitwise)
        (chicken condition)
        (chicken errno)
        (chicken file posix)
        (chicken port)
        srfi-18
        srfi-71)

(define ((fd-read-char fd))
  (thread-wait-for-i/o! fd #:input)
  (let* ((str+len (file-read fd 1)))
    (if (= (cadr str+len) 0)
        '#!eof
        (string-ref (car str+len) 0))))

(define ((input-char-ready? fd))
  (let ((r _ (file-select fd #f 0)))
    r))

(define (open-input-file*/nonblocking fd)
  (make-input-port (fd-read-char fd)
                   (input-char-ready? fd)
                   (lambda () (file-close fd))))



(define ((fd-write-string fd) str)
  (condition-case (file-write fd str)
    (exn (exn i/o file) (if (or (= (errno) errno/again)
                                (= (errno) errno/wouldblock))
                            (begin
                              (thread-wait-for-i/o! fd)
                              ((fd-write-string fd) str))
                            (signal exn)))))

(define (open-output-file*/nonblocking fd)
  (file-control fd fcntl/setfl (bitwise-ior (file-control fd fcntl/getfl)
                                            open/nonblock))
  (make-output-port (fd-write-string fd)
                    (lambda () (file-close fd))))
)