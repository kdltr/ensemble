(declare (disable-interrupts))

(module nonblocking-ports (open-input-file*/nonblocking
                           open-output-file*/nonblocking)
(import scheme chicken foreign ports)
(use posix srfi-18 srfi-71)

(define BUFSIZE 1024)

;; INPUT ports
;; ===========

(define (open-input-file*/nonblocking fd)
  (define buf (make-string BUFSIZE #\null))
  (define len 0)
  (define pos 0)

  (define (fd-read-char)
    (if (< pos len)
        (let ((c (string-ref buf pos)))
          (set! pos (add1 pos))
          c)
        (begin
          (thread-wait-for-i/o! fd #:input)
          (let* ((str+len (file-read fd BUFSIZE buf))
                 (read-len (cadr str+len)))
            (if (zero? read-len)
                '#!eof
                (begin
                  (set! pos 0)
                  (set! len read-len)
                  (fd-read-char)))))))

  (define (input-char-ready?)
    (or (< pos len)
        (let ((r _ (file-select fd #f 0)))
          r)))

  (make-input-port fd-read-char
                   input-char-ready?
                   (lambda () (file-close fd))))

;; OUTPUT ports
;; ============

(define (open-output-file*/nonblocking fd)
  (define buf (make-string BUFSIZE #\null))
  (define pos 0)

  (define (write-buf str)
    (copy-string! buf pos str)
    (set! pos (+ pos (string-length str))))

  (define (flush-buf)
    (write-all-data fd buf pos)
    (set! pos 0))

  (define (fd-write-string str)
    (let ((str-len (string-length str))
          (buf-free (- BUFSIZE pos)))
      (cond ((> str-len BUFSIZE)
             (flush-buf)
             (write-all-data fd str str-len))
            ((<= str-len buf-free)
             (write-buf str))
            (else
              (flush-buf)
              (write-buf str)))))

  (file-control fd fcntl/setfl (bitwise-ior (file-control fd fcntl/getfl)
                                            open/nonblock))
  (make-output-port fd-write-string
                    (lambda () (file-close fd))
                    flush-buf))

(define (with-nonblocking-write fd thunk)
  (condition-case
    (thunk)
    (exn (exn i/o file)
         (if (or (= (errno) errno/again)
                 (= (errno) errno/wouldblock))
             (begin
               (thread-wait-for-i/o! fd)
               (with-nonblocking-write fd thunk))))))

(define (copy-string! target tpos source)
  (let lp ((spos 0)
           (tpos tpos))
    (unless (= spos (string-length source))
      (string-set! target tpos (string-ref source spos))
      (lp (add1 spos) (add1 tpos)))))

(define (write-all-data fd str len)
  (with-nonblocking-write fd
    (lambda ()
      (let lp ((written-len (file-write fd str len)))
        (when (< written-len len)
          (set! str (substring str written-len len))
          (set! len (- len written-len))
          (lp (file-write fd str len)))))))

) ; module nonblocking-ports