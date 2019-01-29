(module (ensemble libs concurrency) (defer receive-defered retry
                                     start-worker worker-wait worker-name
                                     worker-incomming worker-outgoing)
(import
  scheme
  (chicken base)
  (chicken condition)
  (chicken file posix)
  (chicken process)
  srfi-18
  srfi-71
  gochan
  (ensemble libs debug)
  (ensemble libs nonblocking-ports))


;; Defering API
;; ============

(define ui-chan (gochan 0))

(define (defer id proc . args)
  (unless (memq id '()) (info "[defer] ~s ~s ~s~%" id proc args))
  (thread-start!
    (lambda ()
      (let ((res (handle-exceptions exn exn (apply proc args))))
        (gochan-send ui-chan (current-thread))
        (if (condition? res)
            (signal (make-composite-condition
                      res
                      (make-property-condition 'defered 'id id 'proc proc 'args args)))
            (values id res))))))

(define (receive-defered)
  (gochan-recv ui-chan))

;; Retry a failed defered procedure
(define +wait-scaling-factor+ (/ (+ 1. (sqrt 5.)) 2.)) ;; Phi
(define (retry exn)
  (let ((id (get-condition-property exn 'defered 'id #f))
        (proc (get-condition-property exn 'defered 'proc #f))
        (args (get-condition-property exn 'defered 'args #f))
        (waiting-time (get-condition-property exn 'defered 'waiting-time 0)))
    (if (and id proc args)
        (begin
          (info "[retry] retrying ~a~%" exn)
          (defer id (lambda (args)
                      (thread-sleep! waiting-time)
                      (handle-exceptions exn
                        (signal
                          (make-composite-condition
                            (make-property-condition 'defered
                              'waiting-time (min 5
                                                 (if (zero? waiting-time)
                                                     +wait-scaling-factor+
                                                     (* +wait-scaling-factor+
                                                        waiting-time))))
                            exn))
                        (apply proc args)))
                 args))
        (info "[retry] failed to retry: ~a~%" exn))))


;; Worker processes
;; ================

(define-record worker name incomming outgoing pid)

(define (start-worker name proc . args)
  (let* ((in1 out1 (create-pipe))
         (in2 out2 (create-pipe))
         (pid (process-fork
                (lambda ()
                  (file-close in1)
                  (file-close out2)
                  (duplicate-fileno in2 0)
                  (duplicate-fileno out1 1)
                  (apply proc args))
                  #t)))
    (file-close in2)
    (file-close out1)
    (let ((input-port (open-input-file*/nonblocking in1))
          (output-port (open-output-file*/nonblocking out2))
          (incomming-chan (gochan 0))
          (outgoing-chan (gochan 0)))
      (thread-start! (lambda () (worker-read-loop input-port incomming-chan)))
      (thread-start! (lambda () (worker-write-loop output-port outgoing-chan)))
      (make-worker name incomming-chan outgoing-chan pid))))

(define (worker-read-loop input-port channel)
  (handle-exceptions exn
    (begin
      (gochan-close channel exn)
      (close-input-port input-port))
    (let ((exp (read input-port)))
      (if (eof-object? exp)
          (signal 'end-of-file)
          (begin
            (gochan-send channel exp)
            (worker-read-loop input-port channel))))))

(define (worker-write-loop output-port channel)
  (handle-exceptions exn
    (begin
      (gochan-close channel exn)
      (close-output-port output-port)
      (signal exn))
    (let ((exp fail _ (gochan-recv channel)))
      (if fail
          (signal 'end-of-file)
          (begin
            (write exp output-port)
            (newline output-port)
            (flush-output output-port)
            (worker-write-loop output-port channel))))))

(define (worker-wait w #!optional (nohang #f))
  (process-wait (worker-pid w) nohang))

)