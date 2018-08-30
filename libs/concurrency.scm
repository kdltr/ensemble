(module (ensemble libs concurrency) (defer receive-defered retry
                                     start-worker worker-wait
                                     worker-receive worker-send
                                     worker-name)
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

(define-record worker name input output pid)

(define (start-worker name proc . args)
  (let* ((in1 out1 (create-pipe))
         (in2 out2 (create-pipe))
         (pid (process-fork
                (lambda ()
                  (file-close in1)
                  (file-close out2)
                  (duplicate-fileno in2 0)
                  (duplicate-fileno out1 1)
                  #;(current-input-port
                    (open-input-file*/nonblocking in2))
                  #;(current-output-port
                    (open-output-file*/nonblocking out1))
                  (apply proc args))
                  #t)))
    (file-close in2)
    (file-close out1)
    (make-worker name
                 (open-input-file*/nonblocking in1)
                 (open-output-file*/nonblocking out2)
                 pid)))

(define (worker-send w msg)
  (let ((out (worker-output w)))
    (write msg out)
    (newline out)
    (flush-output out)))

(define (worker-receive w)
  (values (read (worker-input w)) w))

(define (worker-wait w #!optional (nohang #f))
  (process-wait (worker-pid w) nohang))
)