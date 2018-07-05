(module concurrency (defer receive-defered retry
                     thread-join-protected!
                     start-worker worker-wait
                     worker-receive worker-send)
(import scheme chicken debug srfi-18)
(use gochan posix nonblocking-ports)

;; Defering API
;; ============

(define ui-chan (gochan 0))

(define (defer id proc . args)
  (unless (memq id '(input sync)) (info "[defer] ~s ~s ~s~%" id proc args))
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
(define (retry exn)
  (let ((id (get-condition-property exn 'defered 'id #f))
        (proc (get-condition-property exn 'defered 'proc #f))
        (args (get-condition-property exn 'defered 'args #f)))
    (if (and id proc args)
        (begin
          (info "[retry] retrying ~a~%" exn)
          (defer id (lambda (args) (thread-sleep! 1) (apply proc args)) args))
        (info "[retry] failed to retry: ~a~%" exn))))

(define (thread-join-protected! thread)
  (receive data (handle-exceptions exn exn (thread-join! thread))
    (if (uncaught-exception? (car data))
        (condition-case (signal (uncaught-exception-reason (car data)))
          (exn (exn i/o net)  (retry exn) (values 'no-one #f))
          (exn (exn http server-error)  (retry exn) (values 'no-one #f))
          (exn (exn http premature-disconnection)
            (retry exn)
            (values 'no-one #f)))
        (apply values data))))



;; Worker processes
;; ================

(define-record worker input output pid)

(define (start-worker proc . args)
  (let*-values (((in1 out1) (create-pipe))
                ((in2 out2) (create-pipe))
                ((pid)
                 (process-fork
                   (lambda ()
                     (file-close in1)
                     (file-close out2)
                     (current-input-port
                       (open-input-file*/nonblocking in2))
                     (current-output-port
                       (open-output-file*/nonblocking out1))
                     (apply proc args))
                   #t)))
    (file-close in2)
    (file-close out1)
    (make-worker (open-input-file*/nonblocking in1)
                 (open-output-file*/nonblocking out2)
                 pid)))

(define (worker-send w msg)
  (let ((out (worker-output w)))
    (write msg out)
    (newline out)))

(define (worker-receive w)
  (list (read (worker-input w)) w))

(define (worker-wait w #!optional (nohang #f))
  (process-wait (worker-pid w) nohang))
)