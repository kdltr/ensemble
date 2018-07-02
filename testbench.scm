(load "ensemble.scm")
(import frontend backend)

(define first-batch (sync))
(define next (handle-sync first-batch))
