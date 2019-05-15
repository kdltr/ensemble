;; (define-ipc-spec NAME RECEIVER ANNOTATED-DSSSL-LAMBDA-LIST)
;; Annonated lambda list element: (name type)

;; Before including this file:
;; (begin-for-syntax
;;   (define *ipc-receiver* (quote frontend/backend))
;;   (define *ipc-send-procedure* (quote whatever)))

(begin-for-syntax
  (import srfi-1 srfi-69)
  
  (define (type->predicate type)
    (case type
      ((integer) 'exact-integer?)
      ((*) 'any?)
      (else
        (symbol-append type '?))))
  
  ;; name -> (lambda-list type-declaration predicates)
  (define *ipc-specs* (make-hash-table)))

(define (any? o) #t)

(define-syntax define-ipc-spec
  (ir-macro-transformer
    (lambda (exp inject compare)
      (let ((receiver (cadr exp))
            (spec (caddr exp)))
        (if (compare receiver *ipc-receiver*)
            (cons 'define-ipc-receiver spec)
            (cons 'define-ipc-sender spec))))))

(define-syntax define-ipc-receiver
  (ir-macro-transformer
    (lambda (exp inject compare)
      (let* ((name (strip-syntax (cadr exp)))
             (typed-args (cddr exp))
             (arg-names (map (lambda (p) (strip-syntax (car p))) typed-args))
             (arg-types (map (lambda (p) (strip-syntax (cadr p))) typed-args))
             (arg-predicates (map type->predicate arg-types)))
        (hash-table-set! *ipc-specs*
                         name
                         (list arg-names
                               arg-types
                               arg-predicates))
        '(void)))))

(define-syntax define-ipc-implementation
  (ir-macro-transformer
    (lambda (exp inject compare)
      (let* ((signature (cadr exp))
             (name (strip-syntax (car signature)))
             (args (cdr signature))
             (body (cddr exp))
             (proc-name (inject (symbol-append '|ipc-impl:| name)))
             (spec (hash-table-ref *ipc-specs* name))
             (types (cadr spec))
             (predicates (caddr spec)))
        (assert (equal? (car spec) (map strip-syntax args)))
        `(begin
           (: ,proc-name (,@types -> *))
           (define ,proc-name
             (lambda ,args
               ,@(map
                   (lambda (arg pred)
                     `(assert (,pred ,arg)))
                   args
                   predicates)
               ,@body))
           (hash-table-set! ,*ipc-hash-table* ',name ,proc-name))))))


(define-syntax define-ipc-sender
  (ir-macro-transformer
    (lambda (exp inject compare)
      (let* ((name (cadr exp))
             (typed-args (cddr exp))
             (args (map car typed-args))
             (types (map cadr typed-args))
             (assertions (map
                           (lambda (arg type)
                             ;; (assert (type-pred? arg))
                             `(assert (,(type->predicate (strip-syntax type)) ,arg)))
                           args
                           types))
             (proc-name (inject (symbol-append '|ipc:| (strip-syntax name)))))
        `(begin
           (: ,proc-name (,@types -> *))
           (define ,proc-name
             (lambda ,args
               ,@assertions
               (,*ipc-send-procedure*
                 (list ',name ,@args)))))))))


;; Backend -> Frontend
;; ===================

(define-ipc-spec frontend
  (notifications (room-id symbol)
                 (new-hls integer)
                 (new-notifs integer)))

(define-ipc-spec frontend
  (read-marker (room-id symbol)
               (event-id symbol)))

(define-ipc-spec frontend
  (room-name (room-id symbol)
             (room-name string)))

(define-ipc-spec frontend
  (room-members (room-id symbol)
                (members list)))

(define-ipc-spec frontend
  (message (room-id symbol)
           (message list)))

(define-ipc-spec frontend
  (message-before (room-id symbol)
                  (event-id symbol)
                  (message list)))

(define-ipc-spec frontend
  (remove (room-id symbol)
          (event-id symbol)))

(define-ipc-spec frontend
  (info (message string)))


;; Frontend -> backend
;; ===================

(define-ipc-spec backend
  (message:text (room-id symbol)
                (str string)))

(define-ipc-spec backend
  (message:emote (room-id symbol)
                 (str string)))

(define-ipc-spec backend
  (mark-last-message-as-read (room-id symbol)))

(define-ipc-spec backend
  (fill-hole (room-id symbol)
             (event-id symbol)
             (count integer)))

(define-ipc-spec backend
  (login (server string) 
         (username string)
         (password string)))

(define-ipc-spec backend
  (join-room (room string)))

(define-ipc-spec backend
  (leave-room (room-id string)))

(define-ipc-spec backend
  (stop))
