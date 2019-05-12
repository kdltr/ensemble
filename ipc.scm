;; (define-ipc-spec NAME RECEIVER ANNOTATED-DSSSL-LAMBDA-LIST)
;; Annonated lambda list element: (name type)

;; Before including this file:
;; (begin-for-syntax
;;   (define *ipc-receiver* (quote frontend/backend))
;;   (define *ipc-send-procedure* (quote whatever)))

(begin-for-syntax
  (import srfi-1 srfi-71)
  
  (define (type->predicate type)
    (case type
      ((integer) 'exact-integer?)
      ((*) 'any?)
      (else
        (symbol-append type '?))))
  
  (define (split-lambda-list l)
    (let lp ((l l)
             (regular-args '()))
      (cond ((null? l)
             (values (reverse! regular-args) #f))
            ((eq? '#!rest (car l))
             (values (reverse! regular-args) (cadr l)))
            (else
              (lp (cdr l) (cons (car l) regular-args))))))
  )

(define (any? o) #t)

(define-syntax define-ipc-spec
  (ir-macro-transformer
    (lambda (exp inject compare)
      (let ((receiver (cadr exp))
            (spec (caddr exp)))
        (if (compare receiver *ipc-receiver*)
            (cons 'define-ipc-receiver spec)
            (cons 'define-ipc-sender spec))))))

;; TODO
(define-syntax define-ipc-receiver
  (ir-macro-transformer
    (lambda (exp inject compare)
      '(void)
      #;(list 'quote exp))))

(define-syntax define-ipc-sender
  (ir-macro-transformer
    (lambda (exp inject compare)
      (let* ((name (cadr exp))
             (typed-args typed-rest-arg (split-lambda-list (cddr exp)))
             (args (map car typed-args))
             (types (map cadr typed-args))
             (assertions (map
                           (lambda (arg type)
                             ;; (assert (type-pred? arg))
                             `(assert (,(type->predicate (strip-syntax type)) ,arg)))
                           args
                           types))
             (proc-name (inject (symbol-append '|ipc:| (strip-syntax name))))
             (rest-arg (if typed-rest-arg (car typed-rest-arg) '(quote ())))
             (rest-type-decl (if typed-rest-arg `(#!rest ,(cadr typed-rest-arg)) '()))
             (rest-assertion (if typed-rest-arg
                                 `(assert (,(type->predicate (strip-syntax (cadr typed-rest-arg)))
                                           ,rest-arg))
                                 '(void)))
             (rest-lambda (if typed-rest-arg `(#!rest ,(car typed-rest-arg)) '())))
        `(begin
           (: ,proc-name (,@types ,@rest-type-decl -> *))
           (define ,proc-name
             (lambda (,@args ,@rest-lambda)
               ,@assertions
               ,rest-assertion
               (,*ipc-send-procedure*
                 (cons* ',name ,@args ,rest-arg)))))))))


;; Backend -> Frontend
;; ===================

(define-ipc-spec frontend
  (bundle-start))

(define-ipc-spec frontend
  (bundle-end))

(define-ipc-spec frontend
  (notifications (room-id symbol)
                 (new-hls integer)
                 (new-notifs integer)))

(define-ipc-spec frontend
  (clear (room-id symbol)))

(define-ipc-spec frontend
  (refresh (room-id symbol)))

(define-ipc-spec frontend
  (response (id integer)
            (datum *)))

(define-ipc-spec frontend
  (read-marker (room-id symbol)
               (event-id symbol)))

(define-ipc-spec frontend
  (room-name (room-id symbol)
             (room-name string)))

(define-ipc-spec frontend
  (message (room-id symbol)
           (message pair)))

(define-ipc-spec frontend
  (info (message string)))


;; Frontend -> backend
;; ===================

(define-ipc-spec backend
  (subscribe (room-id symbol)))

(define-ipc-spec backend
  (unsubscribe (room-id symbol)))

(define-ipc-spec backend
  (fetch-events (room-id symbol)
                (limit integer)
                (offset integer)))

(define-ipc-spec backend
  (message:text (room-id symbol)
                (str string)))

(define-ipc-spec backend
  (message:emote (room-id symbol)
                 (str string)))

(define-ipc-spec backend
  (mark-last-message-as-read (room-id symbol)))

(define-ipc-spec backend
  (login (server string) 
         (username string)
         (password string)))

(define-ipc-spec backend
  (join-room (room string)))

(define-ipc-spec backend
  (leave-room (room-id string)))

(define-ipc-spec backend
  (query (query-id integer)
         (what symbol)
         #!rest (args *)))
