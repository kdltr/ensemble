(include "low-level.scm")

;; Helper procedures
;; =================

(define (json-false? o)
  (or (equal? o #f)
      (equal? o 'null)
      (equal? o "")
      (equal? o 0)))

(define (json-true? o)
  (and (not (json-false? o)) o))

(define (mref keys alist)
  (if (null? keys)
      alist
      (and-let* ((o (alist-ref (car keys) alist equal?)))
           (mref (cdr keys) o))))

(define (mupdate keys val alist)
  (if (null? (cdr keys))
      (alist-update (car keys) val alist equal?)
      (alist-update (car keys)
                    (mupdate (cdr keys) val (or (alist-ref (car keys) alist) '()))
                    alist
                    equal?)))

(define (mdelete keys alist)
  (if (null? (cdr keys))
      (alist-delete (car keys) alist equal?)
      (alist-update (car keys)
                    (mdelete (cdr keys) (or (alist-ref (car keys) alist) '()))
                    alist
                    equal?)))

;; Defering API
;; ============

(define (defer id proc . args)
  (unless (eq? id 'input) (info "[defer] ~s ~s ~s~%" id proc args))
  (thread-start!
    (lambda ()
      (let ((res (handle-exceptions exn exn (apply proc args))))
        (gochan-send ui-chan (current-thread))
        (if (condition? res)
            (signal (make-composite-condition
                      res
                      (make-property-condition 'defered 'id id 'proc proc 'args args)))
            (values id res))))))

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


;; High level API
;; ==============

(define new-txnid
  (let ((cnt 0))
    (lambda ()
      (set! cnt (add1 cnt))
      (sprintf "~a-~a" (current-seconds) cnt))))

(define mxid (make-parameter #f))
(define (password-login user password)
  (let ((res (login `((type . "m.login.password")
                      (user . ,user)
                      (password . ,password)))))
    (mxid (alist-ref 'user_id res))
    (access-token (alist-ref 'access_token res))))

;; TODO do that properly when low-level is a module
(define logout
  (let ((old-logout logout))
    (lambda () (old-logout '()))))


(define (message:text room text)
  (defer 'message
    room-send room
              'm.room.message
              (new-txnid)
              `((msgtype . "m.text")
                (body . ,text))))

(define (message:emote room text)
  (defer 'message
    room-send room
              'm.room.message
              (new-txnid)
              `((msgtype . "m.emote")
                (body . ,text))))

(define (room-mark-read room evt)
  (defer 'receipt
    room-receipt room 'm.read evt '()))