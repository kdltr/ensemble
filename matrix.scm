(include "low-level.scm")

(use clojurian-syntax)

;; Helper procedures
;; =================

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

;; High level API
;; ==============

(define new-txnid
  (let ((cnt 0))
    (lambda ()
      (set! cnt (add1 cnt))
      (sprintf "~a-~a" (current-seconds) cnt))))

(define account-name (make-parameter #f))
(define mxid (make-parameter #f))
(define (password-login user password)
  (let ((res (login `((type . "m.login.password")
                      (user . ,user)
                      (password . ,password)))))
    (account-name user)
    (mxid (alist-ref 'user_id res))
    (access-token (alist-ref 'access_token res))))

;; TODO do that properly when low-level is a module
(define logout
  (let ((old-logout logout))
    (lambda () (old-logout '()))))


(define (message:text room text)
  (room-send room
             'm.room.message
             (new-txnid)
             `((msgtype . "m.text")
               (body . ,text))))

(define (message:emote room text)
  (room-send room
             'm.room.message
             (new-txnid)
             `((msgtype . "m.emote")
               (body . ,text))))

