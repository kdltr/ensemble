(load "low-level")

(use clojurian-syntax)

;; Helper procedures
;; =================

(define (ref keys alist)
  (if (null? keys)
      alist
      (and-let* ((o (alist-ref (car keys) alist)))
           (ref (cdr keys) o))))


;; High level API
;; ==============

(define transaction-id (make-parameter 0))
(define (new-txnid)
  (transaction-id (add1 (transaction-id))))

(define (password-login user password)
  (->> (login `((type . "m.login.password")
                (user . ,user)
                (password . ,password)))
       (alist-ref 'access_token)
       (access-token)))

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

