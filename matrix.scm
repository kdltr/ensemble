(load "low-level")

(use clojurian-syntax)

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
