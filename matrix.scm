(load "low-level")

(define (password-login user password)
  (login `((type . "m.login.password")
           (user . ,user)
           (password . ,password))))
