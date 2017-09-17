(module client-mod (init! password-login access-token account-name sync-filter server-uri startup
                    save-config)
(import 
  (except scheme
          string-length string-ref string-set! make-string string substring
          string->list list->string string-fill! write-char read-char display)
  (except chicken
          reverse-list->string print print*)
  (except data-structures
          ->string conc string-chop string-split string-translate
          substring=? substring-ci=? substring-index substring-index-ci)
  (except extras
          read-string write-string read-token))
(use utf8 utf8-srfi-13 vector-lib clojurian-syntax uri-common openssl
     ncurses gochan miscmacros srfi-1 posix irregex srfi-18)

(include "client.scm")
)

(module main ()
(import scheme (except chicken reset) extras client-mod)

(cond-expand
      (csi (enable-warnings #t))
      (else (enable-warnings #f)))

(define (conceal) (display "\x1b[8m") (flush-output))
(define (reset) (display "\x1b[0m") (flush-output))

(define (prompt msg #!optional (passwd #f))
  (display msg)
  (flush-output)
  (if passwd
      (dynamic-wind conceal read-line reset)
      (read-line)))

(define (prompt-credentials)
  (init! (prompt "Enter your server URL (example: https://matrix.org): "))
  (password-login (prompt "Username: ") (prompt "Password: " #t))
  (save-config))

;; TODO ugly hack
(eval '(begin
         (define init! client-mod#init!)
         (define access-token client-mod#access-token)
         (define account-name client-mod#account-name)
         (define sync-filter client-mod#sync-filter)))
(when (file-exists? "config.scm")
  (for-each eval (read-file "config.scm")))

(unless (and (server-uri) (access-token))
  (prompt-credentials))

(startup)
)