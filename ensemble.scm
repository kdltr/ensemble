(module client-mod (init! password-login access-token server-uri mxid startup
                    config-ref config-set!)
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
          read-string write-string read-token)
  ports files)

(use utf8 utf8-srfi-13 utf8-srfi-14 unicode-char-sets
     vector-lib clojurian-syntax uri-common openssl
     ncurses gochan miscmacros srfi-1 posix irregex
     srfi-18 intarweb (except medea read-json) cjson
     rest-bind uri-common (prefix http-client http:)
     ensemble.utils sql-de-lite lru-cache)

(include "db.scm")
(include "client.scm")
)


(module main ()
(import scheme (except chicken reset) extras client-mod (only ncurses endwin))
(use (only uri-common uri->string))

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
  (config-set! 'mxid (mxid))
  (config-set! 'server-uri (uri->string (server-uri)))
  (config-set! 'access-token (access-token)))

(unless (and (server-uri) (access-token) (mxid))
  (prompt-credentials))

(handle-exceptions exn
  (begin
    (on-exit void)
    ;; Disable ncurses before printing the error message and call trace
    (endwin)
    (signal exn))
  (on-exit endwin)
  (startup))
)
