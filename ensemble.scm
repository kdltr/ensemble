(module debug (info)
(import scheme chicken extras)
(cond-expand
      (debug (define (info fmt . args)
               (apply fprintf (current-error-port) fmt args)))
      (else (define-syntax info
              (syntax-rules ()
                ((info . rest) (void)))))))



(module defer (defer receive-defered retry
               thread-join-protected!)
(import scheme chicken debug srfi-18)
(use gochan)
(include "defer.scm"))



(module backend (init! password-login access-token server-uri mxid
                 config-ref config-set!
                       mref mupdate mdelete
                       sync any-room room-display-name read-marker-ref
                       room-timeline branch-last-sequence-number
                       request-hole-messages print-event room-exists?
                       room-name json-true? member-displayname room-context
                       joined-rooms handle-sync fill-hole message:emote
                       events-previous events-next message:text
                       mark-last-message-as-read room-members
                       *requested-holes*
                       )
(import
  (except scheme
          string-length string-ref string-set! make-string string substring
          string->list list->string string-fill! write-char read-char display)
  (except chicken
          reverse-list->string print print*)
  (except data-structures
          ->string conc string-chop string-split string-translate
          substring=? substring-ci=? substring-index substring-index-ci)
  ports files posix srfi-1 extras
  defer debug)

(use utf8 utf8-srfi-13 vector-lib uri-common openssl
     intarweb (except medea read-json) cjson
     rest-bind (prefix http-client http:)
     (except sql-de-lite reset))

(define +ensemble-version+ "dev")

(include "db.scm")
(include "matrix.scm")
(include "client.scm")
)



(module frontend *
(import
  (except scheme
          string-length string-ref string-set! make-string string substring
          string->list list->string string-fill! write-char read-char display)
  (except chicken
          reverse-list->string print print*)
  (except data-structures
          ->string conc string-chop string-split string-translate
          substring=? substring-ci=? substring-index substring-index-ci)
  srfi-1 posix data-structures irregex srfi-18 miscmacros extras
  defer debug backend)
(use ioctl ncurses utf8 utf8-srfi-13 utf8-srfi-14 unicode-char-sets)
(include "tui.scm")
(include "tui/input.scm")
)


(cond-expand (csi)
      (else
(module main ()
(import scheme chicken extras foreign backend frontend)
(use (only uri-common uri->string)
     (only ncurses endwin)
     stty)

(foreign-declare "#include <locale.h>")
(foreign-code "setlocale(LC_ALL, \"C.UTF-8\");")

(cond-expand
      (csi (enable-warnings #t))
      (else (enable-warnings #f)))

(define (prompt msg #!optional (passwd #f))
  (display msg)
  (flush-output)
  (if passwd
      (with-stty '(not echo)
        (lambda ()
          (let ((in (read-line)))
            (newline)
            in)))
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
    (print exn)
    (signal exn))
  (on-exit endwin)
  (startup))
)))
