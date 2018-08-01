(include "debug.scm")
(include "locations.scm")
(include "nonblocking-ports.scm")
(include "concurrency.scm")
(include "tui/base.scm")

(module main ()
(import foreign tui ncurses chicken scheme)

(foreign-declare "#include <locale.h>")
(foreign-code "setlocale(LC_ALL, \"C.UTF-8\");")

(handle-exceptions exn
  (begin
    (on-exit void)
    ;; Disable ncurses before printing the error message and call trace
    (endwin)
    (print exn)
    (signal exn))
  (on-exit endwin)
  (run)))

#;(cond-expand (csi)
      (else
(module main ()
(import scheme chicken extras foreign backend tui)
(use (only uri-common uri->string)
     (only ncurses endwin)
     stty)
(cond-expand
      (debug (enable-warnings #t)
             (let* ((tty (get-environment-variable "DEBUG_TTY"))
                    (in (open-input-file tty))
                    (out (open-output-file tty)))
               (current-input-port in)
               (current-output-port out)
               (current-error-port out)))
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

))) ;; main module