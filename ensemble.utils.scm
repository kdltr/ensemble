(module ensemble.utils (char-width string-width)
(import (except scheme string->list)
        chicken srfi-1 foreign)
(use utf8)

(foreign-declare "#include <wchar.h>")
(foreign-declare "#include <locale.h>")
(foreign-code "setlocale(LC_ALL, \"\");")

(define (string-width str)
  (fold (lambda (c rest) (+ rest (char-width c))) 0 (string->list str)))

(define (char-width c)
  (let ((ret ((foreign-lambda integer "wcwidth" integer)
              (char->integer c))))
    (if (= ret -1)
        (error "wcwidth returned -1")
        ret)))
)