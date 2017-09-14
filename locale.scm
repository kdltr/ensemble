(import foreign)

(foreign-declare "#include <locale.h>")
(foreign-code "setlocale(LC_ALL, \"\");")
