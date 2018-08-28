(module main ()
(import
  scheme
  (chicken base)
  (chicken condition)
  (chicken foreign)
  (chicken format)
  ncurses
  (ensemble libs bindings)
  (ensemble interface console))

(setlocale "C.UTF-8")

(handle-exceptions exn
  (begin
    (on-exit void)
    ;; Disable ncurses before printing the error message and call trace
    (endwin)
    (fprintf (current-error-port) "Exception caught: ~s" exn)
    (signal exn))
  (on-exit endwin)
  (run)))
