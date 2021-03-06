(module main ()
(import
  scheme
  (chicken base)
  (chicken condition)
  (chicken format)
  ncurses
  (ensemble interface console))

(handle-exceptions exn
  (begin
    (on-exit void)
    ;; Disable ncurses before printing the error message and call trace
    (endwin)
    (fprintf (current-error-port) "Exception caught: ~s" exn)
    (signal exn))
  (on-exit endwin)
  (run)))
