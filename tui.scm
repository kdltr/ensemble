(use utf8)

(include "tui/input.scm")

(define (switch-room room-id)
  (let ((room (alist-ref room-id *rooms*)))
    (if room
        (begin 
          (current-room room-id)
          (wclear statuswin)
          (wattron statuswin A_REVERSE)
          (waddstr statuswin (string-pad-right (sprintf "Room: ~a"
                                                        (or (mref '(name) (room-context room))
                                                            room-id))
                                         (sub1 cols) #\space))
          (redrawwin (room-window room))
          )
        #f)))

(define (find-room regex)
  (define (searched-string ctx)
    (or (mref '(name) ctx)
        (string-concatenate
         (map cdr (alist-delete (mxid) (or (mref '(member-names) ctx) '()) string=?)))
        ""))
  (cond
    ((find (lambda (room-data)
             (irregex-search (irregex regex 'i)
                             (searched-string (room-context (cdr room-data)))))
           *rooms*)
     => car)
    (else
      #f)))

(define (initialize-rooms! batch)
  (for-each
    initialize-room!
    (mref '(rooms join) batch)))

(define (initialize-room! room-data)
  (let* ((room-id (car room-data))
         (events (mref '(state events) (cdr room-data)))
         (win (doto (newwin (- rows 2) cols 0 0)
                    (scrollok #t)
                    (idlok #t)
                    (wmove (- rows 3) 0)))
         (ctx (initial-context events))
         (room (make-room win ctx)))
    (push! (cons room-id room) *rooms*)
    room))

(define (startup)
  (start-interface)
  (let ((first-batch (sync)))
    (initialize-rooms! first-batch)
    (switch-room (caar *rooms*))
    (defer 'sync sync timeout: 30000 since: (handle-sync first-batch))
    (defer 'input get-input)
    (main-loop)))

(define (main-loop)
  (wnoutrefresh (room-window (alist-ref (current-room) *rooms*)))
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
  (info "[~A] waiting for sync~%" (seconds->string))
  (let ((th (gochan-recv ui-chan)))
    (receive (who datum) (thread-join! th)
      (case who
        ((sync) (defer 'sync sync timeout: 30000 since: (handle-sync datum)))
        ((input) (handle-input datum) (defer 'input get-input))
         ))
      )
  (main-loop)
  )

(define (handle-sync batch)
  (set! *batch* batch)
  (info "[~A] update~%" (seconds->string))
  (for-each advance-room (mref '(rooms join) batch))
  (mref '(next_batch) batch))

(define (defer id proc . args)
  (thread-start! (lambda ()
                   (let ((res (handle-exceptions exn exn (apply proc args))))
                     (gochan-send ui-chan (current-thread))
                     (if (condition? res)
                         (signal (list id res))
                         (values id res))))))

(define (get-input)
  (thread-wait-for-i/o! tty-fileno #:input)
  (wget_wch inputwin))



(define (advance-room room-data)
  (let* ((room-id (car room-data))
         (room (or (alist-ref room-id *rooms*)
                   (initialize-room! room-data)))
         (win (room-window room))
         (events (mref '(timeline events) (cdr room-data))))
    (vector-for-each (lambda (i evt)
                       (wprintw win "~%~A" (print-event evt (room-context room)))
                       (room-context-set! room (update-context (room-context room) evt)))
                     events)
  ))
