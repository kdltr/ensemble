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
          (show-win (room-window room)))
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
    (make-sync-thread (handle-sync first-batch))
    (make-input-thread)
    (main-loop)))

(define (main-loop)
  (info "[~A] waiting for sync~%" (seconds->string))
  (let ((th (gochan-recv ui-chan)))
    (receive (who datum) (thread-join! th)
      (case who
        ((sync) (make-sync-thread (handle-sync datum)))
        ((input) (register-input datum) (make-input-thread))
         ))
      )
  (main-loop)
  )

(define (handle-sync batch)
  (set! *batch* batch)
  (info "[~A] update~%" (seconds->string))
  (for-each advance-room (mref '(rooms join) batch))
  (mref '(next_batch) batch))

(define (make-sync-thread since)
    (thread-start! (make-thread (lambda ()
                                  (handle-exceptions exn
                                    (begin
                                      (gochan-send ui-chan (current-thread))
                                      (signal (list 'sync exn)))
                                    (let ((new-batch (sync timeout: 30000 since: since)))
                                      (gochan-send ui-chan (current-thread))
                                      (values 'sync new-batch))))
                   )))

(define (make-input-thread)
  (thread-start! (lambda ()
                   (thread-wait-for-i/o! tty-fileno #:input)
                   (gochan-send ui-chan (current-thread))
                   (values 'input (wget_wch inputwin)))
                 ))

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
    (when (eq? room-id (current-room))
      (show-win win))
  ))

(define (show-win win)
  (wrefresh win)
  (wrefresh statuswin)
  (wrefresh inputwin))
