(include "tui/input.scm")

(define (refresh-statuswin)
  (let* ((notifs (filter-map (lambda (r)
                               (and (room-notification (cdr r))
                                    (room-name (car r))))
                             *rooms*))
         (status (sprintf "Room: ~a | ~a" (room-name (current-room)) notifs)))
    (werase statuswin)
    (waddnstr statuswin status (sub1 cols))))

(define (room-name id)
  (let* ((ctx (room-context (alist-ref id *rooms*)))
         (members (or (alist-ref 'members ctx) '()))
         (member-names (alist-ref 'member-names ctx))
         (others (delete (mxid) members)))
    (or (alist-ref 'name ctx)
        (and (= (length members) 2)
             (or (alist-ref (car others) member-names string=?)
                 (car others)))
        (symbol->string id))))

(define (switch-room room-id)
  (let ((room (alist-ref room-id *rooms*)))
    (if room
        (begin
          (current-room room-id)
          (room-notification-set! room #f)
          (refresh-statuswin)
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
         (room (make-room win ctx #f)))
    (push! (cons room-id room) *rooms*)
    room))

(define (startup)
  (start-interface)
  (let ((first-batch (sync)))
    (initialize-rooms! first-batch)
    (switch-room (caar *rooms*))
    (defer 'sync sync timeout: 30000 since: (handle-sync first-batch))
    (defer 'input get-input)
    (for-each (lambda (r) (room-notification-set! (cdr r) #f)) *rooms*)
    (refresh-statuswin)
    (main-loop)))

(define (main-loop)
  (wnoutrefresh (room-window (alist-ref (current-room) *rooms*)))
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
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
    (unless (or (eq? (current-room) room-id) (vector-empty? events))
      (room-notification-set! room #t)
      (refresh-statuswin))
  ))
