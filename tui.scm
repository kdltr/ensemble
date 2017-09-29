(include "tui/input.scm")

(define (refresh-statuswin)
  (let* ((notifs (map room-display-name *notifications*))
         (status (sprintf "Room: ~a | ~a" (room-display-name (current-room)) notifs)))
    (werase statuswin)
    (waddnstr statuswin status (sub1 cols))))

(define (refresh-messageswin)
  (let ((timeline (room-timeline (current-room) limit: rows)))
    (werase messageswin)
    (for-each
      (lambda (evt+ctx)
        (let-values (((l c) (getyx messageswin)))
          (unless (zero? c) (wprintw messageswin "~%")))
        (wprintw messageswin "~A" (print-event (car evt+ctx) (cadr evt+ctx))))
      (reverse timeline))))

(define (room-display-name id)
  (let ((ctx (room-context id)))
    (or (room-name ctx)
        (json-true? (mref '(("" . m.room.canonical_alias) alias) ctx))
        (and-let* ((v (json-true? (mref '(("" . m.room.aliases) aliases) ctx))))
             (vector-ref v 0))
        (and-let* ((members (room-members ctx))
                   (check (= (length members) 2))
                   (others (remove (lambda (p) (equal? (caar p) (mxid))) members)))
             (or (member-displayname (caaar others) ctx)
                 (caaar others)))
        (symbol->string id))))

(define (switch-room room-id)
  (let ((room (room-exists? room-id)))
    (if room
        (begin
          (current-room room-id)
          (set! *notifications* (delete! room-id *notifications* eq?))
          (refresh-statuswin)
          (refresh-messageswin)
          )
        #f)))

(define (find-room regex)
  (define (searched-string ctx)
    (or (room-name ctx)
        (json-true? (mref '(("" . m.room.canonical_alias) alias) ctx))
        (and-let* ((v (json-true? (mref '(("" . m.room.aliases) aliases) ctx))))
             (vector-ref v 0))
        (string-join
         (filter-map (lambda (p)
                       (and (equal? (cdar p) 'm.room.member)
                            (or (member-displayname (caar p) ctx)
                                (caar p))))
                     ctx))
        ""))
  (find (lambda (room-id)
          (irregex-search (irregex regex 'i)
                          (searched-string (room-context room-id))))
        (joined-rooms)))

(define (initialize-room! room-data)
  (let* ((room-id (car room-data))
         (events (mref '(state events) (cdr room-data)))
         (state (initial-context events))
         (state-id (mref '(event_id) (vector-ref events (sub1 (vector-length events))))))
    (state-set! state-id state)
    (list state-id state)))

(define (startup)
  (set! (signal-handler signal/winch)
    (lambda (_) (defer 'resize (lambda () #t))))
  (start-interface)
  (let* ((first-batch (sync since: (config-ref 'next-batch)))
         (next (handle-sync first-batch #f)))
    (switch-room (any-room))
    (defer 'sync sync timeout: 30000 since: next)
    (defer 'input get-input)
    (refresh-statuswin)
    (main-loop)))

(define (main-loop)
  (wnoutrefresh messageswin)
  (wnoutrefresh statuswin)
  (wnoutrefresh inputwin)
  (doupdate)
  (let ((th (gochan-recv ui-chan)))
    (receive (who datum) (thread-join! th)
      (case who
        ((sync) (defer 'sync sync timeout: 30000 since: (handle-sync datum)))
        ((input) (handle-input datum) (defer 'input get-input))
        ((resize) (resize-terminal))
         ))
      )
  (main-loop)
  )

(define (handle-sync batch #!optional (update-ui #t))
  (let ((next (mref '(next_batch) batch)))
    (info "[~A] update~%" (seconds->string))
    (with-transaction db
      (lambda ()
        (for-each (cut advance-room <> update-ui) (mref '(rooms join) batch))
        (config-set! 'next-batch next)))
    next))

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

(define (advance-room room-data #!optional (update-ui #t))
  (let* ((room-id (car room-data))
         (events (mref '(timeline events) (cdr room-data)))
         (init-ctx (if (room-exists? room-id) #f (initialize-room! room-data))))
    (vector-for-each (lambda (i evt)
                       (let* ((id+old-ctx (or init-ctx (room-context-and-id room-id)))
                              (prev-ctx-id (car id+old-ctx))
                              (old-ctx (cadr id+old-ctx))
                              (new-ctx (update-context old-ctx evt))
                              (evt-id (mref '(event_id) evt)))
                         (event-set! evt-id evt prev-ctx-id)
                         (branch-insert! room-id evt-id)
                         (when (not (eq? old-ctx new-ctx))
                           (state-set! evt-id evt))
                         ))
                     events)
    (when (and update-ui (not (vector-empty? events)))
      (if (eq? (current-room) room-id)
          (refresh-messageswin)
          (begin
            (set! *notifications* (lset-adjoin eq? *notifications* room-id))
            (refresh-statuswin))))
  ))

(define (resize-terminal)
  (let ((rows+cols (ioctl-winsize tty-fileno)))
    (set! rows (car rows+cols))
    (set! cols (cadr rows+cols))
    (resizeterm rows cols)
    (wresize messageswin (- rows 2) cols)
    (mvwin messageswin 0 0)
    (wresize statuswin 1 cols)
    (mvwin statuswin (- rows 2) 0)
    (wresize inputwin 1 cols)
    (mvwin inputwin (- rows 1) 0)
    (refresh-messageswin)
    (refresh-statuswin)
    (refresh-inputwin)))
