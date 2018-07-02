(void)

(define +database-version+ 3)

(define db-file
  (make-pathname (or (get-environment-variable "XDG_CONFIG_HOME")
                     (make-pathname (get-environment-variable "HOME") ".config"))
                 "ensemble.dat"))

(define db (open-database db-file))
(set-busy-handler! db (busy-timeout 10000))
(exec (sql db "PRAGMA foreign_keys = ON;"))

(define init-config-stmts (sql db #<<EOF
CREATE TABLE config (
    key TEXT PRIMARY KEY NOT NULL,
    value TEXT NOT NULL
);
EOF
))

(define init-states-stmts (sql db #<<EOF
CREATE TABLE states(
    id TEXT PRIMARY KEY NOT NULL,
    content TEXT NOT NULL
);
EOF
))

(define init-events-stmts (sql db #<<EOF
CREATE TABLE events(
    id TEXT PRIMARY KEY NOT NULL,
    content TEXT NOT NULL,
    context TEXT NOT NULL,
    FOREIGN KEY(context) REFERENCES states(id)
);
EOF
))

(define init-branches-stmts (sql db #<<EOF
CREATE TABLE branches(
    id TEXT PRIMARY KEY NOT NULL,
    last_state TEXT NOT NULL,
    read_marker TEXT,
    FOREIGN KEY(last_state) REFERENCES states(id)
);
EOF
))

(define init-branches_events-stmts (sql db #<<EOF
CREATE TABLE branches_events(
    id INTEGER PRIMARY KEY ASC AUTOINCREMENT NOT NULL,
    branch_id TEXT NOT NULL,
    sequence_number REAL NOT NULL,
    event_id TEXT NOT NULL,
    UNIQUE(branch_id, sequence_number),
    FOREIGN KEY(branch_id) REFERENCES branches(id),
    FOREIGN KEY(event_id) REFERENCES events(id)
);
EOF
))

(define (initialize-db)
  (exec init-states-stmts)
  (exec init-events-stmts)
  (exec init-branches-stmts)
  (exec init-branches_events-stmts)
  (config-set! 'database-version +database-version+))

(define (sexp->string sexp)
  (with-output-to-string (lambda () (write sexp))))

(define (string->sexp str)
  (if (number? str) str ;; sqlite numerical ids
      (with-input-from-string str read)))

(define (fetch-sexp o)
  (let ((v (fetch-value o)))
    (and v (not (null? v)) (string->sexp v))))

(define (fetch-column-sexps o)
  (map! string->sexp (fetch-column o)))

(define (fetch-row-sexps o)
  (map! string->sexp (fetch-row o)))

(define (fetch-rows-sexps o)
  (let lp ((row (fetch-row-sexps o)))
    (if (null? row)
        '()
        (cons row (lp (fetch-row-sexps o))))))



(define (config-ref key)
  (query fetch-sexp
         (sql db "SELECT value FROM config WHERE key = ?;")
         (sexp->string key)))

(define (config-set! key val)
  (exec (sql db "INSERT OR REPLACE INTO config (key, value) VALUES (?, ?);")
        (sexp->string key) (sexp->string val)))

(define (state-ref id)
  (query fetch-sexp
         (sql db "SELECT content FROM states WHERE id = ?;")
         (sexp->string id)))

(define (state-set! id content)
  (exec (sql db "INSERT OR REPLACE INTO states (id, content) VALUES (?, ?);")
        (sexp->string id) (sexp->string content)))

(define (last-state-ref room-id)
  (query fetch-sexp
         (sql db "SELECT last_state FROM branches WHERE id = ?;")
         (sexp->string room-id)))

#;(define (last-state-set! room-id state-id)
  (exec (sql db "INSERT OR REPLACE INTO branches (id, last_state, read_marker) VALUES (?, ?, ?);")
        (sexp->string room-id) (sexp->string state-id) (sexp->string (read-marker-ref room-id))))

#;(define (read-marker-ref room-id)
  (query fetch-sexp
         (sql db "SELECT read_marker FROM branches WHERE id = ?;")
         (sexp->string room-id)))

#;(define (read-marker-set! room-id event-id)
  (exec (sql db "UPDATE branches SET read_marker = ? WHERE id = ?;")
        (sexp->string (->string event-id)) (sexp->string room-id)))

(define (event-ref id)
  (query fetch-row-sexps
         (sql db "SELECT content, context FROM events WHERE id = ?;")
         (sexp->string id)))

(define (event-set! id content context-id)
  (exec (sql db "INSERT OR REPLACE INTO events (id, content, context) VALUES (?, ?, ?);")
        (sexp->string id) (sexp->string content) (sexp->string context-id)))

#;(define (branch-insert! room-id sequence-number event-id)
  (exec (sql db "INSERT INTO branches_events (branch_id, sequence_number, event_id) VALUES (?, ?, ?);")
        (sexp->string room-id) (sexp->string sequence-number) (sexp->string event-id)))

(define (branch-remove! id)
  (exec (sql db "DELETE FROM branches_events WHERE id = ?;")
        id))

(define (branch-last-sequence-number branch-id)
  (or (query fetch-value
             (sql db "SELECT sequence_number FROM branches_events WHERE branch_id = ? ORDER BY sequence_number DESC;")
             (sexp->string branch-id))
      0))

(define (event-sequence-number branch-event-id)
  (query fetch-value
         (sql db "SELECT sequence_number FROM branches_events WHERE id = ?;")
         (sexp->string branch-event-id)))

#;(define (joined-rooms)
  (query fetch-column-sexps
         (sql db "SELECT id FROM branches;")))

#;(define (any-room)
  (query fetch-sexp
         (sql db "SELECT id FROM branches LIMIT 1;")))

#;(define (room-exists? id)
  (query fetch-sexp
         (sql db "SELECT id FROM branches WHERE id = ?;")
         (sexp->string id)))

#;(define (room-timeline id #!key (limit -1) (offset (branch-last-sequence-number id)))
  (let ((tmp (query fetch-rows-sexps
                    (sql db "SELECT events.content, events.context, branches_events.id, events.id
                         FROM branches_events
                         INNER JOIN events ON branches_events.event_id = events.id
                         WHERE branches_events.branch_id = ? AND branches_events.sequence_number <= ?
                         ORDER BY branches_events.sequence_number DESC
                         LIMIT ?;")
                    (sexp->string id) offset limit)))
    (map (lambda (l) (list (car l) '() (caddr l) (cadddr l)))
         tmp)))

(define (events-previous room-id base-seq limit)
  (query fetch-column-sexps
         (sql db "SELECT sequence_number FROM branches_events WHERE branch_id = ? AND sequence_number < ? ORDER BY sequence_number DESC limit ?;")
         (sexp->string room-id) base-seq limit))

(define (events-next room-id base-seq limit)
  (query fetch-column-sexps
         (sql db "SELECT sequence_number FROM branches_events WHERE branch_id = ? AND sequence_number > ? ORDER BY sequence_number ASC limit ?;")
         (sexp->string room-id) base-seq limit))

(define (event-neighbors room-id event-branch-id)
  (let* ((base-seq (event-sequence-number event-branch-id))
         (next (events-next room-id base-seq 1))
         (prev (events-previous room-id base-seq 1)))
    (list (if (null? prev) #f (car prev)) base-seq (if (null? next) #f (car next)))))

(define state-by-id state-ref)

(define (room-last-state-id-and-state room-id)
  (let ((state-id (last-state-ref room-id)))
    (list state-id (state-by-id state-id))))

(define room-context-and-id room-last-state-id-and-state)

#;(define (room-context room-id) (cadr (room-context-and-id room-id)))

(handle-exceptions exn
  (begin
    (exec init-config-stmts)
    (initialize-db))
  (when (not (= +database-version+ (config-ref 'database-version)))
    (print "Database outdated, refreshing…")
    (exec (sql db "DROP TABLE IF EXISTS branches_events;"))
    (exec (sql db "DROP TABLE IF EXISTS branches;"))
    (exec (sql db "DROP TABLE IF EXISTS events;"))
    (exec (sql db "DROP TABLE IF EXISTS states;"))
    (exec (sql db "DELETE FROM config WHERE key = 'next-batch';"))
    (initialize-db)))
