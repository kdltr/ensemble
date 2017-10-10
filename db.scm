(void)

(define +database-version+ 2)

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
    FOREIGN KEY(last_state) REFERENCES states(id)
);
EOF
))

(define init-branches_events-stmts (sql db #<<EOF
CREATE TABLE branches_events(
    sequence_number INTEGER PRIMARY KEY ASC AUTOINCREMENT NOT NULL,
    branch_id TEXT NOT NULL,
    event_id TEXT NOT NULL,
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
  (with-input-from-string str read))

(define (fetch-sexp o)
  (let ((v (fetch-value o)))
    (and v (string->sexp v))))

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

(define (last-state-set! room-id state-id)
  (exec (sql db "INSERT OR REPLACE INTO branches (id, last_state) VALUES (?, ?);")
        (sexp->string room-id) (sexp->string state-id)))

#;(define (event-ref id)
  (query fetch-sexps
         (sql db "SELECT content, context FROM events WHERE id = ?;")
         (sexp->string id)))

(define (event-set! id content context-id)
  (exec (sql db "INSERT OR REPLACE INTO events (id, content, context) VALUES (?, ?, ?);")
        (sexp->string id) (sexp->string content) (sexp->string context-id)))

(define (branch-insert! room-id event-id)
  (exec (sql db "INSERT INTO branches_events (branch_id, event_id) VALUES (?, ?);")
        (sexp->string room-id) (sexp->string event-id)))

(define (joined-rooms)
  (query fetch-column-sexps
         (sql db "SELECT id FROM branches WHERE id LIKE '!%';")))

(define (any-room)
  (query fetch-sexp
         (sql db "SELECT id FROM branches WHERE id LIKE '!%' LIMIT 1;")))

(define (room-exists? id)
  (query fetch-sexp
         (sql db "SELECT id FROM branches WHERE id = ?;")
         (sexp->string id)))

(define (room-timeline id #!key (limit -1) (offset 0))
  (let ((tmp (query fetch-rows-sexps
                    (sql db "SELECT events.content, events.context
                         FROM branches_events
                         INNER JOIN events ON branches_events.event_id = events.id
                         WHERE branches_events.branch_id = ?
                         ORDER BY branches_events.sequence_number DESC
                         LIMIT ? OFFSET ?;")
                    (sexp->string id) limit offset)))
    (map (lambda (l) (list (car l) (state-by-id (cadr l))))
         tmp)))

(define *state-cache* (make-lru-cache 100 equal?))

(define (state-by-id state-id)
  (or (lru-cache-ref *state-cache* state-id)
      (let ((state (state-ref state-id)))
        (lru-cache-set! *state-cache* state-id state)
        state)))

(define (room-last-state-id-and-state room-id)
  (let ((state-id (last-state-ref room-id)))
    (list state-id (state-by-id state-id))))

(define room-context-and-id room-last-state-id-and-state)

(define (room-context room-id) (cadr (room-context-and-id room-id)))

(handle-exceptions exn
  (begin
    (exec init-config-stmts)
    (initialize-db))
  (when (not (= +database-version+ (config-ref 'database-version)))
    (exec (sql db "DROP TABLE IF EXISTS events;"))
    (exec (sql db "DROP TABLE IF EXISTS states;"))
    (exec (sql db "DROP TABLE IF EXISTS branches;"))
    (exec (sql db "DROP TABLE IF EXISTS branches_events;"))
    (exec (sql db "DELETE FROM config WHERE key = 'next-batch';"))
    (initialize-db)))
