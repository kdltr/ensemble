(void)

(define +database-version+ 1)

(define db-file
  (make-pathname (or (get-environment-variable "XDG_CONFIG_HOME")
                     (make-pathname (get-environment-variable "HOME") ".config"))
                 "ensemble.dat"))

(define db (open-database db-file))
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
    sequence_number INTEGER PRIMARY KEY ASC AUTOINCREMENT NOT NULL,
    id TEXT NOT NULL,
    event TEXT NOT NULL,
    FOREIGN KEY(event) REFERENCES events(id)
);
EOF
))

(define (initialize-db)
  (exec init-states-stmts)
  (exec init-events-stmts)
  (exec init-branches-stmts))

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

#;(define (event-ref id)
  (query fetch-sexps
         (sql db "SELECT content, context FROM events WHERE id = ?;")
         (sexp->string id)))

(define (event-set! id content context-id)
  (exec (sql db "INSERT OR REPLACE INTO events (id, content, context) VALUES (?, ?, ?);")
        (sexp->string id) (sexp->string content) (sexp->string context-id)))

(define (branch-insert! room-id event-id)
  (exec (sql db "INSERT INTO branches (id, event) VALUES (?, ?);")
        (sexp->string room-id) (sexp->string event-id)))

(define (joined-rooms)
  (query fetch-column-sexps
         (sql db "SELECT id FROM branches WHERE id LIKE '!%' GROUP BY id;")))

(define (any-room)
  (query fetch-sexp
         (sql db "SELECT id FROM branches WHERE id LIKE '!%' LIMIT 1;")))

(define (room-exists? id)
  (query fetch-sexp
         (sql db "SELECT id FROM branches WHERE id = ?;")
         (sexp->string id)))

(define (room-timeline id #!key (limit -1) (offset 0))
  (query fetch-rows-sexps
         (sql db "SELECT events.content, states.content
                  FROM branches
                  INNER JOIN events ON branches.event = events.id
                  INNER JOIN states ON events.context = states.id
                  WHERE branches.id = ?
                  ORDER BY branches.sequence_number DESC
                  LIMIT ? OFFSET ?;")
         (sexp->string id) limit offset))

(define (room-context-and-id room-id)
  (query fetch-row-sexps
         (sql db "SELECT states.id, states.content
                  FROM branches
                  INNER JOIN events ON branches.event = events.id
                  INNER JOIN states ON events.context = states.id
                  WHERE branches.id = ?
                  ORDER BY branches.sequence_number DESC LIMIT 1;")
         (sexp->string room-id)))

(define (room-context room-id) (cadr (room-context-and-id room-id)))

(handle-exceptions exn
  (begin
    (initialize-db)
    (exec init-config-stmts)
    (config-set! 'database-version +database-version+))
  (when (not (= +database-version+ (config-ref 'database-version)))
    (exec (sql db "DROP TABLE IF EXISTS events;"))
    (exec (sql db "DROP TABLE IF EXISTS states;"))
    (exec (sql db "DROP TABLE IF EXISTS branches;"))
    (initialize-db)))
