(void)
(use sql-de-lite)

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
  (exec init-config-stmts)
  (exec init-states-stmts)
  (exec init-events-stmts)
  (exec init-branches-stmts))

(when (not (= 4 (length (schema db))))
  (exec (sql db "DROP TABLE IF EXISTS config;"))
  (exec (sql db "DROP TABLE IF EXISTS events;"))
  (exec (sql db "DROP TABLE IF EXISTS states;"))
  (exec (sql db "DROP TABLE IF EXISTS branches;"))
  (initialize-db))
