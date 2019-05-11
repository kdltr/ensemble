;; (define-ipc-spec NAME RECEIVER ANNOTATED-DSSSL-LAMBDA-LIST)
;; Annonated lambda list element: (name type)


;; Backend -> Frontend
;; ===================

(define-ipc-spec frontend
  (bundle-start))

(define-ipc-spec frontend
  (notifications (room-id symbol)
                 (new-hls integer)
                 (new-notifs integer)))

(define-ipc-spec frontend
  (clear (room-id symbol)))

(define-ipc-spec frontend
  (refresh (room-id symbol)))

(define-ipc-spec frontend
  (response (id integer)
            (datum *)))

(define-ipc-spec frontend
  (read-marker (room-id symbol)
               (event-id symbol)))

(define-ipc-spec frontend
  (room-name (room-id symbol)
             (room-name string)))

(define-ipc-spec frontend
  (message (room-id symbol)
           (message pair)))

(define-ipc-spec frontend
  (info (message string)))


;; Frontend -> backend
;; ===================

(define-ipc-spec backend
  (subscribe (room-id symbol)))

(define-ipc-spec backend
  (unsubscribe (room-id symbol)))

(define-ipc-spec backend
  (fetch-events (room-id symbol)
                (limit integer)
                (offset integer)))

(define-ipc-spec backend
  (message:text (room-id symbol)
                (str string)))

(define-ipc-spec backend
  (message:emote (room-id symbol)
                 (str string)))

(define-ipc-spec backend
  (mark-last-message-as-read (room-id symbol)))

(define-ipc-spec backend
  (login (server string) 
         (username string)
         (password string)))

(define-ipc-spec backend
  (join-room (room string)))

(define-ipc-spec backend
  (leave-room (room-id string)))

(define-ipc-spec backend
  (query (query-id integer)
         (what symbol)
         #!rest (args *)))
