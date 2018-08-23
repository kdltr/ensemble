;; TODO Better error reporting and recovery
;; TODO Empty profile
;; TODO Environment variables for config and cache/state directories
;; TODO Show the result of message sending and read marker immediately
;; TODO profile locking to avoid multiple instances using the same profiles

(module (ensemble backend matrix) (run)
(import
  scheme
  (chicken base)
  (chicken bitwise)
  (chicken condition)
  (chicken file)
  (chicken file posix)
  (chicken format)
  (chicken io)
  (chicken irregex)
  (chicken pathname)
  (chicken plist)
  (chicken port)
  (chicken process-context)
  (chicken time)
  (chicken time posix)
  miscmacros
  srfi-1
  srfi-71
  utf8
  utf8-srfi-13
  utf8-srfi-14
  vector-lib
  uri-common
  openssl
  intarweb
  (except medea read-json)
  cjson
  rest-bind
  (prefix http-client #:http)
  sandbox
  (ensemble libs concurrency)
  (ensemble libs debug)
  (ensemble libs locations)
  (ensemble libs nonblocking-ports))

(define *lock-fd*)
(define *profile-dir*)
(define *cache-dir*)
(define *state-file*)

(define +ensemble-version+ "dev")

(define rpc-env (make-safe-environment name: 'rpc-environment
                                       mutable: #f
                                       extendable: #f))

(include-relative "matrix.scm")
(include-relative "client.scm")

(define (run . args)
  (when (not (= 1 (length args)))
    (error "Usage: backend PROFILE"))
  (set! *profile-dir* (make-pathname (config-home) (car args)))
  (set! *cache-dir* (make-pathname (cache-home) (car args)))
  (create-directory *profile-dir* #t)
  (create-directory *cache-dir* #t)
  (set! *state-file* (make-pathname *cache-dir* "state"))
  (set! *lock-fd* (file-open (make-pathname *profile-dir* "lock")
                              (bitwise-ior open/rdwr open/creat)
                              (bitwise-ior perm/irusr perm/iwusr)))
  (handle-exceptions exn
    (error "This profile is already in use")
    (file-lock (open-output-file* *lock-fd*)))
  (let ((err-port (open-output-file (make-pathname *profile-dir* "backend.log")))
        (in-port (open-input-file*/nonblocking 0))
        (out-port (open-output-file*/nonblocking 1)))
    (current-error-port err-port)
    (info-port err-port)
    (current-input-port in-port)
    (current-output-port out-port)
    (on-exit (lambda ()
               (info "EXITING")
               (when *next-batch* (save-state))
               (close-output-port err-port)
               (close-output-port out-port)
               (close-input-port in-port))))
  ;; Enable server certificate validation for https URIs.
  (http:server-connector
    (make-ssl-server-connector
      (ssl-make-client-context* verify?: (not (member "--no-ssl-verify"
                                                      args)))))
  (defer 'rpc read)
  (main-loop))

(define ((make-ssl-server-connector ctx) uri proxy)
  (let ((remote-end (or proxy uri)))
    (if (eq? 'https (uri-scheme remote-end))
        (ssl-connect (uri-host remote-end)
                     (uri-port remote-end)
                     ctx)
        (http:default-server-connector uri proxy))))

(define (read-file file-name)
  (with-input-from-file file-name read-list))

(define (load-profile)
  (let* ((creds (handle-exceptions exn '()
                  (read-file (make-pathname *profile-dir* "credentials"))))
         (c:server-uri (alist-ref 'server-uri creds))
         (c:access-token (alist-ref 'access-token creds))
         (c:mxid (alist-ref 'mxid creds)))
    (unless (and c:server-uri c:access-token c:mxid)
      (error "Please log in"))
    (init! c:server-uri)
    (access-token c:access-token)
    (mxid c:mxid)))

(define (main-loop)
  (let* ((th (receive-defered))
         (who datum (thread-join-protected! th)))
    (case who
      ((sync) (defer 'sync sync timeout: 30000 since: (handle-sync datum)))
      ((rpc) (handle-rpc datum) (defer 'rpc read))
      ((hole-messages) (apply fill-hole datum))
      (else  (info "Unknown defered procedure: ~a ~s~%" who datum))
      ))
  (flush-delayed-responses)
  (main-loop))

(define +delayed-reply-marker+ (gensym 'delayed-reply))
(define +error-marker+ (gensym 'error))

(define (handle-rpc exp)
  (info "received: ~s" exp)
  (if (eof-object? exp)
      (exit)
      (handle-exceptions exn
        (begin
          (cond-expand
            (debug (info "Exception caugth: ~a~%~a" exn
                         (exception->string exn))
                   (print-call-chain (info-port)))
            (else (void)))
          (ipc-info "Error in backend: ~a" (exception->string exn)))
        (let ((quoted-exp (cons (car exp)
                                (map (lambda (o) (list 'quote o))
                                     (cdr exp)))))
          (safe-eval quoted-exp environment: rpc-env)))))

(define (exception->string exn)
  (with-output-to-string
    (lambda ()
      (print-error-message exn (current-output-port) "Backend error"))))



;; IPC definitions
;; ===============

(define (ipc-send . exps)
  (info "IPC Sending: ~s" exps)
  (write exps)
  (newline)
  (flush-output))

(define (ipc-info msg . rest)
  (ipc-send 'info (sprintf "~?" msg rest)))

(define *delayed-responses* '())
(define +delay-marker+ (gensym 'delay))

(define (delay-response pred thunk)
  (push! (cons pred thunk) *delayed-responses*))

(define (flush-delayed-responses)
  (info "Number of delayed responses: ~a" (length *delayed-responses*))
  (set! *delayed-responses*
    (fold (lambda (delayed rest)
            (if ((car delayed))
                (begin ((cdr delayed)) rest)
                (cons delayed rest)))
          '()
          *delayed-responses*)))

;; ASYNC IPC calls

(safe-environment-set!
  rpc-env 'connect
  (lambda ()
    (load-profile)
    (load-state)
    (ipc-info "Connecting…")
    (for-each send-notifications (joined-rooms))
    (defer 'sync sync since: *next-batch*)))

(safe-environment-set!
  rpc-env 'subscribe
  (lambda (room-id)
    (cond ((memv room-id (joined-rooms))
           (put! room-id 'frontend-subscribed #t)
           (and-let* ((mark (get room-id 'read-marker)))
                (ipc-send 'read-marker room-id mark)))
          (else (ipc-info "Unable to subscribe to unknown room: ~a" room-id)))))

(safe-environment-set!
  rpc-env 'unsubscribe
  (lambda (room-id)
    (put! room-id 'frontend-subscribed #f)))

(safe-environment-set!
  rpc-env 'fetch-events
  (lambda (room-id limit offset)
    (let* ((tl (room-timeline room-id limit: limit offset: offset))
           (holes (filter hole-event? tl)))
      (for-each
        (lambda (hole)
          ;; TODO hide this state manipulation in request-hole-messages
          (unless (member hole *requested-holes*)
            (push! hole *requested-holes*)
            (defer 'hole-messages request-hole-messages room-id hole limit)))
        holes)
      (ipc-send 'bundle-start)
      (ipc-send 'clear room-id)
      (for-each
        (lambda (m)
          (ipc-send 'message room-id (cleanup-event m)))
        (reverse tl))
      (ipc-send 'bundle-end))))

(safe-environment-set!
  rpc-env 'message:text
  (lambda (room-id str)
    (message:text room-id str)))

(safe-environment-set!
  rpc-env 'message:emote
  (lambda (room-id str)
    (message:emote room-id str)))

(safe-environment-set!
  rpc-env 'mark-last-message-as-read
  (lambda (id)
    (mark-last-message-as-read id)))


;; Synchronous IPC calls

(safe-environment-set!
  rpc-env 'query
  (lambda (query-id what . args)
    (let ((pred (case what
                  ((any-room joined-rooms) (lambda () (pair? (joined-rooms))))
                  (else yes)))
          (proc (case what
                  ((find-room) find-room)
                  ((any-room) any-room)
                  ((joined-rooms) joined-rooms)
                  ((room-members) query-room-members)
                  ((room-display-name) room-display-name)
                  ((read-marker) read-marker)
                  (else oops))))
      (delay-response
        pred
        (lambda ()
          (ipc-send 'response query-id (apply proc args)))))))

(define (oops . args)
  (ipc-info "Wrong query! ~s" args)
  'oops)

(define (yes) #t)

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

(define (query-room-members id)
  (let* ((ctx (room-context id))
         (members (room-members ctx)))
    (map
      (lambda (m)
        (or (json-true? (mref '(displayname) m))
            (caar m)))
      members)))

(define (read-marker id)
  (let ((marker (read-marker-ref id)))
    (and marker (symbol->string marker))))


(cond-expand (csi (void)) (else (apply run (command-line-arguments))))

) ;; backend module