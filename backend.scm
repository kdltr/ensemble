(include "debug.scm")
(include "locations.scm")
(include "nonblocking-ports.scm")
(include "concurrency.scm")

;; TODO Better error reporting and recovery
;; TODO Empty profile
;; TODO Environment variables for config and cache/state directories
;; TODO Show the result of message sending and read marker immediately

(module backend (run)
(import
  scheme
  (chicken base)
  (chicken condition)
  (chicken file)
  (chicken format)
  (chicken io)
  (chicken irregex)
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
  concurrency
  debug
  locations
  nonblocking-ports)

(define +ensemble-version+ "dev")

(define rpc-env (make-safe-environment name: 'rpc-environment
                                       mutable: #f
                                       extendable: #f))

(include "matrix.scm")
(include "client.scm")

(define (run)
  (current-error-port (open-output-file "backend.log"))
  (info-port (current-error-port))
  (current-input-port (open-input-file*/nonblocking 0))
  (current-output-port (open-output-file*/nonblocking 1))
  ;; Enable server certificate validation for https URIs.
  (http:server-connector
    (make-ssl-server-connector
      (ssl-make-client-context* verify?: (not (member "--no-ssl-verify"
                                                      (command-line-arguments))))))
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
  (let* ((creds (handle-exceptions exn '() (read-file "credentials")))
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
  (main-loop))

(define +delayed-reply-marker+ (gensym 'delayed-reply))
(define +error-marker+ (gensym 'error))

(define (handle-rpc exp)
  (info "received: ~s" exp)
  (set! *frontend-idling* #f)
  (if (eof-object? exp)
      (save-and-exit)
      (let (((values first . rest) (handle-exceptions exn
                                     (values +error-marker+ exn)
                                     (safe-eval exp environment: rpc-env))))
        (unless (eqv? first +delayed-reply-marker+)
          (let ((response (if (eqv? first +error-marker+)
                              (list 'error (exception->string (car rest)))
                              (cons* 'values first rest))))
            (info "replied: ~s" response)
            (write response)
            (newline))))))

(define (save-and-exit)
  (when *next-batch* (save-state))
  (exit))

(define (exception->string exn)
  (with-output-to-string
    (lambda ()
      (print-error-message exn (current-output-port) "Backend error"))))



;; RPC Procedures
;; ==============

(define *frontend-idling* #f)
(define *frontend-idle-msgs* '())

(define (notify-frontend type)
  (if *frontend-idling*
      (begin
        (info "replied: ~s" type)
        (print type))
      (unless (memv type *frontend-idle-msgs*)
        (push! type *frontend-idle-msgs*)))
  (set! *frontend-idling* #f))

(safe-environment-set!
  rpc-env 'idle
  (lambda ()
    (if (null? *frontend-idle-msgs*)
        (begin
          (set! *frontend-idling* #t)
          +delayed-reply-marker+)
        (begin
          (write (pop! *frontend-idle-msgs*))
          (newline)
          +delayed-reply-marker+))))

(safe-environment-set!
  rpc-env 'stop
  (lambda ()
    (write 'stopped)
    (newline)
    +delayed-reply-marker+))


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

(safe-environment-set!
  rpc-env 'find-room find-room)


(safe-environment-set!
  rpc-env 'connect
  (lambda ()
    (load-profile)
    (load-state)
    (let ((batch (sync since: *next-batch*)))
      (defer 'sync sync timeout: 30000 since: (handle-sync batch))
      #t)))

(safe-environment-set!
  rpc-env 'fetch-events
  (lambda (room-id limit)
    (let* ((tl (room-timeline room-id limit: limit))
           (holes (filter hole-event? tl)))
      (for-each
        (lambda (hole)
          ;; TODO hide this state manipulation in request-hole-messages
          (unless (member hole *requested-holes*)
            (push! hole *requested-holes*)
            (defer 'hole-messages request-hole-messages room-id hole limit)))
        holes)
      tl)))

(safe-environment-set!
  rpc-env 'any-room any-room)

(safe-environment-set!
  rpc-env 'message:text
  (lambda (room-id str)
    (message:text room-id str)
    #t))

(safe-environment-set!
  rpc-env 'message:emote
  (lambda (room-id str)
    (message:emote room-id str)
    #t))

(safe-environment-set!
  rpc-env 'fetch-notifications
  (lambda ()
    (filter (lambda (r)
              (let ((n (get r 'notifications)))
                (and n (not (zero? n)))))
            *rooms*)))

(safe-environment-set!
  rpc-env 'fetch-highlights
  (lambda ()
    (filter (lambda (r)
              (let ((n (get r 'highlights)))
                (and n (not (zero? n)))))
            *rooms*)))

(safe-environment-set!
  rpc-env 'room-display-name room-display-name)

(safe-environment-set!
  rpc-env 'mark-last-message-as-read
  (lambda (id)
    (mark-last-message-as-read id)
    #t))

(safe-environment-set!
  rpc-env 'read-marker
  (lambda (id)
    (let ((marker (read-marker-ref id)))
      (and marker (symbol->string marker)))))

(safe-environment-set!
  rpc-env 'room-members
  (lambda (id)
    (let* ((ctx (room-context id))
           (members (room-members ctx)))
      (map
        (lambda (m)
          (or (json-true? (mref '(displayname) m))
              (caar m)))
        members))))

(cond-expand (csi (void)) (else (run)))

) ;; backend module