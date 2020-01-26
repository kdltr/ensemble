;; TODO Better error reporting and recovery
;; TODO Make a queue of messages to send, to avoid reordering
;; TODO send a room-name message when a room name change event is received
;; TODO add unit tests, especially for state updating and event printing procedures
;; TODO treat http throttling errors as retriable

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
  (chicken process)
  (chicken process-context)
  (chicken process-context posix)
  (chicken syntax)
  (chicken time)
  (chicken time posix)
  (chicken type)
  miscmacros
  srfi-1
  srfi-18
  srfi-69
  srfi-71
  utf8
  utf8-srfi-13
  vector-lib
  uri-common
  openssl
  intarweb
  (except medea read-json)
  cjson
  rest-bind
  (prefix http-client |http:|)
  (ensemble libs bindings)
  (ensemble libs concurrency)
  (ensemble libs debug)
  (ensemble libs json-help)
  (ensemble libs locations)
  (ensemble libs nonblocking-ports))

(define *lock-fd*)
(define *profile-dir*)
(define *cache-dir*)
(define *state-file*)
(define *last-known-limit* 10)

(define +ensemble-version+ "dev")
(define +state-cache-version+ 1)

(include-relative "data.scm")
(include-relative "low-level.scm")
(include-relative "client.scm")

(define (run . args)
  ;; Enable server certificate validation for https URIs.
  (http:server-connector
    (make-ssl-server-connector
      (ssl-make-client-context* verify?: (not (member "--no-ssl-verify"
                                                      args)))))
  (cond ((member "-u" args)
         (apply run-as-uploader args))
        ((= 1 (length args))
         (apply run-as-backend args))
        (else
          (error "Usage: backend PROFILE"))))

(define (run-as-backend profile-name . args)
  (enable-warnings #f) ;; suppress warnings about exceptions in threads
  (set! *profile-dir* (make-pathname (config-home) profile-name))
  (set! *cache-dir* (make-pathname (cache-home) profile-name))
  (create-directory *profile-dir* #t)
  (create-directory *cache-dir* #t)
  (set! *state-file* (make-pathname *cache-dir* "state"))
  (set! *lock-fd* (file-open (make-pathname *profile-dir* "lock")
                              (bitwise-ior open/rdwr open/creat)
                              (bitwise-ior perm/irusr perm/iwusr)))
  (handle-exceptions exn
    (error "This profile is already in use")
    (flock *lock-fd*))
  (cond-expand (debug (info-port (open-output-file (string-append "backend-" profile-name ".log"))))
               (else))
  (let ((in-port (open-input-file*/nonblocking 0))
        (out-port (open-output-file*/nonblocking 1)))
    (current-input-port in-port)
    (current-output-port out-port)
    (on-exit (lambda ()
               (info "EXITING")
               (when *next-batch* (save-state))
               (close-output-port out-port)
               (close-input-port in-port))))
  (defer 'rpc read)
  (connect)
  (main-loop))

(define (run-as-uploader profile-name . rest)
  (let ((upload-directive (cdr (member "-u" rest))))
    (unless (= 3 (length upload-directive))
      (error "Usage: backend PROFILE -u ROOM-ID MIME-TYPE FILENAME"))
    (let* ((room-id (find-association profile-name (car upload-directive)))
           (mime-type (cadr upload-directive))
           (filename (caddr upload-directive))
           (data (with-input-from-file filename read-string))
           (message-type (case (string->symbol (car (string-split mime-type "/")))
                           ((audio) "m.audio")
                           ((video) "m.video")
                           ((image) "m.image")
                           (else "m.file"))))
      (set! *profile-dir* (make-pathname (config-home) profile-name))
      (load-profile)
      (room-send room-id
                 'm.room.message
                 (new-transaction-id)
                 `((msgtype . ,message-type)
                   (body . ,(pathname-strip-directory filename))
                   ,@(if (string=? message-type "m.file")
                         `((filename . ,filename))
                         '())
                   (url . ,(mref '(content_uri)
                                 (media-upload (string->symbol mime-type) filename data)))))
      )))

;; TODO put all that on the server as custom room config
;; Window associations should not appear in the backend at all
(define (find-association profile name-or-id)
  (let* ((conf (with-input-from-file (make-pathname (config-home) "window-associations")
                 read-list)))
    (or (and-let* ((_ (not (null? conf)))
                   (assocs (mref '(associations) conf))
                   (assocs (car assocs))
                   (assocs (alist-ref profile assocs string=?))
                   (found (rassoc (string->symbol name-or-id) assocs)))
             (car found))
        name-or-id)))

(define (restart)
  (flock *lock-fd* #f)
  ;; FIXME executable may not be in $PATH, frontend also tries to find
  ;;       it in the current directory
  (process-execute (program-name) (command-line-arguments)))

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
  (ipc-info "Loading profile")
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

(define (save-profile)
  (with-output-to-file (make-pathname *profile-dir* "credentials")
    (lambda ()
      (write `(server-uri . ,(uri->string (server-uri))))
      (write `(access-token . ,(access-token)))
      (write `(mxid . ,(mxid))))))

(define (connect)
  (handle-exceptions exn
    (begin
      (ipc-info "Error: ~a" (exception->string exn))
      (main-loop))
    (load-profile))
  (load-state)
  (for-each
    (lambda (r)
      (send-notifications r)
      (and-let* ((mark (get r 'read-marker)))
           (ipc:read-marker r mark))
      (ipc:room-name r (room-display-name r (room-context r)))
      (ipc:room-members r (room-member-names (room-context r)))
      (send-timeline-events r (room-timeline r)))
    (joined-rooms))
  (ipc-info "Synchronizing…")
  (defer 'initial-sync sync since: *next-batch*))

(define (main-loop)
  (let* ((th (receive-defered))
         (who datum (thread-join-protected! th)))
    (case who
      ((initial-sync)
       (let ((next-batch (handle-sync datum)))
         (ipc-info "Initial synchronization finished! You are in ~a rooms."
                   (length (joined-rooms)))
         (defer 'sync sync timeout: 30000 since: next-batch)))
      ((sync) (defer 'sync sync timeout: 30000 since: (handle-sync datum)))
      ((rpc) (handle-rpc datum) (defer 'rpc read))
      ((hole-messages) (apply fill-hole datum))
      (else  (info "Unknown defered procedure: ~a ~s~%" who datum))
      ))
  (main-loop))

(define (thread-join-protected! thread)
  (define (defered-id-name exn)
    (->string (get-condition-property exn 'defered 'id)))
  (define (do-retry exn)
    (ipc-info "Retrying in ~a seconds"
              (get-condition-property exn 'defered 'waiting-time 0))
    (retry exn))
  (receive data (handle-exceptions exn exn (thread-join! thread))
    (if (uncaught-exception? (car data))
        (condition-case (signal (uncaught-exception-reason (car data)))
          (exn (exn i/o net)
            (ipc-info "Network error (in ~a): ~a"
                      (defered-id-name exn)
                      (exception->string exn))
            (do-retry exn)
            (values 'no-one #f))
          (exn (exn http server-error)
            (ipc-info "Server error (in ~a): ~a"
                      (defered-id-name exn)
                      (exception->string exn))
            (do-retry exn)
            (values 'no-one #f))
          (exn (exn http client-error)
            (ipc-info "Client error (in ~a): ~a"
                      (defered-id-name exn)
                      (exception->string exn))
            (values 'no-one #f))
          (exn (exn http premature-disconnection)
            (ipc-info "Premature disconnection (in ~a): ~a"
                      (defered-id-name exn)
                      (exception->string exn))
            (do-retry exn)
            (values 'no-one #f)))
        (apply values data))))


(define +delayed-reply-marker+ (gensym 'delayed-reply))
(define +error-marker+ (gensym 'error))

(define (handle-rpc exp)
  (info "received: ~s" exp)
  (if (eof-object? exp)
      (exit)
      (handle-exceptions exn
        (begin
          (cond-expand
            (debug (info "Exception caugth: ~a~%" exn
                         (exception->string exn)))
            (else (void)))
          (ipc-info "Error in backend: ~a" (exception->string exn)))
        (let ((procedure (hash-table-ref/default *ipc-procedures* (car exp) #f)))
          (if procedure
              (apply procedure (cdr exp))
              (ipc-info "Error in backend: unknown IPC message received: ~a" exp))))))

(define (exception->string exn)
  (with-output-to-string
    (lambda ()
      (print-error-message exn)
      (print-call-chain))))

;; IPC definitions
;; ===============

(begin-for-syntax
  (define *ipc-receiver* 'backend)
  (define *ipc-send-procedure* 'ipc-send)
  (define *ipc-hash-table* '*ipc-procedures*))
(include-relative "../../ipc.scm")

(define *ipc-procedures* (make-hash-table))

(define (ipc-send exp)
  (info "IPC Sending: ~s" exp)
  (write exp)
  (newline)
  (flush-output))

(define (ipc-info msg . rest)
  (ipc:info (sprintf "[~a] ~?"
                     (time->string (seconds->local-time)
                                   "%d/%m %H:%M")
                     msg rest)))

(define-ipc-implementation (message:text room-id str)
  (let ((transaction-id (new-transaction-id))
        (event-contents `((msgtype . "m.text")
                          (body . ,str))))
    (add-temporary-message! room-id
                            transaction-id
                            event-contents)
    (defer 'message room-send
           room-id 'm.room.message
           transaction-id event-contents)))

(define-ipc-implementation (message:emote room-id str)
  (let ((transaction-id (new-transaction-id))
        (event-contents `((msgtype . "m.emote")
                          (body . ,str))))
    (add-temporary-message! room-id
                            transaction-id
                            event-contents)
    (defer 'message room-send
           room-id 'm.room.message
           transaction-id event-contents)))

(define-ipc-implementation (mark-last-message-as-read room-id)
  (let ((evt-id (mark-last-message-as-read room-id)))
    (ipc:read-marker room-id (string->symbol evt-id))
    (ipc:notifications room-id 0 0)))

(define-ipc-implementation (fill-hole room-id event-id count)
  (let* ((maybe-timeline-from-hole (find-tail (lambda (e) (equal? (alist-ref 'event_id e)
                                                                  (symbol->string event-id)))
                                              (room-timeline room-id))))
    (cond ((not maybe-timeline-from-hole)
           (void))
          ((null? (cdr maybe-timeline-from-hole))
           ;; Hole at the end of the timeline
           (request-hole-messages room-id (car maybe-timeline-from-hole) #f count))
          ;; Hole somewhere in the timeline
          (else
            (let ((hole-event (car maybe-timeline-from-hole))
                  (checkpoint-event (cadr maybe-timeline-from-hole)))
              (request-hole-messages room-id hole-event checkpoint-event count))))))

(define-ipc-implementation (login server username password)
  (delete-file* (make-pathname *profile-dir* "credentials"))
  (delete-file* *state-file*)
  (init! server)
  (password-login username password)
  (ipc-info "Login successful")
  (save-profile)
  (restart))

(define-ipc-implementation (join-room room)
  (set! *rooms-invited* (alist-delete! room *rooms-invited*))
  (defer 'join-room alias-join room '()))

(define-ipc-implementation (leave-room room-id)
  (defer 'leave-room room-leave room-id '()))

(define-ipc-implementation (stop)
  (exit))


;; Temporary messages
;; ==================

(define (event-transaction-id evt)
  (json-true? (mref '(unsigned transaction_id) evt)))

(define (room-temporary-messages room-id)
  (map cdr (or (get room-id 'temporary-messages) '())))

(define (add-temporary-message! room-id txid content)
  (let* ((event `((sender . ,(mxid))
                  (content . ,content)
                  (type . "m.room.message")
                  (origin_server_ts . ,(* 1000 (current-seconds)))))
         (formated (print-event event (room-context room-id)))
         (fake-event `((event_id . ,(string-append "$fake-" txid))
                       (formated . ,formated)
                       (lowlight . #t))))
    (put! room-id 'temporary-messages
      (append! (or (get room-id 'temporary-messages) '())
               (list (cons txid fake-event))))
    (ipc:message room-id fake-event)))

(define (remove-temporary-messages! room-id events)
  (let* ((txids-to-remove (filter-map event-transaction-id events))
         (old-temps (or (get room-id 'temporary-messages) '()))
         (new-temps (remove (lambda (temp) (member (car temp) txids-to-remove))
                            old-temps)))
    (info "removing temps: ~s" txids-to-remove)
    (put! room-id 'temporary-messages new-temps)
    (for-each
      (lambda (txid)
        (ipc:remove room-id (string->symbol (string-append "$fake-" txid))))
      txids-to-remove)))


;; Startup
;; =======
(cond-expand (csi (void)) (else (apply run (command-line-arguments))))

) ;; backend module
