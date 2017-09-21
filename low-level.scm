;; Low-level procedures for Matrix clients’ API

;; Emulates medea’s read-json with cjson
(define (read-json port)
  (string->json (read-string #f port)))

(cond-expand
      (csi (use describe trace)
           (trace read-json json->string))
      (else))

;; Chaque requête :
;; + HTTPS
;; + Accept: application/json
;; + Pour les POST et PUT : Content-Type: application/json
;; + décodage JSON
;; - gestion des codes HTTP
;; - gestion des codes erreur JSON
;; + access_token comme query parameter automatique
;; Préférer les PUT aux POST (idempotence des requêtes)

;; Chaque réponse :
;; - Vérifier le Content-Type
;; - Vérifier le retour de medea

(define +supported-version+ "r0.2.0")

;; The real scheme, host and port will be rewritten when doing the actual requests
(define api-uri (uri-reference "http://PLACEHOLDER/_matrix/client/r0"))

(define server-uri (make-parameter #f))
(define access-token (make-parameter #f))

;; Custom version of call-with-input-request that adds the Matrix access token to
;; the query parameters and fiddles with headers.
;; (this procedure is a free binding in rest-bind defined procedures)
(define (call-with-input-request req writer reader)
  (unless (server-uri)
    (error "Server URI not set, use (init!) first"))
  (let* ((uri-rewritten (update-uri (request-uri req)
                                    scheme: (uri-scheme (server-uri))
                                    host: (uri-host (server-uri))
                                    port: (uri-port (server-uri))
                                    query: (append (if (access-token) `((access_token . ,(access-token))) '())
                                                   (uri-query (request-uri req)))))
         (headers-rewritten (headers (cons* '(accept application/json)
                                            (if (member (request-method req) '(PUT POST))
                                                '((content-type application/json))
                                                '()))
                                     (request-headers req)))
         (request-rewritten (update-request req
                                            uri: uri-rewritten
                                            headers: headers-rewritten)))
    (cond-expand
          (csi (fprintf (current-error-port) "SENDING REQUEST:\n")
               (describe request-rewritten (current-error-port))
               (newline (current-error-port)))
          (else))
    (http:call-with-input-request request-rewritten writer reader)))

(define-syntax define-endpoint
  (syntax-rules (GET POST PUT)
    ((define-endpoint GET decl)
     (define-method decl api-uri #f read-json))
    ((define-endpoint POST decl)
     (define-method decl api-uri json->string read-json))
    ((define-endpoint PUT decl)
     (define-method decl (make-request uri: api-uri method: 'PUT) json->string read-json))))


(define-method (server-versions) "http://PLACEHOLDER/_matrix/client/versions" #f read-json)

(define-endpoint GET (login-schemes "login"))
(define-endpoint POST (login "login"))
(define-endpoint POST (logout "logout"))

(define-endpoint GET (sync "sync" #!key filter since timeout full_state set_presence timeout))

(define-endpoint POST (room-join "rooms" room-id "join"))
(define-endpoint PUT (room-send "rooms" room-id "send" event-type transaction-id))
(define-endpoint GET (room-messages "rooms" room-id "messages" #!key from to dir limit))

(define-endpoint GET (get-filter "user" user-id "filter" filter-id))
(define-endpoint POST (create-filter "user" user-id "filter"))

(define (init! uri)
  (server-uri (uri-reference uri))
  (vector-any (cut equal? +supported-version+ <>)
              (or (alist-ref 'versions (server-versions)) #())))
