;; Low-level procedures for Matrix clients’ API

;; Emulates medea’s read-json with cjson
(define (read-json port)
  (string->json (read-string #f port)))

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

(define +supported-version+ "r0.4.0")

;; The real scheme, host and port will be rewritten when doing the actual requests
(define api-uri (uri-reference "http://PLACEHOLDER/_matrix/client/r0"))

(define mxid (make-parameter #f))
(define server-uri (make-parameter #f))
(define access-token (make-parameter #f))

;; http-client configuration

(http:max-retry-attempts 0)
(http:client-software `(("Ensemble Matrix client" ,+ensemble-version+ #f)))

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
         (hdrs (request-headers req))
         (headers-rewritten (headers (cons* '(accept application/json)
                                            (if (and (member (request-method req) '(PUT POST))
                                                     (not (header-value 'content-type hdrs #f)))
                                                '((content-type application/json))
                                                '()))
                                     hdrs))
         (request-rewritten (update-request req
                                            uri: uri-rewritten
                                            headers: headers-rewritten)))
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

(define-endpoint POST (alias-join "join" room-id-or-alias))
(define-endpoint POST (room-join "rooms" room-id "join"))
(define-endpoint POST (room-leave "rooms" room-id "leave"))
(define-endpoint PUT (room-send "rooms" room-id "send" event-type transaction-id))
(define-endpoint GET (room-messages "rooms" room-id "messages" #!key from to dir limit))
(define-endpoint POST (room-receipt "rooms" room-id "receipt" receipt-type event-id))
(define-endpoint POST (room-read-markers "rooms" room-id "read_markers"))

(define-endpoint GET (get-filter "user" user-id "filter" filter-id))
(define-endpoint POST (create-filter "user" user-id "filter"))

(define (init! uri)
  (server-uri (uri-reference uri))
  (vector-any (cut equal? +supported-version+ <>)
              (or (alist-ref 'versions (server-versions)) #())))


;; Media endpoints

(define media-api-uri (uri-reference "http://PLACEHOLDER/_matrix/media/r0"))

(define-method (media-config "config") media-api-uri #f read-json)

(define (media-upload mime filename data)
  (call-with-input-request
    (make-request uri: (update-uri media-api-uri
                                   path: (append (uri-path media-api-uri) '("upload"))
                                   query: `((filename . ,filename)))
                  headers: (headers `((content-type ,mime)))
                  method: 'POST)
    data
    read-json))
