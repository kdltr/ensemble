(module backend (run)
(import
  (except scheme
          string-length string-ref string-set! make-string string substring
          string->list list->string string-fill! write-char read-char display)
  (except chicken
          reverse-list->string print print*)
  (except data-structures
          ->string conc string-chop string-split string-translate
          substring=? substring-ci=? substring-index substring-index-ci)
  ports files posix srfi-1 extras miscmacros
  concurrency debug locations)

(use utf8 utf8-srfi-13 vector-lib uri-common openssl
     intarweb (except medea read-json) cjson
     rest-bind (prefix http-client http:)
     sandbox)

(define +ensemble-version+ "dev")

(include "matrix.scm")
(include "client.scm")

(define (run base-directory)
  (create-directory base-directory #t)
  (change-directory base-directory)
  ;; Enable server certificate validation for https URIs.
  (http:server-connector
    (make-ssl-server-connector
      (ssl-make-client-context* verify?: (not (member "--no-ssl-verify"
                                                      (command-line-arguments))))))
  (load-profile)
  (defer 'rpc read)
  (main-loop))

(define ((make-ssl-server-connector ctx) uri proxy)
  (let ((remote-end (or proxy uri)))
    (if (eq? 'https (uri-scheme remote-end))
        (ssl-connect (uri-host remote-end)
                     (uri-port remote-end)
                     ctx)
        (http:default-server-connector uri proxy))))

(define (load-profile)
  (let ((uri (config-ref 'server-uri)))
    (when uri (init! uri))
    (access-token (config-ref 'access-token))
    (mxid (config-ref 'mxid))))

(define (main-loop)
  (let ((th (receive-defered)))
    (receive (who datum) (thread-join-protected! th)
      (case who
        ((sync) (defer 'sync sync timeout: 30000 since: (handle-sync datum)))
        ((rpc) (handle-rpc datum) (defer 'rpc read))
        ((hole-messages) (apply fill-hole datum))
        (else  (info "Unknown defered procedure: ~a ~s~%" who datum))
      )))
  (main-loop))

(define (handle-rpc exp)
  (safe-eval exp))

) ;; backend module