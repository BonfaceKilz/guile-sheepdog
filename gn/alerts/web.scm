(define-module (gn alerts web)
  #:use-module (gn alerts matrix-chat)
  #:use-module (gn alerts redis)
  #:use-module (gn alerts structlog-monitor)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-26)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:export (start-web-server))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (build-json-response status-code json)
  (list (build-response #:code status-code
                        #:headers `((content-type application/json)))
        (lambda (port)
          (scm->json json port))))

(define (emit-handler node app request body)
  (let* ((payload (json-string->scm (utf8->string body)))
	 (entry (alist->hash-table payload)))
    (if (anomaly? entry)
	(let* ((alert (log-entry->alert-html entry node app))
	       (redis-key (string-append "guile-sheepdog_" (hash-string alert))))
	  ;; Only send an alert if it's not in redis
	  (unless (cached? redis-key)
	    (setenv "MATRIX_TOKEN" (getenv "MATRIX_TOKEN"))
	    (matrix-send alert
			 (make-matrix-config
			  (getenv "MATRIX_USER")
			  (getenv "MATRIX_PASSWORD")
			  (getenv "MATRIX_TOKEN")
			  (getenv "ROOM_ID")
			  (getenv "HOMESERVER")))
	    (cache-html! alert)
	    (build-json-response
	     200 '(("status" . "Message sent to matrix")))))
	(build-json-response
	 500 '(("status" . "Something went wrong"))))))

(define (controller request body)
  (match-lambda
    (('POST "emit" node app)
     (emit-handler node app request body))
    (_
     (build-json-response
      404 '(("status" . "End point doesn't exist"))))))

(define (handler request body)
  (format #t "~a ~a\n"
          (request-method request)
          (uri-path (request-uri request)))
  (apply values
         ((controller request body)
          (cons (request-method request)
                (request-path-components request)))))

(define (start-web-server address port)
  (format (current-error-port)
          "GN REST API web server listening on http://~a:~a/~%" address port)
  (run-server (cut handler <> <>)
              'http
              (list #:addr (inet-pton AF_INET address)
                    #:port port)))
