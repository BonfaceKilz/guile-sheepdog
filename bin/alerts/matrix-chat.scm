(define-module (alerts matrix-chat)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (uuid generate)
  #:use-module (web client)
  #:export (log-entry->alert-html
	    make-matrix-config
	    matrix-send))

(define-immutable-record-type <matrix-config>
  (make-matrix-config token room-id homeserver)
  matrix-config?
  (token	matrix-config-token      set-matrix-config-token)
  (room-id	matrix-config-room-id    set-matrix-config-room-id)
  (homeserver	matrix-config-homeserver set-matrix-config-homeserver))

(define (matrix-send body-text config)
  (let* ((txn-id (generate-string-uuid))
	 (url (match config
		(($ <matrix-config> token room-id homeserver)
		 (string-append
		  homeserver
		  "/_matrix/client/r0/rooms/"
		  room-id
		  "/send/m.room.message/"
		  txn-id
		  "?access_token="
		  token))
		(_
		 (error "Not a matrix-config"))))
	 (payload (scm->json-string
		   `((msgtype . "m.text")
		     (format . "org.matrix.custom.html")
		     (body . ,body-text)
		     (formatted_body . ,body-text)))))
    (http-put url #:body payload)))

(define (html-escape str)
  (let loop ((chars (string->list str))
             (acc ""))
    (if (null? chars)
        acc
        (let ((c (car chars)))
          (loop (cdr chars)
                (string-append acc
                               (case c
                                 ((#\<) "&lt;")
                                 ((#\>) "&gt;")
                                 ((#\&) "&amp;")
                                 ((#\") "&quot;")
                                 (else (string c)))))))))

(define* (log-entry->alert-html log-entry-hash #:optional (node #f) (app-name #f))
  (let* ((ts (hash-ref log-entry-hash "timestamp" "1970-01-01T00:00:00+03:00"))
	 (lvl (string-upcase (hash-ref log-entry-hash "level" "info")))
	 (evt (hash-ref log-entry-hash "event" "unknown"))
	 (msg (hash-ref log-entry-hash "message" #f))
	 (trace (hash-ref log-entry-hash "exception" #f))
	 (method (hash-ref log-entry-hash "method" #f))
	 (path (hash-ref log-entry-hash "path" #f))
	 (remote_addr (hash-ref log-entry-hash "remote_addr" #f))
	 (user_agent (hash-ref log-entry-hash "user_agent" #f)))
    (string-append
     (if node
	 (string-append "<strong>(" node ")</strong>")
	 "")
     (if app-name
	 (string-append "<strong>(" app-name ")</strong>")
	 "")
     "<strong>ALERT: " lvl "</strong>\n"
     "(" ts ")<br/>\n"
     (if method
	 (string-append "Method: " method "<br/>\n")
	 "")
     (if path
	 (string-append "Path: " path "<br/>\n")
	 "")
     (if remote_addr
	 (string-append "Remote Address: " remote_addr "<br/>\n")
	 "")
     (if user_agent
	 (string-append "User Agent: " user_agent "<br/>\n")
	 "")
     "Event: " (html-escape evt) "\n"
     (if msg
	 (string-append "Message: "
			(html-escape msg) "<br/>\n")
	 "")
     (if trace
	 (string-append "<br/>\n<pre>" (html-escape trace) "</pre>\n")
	 ""))))
