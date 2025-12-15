(define-module (gn alerts redis)
  #:use-module (redis)
  #:use-module (redis commands)
  #:use-module (srfi srfi-26)
  #:use-module ((gn alerts structlog-monitor) #:select (hash-string))
  #:export (cache-html!
	    cached?))

(define* (call-with-redis proc #:key (host "127.0.0.1") (port 6379))
  (let ((conn #f))
    (dynamic-wind (lambda ()
		    (set! conn (redis-connect)))
		  (cut proc conn)
		  (cut redis-close conn))))

(define* (cache-html! html
		      #:key
		      ;; 12 hours
		      (ttl 43200)
		      (host "127.0.0.1")
		      (port 6379))
  (let ((key (string-append "guile-sheepdog_" (hash-string html))))
    (call-with-redis
     (lambda (conn)
       (redis-send conn
		   (setex (list key (number->string ttl) html))))
     #:host host #:port port)
    key))

(define* (cached? key
		  #:key
		  (host "127.0.0.1")
		  (port 6379))
  (call-with-redis
   (lambda (conn)
     (> (redis-send conn (ttl (list key)))
	0))
   #:host host #:port port))
