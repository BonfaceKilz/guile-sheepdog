(define-module (alerts structlog-monitor)
  #:use-module (hashing md5)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9 gnu)
  #:export (hash-string
	    make-structlog-monitor-config
	    monitor))

(define-immutable-record-type <structlog-monitor-config>
  (make-structlog-monitor-config log-file poll-interval)
  structlog-monitor-config?
  (log-file structlog-monitor-config-log-file set-structlog-monitor-config-log-file)
  (poll-interval structlog-monitor-config-poll-interval set-structlog-monitor-config-poll-interval))

(define (hash-string str)
  (md5->string
   (md5 (string->utf8 str))))

(define (anomaly? log-entry-hash)
  (let ((level (hash-ref log-entry-hash "level"))
	(event (hash-ref log-entry-hash "event" ""))
	(msg (hash-ref log-entry-hash "msg" "")))
    (or (member level '("error" "critical" "exception" "warning"))
	(string-contains-ci event "fail")
	(string-contains-ci event "error")
	(string-contains-ci event "timeout")
	(string-contains-ci msg "ConnectionError|5\\d\\d"))))

(define (make-string-change-detector initial-value)
  (let ((last initial-value))
    (lambda (new)
      (let ((changed (not (string=? new last))))
        (set! last new)
        initial-value))))

(define (monitor config actions)
  (define line-change? (make-string-change-detector ""))
  (match config
    (($ <structlog-monitor-config> log-file poll-interval)
     (let ((port (open-file log-file "r")))
       (seek port 0 SEEK_END)
       (let loop ()
	 (let* ((line (read-line port 'concat)))
	   (cond
	    ((eof-object? line)
	     (usleep poll-interval)
	     (loop))
	    ((or (string-null? line) (string=? line "\n"))
	     (loop))
	    (else
	     (false-if-exception
	      (let ((json (alist->hash-table (json-string->scm line))))
		(when (and (hash-table? json) (anomaly? json))
		  (let ((h (hash-string line))
			(new-hash (hash-string (line-change? line))))
		    (unless (string=? h new-hash)
		      (for-each (lambda (f)
				  (f line))
				actions)))))))))
	 (loop))))
    (_
     (error "Not a matrix-config"))))
