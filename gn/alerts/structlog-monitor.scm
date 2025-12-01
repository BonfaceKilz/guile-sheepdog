(define-module (gn alerts structlog-monitor)
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
        changed))))

(define (read-last-line file)
  (call-with-input-file file
    (lambda (port)
      (let* ((size (seek port 0 SEEK_END)))
        (let loop ((pos size)
                   (chars '()))
          (if (zero? pos)
              ;; start of file: reverse collected chars
              (list->string chars)
              (begin
                (seek port (- pos 1) SEEK_SET)
                (let ((c (read-char port)))
                  (if (char=? c #\newline)
                      ;; Skip trailing newline (if any) and return collected
                      (list->string chars)
                      (loop (- pos 1) (cons c chars)))))))))))

(define (monitor config actions)
  (match config
    (($ <structlog-monitor-config> log-file poll-interval)
     (let ((line-change? (make-string-change-detector "")))
       (let loop ()
	 (let* ((line (read-last-line log-file)))
	   (cond
	    ((eof-object? line)
	     #f)
	    ((or (string-null? line) (string=? line "\n"))
	     #f)
	    (else
	     (false-if-exception
	      (let ((json (alist->hash-table (json-string->scm line))))
		(when (and (hash-table? json) (anomaly? json))
		  (unless (line-change? (hash-string line))
		    (for-each (lambda (f)
				(f line))
			      actions)))))))
	   (usleep poll-interval)
	   (loop)))))
    (_
     (error "Not a matrix-config"))))
