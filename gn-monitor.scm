#! /usr/bin/env guile
!#

(use-modules (alerts matrix-chat)
	     (alerts structlog-monitor)
	     (fibers)
	     (ice-9 hash-table)
	     (json)
	     (srfi srfi-26))

(define* (matrix-alert-action
	  initial-value
	  #:optional
	  (node #f) (app-name #f)
	  (config (make-matrix-config "token" "room-id" "home-server")))
  (let ((last initial-value))
    (lambda (new)
      (unless (string=? last new)
	(matrix-send (log-entry->alert-html
		      (alist->hash-table (json-string->scm last))
		      node app-name)
		     config)
	(set! last new))
      last)))


(run-fibers
 (lambda ()
   (let* ((settings (call-with-input-file "/tmp/conn.scm" read))
	  (room (assq-ref settings 'matrix-room))
	  (token (assq-ref settings 'matrix-token))
	  (homeserver (assq-ref settings 'matrix-homeserver))
	  (nodes (assq-ref settings 'nodes)))
     (for-each
      (lambda (node)
	(for-each
	 (lambda (config)
	   (let* ((node-name (assq-ref node 'node))
		  (log-file-path (assq-ref config 'log-file))
		  (app-name (assq-ref config 'app))
		  (interval (string->number (assq-ref config 'poll-interval)))
		  (monitor-config
		   (make-structlog-monitor-config log-file-path interval))
		  (matrix-config (make-matrix-config token room homeserver))
		  (matrix-action
		   (matrix-alert-action
		    (format #f
			    "{\"event\": \"starting up ~a: ~a logging\"}"
			    node-name app-name)
		    node-name app-name
		    matrix-config)))
	     (format #t "spawning fiber for ~a ~a\n" node-name app-name)
	     (spawn-fiber (cut (monitor monitor-config (list matrix-action))))))
	 (assq-ref node 'config)))
      nodes)
     (let loop ()
       (usleep 50)
       (gc)
       (loop)))))

