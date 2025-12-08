#! /usr/bin/env guile
!#

(use-modules (gn alerts matrix-chat)
	     (gn alerts structlog-monitor)
	     (fibers)
	     (ice-9 match)
	     (ice-9 hash-table)
	     (json)
	     (srfi srfi-26))

(define main
  (match-lambda*
    ((_ settings-file)
     (let* ((settings (call-with-input-file settings-file read))
	    (room (assq-ref settings 'matrix-room))
	    (token (assq-ref settings 'matrix-token))
	    (homeserver (assq-ref settings 'matrix-homeserver))
	    (room-settings (make-matrix-config token room homeserver))
	    (nodes (assq-ref settings 'nodes)))

       (define* (matrix-alert-action
		 initial-value
		 #:optional
		 (node #f) (app-name #f))
	 (let ((last initial-value))
	   (lambda (new)
	     (unless (string=? last new)
	       (matrix-send (log-entry->alert-html
			     (alist->hash-table (json-string->scm new))
			     node app-name)
			    room-settings)
	       (set! last new))
	     last)))

       (run-fibers
	(lambda ()
	  (let* ()
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
			 (matrix-action (matrix-alert-action "{}" node-name app-name)))
		    (format #t "spawning fiber for ~a ~a\n" node-name app-name)
		    (spawn-fiber (cut (monitor monitor-config (list matrix-action))))))
		(assq-ref node 'config)))
	     nodes)
	    (let loop ()
	      (usleep 100)
	      (loop)))))))
    ((arg0 _ ...)
     (format (current-error-port) "Usage: ~a CONNECTION-SETTINGS-FILE~%" arg0)
     (exit #f))))

(apply main (command-line))
