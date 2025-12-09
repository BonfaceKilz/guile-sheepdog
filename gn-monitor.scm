#! /usr/bin/env guile
!#

(use-modules (ice-9 match)
	     (gn alerts matrix-chat)
	     (gn alerts web))

(define main
  (match-lambda*
    ((_ settings-file)
     (let* ((settings (call-with-input-file settings-file read))
	    (user (assq-ref settings 'matrix-user))
	    (password (assq-ref settings 'matrix-password))
	    (room-id (assq-ref settings 'matrix-room-id))
	    (token (assq-ref settings 'matrix-token))
	    (homeserver (assq-ref settings 'matrix-homeserver))
	    (node (assq-ref settings 'node))
	    (port (string->number (assq-ref settings 'sheepdog-port))))
       (setenv "MATRIX_TOKEN" token)
       (setenv "ROOM_ID" room-id)
       (setenv "HOMESERVER" homeserver)
       (setenv "MATRIX_USER" user)
       (setenv "MATRIX_PASSWORD" password)
       (start-web-server "127.0.0.1" port)))
    ((arg0 _ ...)
     (format (current-error-port) "Usage: ~a CONNECTION-SETTINGS-FILE~%" arg0)
     (exit #f))))

(apply main (command-line))
