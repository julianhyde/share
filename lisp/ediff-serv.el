;; server-merge-files
;;
;; Invoke ediff-merge-files-with-ancestor from gnudoit; on exit, notify the
;; client, and save the merged output.  See gnuMerge.sh.
;;
;; Julian Hyde, 980719.
;;
;; Bugs:
;; 1. The ediff control panel window does not get focus properly on startup.
;; 2. If, when we save the *ediff-merge* buffer to the output file, there is
;; already a buffer looking at that output file, we ignore it, and you end up
;; with two buffers for the same file.
;; 3. I've had to redefine server-eval.

;; Call this from 'gnudoit'.
(defun server-merge-files (f1 f2 f3 f4)
  "Merge files f1 and f2 with ancestor f3 and write result to f4."
  (let ((buffer (if (string-equal f3 "")
		    (ediff-files f1 f2)
		  (ediff-merge-files-with-ancestor f1 f2 f3))))
    (set-buffer buffer)
    (setq server-eval-wait-a-bit t)	;tell server-eval not to return
    (select-frame ediff-control-frame)
    (setq server-merge-result-file f4)
    (server-make-window-visible)
    (let ((old-clients (assq current-client server-clients)))
	(setq server-buffer-clients
	      (cons current-client server-buffer-clients))
	(if old-clients			;client already waiting for buffers?
	    (nconc old-clients (list buffer)) ;yes -- append this one as well
	  (setq server-clients		;nope -- make a new record
		(cons (list current-client buffer)
		      server-clients))))))

(make-variable-buffer-local 'server-merge-result-file)


;; Modified version of server-eval from gnuserv.el. Original was:
;;(defun server-eval (form)
;;  "Evaluate form and return result to client."
;;  (server-write-to-client current-client (eval form))
;;  (setq current-client nil))
(defun server-eval (form)
  "Evaluate FORM and return result to client.  If FORM sets
server-eval-wait-a-bit to non-nil, the server must call " 
  (let* ((server-eval-wait-a-bit nil)
	 (out (eval form)))
    (if server-eval-wait-a-bit
	nil
      (server-write-to-client current-client out)
      (setq current-client nil))))

;; If this ediff session was invoked from 'server-merge-files', save
;; *ediff-merge* buffer, and inform the client.
(defun server-end-merge ()
  (if server-merge-result-file
      (if (y-or-n-p (format "Write merged output to %s? "
			    server-merge-result-file))
	  (condition-case oops
	      (let ((merge-result-file server-merge-result-file))
		(save-excursion
		  (set-buffer ediff-buffer-C)
		  (set-buffer-modified-p t)
		  (set-visited-file-name merge-result-file)
		  (save-buffer)))
	    (quit nil))))
  (server-buffer-done (current-buffer)))

(add-hook 'ediff-cleanup-hook 'server-end-merge)

;; End ediff-server.el
