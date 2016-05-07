;

(defun format-debug (&optional args)
  (save-excursion
    (set-buffer "*scratch*")
    (goto-char (point-max))
    (insert-string (prin1-to-string args))
    (insert-string "\n")))

(defun format-region (start end)
  (interactive "r")
  (let ((tokens (format-tokenize start end)))
    tokens))

(defun format-sexp ()
  (format-region (point) (save-excursion (forward-sexp) (point))))

; return packet consisting of:
;  - token text
;  - token start
;  - token end
(defun format-current-token ()
  (cond ((looking-at "[a-zA-Z0-9_]+") ;identifer
	 (cons (match-beginning 0) (match-end 0)))
	((looking-at "==\\|!=\\|||\\|&&") ;operator
	 (cons (match-beginning 0) (match-end 0)))
	(t				;catch-all: any character
	 (cons (point) (+ (point) 1)))))

(defun format-tokenize (start end)
  (save-excursion
    (let ((tokens))
      (goto-char start)
      (while (< (point) end)
	(let* ((token (format-current-token))
	       (token-start (car token))
	       (token-end (cdr token)))
	  (setq tokens (cons
			(list (buffer-substring token-start token-end)
			      token-start token-end)
			tokens))
	  (format-debug (format "token: %s" (car tokens)))
	  (forward-char (- token-end token-start))))
      (nreverse tokens))))

(format-debug (format-sexp))(hello + 5)
	  