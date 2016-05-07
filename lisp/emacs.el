;; Emacs initialization commands
;; Julian Hyde, 20-Jun-96.

;; Where we are.
(or (boundp 'utils-dir)
    (set-variable 'utils-dir "//delphi/eng/public/utils"))
(or (string-match "/$" utils-dir) (setq utils-dir (concat utils-dir "/")))

(or (boundp 'jhyde-p) (setq jhyde-p nil))
(or (boundp 'jhyde-offline-p) (setq jhyde-offline-p nil))

;; -- Patched/enhanced emacs files --------------------------------------------
(load (concat utils-dir 
			  (if (string-match "^2" emacs-version)
				  "lisp/compile.el" "lisp/compile19.el")))

;; -- C -----------------------------------------------------------------------
(load "cc-mode")
(defun add-mode-assoc (suffix mode)
	(setq auto-mode-alist (cons (cons suffix mode) auto-mode-alist)))
(mapcar (lambda (x) (add-mode-assoc x 'c++-mode))
		'("\\.h\\>" "\\.hpp\\>" "\\.cpp\\>" "\\.idl\\>"
		  "\\.idh\\>" "psrlex\\.l\\>" "\\.y\\>" "\\.msg\\>"))
(add-mode-assoc "\\.java\\>" 'java-mode)
(add-mode-assoc "\\.js\\>" 'java-mode)	;JScript
(add-mode-assoc "\\.oj\\>" 'java-mode)	;OpenJava
(add-mode-assoc "\\.jj\\>" 'java-mode)	;JavaCC

;; ----------------------------------------------------------------------------
;; Create indentation styles for editing C, C++ and Java files. Inline
;; methods such are indented in C++ but not Java; hence
;;     class MyCppClass
;;     {
;;        int GetOrdinal()
;;            { return m_ordinal; }
;;        // etc.
;; and
;;     class MyJavaClass
;;     {
;;        int getOrdinal()
;;        {
;;            return ordinal;
;;        }
;;        // etc.
;;
;; See also 'bsd-braces'.
;;
(defun add-a-style (style)
  (let ((is-java (string-equal style "my-java-style")))
	(c-add-style
	 style
	 `((c-basic-offset . 4)
	   (c-offsets-alist
		. ((substatement-open . 0)
		   ,@(if (or is-java (not indent-inlines)) `((inline-open . 0)))
		   (label . -1000)
           (arglist-intro . ++)
           (arglist-cont-nonempty . c-lineup-arglist)
           (arglist-cont . c-lineup-arglist)
		   (access-label . -)))
	   (c-cleanup-list empty-defun-braces
					   defun-close-semi
					   list-close-comma
					   scope-operator
					   ,@(if bsd-braces `(brace-else-brace brace-catch-brace
										  brace-elseif-brace)))
	   (c-hanging-braces-alist
		. ((substatement-open ,@(if bsd-braces `(after) `(before after)))
		   ;; Uncomment to keep { on same line as function
		   ;; ,@(if bsd-braces `((inline-open after)))
		   ,@(if bsd-braces `((brace-list-open after)))))
		(c-hanging-colons-alist
		 . ((label after)
			(case-label after)))))))

;; The variable 'bsd-braces' tells add-a-style whether you like
;; writing code like
;;     if (true) {
;;   	 // stuff
;;     } else {
;; or
;;     if (true)
;;     {
;;   	 // stuff
;;     }
;;     else
;;     { 
;;
(setq bsd-braces
	  (or
	   jhyde-p
	   (string-equal (getenv "USERNAME") "jsichi")
	   (string-equal (getenv "USERNAME") "lkrivopaltsev")))
(setq indent-inlines
	  (or
	   jhyde-p
	   (string-equal (getenv "USERNAME") "klo")))

(add-a-style "my-c-style")
(add-a-style "my-java-style")
(add-a-style "my-awk-style")

;; Fun with backquote:
;; `(a ,nil b)         --> (a nil b)
;; `(a ,@nil b)        --> (a b)
;; `(a ,`(x . y) b)    --> (a (x . y) b)
;; `(a ,@`(x . y) b)   --> error
;; `(a ,@`(x y) b)     --> (a x y b)

(defun guess-tab-width()
  "Set `tab-width', based upon the contents of this buffer.  If cannot
guess, leave it as is.

Restriction #1: Only works in C-like languages (those which use
`c-indent-command' to indent lines).

Restriction #2: Assumes that `c-basic-offset' is already set correctly.
(It should try to guess both simultaneously, but this is tricky.)

Restriction #3: It should figure out whether a buffer uses tabs at
all, and set `indent-tabs-mode' accordingly.

jhyde, 9/24/2001."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(if (not (search-forward-regexp "^\t+[^\t\n]" (point-max) t))
		nil
	  (back-to-indentation)
	  (if nil
		  (print (concat "buffer=" (buffer-name)
						 ", point=" (point)
						 ", text=" (buffer-substring (point) (+ (point) 10)))))

	  ;; Figure out what column we should be at. Now suppose our desired column
	  ;; is d = 12, and we have t = 2 tabs and s = 4 spaces. Then d = s + t *
	  ;; w, so the ideal tab width w = (d - s) / t = (12 - 4) / 2 = 4.
	  (let* ((desired-column
			  ;; this code adapted from c-indent-command in simple.el
			  (let* ((c-syntactic-context (c-guess-basic-syntax))
					 (pos (- (point-max) (point))))
				(apply '+ (mapcar 'c-get-offset c-syntactic-context))))
			 (indentation
			  (buffer-substring (point) (progn (beginning-of-line) (point))))
			 (tab-count
			  (let ((i 0)
					(count 0))
				(while (< i (length indentation))
				  (if (string-equal (substring indentation i (1+ i))
									"\t")
					  (setq count (1+ count)))
				  (setq i (1+ i)))
				count))
			 (space-count (- (length indentation) tab-count))
			 (desired-tab-width (/ (- desired-column space-count) tab-count)))
		(if nil
			(print (concat "d=" desired-column
						   ", i=" (length indentation)
						   ", t=" tab-count
						   ", s=" space-count)))
 	;; don't set tab width to something silly
	(if (>= desired-tab-width 2)
		(set-variable 'tab-width desired-tab-width))))))

(defun my-c-mode-common-hook ()
  (if jhyde-p
	  (guess-tab-width)
	(set-variable 'tab-width
				  (cond
				   ((string-match "/OpenJava_1.0/" buffer-file-name) 8)
				   ((string-match "/openjava/" buffer-file-name) 8)
				   ((string-match "/VCafe/" buffer-file-name) 8)
				   (t 4))))
  (set-variable 'c-basic-offset 4)
  (let ((style (cond ((eq major-mode 'java-mode) "my-java-style")
					 ((eq major-mode 'awk-mode) "my-awk-style")
					 (t "my-c-style"))))
	(if (string-match "^2" emacs-version)
		(c-set-style style)
	  (set-c-style style)))
  (c-toggle-auto-hungry-state 1)		    ; switch hungry on
  (local-set-key "\M-\C-a" 'c-beginning-of-defun)
  (local-set-key "\M-\C-e" 'c-end-of-defun)
  (if (or jhyde-p
	  (string-equal (getenv "USERNAME") "qtran"))
      (abbrev-mode 1))
  (auto-fill-mode 1)
  (set-fill-column 79)
  (and (eq (point-min) (point-max))
	   (not (string-match "\.log$" buffer-file-name))
	   (bb-header)))
(setq c-mode-common-hook 'my-c-mode-common-hook)
(setq c++-mode-hook nil)
(setq java-mode-hook 'my-c-mode-common-hook)
(setq awk-mode-hook 'my-c-mode-common-hook)

;;(add-hook  'java-mode-hook
;;	(function (lambda()
;;		 (c-set-style "java")
;;		 (setq c-basic-offset 4)
;;		 (c-set-offset 'arglist-intro '+)
;;		 (c-set-offset 'substatement-open 0)
;;		 (c-set-offset 'arglist-cont 0)
;;		 (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)
;;		 (c-set-offset 'arglist-close '+))))


;; -- Motion ------------------------------------------------------------------
(defun ms-home ()
  (interactive)
  (let ((p (point)))
	(back-to-indentation)
	(if (eq p (point)) (beginning-of-line))))
(define-key global-map [home] 'ms-home)
(define-key global-map [end] 'end-of-line)
(defun scroll-up-one ()
  "Scroll up one line (jhyde)." (interactive) (scroll-up 1))
(defun scroll-down-one ()
  "Scroll down one line (jhyde)." (interactive) (scroll-down 1))

(define-key global-map [C-kp-right] 'forward-word)
(define-key global-map [C-kp-left] 'backward-word)

(define-key global-map [C-up] 'scroll-down-one)
(define-key global-map [C-kp-up] 'scroll-down-one)
(define-key global-map [C-down] 'scroll-up-one)
(define-key global-map [C-kp-down] 'scroll-up-one)

(define-key global-map [M-left] 'backward-sexp)
(define-key global-map [M-kp-left] 'backward-sexp)
(define-key global-map [M-right] 'forward-sexp)
(define-key global-map [M-kp-right] 'forward-sexp)
(define-key global-map [M-up] 'backward-list)
(define-key global-map [M-kp-up] 'backward-list)
(define-key global-map [M-down] 'forward-list)
(define-key global-map [M-kp-down] 'forward-list)

;removed jhyde 981112 (M-% is query-replace, C-M-% is query-replace-regexp)
;(define-key esc-map "%" 'query-replace-regexp)

(set-variable 'next-line-add-newlines nil)
(set-variable 'scroll-step 1)

(defun delete-horizontal-space-forward ()
  "Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))
;(define-key esc-map "\\" 'delete-horizontal-space)
(define-key global-map "\C-\\" 'delete-horizontal-space-forward)
(define-key ctl-x-map "r\\" 'close-rectangle)

(global-set-key "\C-xg" 'goto-line)

;; -- Compilation, shells -----------------------------------------------------

;; Use MKS shell (default in winnt.el is "CMD")
;; For the interactive shell
(setq explicit-shell-file-name (getenv "SHELL"))
;; For subprocesses invoked via the shell (e.g., "shell -c command")
(setq shell-file-name (getenv "SHELL"))
(setq w32-quote-process-args t)
(setq shell-command-switch "-c")

;; No longer use 'cmd' or 'cmdproxy'.
(cond (nil (set-variable 'shell-file-name "cmd")
		   (set-variable 'shell-command-switch "/c"))
	  (nil (set-variable 'shell-file-name "cmdproxy")
		   (set-variable 'shell-command-switch "/c")))

(defun master-occur-buffer ()
  (let ((b (current-buffer))
	(ob (get-buffer "*Occur*")))
    (and ob
	 (save-excursion (and (set-buffer ob)
			      (eq occur-buffer b)
			      ob)))))
(defun my-next-or-prev-hit (n)
  (let ((ob (master-occur-buffer)))
    (if ob
	(progn (switch-to-buffer-other-window ob)
	       (if (> n 0) (occur-next n) (occur-prev (- n)))
	       (recenter 0)
	       (occur-mode-goto-occurrence))
      (next-error n))))
(defun my-next-error () (interactive) (my-next-or-prev-hit 1))
(define-key global-map [f4] 'my-next-error)
(defun my-prev-error () (interactive) (my-next-or-prev-hit -1))
(define-key global-map [S-f4] 'my-prev-error)
(define-key global-map [S-f7] 'compile)
(defun save-jump-compile ()
  (interactive)
  (save-some-buffers t)
  (let ((b (get-buffer "*compilation*")))
	(if b (switch-to-buffer-other-window b)))
  (compile compile-command))
(define-key global-map [f7] 'save-jump-compile)
(define-key global-map [f12] 'call-last-kbd-macro)

;; -- Buffers and files -------------------------------------------------------
(load "uniquify")

(load (concat utils-dir "lisp/bubble.el")) ; find most recently-used buffer
(define-key global-map [C-tab] 'bubble-buffer)
(define-key global-map [C-S-tab] 'bubble-buffer-back)
(define-key global-map [C-S-iso-lefttab] 'bubble-buffer-back)
;...replaces...
;(define-key global-map [C-tab] 'bury-buffer)
;(defun unbury-buffer()
;  (interactive)
;  (switch-to-buffer (nth (1- (length (buffer-list))) (buffer-list))))
;(define-key global-map [C-S-tab] 'unbury-buffer)

;(fset 'old-upcase-word (function upcase-word))
;(defun upcase-word (&optional arg)
;  (interactive "P")
;  (if (= arg 0)
;	   (let ((c (char-after (point))))
;		 (delete-char 1)
;		 (insert (upcase c)))G
;	   (old-upcase-word arg)))
;(funcall 'old-upcase-word nil)

(defun remove-nils (lis)
  "Return list LIST with nils removed."
  (let ((res nil))
    (while lis
      (if (car lis)
	   (if res
	       (nconc res (cons (car lis) nil))
	     (setq res (cons (car lis) nil))))
      (setq lis (cdr lis)))
    res))

(setq completion-ignored-extensions
      (cons ".class"
	    (remove-nils (mapcar (lambda (x)
				   (if (string-equal x ".log") nil x))
				 completion-ignored-extensions))))

(defun revert-buffer-noconfirm ()
  "Revert the current buffer without asking for confirmation (jhyde)."
  (interactive)
  (revert-buffer nil t))
(global-set-key "\C-x\M-\C-v" 'revert-buffer-noconfirm)

(defun make-buffer-file-writeable ()
  "Make the current buffer's file writeable."
  (interactive)
  (if (not (buffer-file-name))
      (error "Buffer has no file."))
  (if (buffer-modified-p)
      (error "Buffer has been modified."))
  (shell-command (format "chmod +w %s" (buffer-file-name)))
  (revert-buffer-noconfirm))
(global-set-key "\C-x\M-\C-R" 'make-buffer-file-writeable)

(defun next-window-all-frames ()
  (interactive) (other-window 1 t) (other-frame 0))
(define-key global-map [f6] 'next-window-all-frames)
(defun previous-window-all-frames ()
  (interactive) (other-window -1 t) (other-frame 0))
(define-key global-map [S-f6] 'previous-window-all-frames)

(defun ediff-with-saved ()
  "Use Ediff to compare the current buffer with the last saved version of the
file. (jhyde, 961211)"
  (interactive)
  (or (buffer-file-name)
      (error "Buffer does not have a file"))
  (let* ((f (buffer-file-name))
		 (a (current-buffer))
		 (b (generate-new-buffer
			 (concat "*Last saved "
					 (file-name-nondirectory (buffer-file-name))
					 "*"))))
    (save-excursion
      (set-buffer b)
      (normal-mode f)
      (insert-file f)
      (not-modified)
      (toggle-read-only))
    (ediff-buffers
	 a
	 b
	 `((lambda ()
		 (make-local-variable 'ediff-cleanup-hook)
		 (setq ediff-cleanup-hook
			   (append ediff-cleanup-hook
					   (cons (lambda ()
							   (kill-buffer ,b)
							   (switch-to-buffer ,a))
							 nil)))
		 (ediff-jump-to-difference
		  (ediff-diff-at-point 'A ,(point))))))))

(setq dired-chmod-program "chmod")	; NT default is "chmode"

(defun visited-files ()
  "The filenames of all buffers."
  (remove-nils (mapcar 'buffer-file-name (buffer-list))))

(defun buffer-search (regexp)
  "Search through all buffers for REGEXP, just like 'tags-search'.

jhyde, 970529."
  (interactive "sBuffer search (regexp): ")
  (tags-search regexp (list 'visited-files)))

(defun my-query-replace-read-args (string regexp-flag)
  (let (from to)
    (if query-replace-interactive
	(setq from (car (if regexp-flag regexp-search-ring search-ring)))
      (setq from (read-from-minibuffer (format "%s: " string)
				       nil nil nil
				       query-replace-from-history-variable
				       nil t)))
    (setq to (read-from-minibuffer (format "%s %s with: " string from)
				   nil nil nil
				   query-replace-to-history-variable from t))
    (list from to current-prefix-arg)))

(defun buffer-query-replace (from to &optional delimited)
  "Query-replace-regexp FROM with TO through all buffers, just like
'tags-query-replace'.

jhyde, 970529."
  (interactive (my-query-replace-read-args "Tags query replace (regexp)" t))
  (tags-query-replace from to delimited (list 'visited-files)))

;; -- Tags --------------------------------------------------------------------
;; Fix so that tags-search 'x::y' finds 'x :: y ()'.
(defun regexp-spacey-scope (string)
  "Replace all occurrences of '::' with ' *:: *' and regexp-quote
rest of string"
  (let ((colon-pos (string-match "::" string)))
	(if colon-pos
		(concat
		 (regexp-quote (substring string 0 colon-pos))
		 " *:: *"
		 (regexp-spacey-scope (substring string (+ colon-pos 2))))
	  (regexp-quote string))))

(defun search-forward-spacey (string &optional bound noerror count)
  "A replacement for search-forward which ignores spaces around the
C++ scope operator '::'"
  (re-search-forward (regexp-spacey-scope string) bound noerror count))

(defun etags-recognize-tags-table-spacey ()
  "Replacement for etags-recognize-tags-table which uses
search-forward-spacey instead of search-forward"
  (if (etags-recognize-tags-table)
	  (set (make-local-variable 'find-tag-search-function)
		   'search-forward-spacey)
	nil))

(setq tags-table-format-hooks
  '(etags-recognize-tags-table-spacey recognize-empty-tags-table))

;; -- Fonts, colors, highlighting ---------------------------------------------
;; Set default frame parameters, and override them for specific
;; people.  To find the current parameters, evaluate
;;   (insert-string (prin1-to-string (frame-parameters)))
;; You can choose another font using shift-left-click.
(setq dark-p nil); (not jhyde-p))
(set-variable 'hilit-background-mode (if dark-p 'dark 'light))
(setq screen-height
	  (cond
	   ((string-equal (getenv "COMPUTERNAME") "SPLAT") 42)
	   ((string-equal (getenv "COMPUTERNAME") "DOPEY") 70)
	   ((string-equal (getenv "COMPUTERNAME") "MEGAGODZILLA") 59)
	   ((string-equal (getenv "COMPUTERNAME") "THOR") 41)
	   ((string-equal (getenv "COMPUTERNAME") "AGREENFIELD") 35)
	   (t 43)))
(and nil
(setq default-frame-alist
	  (append
	   (cond
		((string-equal (getenv "USERNAME") "agreenfield")
		 `((top . 4) (left . 4) (height . ,(- screen-height 7))))
		((string-equal (getenv "COMPUTERNAME") "JHYDE2")
		 `((top . 5) (left . 439) (height . ,screen-height)))
		((string-equal (getenv "COMPUTERNAME") "SPLAT")
		 `((top . 2) (left . 84) (height . ,screen-height)))
		((string-equal (getenv "USERNAME") "boris")
		 '((top . 0) (left . 0) (height . 35) (width . 82)
		   (font . "-*-Courier New-normal-r-*-*-16-120-*-*-c-*-*-ansi-")))
		((string-equal (getenv "USERNAME") "mbhavsar")
		 '((top . 0) (left . 535) (height . 52)))
		((string-equal (getenv "USERNAME") "qtran")
		 '((top . 5) (left . 176) (height . 44)))
		(jhyde-p
		 `((top . 117) (left . 84) (height . ,(- screen-height 7))))
		(t
		 `((top . 4) (left . 4) (height . ,screen-height))))
	   `((width . 80)
	     (height . 55)
	     (menu-bar-lines . 1)
	     (font . "-*-Courier New-normal-r-*-*-12-90-*-*-c-*-*-ansi-")
	     (foreground-color . ,(if dark-p "white" "black"))
	     (background-color . ,(if dark-p "black" "white"))
	     (cursor-color . "gold"))))
)

(and jhyde-p
	 (string-equal (getenv "COMPUTERNAME") "DOPEY")
	 (setq initial-frame-alist
		   `((top . 38) (left . 1017) (height . ,(- screen-height 2)))))

(if (string-equal (getenv "USERNAME") "agreenfield")
	(setq initial-frame-alist
		  (if (string-equal (getenv "COMPUTERNAME") "AGREENFIELD")
			  `((top . 4) (left . 4) (height . ,(- screen-height 2)))
			`((top . 4) (left . 4) (height . ,screen-height)))))

;; Workaround bug whereby status bars appeared in reverse video.
(defun my-window-setup-hook ()
  (set-foreground-color (cdr (assq 'foreground-color (frame-parameters)))))
(add-hook 'window-setup-hook 'my-window-setup-hook)

(load "hilit19")
(load (concat utils-dir "lisp/hilit.el")) ; highlighting for extra languages
(if dark-p
	(progn
	  (hilit-translate comment 'yellow)
	  (hilit-translate type 'cyan)))

;; -- Comments ----------------------------------------------------------------
(defun right-justify-comment ()
  (interactive)
  (end-of-line)
  (if (search-backward-regexp comment-start
			      (save-excursion (beginning-of-line) (point)) t)
      (let ((p (progn (delete-horizontal-space) (point))))
	(insert-char ?  (- fill-column (save-excursion (end-of-line)
						       (current-column))))
	;; find which variable controls whether tabs are expanded
	(tabify p (point)))))
;(define-key global-map "\C-;" 'right-justify-comment); todo: fix this binding
(define-key global-map [f3] 'right-justify-comment)

;; -- Miscellaneous -----------------------------------------------------------
(set-variable 'dabbrev-case-replace nil)
(load "time-stamp")

;; Perforce
(if nil ; disable perforce temporarily
    nil
  (load-library (concat utils-dir "lisp/p4"))
  (p4-set-p4-executable
   (cond
	((file-exists-p "/usr/bin/p4") "/usr/bin/p4")
	((file-exists-p "/usr/local/bin/p4") "/usr/local/bin/p4")
	((file-exists-p "c:/perforce/p4.exe") "c:/perforce/p4.exe")
	((file-exists-p "c:/p4/p4.exe") "c:/p4/p4.exe")
	((file-exists-p "c:/perforce/p4.exe") "c:/perforce/p4.exe")
	((file-exists-p "d:/perforce/p4.exe") "d:/perforce/p4.exe")
	((file-exists-p "e:/perforce/p4.exe") "e:/perforce/p4.exe")
	((file-exists-p "f:/perforce/p4.exe") "f:/perforce/p4.exe")
	((file-exists-p "u:/bin/p4.exe") "u:/bin/p4.exe")
	((file-exists-p (concat utils-dir "p4.exe")) (concat utils-dir "p4.exe"))
	"todo: add the location of YOUR p4"))
  (setq p4-null-device nil)
  (p4-set-p4-port nil)
  (if (string-equal (getenv "COMPUTERNAME") "MEGAGODZILLA")
     (setq p4-do-find-file nil)))

(defun p4-annotate-file ()
  (interactive)
  "Invoke p4pr.pl to show the party responsible for each line in a source file"
  (shell-command (concat bb-source-root "util/p4pr.sh " (buffer-file-name))))

;; in dired mode, position the cursor over a "dif" file, press '_', and see the
;; differences in ediff.
(load "dired")
(defun dired-ediff-dif ()
  (interactive)
  "If the current file is f.dif, use ediff to compare f.dif.ref and f.log"
  (let* ((filename (dired-get-filename))
		 (filename-sans-dif
		  (if (string-match "\\(.*\\)\\.dif$" filename)
			  (substring filename (match-beginning 1) (match-end 1))
			(error "Filename does not end in '.dif'")))
		 (filename-log (if (string-match "\\.log$" filename-sans-dif)
						   filename-sans-dif
						 (concat filename-sans-dif ".log")))
		 (filename-ref-log (concat filename-sans-dif ".dif.ref")))
		 (ediff-files filename-ref-log filename-log)))
(define-key dired-mode-map "_" 'dired-ediff-dif)

;; abbreviations
(set-variable 'abbrev-file-name (concat utils-dir "lisp/abbrev.el"))
(read-abbrev-file abbrev-file-name)

(defun string-replace-match (string regexp to)
  "Return STRING with each occurrence of REGEXP replaced with TO."
  (let ((start 0))
	(while (string-match regexp string start)
	  (setq string (concat (substring string 0 (match-beginning 0))
						   to
						   (substring string (match-end 0) (length string)))
			start (+ (match-beginning 0) (length to)))))
  string)

(defun to-header (s cstart cmid cempty cend basename author date year classname
					packname protector)
  (string-replace-match
   (string-replace-match
	(string-replace-match
	 (string-replace-match
	  (string-replace-match
	   (string-replace-match
		(string-replace-match
		 (string-replace-match
		  (string-replace-match
           (string-replace-match
            (string-replace-match s "^/\\*" cstart)
            "^//$" cempty)
           "^// " cmid)
          "^\\*/" cend)
         "%basename%" basename)
        "%year%" year)
       "%author%" author)
      "%date%" date)
     "%classname%" classname)
    "%packname%" packname)
    "%protector%" protector))

(defun bb-header ()
  (interactive)
  (let* ((basename (file-name-nondirectory (buffer-file-name)))
		 (cstart (if (string-equal comment-start "// ") "/*" comment-start))
		 (cmid comment-start)
		 (cempty (string-replace-match cmid " +$" ""))
		 (cend (if (string-equal comment-start "// ") "*/" comment-start))
		 (is-cpp (string-match "\\.\\(cpp\\|cc\\)\\>$" basename))
		 (is-h (string-match "\\.\\(h\\|hh\\)\\>" basename))
		 (is-java (string-match "\\.java\\>$" basename))
		 (classname (file-name-sans-extension basename))
		 (packname (string-replace-match
					(string-replace-match (directory-file-name
										   (file-name-directory
											buffer-file-name))
										  "^.*/java/lib/" "") "/" "."))
         (protector (string-replace-match basename "\.h$" "_Included")))
    (goto-char (point-min))
    (insert-string
	 (to-header
      (cond ((string-match "/saffron/" (buffer-file-name)) "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// Saffron preprocessor and data engine\n\
// (C) Copyright %year%-%year% Julian Hyde\n\
//\n\
// This library is free software; you can redistribute it and/or\n\
// modify it under the terms of the GNU Library General Public\n\
// License as published by the Free Software Foundation; either\n\
// version 2 of the License, or (at your option) any later version.\n\
//\n\
// This library is distributed in the hope that it will be useful,\n\
// but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n\
// Library General Public License for more details.\n\
//\n\
// You should have received a copy of the GNU Library General Public\n\
// License along with this library; if not, write to the\n\
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n\
// Boston, MA  02111-1307, USA.\n\
//\n\
// See the LICENSE.html file located in the top-level-directory of\n\
// the archive of this library for complete text of license.\n\
//\n\
// %author% %date%\n\
*/\n\
")
            ((string-match "/fennel/" (buffer-file-name)) "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// Fennel is a relational database kernel.\n\
// (C) Copyright %year%-%year% Disruptive Tech\n\
//\n\
// This library is free software; you can redistribute it and/or\n\
// modify it under the terms of the GNU Library General Public\n\
// License as published by the Free Software Foundation; either\n\
// version 2 of the License, or (at your option) any later version.\n\
//\n\
// This library is distributed in the hope that it will be useful,\n\
// but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n\
// Library General Public License for more details.\n\
//\n\
// You should have received a copy of the GNU Library General Public\n\
// License along with this library; if not, write to the\n\
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n\
// Boston, MA  02111-1307, USA.\n\
//\n\
// See the LICENSE.html file located in the top-level-directory of\n\
// the archive of this library for complete text of license.\n\
//\n\
// %author% %date%\n\
*/\n\
\n\
#include \"fennel/common/CommonPreamble.h\"\n\
\n\
FENNEL_BEGIN_CPPFILE(\"$Id: //open/users/jhyde/home/lisp/emacs.el#2 $\");\n\
\n\
FENNEL_END_CPPFILE(\"$Id: //open/users/jhyde/home/lisp/emacs.el#2 $\");\n")
			 ((string-match "/mondrian/" (buffer-file-name)) "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// This software is subject to the terms of the Common Public License\n\
// Agreement, available at the following URL:\n\
// http://www.opensource.org/licenses/cpl.html.\n\
// (C) Copyright %year% Kana Software, Inc. and others.\n\
// All Rights Reserved.\n\
// You must accept the terms of that agreement to use this software.\n\
//\n\
// %author%, %date%\n\
*/\n\
")
			 ((and (string-match "/aspen/" (buffer-file-name)) is-cpp) "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// Aspen dataflow server
// (C) Copyright %year%-%year% Disruptive Tech
//\n\
// %author%, %date%\n\
*/\n\
\n\
#include \"aspen/common/CommonPreamble.h\"\n\
\n\
ASPEN_BEGIN_CPPFILE(\"$Id: //open/users/jhyde/home/lisp/emacs.el#2 $\");\n\
\n\
ASPEN_END_CPPFILE(\"$Id: //open/users/jhyde/home/lisp/emacs.el#2 $\");\n")
			 ((and (string-match "/aspen/" (buffer-file-name)) is-h) "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// Aspen dataflow server
// (C) Copyright %year%-%year% Disruptive Tech
//\n\
// %author%, %date%\n\
*/\n\
\n\
#ifndef Aspen_%protector%\n\
#define Aspen_%protector%\n\
\n\
ASPEN_BEGIN_NAMESPACE\n\
\n\
ASPEN_END_NAMESPACE\n\
\n\
#endif\n\
")
			 ((and (string-match "/metadata/" (buffer-file-name)) is-cpp) "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// (C) Copyright 2004-2004 Julian Hyde and Mike Benham\n\
//\n\
// %author%, %date%\n\
*/\n\
\n\
#include \"event/CommonPreamble.h\"\n\
\n")
			 ((and (string-match "/metadata/" (buffer-file-name)) is-h) "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// (C) Copyright 2004-2004 Julian Hyde and Mike Benham\n\
//\n\
// %author%, %date%\n\
*/\n\
\n\
#ifndef Metadata_%protector%\n\
#define Metadata_%protector%\n\
\n\
#endif\n\
")
			 (t "\
/*\n\
// $Id: //open/users/jhyde/home/lisp/emacs.el#2 $\n\
// (C) Copyright %year% Kana Software, Inc.\n\
// %author%, %date%\n\
*/\n\
"))
	  cstart cmid cempty cend basename (user-login-name)
	  (time-stamp-string "%:d %:b, %:y") (time-stamp-string "%:y")
	  classname packname protector))
    (save-excursion
      (goto-char (point-max))
      (insert-string (format "\
\n\
%sEnd %s\n" cmid basename)))))

(defun bb-make-comment (comment)
  (concat comment-start comment comment-end))

(defun bb-ifxdef (n beg end &optional name)
  (save-excursion
    (let* ((ifdef (if n "#ifndef" "#ifdef"))
	   (name (or name
		     (read-string (format "%s: " ifdef)
				  (if n nil "NEVER"))))
	   (pbeg (save-excursion (goto-char beg) (point-marker)))
	   (pend (save-excursion (goto-char end) (point-marker))))
      (save-excursion
	(goto-char (marker-position pend))
	(insert-string (format "#endif %s\n" (bb-make-comment name))))
      (goto-char (marker-position pbeg))
      (insert-string (format "%s %s\n" ifdef name)))))

(defun bb-ifndef (beg end &optional name)
  (interactive "r" "sName:")
  (bb-ifxdef t beg end name))

(defun bb-ifdef (beg end &optional name)
  (interactive "r" "sName:")
  (bb-ifxdef nil beg end name))

(defun strip-spaces ()
  "Remove spaces from the ends of lines, and ensure that the last line
is empty."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ 	
]+$" nil t)
      (replace-match "" nil nil))
    (goto-char (point-max))
    (or (bolp)
	(insert-string "\n"))))
(define-key global-map [f8] 'strip-spaces)

;; -- Startup -----------------------------------------------------------------
(cd (downcase default-directory))
(if (boundp 'favorite-files)
	(mapcar 'find-file favorite-files))
(and jhyde-p
	 (byte-recompile-directory (concat utils-dir "lisp")))

; -----------------------------------------------------------------------------
;Here is some code to print under NT. It is much simpler that the code you
;have on your page and the main advantage is that you don't need to modify
;the ps-print.el package.
;This is freeware and you can do whatever you want with it.;Pascal.
;;; ##################################################  NT - PS-Print
;(require 'ps-print);(setq ps-paper-type 'ps-a4);(setq ps-lpr-command "print")
;(setq ps-lpr-switches '("/D:\\\\PLUTON\\HP4SIMX")) ; the printer name
;(setq ps-lpr-buffer "c:\\temp\\psspool.ps")       ; a tmp spool file
;(defun nt-ps-print-buffer-with-faces ();  (interactive)
;  (ps-print-buffer-with-faces ps-lpr-buffer);  (shell-command
;   (apply 'concat (append (list ps-lpr-command " ")
;                          ps-lpr-switches
;                          (list " " ps-lpr-buffer))));)

; -----------------------------------------------------------------------------
; commands for loading tags files for various projects; you must
; redefine variable bb-source-root to something like "c:/src/"

(load "etags")

;; -- GNU Server---------------------------------------------------------------
;; GNU Server allows you to edit files using 'gnuclient', and send arbitrary
;; lisp to the server using 'gnudoit'.  'ediff-serv.el' provides
;;(require 'gnuserv)
;;(gnuserv-start)
;;(load (concat utils-dir "lisp/ediff-serv.el"))

;; -- Align -------------------------------------------------------------------
(require 'align (concat utils-dir "lisp/align.el"))
(global-set-key "\M-[" 'align)

;; -- Insert an XML/HTML tag --------------------------------------------------
(defun insert-tag (tag)
  "Insert an XML/HTML tag.  Useful for writing Javadoc.

For example, to insert '<dfn>A definition</dfn>', type
'M-C-y dfn RET A definition'.

jhyde, 2001/7/19"
  (interactive "sTag: ")
  (insert-string (concat "<" tag ">"))
  (save-excursion
	(insert-string (concat "</" tag ">"))))
(global-set-key "\M-\C-y" 'insert-tag)


(defun c-required-indent (&optional syntax)
  ;; indent the current line as C/C++/ObjC code. Optional SYNTAX is the
  ;; syntactic information for the current line. Returns the amount of
  ;; indentation change (in columns).
  (let* ((c-syntactic-context (or syntax (c-guess-basic-syntax)))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'c-get-offset c-syntactic-context))))
	indent))


(c-required-indent)

;; End emacs.el
