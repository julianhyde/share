;; hilit.el: hilight patterns for more modes
;; jhyde, 21 August, 1996

;; -- Bourne shell ------------------------------------------------------------
(hilit-set-mode-patterns
 'sh-mode
 '(("\\s #.*$" nil comment)
   ("^#.*$" nil comment)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("^\\(\\w\\|[_']\\)+()" nil defun)
   ("\\<\\(case\\|do\\|done\\|break\\|continue\\|echo\\|elif\\|else\\|esac\\|eval\\|exit\\|export\\|false\\|fi\\|for\\|function\\|if\\|in\\|shift\\|then\\|test\\|trap\\|true\\|until\\|while\\)\\>" nil keyword)
   ))

;; -- SGML (or XML) -----------------------------------------------------------
(hilit-set-mode-patterns
 'sgml-mode
 '(("<!--" "-->" comment)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("^<!\\(ELEMENT\\|ATTLIST\\)\\s *\\w+" nil defun)
   ("\\<\\(CDATA\\|EMPTY\\)\\>\\|#\\(REQUIRED\\|PCDATA\\|IMPLIED\\)" nil keyword)))

(hilit-set-mode-patterns
 'html-mode
 '(("<!--" "-->" comment)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("^<!\\(ELEMENT\\|ATTLIST\\)\\s *\\w+" nil defun)
   ("\\<\\(CDATA\\|EMPTY\\)\\>\\|#\\(REQUIRED\\|PCDATA\\|IMPLIED\\)" nil keyword)))

;; -- Java-CUP ----------------------------------------------------------------
(defun cup-mode ()
  "Major mode for editing Java CUP code.
Does not do much except highlighting.

Turning on Java CUP mode calls the value of the variable `cup-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
; (require 'cc-mode)
; (use-local-map c-mode-map)
  (setq major-mode 'cup-mode)
  (setq mode-name "CUP")
; (setq local-abbrev-table cup-mode-abbrev-table)
; (set-syntax-table cup-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (run-hooks 'cup-mode-hook))
(add-mode-assoc "\\.cup\\>" 'cup-mode)

;; -- sql ---------------------------------------------------------------------
(defun sql-mode ()
  "Major mode for editing SQL code.
Does not do much except highlighting.

Turning on SQL mode calls the value of the variable `sql-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
; (require 'cc-mode)
; (use-local-map c-mode-map)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
; (setq local-abbrev-table sql-mode-abbrev-table)
; (set-syntax-table sql-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (run-hooks 'sql-mode-hook))

(defun or-string (lis)
  (concat "\\<\\("
	  (car lis)
	  (eval (cons 'concat
		      (mapcar (lambda (x) (concat "\\|" x)) (cdr lis))))
	  "\\)\\>"))

(let ((k '(
	   "action"
	   "and"
	   "as"
	   "asc"
	   "by"
	   "create"
	   "delete"
	   "desc"
	   "drop"
	   "execute"
	   "exists"
	   "foreign"
	   "from"
	   "group"
	   "having"
	   "in"
	   "index"
	   "insert"
	   "intersect"
	   "into"
	   "is"
	   "join"
	   "key"
	   "left"
	   "like"
	   "minus"
	   "not"
	   "null"
	   "on"
	   "or"
	   "order"
	   "outer"
	   "primary"
	   "replace"
	   "script"
	   "select"
	   "table"
	   "tablespace"
	   "union"
	   "update"
	   "where"
	   )))
  (hilit-set-mode-patterns
   'sql-mode
   (list '("--.*$" nil comment)
	 '("/\\*" "\\*/" comment)
	 '("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
	 (list (or-string k) nil 'keyword)
	 (list (upcase (or-string k)) nil 'keyword))))
(add-mode-assoc "\\.cup\\>" 'cup-mode)
(defun my-sql-mode-hook () (auto-fill-mode) (set-variable 'tab-width 4))
(setq sql-mode-hook (list 'my-sql-mode-hook))

;; -- awk ---------------------------------------------------------------------
(hilit-set-mode-patterns
   'awk-mode
   '(
     ("\\s #.*$" nil comment)
     ("^#.*$" nil comment)
     (hilit-string-find ?' string)
     
     ("^\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
     ;; no decl or type
     ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\)\\>[^_]" 1 keyword)
     ))

;; -- c++ & java --------------------------------------------------------------
(let ((comments     '(("/\\*" "\\*/" comment)))
      (c++-comments '(("//.*$" nil comment)
		      ("^/.*$" nil comment)))
      (strings      '((hilit-string-find ?' string)))
      (preprocessor '(("^#[ \t]*\\(undef\\|define\\).*$" "[^\\]$" define)
		      ("^#.*$" nil include))))

  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
  ;; Patched from hilit19.el, which didn't recognise virtual, static,
  ;; const, inline, bool, true or false as keywords.
  (hilit-set-mode-patterns
   'c++-mode
   (append
    comments c++-comments strings preprocessor
    '(
      ;; function decls are expected to have types on the previous line
      ("^\\(\\(\\w\\|[$_]\\)+::\\)?\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(\\(\\w\\|[$_]\\)+[ \t]*::[ \t]*\\)?\\(\\(\\w\\|[$_]\\)+\\|operator.*\\)\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(template\\|typedef\\|struct\\|union\\|class\\|enum\\|public\\|private\\|protected\\).*$" nil decl)
      ;; datatype -- black magic regular expression
      ("[ \n\t({]\\(\\(const\\|register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|\\(\\(struct\\|union\\|enum\\|class\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
      ;; key words
      ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\|public\\|protected\\|private\\|delete\\|new\\|virtual\\|const\\|static\\|inline\\|bool\\|true\\|false\\)\\>[^_]"
       1 keyword))))

  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
  (hilit-set-mode-patterns
   'java-mode
   (append
    comments c++-comments strings preprocessor
    '(
      ;; function decls are expected to have types on the previous line
      ("^\\(\\(\\w\\|[$_]\\)+::\\)?\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(\\(\\w\\|[$_]\\)+[ \t]*::[ \t]*\\)?\\(\\(\\w\\|[$_]\\)+\\|operator.*\\)\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(template\\|typedef\\|struct\\|union\\|class\\|enum\\|public\\|private\\|protected\\).*$" nil decl)
      ;; datatype -- black magic regular expression
      ("[ \n\t({]\\(\\(const\\|register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|String\\|boolean\\|\\(\\(struct\\|union\\|enum\\|class\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
      ;; key words
      ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\|public\\|protected\\|private\\|delete\\|new\\|static\\|inline\\|true\\|false\\)\\>[^_]"
       1 keyword)
      ;; java-specific key words
      ("[^_]\\<\\(abstract\\|catch\\|final\\|finally\\|import\\|instanceof\\|null\\|package\\|throws\\|try\\|synchronized\\|\\)\\>[^_]"
       1 keyword))))

  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
  (hilit-set-mode-patterns
   'cup-mode
   (append
    comments c++-comments strings preprocessor
    '(
      ;; function decls are expected to have types on the previous line
      ("^\\(\\(\\w\\|[$_]\\)+::\\)?\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(\\(\\w\\|[$_]\\)+[ \t]*::[ \t]*\\)?\\(\\(\\w\\|[$_]\\)+\\|operator.*\\)\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
	  ("^[a-zA-Z0-9_]*[ \t]*::=" nil defun)	; lhs of production
      ("^\\(template\\|typedef\\|struct\\|union\\|class\\|enum\\|public\\|private\\|protected\\).*$" nil decl)
      ;; datatype -- black magic regular expression
      ("[ \n\t({]\\(\\(const\\|register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|String\\|boolean\\|\\(\\(struct\\|union\\|enum\\|class\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
      ;; key words
      ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\|public\\|protected\\|private\\|delete\\|new\\|static\\|inline\\|true\\|false\\)\\>[^_]"
       1 keyword)
      ;; java-specific key words
      ("[^_]\\<\\(catch\\|finally\\|import\\|package\\|final\\|throws\\|try\\|null\\|abstract\\|\\)\\>[^_]"
       1 keyword)))))

;; End hilit.el
