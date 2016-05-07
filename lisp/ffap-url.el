;;; ffap-url.el: a fancy optional ffap-url-fetcher for ffap
;; Time-stamp: <97/04/04 13:08:13 mic>

;; This file is an optional add-on for the ffap package.
;;
;; We set variable `ffap-url-fetcher' to function ffap-url-fetcher,
;; which prompts user to choose among valid url-fetching options.
;; The function is highly configurable via `ffap-url-methods'.
;;
;; This packages notices functions from w3, browse-url, vm, gnus, and
;; maybe other packages.  It does not matter whether this file is
;; loaded before or after those other packages.  So many people have
;; skinned the mosaic/netscape-url-fetching cat, I did not write any
;; myself.  I recommend browse-url, since it has the least overhead,
;; and should also work from "emacs -nw" inside an xterm.

(require 'ffap)
(provide 'ffap-url)


;; Installation:
;;
;; (require 'ffap-url)
;;
;; Optionally:
;;
;; (autoload 'w3-fetch "w3" nil t)      ; enable w3 option
;; (and window-system (require 'browse-url)) ; netscape, mosaic options
;; (setq lynx-program nil)              ; suppress lynx options
;; (setq ffap-url-unwrap-local nil)     ; so we see these as urls
;; (setq ffap-url-unwrap-remote nil)    ; so we see these as urls
;; (setq agora-address "agora@mail.w3.org") ; enable agora option
;;
;; ;; Promote the "netscape" method, so it becomes the default for "http:":
;; (let ((method (assoc "netscape" ffap-url-methods)))
;;   (and method
;;        (setq ffap-url-methods
;;              (cons method (delete method ffap-url-methods)))))
;;
;; Of course you may also modify `ffap-url-methods' as you like.
;; Note that methods with undefined functions are simply ignored.

;; Latest Version: ftp://ftp.mathcs.emory.edu/pub/mic/emacs/


;;; Variables:

(defvar ffap-url-methods
  ;; Putting defun and defvar forms in here makes it harder to read,
  ;; but much easier to keep related definitions together!
  (list
   ;; Offer the `low overhead' methods first if possible.
   (list
    "vm"
    (defun ffap-url-vm (url)
      (vm-mail) (mail-to) (insert (substring url 7)) (mail-subject))
    "\\`mailto:"
    '(featurep 'vm)
    )
   (list
    "mail"
    (defun ffap-url-mail (url) (mail nil (substring url 7)))
    "\\`mailto:"
    )
   (list
    "gnus"                              ; (ding) gnus version?
    ;; The following reported buggy in gnus5, by yasuro@maekawa.is.uec.ac.jp
    (defun ffap-url-gnus (url)
      (if (string-match "@" url)
          ;; message-id
          (save-excursion
            ;; does this save-ex help in gnus 5?
            (set-buffer gnus-summary-buffer)
            (gnus-summary-refer-article (substring url 5)))
        ;; assume a group!
        (let ((group (substring url 5)))
          (gnus-group-jump-to-group group)
          (gnus-group-read-group nil))))
    "\\`news:[^/]*\\'"
    '(get-buffer gnus-group-buffer)     ; gnus running?
    )
   (list
    "find-file"
    (defun ffap-url-find-local (url)
      (find-file (ffap-url-unwrap-local url)))
    '(ffap-url-unwrap-local url)
    )
   (list
    "find-file"
    (defun ffap-url-find-remote (url)
      (find-file (ffap-url-unwrap-remote url)))
    '(and ffap-ftp-regexp (ffap-url-unwrap-remote url))
    )
   (list
    "copy-find"
    ;; Find a copy: has advantage of avoiding a remote dir listing.
    ;; Suggested by a complaint made 13 Oct 95 to ange-ftp-lovers,
    ;; by frs0139@trt-philips.fr (Damien WETZEL).  Code here based
    ;; on a reply by Erik Naggum <erik@naggum.no> (4 lines in common):
    (progn
      (defvar ffap-find-copy-dir "/tmp")
      (defun ffap-find-copy (url)
        (let* ((file (or (ffap-url-unwrap-remote url) url))
               (copy (expand-file-name
                      (file-name-nondirectory file)
                      ffap-find-copy-dir)))
          (copy-file file copy)
          (find-file copy))))
    "[^/]\\'"                           ; cannot be a directory
    '(and ffap-ftp-regexp (ffap-url-unwrap-remote url))
    )
   '("w3" w3-fetch)
   '("w3-frame" w3-fetch-other-frame window-system)
   (list
    "w3-disk"
    (defun w3-fetch-to-disk (url)
      (let ((w3-dump-to-disk t)) (w3-fetch url)))
    '(fboundp 'w3-fetch))
   ;; (setq `lynx-program' nil) to disable the next two:
   (list
    "lynx"
    (defun lynx-fetch (url)
      "Use `lynx-program' to fetch formatted URL."
      ;; (shell-command (format "%s -dump '%s' &" lynx-program url))
      (with-output-to-temp-buffer url
        (call-process lynx-program nil standard-output nil "-dump" url)))
    "\\`http:"
    (defvar lynx-program "lynx")
    )
   (list
    ;; WWW by mail agent:
    "agora"
    (defun agora-fetch (url)
      (mail nil agora-address)
      (mail-text)
      (insert "SEND " url)
      (beginning-of-line)
      (message "Send it yourself (maybe you prefer \"SOURCE\")?"))
    ;; try
    (defvar agora-address nil
      "WWW mail server. Try \"agora@www.undp.org\" or \"agora@mail.w3.org\".")
    "\\`http:"                          ; good for anything else?
    ;; For info: http://www.w3.org/hypertext/WWW/Agora/Help.txt
    )
   (list
    "lynx-src"
    (defun lynx-src-fetch (url)
      "Use `lynx-program' to fetch source of URL."
      (with-output-to-temp-buffer (concat "SRC:" url)
        (call-process lynx-program nil standard-output nil "-source" url)))
    "\\`http:"
    'lynx-program
    )
   ;; External X browsers: these all ought to have the condition
   ;; (getenv "DISPLAY"), but presumably you would not load these
   ;; definitions without access to a display?
   '("netscape" browse-url-netscape)
   '("mosaic" browse-url-mosaic)
   '("cci-mosaic" browse-url-cci)
   ;; '("iximosaic" browse-url-iximosaic)
   ;; '("java" browse-url-java)         ; fiction?
   ;; If you have vm: (apropos "vm-.*url")
   '("netscape-vm" vm-mouse-send-url-to-netscape (getenv "DISPLAY"))
   '("mosaic-vm" vm-mouse-send-url-to-mosaic (getenv "DISPLAY"))
   ;; '("java-vm" vm-mouse-send-url-to-hotjava) ; fiction?
   ;; Have not tested these (from ding or xemacs?):
   '("netscape-hh" highlight-headers-follow-url-netscape)
   '("mosaic-hh" highlight-headers-follow-url-mosaic)
   ;; '("java-hh" highlight-headers-follow-url-java) ; fiction?
   )
  "List of (PROMPT FETCHER [COND...]) methods used by ffap-url-fetch.
PROMPT is a string used for prompting, FETCHER is a function symbol,
and each COND is a regular-expression or lisp form.

On argument `url', ffap-url-fetcher considers a method valid if:
  FETCHER is bound to a valid function, and
  each COND is a regexp matching url or a form that evals to non-nil.

It puts up a menu of the valid METHODS, and finally calls the
selected FETCHER on `url'.")

;; The validity test:
(defun ffap-url-method-valid-p (method url)
  "Test whether METHOD if valid for URL.  See `ffap-url-methods'."
  (and
   (fboundp (nth 1 method))             ; valid fetcher
   (let ((cnds (nthcdr 2 method)) (ret t)) ; list of conditions
     (while (and cnds ret)
       (setq ret
             (if (stringp (car cnds))
                 (string-match (car cnds) url)
               (condition-case nil (eval (car cnds)) (error nil)))
             cnds (cdr cnds)))
     ret)))


;;; The main function: ffap-url-fetch

(defvar ffap-url-fetch-ask-if-unique t  ; arguably t is less confusing
  "Should ffap-url-fetcher prompt if there is only one answer?")

(defun ffap-url-fetcher (url &optional noask)
  "Fetch URL, after prompting user to choose a method.
Offers valid methods from `ffap-url-methods'.
Optional NOASK means use the first valid method without asking.
Interactively: expects URL around point, and prefix means NOASK."
  ;; Not great as a command, but it works:
  (interactive
   (list (or (ffap-url-at-point) (error "No URL at point!"))
         current-prefix-arg))
  (let ((methods ffap-url-methods) method actions fetcher)
    (while (and methods (not (and noask actions)))
      (setq method (car methods) methods (cdr methods))
      (and (ffap-url-method-valid-p method url)
           (setq actions (cons (list (car method) (nth 1 method) url)
                               actions))))
    (or actions (error "ffap-url-fetcher: no valid methods found!"))
    (setq actions (nreverse actions)) ; put best first
    (if (or noask (not (or (cdr actions) ffap-url-fetch-ask-if-unique)))
        (ffap-url-fetcher-cont (car actions))
      (ffap-menu-ask "URL Method" actions 'ffap-url-fetcher-cont))))
(defun ffap-url-fetcher-cont (action)   ; continuation
  (let ((fetcher (nth 1 action)) (url (nth 2 action)))
  (message "Using %s on `%s'" fetcher url)
  (funcall fetcher url)))

(setq ffap-url-fetcher 'ffap-url-fetcher) ; install it!

;; Helper function:
;(defun shorten-string (str len)
;  (if (< len 0) (setq len (+ (frame-width) len)))
;  (if (< (length str) len) str
;    (concat (substring str 0 (1- (/ len 2))) ".."
;           (substring str (- (1- (/ len 2)))))))

;; (defadvice x-popup-menu (before elog act) (elog-quiet (ad-get-args 0)))
;; (ad-unadvise 'x-popup-menu)


;; Todo:
;; * reorder ffap-url-methods according to user choices
;; * add "grail" support, http://monty.cnri.reston.va.us/grail/
;; eof
