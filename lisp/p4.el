;;; p4.el --- Simple Perforce-Emacs Integration
;;
;; $Id: //open/users/jhyde/home/lisp/p4.el#1 $

;;; Commentary:
;;
;;    Applied the GNU G.P.L. to this file - rv 3/27/1997

;;    Programs for  Emacs <-> Perforce Integration.
;;    Copyright (C) 1996, 1997	Eric Promislow
;;    Copyright (C) 1997-1999  Rajesh Vaidheeswarran
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;    If you have any problems to report, or suggestions, please send them
;;    to rv@fore.com

;; LCD Archive Entry:
;; p4|Rajesh Vaidheeswarran|rv@fore.com|
;; P4 SCM Integration into Emacs/XEmacs|
;; 1999/10/28|8.0|not_assigned_yet|

;;
;; WARNING:
;; --------
;;
;;    % p4 edit foo.c
;;    ... make changes to foo.c in emacs
;;    % p4 submit
;;     ... keep the writable copy of foo.c in emacs.  Start making changes
;;     to it.  Discover that you can't save it.	 If you do M-x:p4-edit,
;;     you'll lose your changes.  You need to do a 'p4 edit' at the
;;     command-line.
;;

;; Original Functions:	 (Contributed by Eric Promislow)
;; p4-exec-p4		     (not exported)
;; p4-buffer-action	     (not exported)
;; p4-edit
;; p4-revert
;; p4-diff


;; NOTES:
;; ------
;;
;; It is best if you take this file and byte compile it. To do that, you
;; need to do the following:
;;
;; % emacs -batch -f batch-byte-compile /full/path/to/file/p4.el
;;
;; This creates a binary file p4.elc in the path. Add the path to your
;; load-path variable in .emacs like this:
;;
;; (setq load-path (cons "/full/path/to/file" load-path))
;;
;; Then add the library like this:
;;
;; (load-library "p4")
;;

;;; Code:

;; We need to remap C-x C-q to p4-toggle-read-only, so, make sure that we
;; load vc first.. or else, when vc gets autoloaded, it will remap C-x C-q
;; to vc-toggle-read-only.
(require 'vc)

(defvar p4-emacs-version "8.0" "The Current P4-Emacs Integration Revision.")

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(defvar p4-running-emacs nil
  "If the current Emacs is not XEmacs, then, this is non-nil.")
(defvar p4-running-xemacs nil
  "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (setq p4-running-xemacs t)
  (setq p4-running-emacs t))

(defvar p4-emacs-maintainer "Rajesh Vaidheeswarran <rv@fore.com>"
  "The maintainer of the emacs-p4 integration. Used for bug reports.")

(defvar p4-web-page "http://www.dsmit.com/p4" "The home of p4.el.")

(eval-and-compile
  (if (< (string-to-int emacs-version) 20)
      (progn
	(defmacro defgroup (sym memb doc &rest args)
	  t)
	(defmacro defcustom (sym val doc &rest args)
	  `(defvar ,sym ,val ,doc)))))

(defgroup p4 nil  "Perforce VC System."  :group 'tools)

;; This can be set to wherever 'p4' lies using p4-set-p4-executable
(defcustom p4-executable
  (let ((lst (list "/usr/swlocal/bin/p4"
		   "/usr/local/bin/p4"
		   "/usr/bin/p4"
		   "/bin/p4"))
	(p4ex nil))
    (while (and lst (not p4ex))
      (if (file-executable-p (car lst))
	  (setq p4ex (car lst)))
      (setq lst (cdr lst)))
    p4ex)
  "This is the p4 executable.
To set this, use the function  `p4-set-p4-executable' or `customize'"
  :type 'string
  :group 'p4)

;; This is a string with default arguments to pass to "p4 diff",
;; "p4 diff2", "p4 describe", etc.
(defcustom p4-default-diff-options "-du"
  "Type of p4 diff output to be displayed. \(regular or context or
unified.\)"
  :type 'string
  :group 'p4)

;; Set this variable to nil to turn off colorized diff buffers.
(defcustom p4-colorized-diffs t
  "Set this to nil to disable colorized diffs."
  :type 'boolean
  :group 'p4)

;; Set whether P4CONFIG should be used exclusively for VC checking
(defcustom p4-use-p4config-exclusively nil
  "Whether P4 mode should use P4CONFIG exclusively to check whether a file
is under P4 version control. If set to nil, `p4-check-mode' is always
called; otherwise, it checks to see if the file named by P4CONFIG exists in
this or a parent directory, and if so, only then runs p4-check-mode.

This provides for a much faster `p4-find-file-hook'."
  :type 'boolean
  :group 'p4)

;; Set the null device
(defcustom p4-null-device
  (if (memq system-type '(ms-dos windows-nt)) "NUL" "/dev/null")
  "Filesystem null device."
  :type 'string
  :group 'p4)

;; Auto-refresh?
(defcustom p4-auto-refresh nil
  "Set this to automatically refresh p4 submitted files in buffers."
  :type 'boolean
  :group 'p4)

;; Auto-refresh?
(defcustom p4-verbose t
  "When set, p4 will pop up the output buffer with the result of the
command."
  :type 'boolean
  :group 'p4)

(defcustom p4-mode-hook nil
  "Hook run by `p4-mode'."
  :type 'sexp
  :group 'p4)


(defvar p4-output-buffer-name "*P4 Output*" "P4 Output Buffer.")
(defvar p4-global-config (getenv "P4CONFIG") "P4 Config to use.")
(defvar p4-global-clt (if p4-global-config nil
			(getenv "P4CLIENT")) "The P4 Client to use.")

;; Set this variable in .emacs if you want p4-set-client-name to complete
;; your client name for you.
(defvar p4-my-clients nil
  "This variable holds the alist of p4 clients that the function
`p4-set-client-name' can complete on.

Set this variable *only* if you don't want P4 to complete on all the clients
in the P4 server.

This is a alist, and should be set using the function
`p4-set-my-clients'. For example, in your .emacs:

\(load-library \"p4\"\)
\(p4-set-my-clients \'(client1 client2 client3)\)")

;; Set this variable in .emacs if you want to alter the completion
;; behavior of p4-set-client-name.

(defcustom p4-strict-complete t
  "Set this variable in .emacs \(or using `customize'\) if you want to alter
the completion behavior of `p4-set-client-name'.
"
  :type 'boolean
  :group 'p4)

(defvar p4-global-server-port  (if p4-global-config
				   (progn
				     (setenv "P4PORT" nil)
				     nil)
				 (getenv "P4PORT"))
  "The P4 Server/Port in use.")
(if (and (eq p4-global-server-port nil) (eq p4-global-config nil))
    (progn
      (setq p4-global-server-port "p4:1666") ;; Default P4 port.
      (setenv "P4PORT" p4-global-server-port)))

(defvar p4-old-notify-list (getenv "P4NOTIFY") "The P4 Notify List.")
(defvar p4-notify-list (getenv "P4NOTIFY") "The P4 Notify List.")

(defcustom p4-sendmail-program (if (boundp 'sendmail-program)
				   sendmail-program
				 nil)
  "The sendmail program. To set this use `p4-set-sendmail-program' or
`customize'."
  :type 'string
  :group 'p4)

(defcustom p4-user-email (if (boundp 'user-mail-address)
			     user-mail-address nil)
  "The e-mail address of the current user. This is used with the
notification system, and must be set if notification should take place. To
set this use `p4-set-user-email' or `customize'."
  :type 'string
  :group 'p4)

(defcustom p4-notify nil
  "If this is t then the users in the notification list set by
`p4-set-notify-list' will get a notification of any P4 change submitted from
within emacs."
  :type 'boolean
  :group 'p4)

;; This can be set with p4-toggle-vc-mode
(defcustom p4-do-find-file t
  "If non-nil, the `p4-find-file-hook' will run when opening files."
  :type 'boolean
  :group 'p4)

;; Now add a hook to find-file-hooks
(add-hook 'find-file-hooks 'p4-find-file-hook)
;; .. and one to kill-buffer-hook
(add-hook 'kill-buffer-hook 'p4-kill-buffer-hook)

;; Tell Emacs about this new kind of minor mode
(defvar p4-mode nil "Is this file under p4?")
(make-variable-buffer-local 'p4-mode)
(put 'p4-mode 'permanent-local t)

(defvar p4-local-client nil "Buffer Local value of the p4 client name.")
(make-variable-buffer-local 'p4-local-client)
(put 'p4-local-client 'permanent-local t)
(set-default 'p4-local-client nil)

(defvar p4-local-server-port nil "Buffer Local value of the p4 server/port.")
(make-variable-buffer-local 'p4-local-server-port)
(put 'p4-local-server-port 'permanent-local t)
(set-default 'p4-local-server-port nil)

(if (not (assoc 'p4-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(p4-mode p4-mode)
				 minor-mode-alist)))

(defvar p4-minor-mode nil
  "The minor mode for editing p4 asynchronous command buffers.")
(make-variable-buffer-local 'p4-minor-mode)
(defvar p4-minor-map (make-keymap) "Keymap for p4 minor mode")
(fset 'p4-minor-map p4-minor-map)

(or (assoc 'p4-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(p4-minor-mode " P4") minor-mode-alist)))

(or (assoc 'p4-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons '(p4-minor-mode . p4-minor-map) minor-mode-map-alist)))

(defvar p4-current-command nil)
(put 'p4-current-command 'permanent-local t)
(set-default 'p4-current-command nil)

(defvar p4-current-args nil)
(put 'p4-current-args 'permanent-local t)
(set-default 'p4-current-args nil)

;; To check if the current buffer's modeline and menu need to be altered

(defvar p4-vc-check nil)
(make-variable-buffer-local 'p4-vc-check)
(put 'p4-vc-check 'permanent-local t)
(set-default 'p4-vc-check nil)

(defvar p4-set-client-hooks nil
  "List of functions to be called after a p4 client is changed.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(if p4-running-emacs (require 'timer))

(defvar p4-timer nil "Timer object that will be set to cleanup the caches
periodically.")

(defcustom p4-cleanup-time 600 "seconds after which `p4-cache-cleanup' will
check for dirty caches."
  :type 'integer
  :group 'p4)

(defcustom p4-cleanup-cache t "`p4-cache-cleanup' will cleanup the
branches/clients/dirs/labels caches once in a while if this is non-nil."
  :type 'boolean
  :group 'p4)

(defvar p4-all-buffer-files nil "An associated list of all buffers and
theirs files under p4 version control. This is to enable autorefreshing of
p4 submitted files being visited by the buffer.")

(defvar p4-file-refresh-timer nil "Timer object that will be set to refresh
the files in Emacs buffers that have been modified by a `p4-submit'.")

(defcustom p4-file-refresh-timer-time 60 "seconds after which
`p4-file-refresh' will check for modified files in Emacs buffers."
  :type 'integer
  :group 'p4)

(defvar p4-async-command-hook nil
  "This hook is run after an async buffer has been set up by
`p4-async-process-command'")

(defvar p4-window-config-stack nil
  "Stack of saved window configurations.")

(defcustom p4-window-config-stack-size 20 "Maximum stack size
for saved window configurations."
  :type 'integer
  :group 'p4)

(defvar p4-basic-map
  (let ((map (make-sparse-keymap)))
    (cond (p4-running-xemacs
	   (define-key map [button2] 'p4-buffer-mouse-clicked))
	  (p4-running-emacs
	   (define-key map [mouse-2] 'p4-buffer-mouse-clicked)))
    (define-key map [return]  'p4-buffer-commands)
    (define-key map "\r" 'p4-buffer-commands)
    (define-key map "q"	 'p4-quit-current-buffer)
    (define-key map "k"	 'p4-scroll-down-1-line)
    (define-key map "j"	 'p4-scroll-up-1-line)
    (define-key map "b"	 'p4-scroll-down-1-window)
    (define-key map [backspace] 'p4-scroll-down-1-window)
    (define-key map " "	 'p4-scroll-up-1-window)
    (define-key map "<"	 'p4-top-of-buffer)
    (define-key map ">"	 'p4-bottom-of-buffer)
    (define-key map "="	 'p4-delete-other-windows)
    map))

(defvar p4-filelog-map
  (let ((map (make-sparse-keymap)))
    (cond (p4-running-xemacs
	   (define-key map [button2] 'p4-buffer-mouse-clicked))
	  (p4-running-emacs
	   (define-key map [mouse-2] 'p4-buffer-mouse-clicked)))
    (define-key map "d"	 'p4-buffer-commands)
    (define-key map [return]  'p4-buffer-commands)
    (define-key map "\r" 'p4-buffer-commands)
    (define-key map "q"	 'p4-quit-current-buffer)
    (define-key map "f"	 'p4-find-file-other-window)
    (define-key map "s"	 'p4-filelog-short-format)
    (define-key map "l"	 'p4-filelog-long-format)
    (define-key map "k"	 'p4-scroll-down-1-line-other-w)
    (define-key map "j"	 'p4-scroll-up-1-line-other-w)
    (define-key map "b"	 'p4-scroll-down-1-window-other-w)
    (define-key map [backspace] 'p4-scroll-down-1-window-other-w)
    (define-key map " "	 'p4-scroll-up-1-window-other-w)
    (define-key map "<"	 'p4-top-of-buffer-other-w)
    (define-key map ">"	 'p4-bottom-of-buffer-other-w)
    (define-key map "="	 'p4-delete-other-windows)
    (define-key map "n"	 'p4-goto-next-change)
    (define-key map "p"	 'p4-goto-prev-change)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting filelog properties.")

(defun p4-make-derived-map (base-map)
  (let (map)
    (cond (p4-running-xemacs
	   (setq map (make-sparse-keymap))
	   (set-keymap-parents map (list base-map)))
	  (p4-running-emacs
	   (setq map (cons 'keymap base-map))))
    map))

(defvar p4-opened-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-next-depot-file)
    (define-key map "p"	 'p4-prev-depot-file)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting opened files.")

(defvar p4-diff-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-goto-next-diff)
    (define-key map "p"	 'p4-goto-prev-diff)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "d"	 'p4-next-depot-diff)
    (define-key map "u"	 'p4-prev-depot-diff)
    map))

(defvar p4-print-rev-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-next-change-rev-line)
    (define-key map "p"	 'p4-prev-change-rev-line)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for browsing print-revs buffers.")

;;; All functions start here.

;; A generic function that we use to execute p4 commands
(defun p4-exec-p4 (output-buffer args &optional clear-output-buffer)
  "Internal function called by various p4 commands."
  (save-excursion
    (if clear-output-buffer
	(progn
	  (set-buffer output-buffer)
	  (delete-region (point-min) (point-max))))
    (apply 'call-process p4-executable p4-null-device
	   output-buffer
	   nil				; update display?
	   args)
    (p4-menu-add)))

(defun p4-push-window-config ()
  "Push the current window configuration on the `p4-window-config-stack'
stack."
  (interactive)
  (setq p4-window-config-stack
	(cons (current-window-configuration)
	      p4-window-config-stack))
  (while (> (length p4-window-config-stack) p4-window-config-stack-size)
    (setq p4-window-config-stack
	  (reverse (cdr (reverse p4-window-config-stack))))))

(defun p4-pop-window-config (num)
  "Pop `num' elements from the `p4-window-config-stack' stack and use
the last popped element to restore the window configuration."
  (interactive "p")
  (while (> num 0)
    (if (eq p4-window-config-stack nil)
	(error "window config stack empty"))
    (set-window-configuration (car p4-window-config-stack))
    (setq p4-window-config-stack (cdr p4-window-config-stack))
    (setq num (1- num)))
  (message "window config popped (stack size %d)"
	   (length p4-window-config-stack)))

;; We use the noinput version for commands like p4 opened, p4 get etc.
;; which don't take any input file name.

(defun p4-noinput-buffer-action (cmd
				 do-revert
				 show-output
				 &optional argument)
  "Internal function called by various p4 commands."
  (save-excursion
    (if (not (stringp cmd))
	(error "p4-noinput-buffer-action: Command not a string."))
    (save-excursion
      (p4-exec-p4 (get-buffer-create p4-output-buffer-name)
		  (if argument
		      (append (list cmd) argument)
		    (list cmd))
		  t))
    (p4-partial-cache-cleanup cmd)
    (if (and do-revert buffer-file-name)
	(revert-buffer t t))
    (if show-output
	(progn
	  (if (and
	       (eq show-output 's)
	       (= (save-excursion
		    (set-buffer p4-output-buffer-name)
		    (count-lines (point-min) (point-max)))
		  1))
	      (save-excursion
		(set-buffer p4-output-buffer-name)
		(message (buffer-substring (point-min)
					   (save-excursion
					     (goto-char (point-min))
					     (end-of-line)
					     (point)))))
	    (p4-push-window-config)
	    (delete-other-windows)
	    (display-buffer p4-output-buffer-name t))))))

;; The p4 edit command
(defun p4-edit (show-output)
  "To open the current depot file for edit, type \\[p4-edit].

Open or re-open an existing file for edit.

If file is already open for edit or delete then it is reopened
for edit and moved into the specified change number (or 'default'
change if no change number is given.)

If -t type is given the file is explicitly opened as the specified
file type, which may be text, ltext, xtext, binary, xbinary, ktext
kxtext, symlink, or resource.  Not all types are supported on all
operating systems.  See the Users' Guide for a description of file
types.	If no type is specified, the type is determined automatically
by examination of the file's contents and execution permission bits.

Argument SHOW-OUTPUT  displays the *P4 Output* buffer on executing the
command if t."

  (interactive (list p4-verbose))
  (let ((args (if (p4-buffer-file-name-2)
		  (p4-buffer-file-name-2)
		""))
	(refresh-after nil))
    (if (or current-prefix-arg (string= "" args))
	(progn
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 edit: " (cons args 0))))
	  (setq refresh-after t))
      (setq args (list args)))
    (p4-noinput-buffer-action "edit" t (and show-output 's) args)
    (if refresh-after
	(p4-refresh-files-in-buffers)))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 reopen command
(defun p4-reopen (show-output)
  "To change the type or changelist number of an opened file, type
\\[p4-reopen].

Reopen takes an already opened file and moves it to a new changelist
or changes its type (text, ltext, xtext, binary, xbinary, ktext,
kxtext, symlink, or resource).

Argument SHOW-OUTPUT  displays the *P4 Output* buffer on executing the
command if t."

  (interactive "P")
  (let ((args (if buffer-file-name
		  buffer-file-name
		"")))
    (setq args (p4-make-list-from-string
		(p4-read-arg-string "p4 reopen: " (cons args 0))))
    (p4-noinput-buffer-action "reopen" t (and show-output 's) args))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 revert command
(defun p4-revert (show-output)
  "Revert all change in the current file.

Argument SHOW-OUTPUT  displays the *P4 Output* buffer on executing the
command if t."
  (interactive (list p4-verbose))
  (let ((args (list (buffer-file-name)))
	(refresh-after nil))
    (if (or current-prefix-arg (not buffer-file-name))
	(progn
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 revert: "
					  (p4-buffer-file-name-2))))
	  (setq refresh-after t)))
    (if (yes-or-no-p "Really revert changes? ")
	(progn
	  (p4-noinput-buffer-action "revert" t (and show-output 's) args)
	  (if refresh-after
	      (p4-refresh-files-in-buffers)))))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 lock command
(defun p4-lock ()
  "Lock an opened file against changelist submission."
  (interactive)
  (let ((args (list (p4-buffer-file-name-2))))
    (if (or current-prefix-arg (not (p4-buffer-file-name-2)))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 lock: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "lock" t 's args)
    (p4-update-opened-list)))

;; The p4 unlock command
(defun p4-unlock ()
  "Release a locked file but leave open."
  (interactive)
  (let ((args (list (p4-buffer-file-name-2))))
    (if (or current-prefix-arg (not (p4-buffer-file-name-2)))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 unlock: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "unlock" t 's args)
    (p4-update-opened-list)))

;; The p4 diff command
(defun p4-diff ()
  "To diff the current file and topmost depot version, type \\[p4-diff].

Run diff (on the client) of a client file against the corresponding
revision in the depot.	The file is only compared if the file is
opened for edit or the revision provided with the file argument is
not the same as the revision had by the client.

If no file argument is given, diff all open files.
This can be used to view pending changes.

The -f flag forces a diff for every file, regardless of whether
they are opened or if the client has the named revision.
This can be used to verify the client contents.

The -s flag outputs reduces the output of diff to the names of files
satisfying the following criteria:

	-sa	Opened files that are different than the revision
		in the depot, or missing.

	-sd	Unopened files that are missing on the client.

	-se	Unopened files that are different than the revision
		in the depot.

	-sr	Opened files that are the same as the revision in the
		depot."

  (interactive)
  (let ((args (p4-make-list-from-string (concat p4-default-diff-options " "
						(p4-buffer-file-name-2)))))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 diff: " p4-default-diff-options))))
    (p4-noinput-buffer-action "diff" nil 's args)
    (p4-activate-diff-buffer "*P4 diff*")))

;; The p4 diff2 command
(defun p4-diff2 (version1 version2)
  "Display diff of two depot files.

When visiting a depot file, type \\[p4-diff2] and
enter the versions.

Example:  (find-file \"/us/rv/tag/main/Construct\")
	  \\[p4-diff2] <RET>
	  First Version to diff: 113
	  Second Version to diff: 100

	  Will produce the diff between the two versions in the
	  output buffer *P4 Output*

Run diff (on the server) of two files in the depot.  Both files
may optionally include a revision specification; the default is
to compare the head revision.  Wildcards may be used, but they
must match between file1 and file2 and they must be escaped from
the user's shell by quoting or with backslashes (\).

The -d flag allows you to pass flags to the underlying diff
program.

-dc passes the -c (context diff) flag.

-du passes the -u (unified diff) flag.

-dn passes the the -n (rcs diff) flag.

Other diff flags are not supported.

Argument VERSION1 First Version to use.
Argument VERSION2 Second Version to use."
  (interactive
   (list (p4-read-arg-string "First Depot File or Version# to diff: ")
	 (p4-read-arg-string "Second Depot File or Version# to diff: ")))
  (let ((p4-diff-version1 p4-vc-check)
	(p4-diff-version2 p4-vc-check)
	(p4-diff-options (p4-make-list-from-string p4-default-diff-options)))
    (if current-prefix-arg
	(setq p4-diff-options (p4-make-list-from-string
			       (p4-read-arg-string "Optional Args: "
						   p4-default-diff-options))))
    ;; try to find out if this is a revision number, or a depot file
    (cond ((string-match "^[0-9]+$" version1)
	   ;; this is a revision of the current file
	   (setq p4-diff-version1 (concat (p4-buffer-file-name-2)
					  "#" version1)))
	  ((string= "" version1)
	   (setq p4-diff-version1 (p4-buffer-file-name-2)))
	  (t
	   ;; this is default.. any random file or a depot file
	   (setq p4-diff-version1 version1)))
    (cond ((string-match "^[0-9]+$" version2)
	   ;; this is a revision of the current file
	   (setq p4-diff-version2 (concat (p4-buffer-file-name-2)
					  "#" version2)))
	  ((string= "" version2)
	   (setq p4-diff-version2 (p4-buffer-file-name-2)))
	  (t
	   ;; this is default.. any random file or a depot file
	   (setq p4-diff-version2 version2)))
    (p4-noinput-buffer-action "diff2" nil t
			      (append p4-diff-options
				      (list p4-diff-version1
					    p4-diff-version2)))
    (p4-activate-diff-buffer "*P4 diff2*")))


;; p4-ediff for all those who diff using ediff

(defun p4-ediff ()
  "Use ediff to compare file with its original client version."
  (interactive)
  (require 'ediff)
  (p4-noinput-buffer-action "print" nil nil
			    (list "-q"
				  (concat (buffer-file-name) "#have")))
  (let ((local (current-buffer))
	(depot (get-buffer-create p4-output-buffer-name)))
    (ediff-buffers local
		   depot
		   `((lambda ()
		       (make-local-variable 'ediff-cleanup-hook)
		       (setq ediff-cleanup-hook
			     (append ediff-cleanup-hook	;; jhyde moved this first
						 (cons (lambda ()
								 (kill-buffer ,depot)
								 (switch-to-buffer ,local) ;; jhyde added
								 (p4-menu-add))
							   nil)))
			   ;; jhyde added...
			   (ediff-jump-to-difference
				(ediff-diff-at-point 'A ,(point))))))))

;; The p4 add command
(defun p4-add ()
  "To add the current file to the depot, type \\[p4-add].

Optional arguments like '-tktext' can be passed as prefix arguments.

Open a new file for adding to the depot.  If the file exists
on the client it is read to determine if it is text or binary.
If it does not exist it is assumed to be text.	The file must
either not exist in the depot, or it must be deleted at the
current head revision.	Files may be deleted and re-added arbitrarily.

If the -c flag is given the open files are associated with the
specified pending change number; otherwise the open files are
associated with the current 'default' change.

If file is already open it is moved into the specified pending
change.	 It is not permissible to reopen a file for add unless
it was already open for add.

If -t type is given the file is explicitly opened as the specified
file type, which may be text, ltext, xtext, binary, xbinary, ktext
kxtext, symlink, or resource.  Not all types are supported on all
operating systems.  See the Users' Guide for a description of file
types.	If no type is specified, the type is determined automatically
by examination of the file's contents and execution permission bits."

  (interactive)
  (if (not (p4-is-vc))
      (progn
	(let ((args (if buffer-file-name
			buffer-file-name
		      "")))
	  (if (or current-prefix-arg (string= "" args))
	      (setq args (p4-make-list-from-string
			  (p4-read-arg-string "p4 add: " (cons args 0))))
	    (setq args (list args)))
	  (p4-noinput-buffer-action "add" nil 's args))
	(p4-check-mode "Add"))
    (message "%s already in depot client %s!" buffer-file-name
	     (p4-current-client)))
  (p4-update-opened-list))


;; The p4 delete command
(defun p4-delete ()
  "To delete the current file from the depot, type \\[p4-delete].

Opens a file that currently exists in the depot for deletion.
If the file is present on the client it is removed.  If a pending
change number is given with the -c flag the opened file is associated
with that change, otherwise it is associated with the 'default'
pending change.

If file is already open it is reopened for delete and moved into
the specified pending change (or 'default' change if no change
number is given.)

Files that are deleted generally do not appear on the have list."

  (interactive)
  (let ((args (buffer-file-name)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 delete: "
					(p4-buffer-file-name-2))))
      (setq args (list args)))
    (if (yes-or-no-p "Really delete from depot? ")
	(p4-noinput-buffer-action "delete" nil 's args)))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 filelog command
(defun p4-filelog ()

  "To view a history of the change made to the current file, type
\\[p4-filelog].

List the revision history of the files named, working backwards
from the latest revision to the most recent revision 'added'.
If file is given as a client file, the depot file last gotten is
listed.	 The -l flag produces long output with the full text of the
change descriptions."

  (interactive)
  (let ((file-name (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not file-name))
	(setq file-name (p4-read-arg-string "p4 filelog: " file-name)))
    (p4-file-change-log "filelog" file-name)))

(defun p4-set-extent-property (start end property value)
  (cond (p4-running-xemacs
	 (set-extent-property (make-extent start end)
			      property value))
	(p4-running-emacs
	 (overlay-put (make-overlay start end)
		      property value))))

(defun p4-create-active-link (start end prop-list)
  (p4-set-extent-property start end 'face 'bold)
  (p4-set-extent-property start end 'mouse-face 'highlight)
  (while prop-list
    (p4-set-extent-property start end (caar prop-list) (cdar prop-list))
    (setq prop-list (cdr prop-list))))

(defun p4-move-buffer-point-to-top (buf-name)
  (if (get-buffer-window buf-name)
      (save-selected-window
	(select-window (get-buffer-window buf-name))
	(goto-char (point-min)))))

(defun p4-file-change-log (cmd filespec)
  (let ((p4-filelog-buffer (concat "*P4 " cmd ": " filespec "*"))
	(p4-cur-rev nil)
	(p4-cur-change nil)
	(p4-cur-action nil)
	(p4-cur-user nil)
	(p4-cur-client nil)
	(p4-filename (p4-make-list-from-string filespec))
	(p4-this-client (p4-current-client))
	(p4-this-server-port (p4-current-server-port)))
    (get-buffer-create p4-output-buffer-name);; We do these two lines
    (kill-buffer p4-output-buffer-name);; to ensure no duplicates
    (p4-noinput-buffer-action cmd nil t (cons "-l" p4-filename))
    (p4-make-depot-list-buffer p4-filelog-buffer)
    (set-buffer p4-filelog-buffer)
    (setq buffer-read-only nil)
    (setq p4-local-client p4-this-client
	  p4-local-server-port p4-this-server-port)
    (make-local-variable 'p4-fname)
    (setq p4-fname (if (equal cmd "filelog")
		       (car p4-filename)
		     nil))
    (goto-char (point-min))
    (while (re-search-forward (concat
			       "^\\(\\.\\.\\. #\\([0-9]+\\) \\)?change "
			       "\\([0-9]+\\) \\([a-z]+\\)?.*on.*by "
			       "\\([^ @]+\\)@\\([^ \n]+\\).*\n"
			       "\\(\\(\\([ \t].*\\)?\n\\)*\\)") nil t)
      (let ((rev-match 2)
	    (ch-match 3)
	    (act-match 4)
	    (user-match 5)
	    (cl-match 6)
	    (desc-match 7))
	(setq p4-cur-rev (match-string rev-match))
	(setq p4-cur-change (match-string ch-match))
	(setq p4-cur-action (match-string act-match))
	(setq p4-cur-user (match-string user-match))
	(setq p4-cur-client (match-string cl-match))

	(if (match-beginning rev-match)
	    (p4-create-active-link (match-beginning rev-match)
				   (match-end rev-match)
				   (list (cons 'rev p4-cur-rev))))
	(p4-create-active-link (match-beginning ch-match)
			       (match-end ch-match)
			       (list (cons 'change p4-cur-change)))
	(if (match-beginning act-match)
	    (p4-create-active-link (match-beginning act-match)
				   (match-end act-match)
				   (list (cons 'action p4-cur-action)
					 (cons 'rev p4-cur-rev))))
	(p4-create-active-link (match-beginning user-match)
			       (match-end user-match)
			       (list (cons 'user p4-cur-user)))
	(p4-create-active-link (match-beginning cl-match)
			       (match-end cl-match)
			       (list (cons 'client p4-cur-client)))
	(p4-set-extent-property (match-beginning desc-match)
				(match-end desc-match)
				'invisible t)))
    (p4-find-change-numbers p4-filelog-buffer (point-min) (point-max))
    (use-local-map p4-filelog-map)
    (setq buffer-invisibility-spec (list))
    (setq buffer-read-only t)
    (p4-move-buffer-point-to-top p4-filelog-buffer)))

;; Scan specified region for references to change numbers
;; and make the change numbers clickable.
(defun p4-find-change-numbers (buffer start end)
  (save-excursion
    (set-buffer buffer)
    (goto-char start)
    (while (re-search-forward "\\(changes?\\|submit\\|p4\\):?[ \t\n]+" end t)
      (while (looking-at
	      (concat "\\(#\\|number\\|no\\.\\|\\)[ \t\n]*"
		      "\\([0-9]+\\)[, \t\n]*"
		      "\\(and/or\\|and\\|or\\|\\)[ \t\n]*"))
	(let ((ch-start (match-beginning 2))
	      (ch-end (match-end 2))
	      (ch-str (match-string 2))
	      (next (match-end 0)))
	  (set-text-properties 0 (length ch-str) nil ch-str)
	  (p4-create-active-link ch-start ch-end (list (cons 'change ch-str)))
	  (goto-char next))))))

;; The p4 files command
(defun p4-files ()
  "List files in the depot. Type, \\[p4-files].

Optional args [file ...] are passed as prefix arguments.

List files named or matching wild card specification.  Display shows depot
file name, revision, file type, change action and change number of the
current head revision.	If client file names are given as arguments the view
mapping is used to list the corresponding depot files."

  (interactive)
  (let ((args (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 files: " (p4-buffer-file-name-2))))
      (setq args (list args)))
    (get-buffer-create p4-output-buffer-name);; We do these two lines
    (kill-buffer p4-output-buffer-name);; to ensure no duplicates
    (p4-noinput-buffer-action "files" nil t args)
    (save-excursion
      (set-buffer p4-output-buffer-name)
      (p4-find-change-numbers p4-output-buffer-name (point-min) (point-max)))
    (p4-make-depot-list-buffer
     (concat "*P4 Files: (" (p4-current-client) ") " (car args) "*"))))

(make-face 'p4-depot-unmapped-face)
(set-face-foreground 'p4-depot-unmapped-face "grey30")

(make-face 'p4-depot-deleted-face)
(set-face-foreground 'p4-depot-deleted-face "red")

(make-face 'p4-depot-added-face)
(set-face-foreground 'p4-depot-added-face "blue")

;; Take the p4-output-buffer-name buffer, rename it to bufname, and
;; make all depot file names active, so that clicking them opens
;; the corresponding client file.
(defun p4-make-depot-list-buffer (bufname)
  (let (args max files p4-client-root p4-server-version p4-opened-buffer
	     depot-regexp (p4-this-client (p4-current-client)))
    (if p4-this-client
	(progn
	  (set-buffer p4-output-buffer-name)
	  (goto-char (point-min))
	  (setq depot-regexp
		"^\\(\\.\\.\\. [^/\n]*\\|==== \\)?\\(//[a-zA-Z]+/[^ #\n]*\\)")
	  (while (re-search-forward depot-regexp nil t)
	    (setq args (cons (match-string 2) args)))
	  (setq max (point-max))
	  (goto-char max)
	  (setq p4-client-root (p4-get-client-root p4-this-client))
	  (setq p4-server-version (p4-get-server-version))
	  (if (memq system-type '(ms-dos windows-nt))
	      ;; For Windows, since the client root will be terminated with
	      ;; a \ as in c:\ or drive:\foo\bar\, we need to strip the
	      ;; trailing \ .
	      (let ((p4-clt-root-len (length p4-client-root)))
		(setq p4-clt-root-len (1- p4-clt-root-len))
		(setq p4-client-root (substring p4-client-root 0
						p4-clt-root-len))))
	  (setq p4-opened-buffer bufname)
	  (get-buffer-create p4-opened-buffer);; We do these two lines
	  (kill-buffer p4-opened-buffer);; to ensure no duplicates
	  (set-buffer p4-output-buffer-name)
	  (delete-region max (point-max))
	  (insert "\n")
	  (apply 'call-process
		 p4-executable nil t nil "where" (reverse args))
	  (goto-char max)
	  (if (< p4-server-version 98)
	      (progn
		(while (re-search-forward
			(concat "^\\([^ \n]+\\) //" p4-this-client
				"\\(.*\\)$") nil t)
		  (setq files (cons
			       (cons
				(match-string 1)
				(concat p4-client-root (match-string 2)))
			       files))))
	    (progn
	      (while (re-search-forward
		      "^\\([^ \n]+\\) //\\([^ \n]+\\) \\(.*\\)$" nil t)
		(setq files (cons
			     (cons
			      (match-string 1)  (match-string 3)) files)))))
	  (delete-region max (point-max))
	  (goto-char (point-min))
	  (rename-buffer p4-opened-buffer t)
	  (while (re-search-forward depot-regexp nil t)
	    (let ((p4-cur-file (cdr (assoc (match-string 2) files)))
		  (p4-depot-file (match-string 2))
		  (start (match-beginning 2))
		  (end (match-end 2)))
	      (cond ((not p4-cur-file)
		     (p4-set-extent-property start end 'face
					     'p4-depot-unmapped-face)
		     (p4-set-extent-property start end 'unmapped-file
					     p4-depot-file))
		    ((save-excursion
		       (goto-char end)
		       (looking-at ".* deleted?[ \n]"))
		     (p4-set-extent-property start end 'face
					     'p4-depot-deleted-face)
		     (p4-set-extent-property start end 'deleted-filename
					     p4-depot-file))
		    ((save-excursion
		       (goto-char end)
		       (looking-at ".* \\(add\\|branch\\)\\(ed\\)?[ \n]"))
		     (p4-create-active-link start end
					    (list
					     (cons 'filename
						   p4-cur-file)
					     (cons 'face
						   'p4-depot-added-face))))
		    (t
		     (p4-create-active-link start end
					    (list (cons 'filename
							p4-cur-file)))))))
	  (use-local-map p4-opened-map)
	  (setq buffer-read-only t)
	  (p4-move-buffer-point-to-top p4-opened-buffer)))))

;; The p4 print command
(defun p4-print ()
  "To print a depot file to a buffer, type \\[p4-print].

Retrieve the contents of a depot file to the client's standard
output.	 The client's gotten list is not affected.  If file is
specified as a client file name, the client view is used to
find the corresponding depot file.  The -q flag suppresses the
initial line that displays the file name and revision."

  (interactive)
  (let ((arg-string (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (p4-read-arg-string "p4 print: " arg-string)))
    (p4-noinput-buffer-action
     "print" nil t (p4-make-list-from-string arg-string))
    (p4-activate-print-buffer "*P4 print*")))

(defun p4-activate-print-buffer (buffer-name)
  (p4-make-depot-list-buffer buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward "^//[a-zA-Z]+/" nil t)
      (let ((fname (get-char-property (match-end 0) 'filename)))
	(p4-set-extent-property (match-beginning 0) (point-max)
				'local-fname fname)))
    (setq buffer-read-only t)))


(defun p4-print-with-rev-history ()
  "To Print a depot file with revision history to a buffer,
type \\[p4-print-with-rev-history]"
  (interactive)
  (let ((arg-string (p4-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (p4-read-arg-string "p4 print-revs: " arg-string)))
    (p4-print-with-rev-history-int arg-string)))

(defun p4-print-with-rev-history-int (file-spec)
  (let ((file-name file-spec)
	(buffer (get-buffer-create p4-output-buffer-name))
	change head-rev fullname headseen ch-alist)
    (if (string-match "\\(.*\\)@\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq change (string-to-int (match-string 2 file-spec)))))
    (if (string-match "\\(.*\\)#\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq head-rev (string-to-int (match-string 2 file-spec)))))
    (p4-exec-p4 buffer (list "files" file-name) t)
    (save-excursion
      (set-buffer buffer)
      (if (> (count-lines (point-min) (point-max)) 1)
	  (error "File pattern maps to more than one file.")))
    (p4-exec-p4 buffer (list "filelog" file-name) t)
    (setq fullname (p4-read-depot-output buffer))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (looking-at (concat "^\\.\\.\\. #\\([0-9]+\\) change \\([0-9]+\\)"
				"\\s-+\\(\\w+\\) .* by \\(.*\\)@"))
	    (let ((rev (string-to-int (match-string 1)))
		  (ch (string-to-int (match-string 2)))
		  (op (match-string 3)))
	      (cond ((and change (< change  ch))
		     nil)
		    ((and head-rev (< head-rev rev))
		     nil)
		    ((string= op "delete")
		     (goto-char (point-max)))
		    (t
		     (setq ch-alist (cons (cons rev ch) ch-alist))
		     (if (not head-rev)
			 (setq head-rev rev))
		     (setq headseen t))))
	  (if headseen
	      (if (looking-at "^\\.\\.\\. \\.\\.\\. branch from")
		  (goto-char (point-max)))))
	(forward-line)))
    (if (< (length ch-alist) 1)
	(error "Head revision not available"))
    (let ((base-rev (int-to-string (caar ch-alist)))
	  (ch-buffer (get-buffer-create "p4-ch-buf"))
	  (tmp-alst (copy-alist ch-alist)))
      (p4-exec-p4 ch-buffer (list "print" "-q"
				  (concat fullname "#" base-rev))
		  t)
      (save-excursion
	(set-buffer ch-buffer)
	(goto-char (point-min))
	(while (re-search-forward ".*\n" nil t)
	  (replace-match (concat base-rev "\n"))))
      (while (> (length tmp-alst) 1)
	(let ((rev-1 (caar tmp-alst))
	      (rev-2 (car (cadr tmp-alst)))
	      ins-string)
	  (setq ins-string (concat rev-2 "\n"))
	  (p4-exec-p4 buffer (list "diff2"
				   (concat fullname "#"
					   (int-to-string rev-1))
				   (concat fullname "#"
					   (int-to-string rev-2)))
		      t)
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (while (re-search-backward
		    (concat "^\\([0-9]+\\),?\\([0-9]*\\)\\([acd]\\)"
			    "\\([0-9]+\\),?\\([0-9]*\\)")
		    nil t)
	      (let ((la (string-to-int (match-string 1)))
		    (lb (string-to-int (match-string 2)))
		    (op (match-string 3))
		    (ra (string-to-int (match-string 4)))
		    (rb (string-to-int (match-string 5))))
		(if (= lb 0)
		    (setq lb la))
		(if (= rb 0)
		    (setq rb ra))
		(cond ((string= op "a")
		       (setq la (1+ la)))
		      ((string= op "d")
		       (setq ra (1+ ra))))
		(save-excursion
		  (set-buffer ch-buffer)
		  (goto-line la)
		  (let ((beg (point)))
		    (forward-line (1+ (- lb la)))
		    (delete-region beg (point)))
		  (while (<= ra rb)
		    (insert ins-string)
		    (setq ra (1+ ra)))))))
	  (setq tmp-alst (cdr tmp-alst))))
      (p4-noinput-buffer-action "print" nil t
				(list (concat fullname "#" (int-to-string
							    head-rev))))
      (let (line rev ch (old-rev 0))
	(save-excursion
	  (set-buffer buffer)
	  (make-local-variable 'p4-fname)
	  (setq p4-fname file-name)
	  (goto-line 2)
	  (move-to-column 0)
	  (insert "  Change  Rev\n")
	  (while (setq line (p4-read-depot-output ch-buffer))
	    (setq rev (string-to-int line))
	    (setq ch (cdr (assq rev ch-alist)))
	    (if (= rev old-rev)
		(insert (format "%13s : " ""))
	      (insert (format "  %6d %4d : " ch rev))
	      (move-to-column 0)
	      (if (looking-at " *\\([0-9]+\\) *\\([0-9]+\\)")
		  (progn
		    (p4-create-active-link (match-beginning 1)
					   (match-end 1)
					   (list (cons 'change
						       (match-string 1))))
		    (p4-create-active-link (match-beginning 2)
					   (match-end 2)
					   (list (cons 'rev
						       (match-string 2)))))))
	    (setq old-rev rev)
	    (forward-line))))

      (kill-buffer ch-buffer))
    (let ((buffer-name (concat "*P4 print-revs " file-name "*")))
      (p4-activate-print-buffer buffer-name)
      (save-excursion
	(set-buffer buffer-name)
	(use-local-map p4-print-rev-map)))))

;; The p4 refresh command
(defun p4-refresh ()
  "Refresh the contents of an unopened file. \\[p4-refresh].

Optional args [file ...] are passed as prefix arguments.

Refresh replaces the files with their contents from the depot.	Refresh only
refreshes unopened files; opened files must be reverted.  This command
requires naming files explicitly."

  (interactive)
  (let ((args (buffer-file-name)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 refresh: ")))
      (setq args (list args)))
    (p4-noinput-buffer-action "refresh" nil t args)
    (p4-refresh-files-in-buffers)
    (p4-make-depot-list-buffer
     (concat "*P4 Refresh: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 get/sync command
(defun p4-sync ()
  (interactive)
  (p4-get))

(defun p4-get ()
  "To synchronise the local view with the depot, type \\[p4-get].

Optional args [-n] [file ...] can be passed as prefix arguments.

Synchronize a client with its view for the files named, or for the entire
client if no files named.  Get handles the case where files have been
updated in the depot and need to be brought up-to-date on the client as well
as the case when the view itself has changed.

Depot files in the clients view not currently gotten on the client will be
added.	Client files that were gotten from the depot but that are no longer
in the clients view (or have been deleted in the depot) will be deleted from
the client. Client files that are still in the client view but which have
been updated in the depot are replaced by the needed revision from the
depot.

If file gives a revision specifier, then retrieve the revision so indicated.

The client view is used to map client file names to depot file names and
vice versa.

If -n is given show what revisions would need to be gotten to synchronize
the client with the view, but do not actually get the files.  If no files
are named show the result for the entire client view."

  (interactive)
  (let ((args 'nil))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 get: "))))
    (p4-noinput-buffer-action "get" nil t args)
    (p4-refresh-files-in-buffers)
    (p4-make-depot-list-buffer
     (concat "*P4 Get: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 have command
(defun p4-have ()
  "To list revisions last gotten, type \\[p4-have].

Optional args [file ...] are passed as prefix arguments.

List revisions of named files that were last gotten from the depot.  If no
file name is given list all files gotten on this client."

  (interactive)
  (let ((args (list "...")))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 have: " (p4-buffer-file-name-2)))))
    (get-buffer-create p4-output-buffer-name) ;; We do these two lines
    (kill-buffer p4-output-buffer-name)	      ;; to ensure no duplicates
    (p4-noinput-buffer-action "have" nil t args)
    (p4-make-depot-list-buffer
     (concat "*P4 Have: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 changes command
(defun p4-changes ()
  "To list changes, type \\[p4-changes].

Optional args [file ...] are passed as prefix arguments.

List pending and submitted changes of named files.  If no file name is
given list all changes affecting the current directory and below."

  (interactive)
  (let ((arg-string "..."))
    (if current-prefix-arg
	(setq arg-string (p4-read-arg-string "p4 changes: " "-m 200")))
    (p4-file-change-log "changes" arg-string)))

;; The p4 help command
(defun p4-help (arg)
  "To print help message , type \\[p4-help].

Print a help message about command.  If no command name is given print a
general help message about Perforce and give a list of available client
commands.

Argument ARG command for which help is needed."

  (interactive "sHelp on which command: ")
  (p4-noinput-buffer-action "help" nil t (p4-make-list-from-string arg))
  (p4-make-basic-buffer "*P4 help*"))

(defun p4-make-basic-buffer (buf-name)
  (get-buffer-create buf-name)
  (kill-buffer buf-name)
  (set-buffer p4-output-buffer-name)
  (goto-char (point-min))
  (rename-buffer buf-name t)
  (use-local-map p4-basic-map)
  (setq buffer-read-only t)
  (p4-move-buffer-point-to-top buf-name))

;; The p4 info command
(defun p4-info ()
  "To print out client/server information, type \\[p4-info].

Info dumps out what the server knows about the client (the user
name, the client name, and the client directory) and some server
information (the server's address, version, and license data)."

  (interactive)
  (p4-noinput-buffer-action "info" nil t)
  (p4-make-basic-buffer "*P4 info*"))

;; The p4 integrate command
(defun p4-integ ()
  "To schedule integrations between branches, type \\[p4-integ].

Optional args [-n -r] [-c change#] [file ...] are passed as prefix
arguments.

Integ determines what integrations are necessary between related files,
according to the branch named and its view.  These integrations, represented
by a list of revisions of branch source files which need to be merged into
the related branch target file, are scheduled for later action.	 The actual
merge and any necessary conflict resolution is performed using the resolve
command.  The -n flag displays what integrations would be necessary but does
not schedule them.  A branch name is required.

If the -r flag is present, the mappings in the branch view are reversed,
with the target files and source files exchanging place.

If no file names are given then the entire branch view is examined for
needed integrations.  Files that are not mapped in the client's view are
ignored.  Files scheduled for integration are opened for the appropriate
action in the default change.  If -c change# is given the files are opened
in the numbered pending change.

Argument BRANCH is the branch to integrate into."

  (interactive)
  (let ((args (p4-make-list-from-string
	       (p4-read-arg-string "p4 integ: " "-b "))))
    (p4-noinput-buffer-action "integ" nil t args)))

(defun p4-rename ()
  "To rename a file in the depot, type \\[p4-rename].

Perforce does not support a single 'rename' command, but files can
be renamed by branching one file into another and then deleting the
original file.

The 'from' and 'to' file arguments may include wildcards as long as
they are matched.

Integrating from files require read access to the files, but deleting
them requires write access.

For further information, see the help for the individual commands."

  (interactive)
  (let (from-file to-file)
    (setq from-file (p4-read-arg-string "rename from: " buffer-file-name))
    (setq to-file (p4-read-arg-string "rename to: " buffer-file-name))
    (p4-noinput-buffer-action "integ" nil t (list from-file to-file))
    (p4-exec-p4 (get-buffer-create p4-output-buffer-name)
		(list "delete" from-file)
		nil)))

(defun p4-scroll-down-1-line ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1))

(defun p4-scroll-up-1-line ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1))

(defun p4-scroll-down-1-window ()
  "Scroll down one window"
  (interactive)
  (scroll-down
   (- (window-height) next-screen-context-lines)))

(defun p4-scroll-up-1-window ()
  "Scroll up one window"
  (interactive)
  (scroll-up
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer ()
  "Top of buffer"
  (interactive)
  (goto-char (point-min)))

(defun p4-bottom-of-buffer ()
  "Bottom of buffer"
  (interactive)
  (goto-char (point-max)))

(defun p4-delete-other-windows ()
  "Make buffer full height"
  (interactive)
  (delete-other-windows))

(defun p4-goto-next-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^====" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-goto-prev-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^====" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-next-depot-file ()
  "Next file"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^//[a-zA-Z]+/" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-file ()
  "Previous file"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^//[a-zA-Z]+/" nil "")
  (set-window-start (selected-window) (point)))


(defun p4-next-depot-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-next-change-rev-line ()
  "Next change/revision line"
  (interactive)
  (let ((c (if (< (current-column) 8) 7 12)))
    (move-to-column 2)
    (re-search-forward "^ +[0-9]+ +[0-9]+ :" nil "")
    (move-to-column c)))

(defun p4-prev-change-rev-line ()
  "Previous change/revision line"
  (interactive)
  (let ((c (if (< (current-column) 8) 7 12)))
    (forward-line -1)
    (move-to-column 16)
    (re-search-backward "^ +[0-9]+ +[0-9]+ :" nil "")
    (move-to-column c)))

(defun p4-quit-current-buffer (pnt)
  "Quit a buffer"
  (interactive "d")
  (if (not (one-window-p))
      (delete-window)
    (bury-buffer)))

(defun p4-buffer-mouse-clicked (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (cond (p4-running-xemacs
	 (select-window (event-window event))
	 (p4-buffer-commands (event-point event)))
	(p4-running-emacs
	 (select-window (posn-window (event-end event)))
	 (p4-buffer-commands (posn-point (event-start event))))))

(defun p4-buffer-commands (pnt)
  "Function to get a given property and do the appropriate command on it"
  (interactive "d")
  (let ((rev (get-char-property pnt 'rev))
	(change (get-char-property pnt 'change))
	(action (get-char-property pnt 'action))
	(user (get-char-property pnt 'user))
	(client (get-char-property pnt 'client))
	(filename (if (boundp 'p4-fname) p4-fname)))
    (cond ((and (not action) rev)
	   (let ((fn1 (concat filename "#" rev)))
	     (p4-noinput-buffer-action "print" nil t (list fn1))
	     (p4-activate-print-buffer "*P4 print*")))
	  (action
	   (let ((rev2 (int-to-string (1- (string-to-int rev))))
		 (fn1 (concat filename "#" rev))
		 (fn2 nil))
	     (setq fn2 (concat filename "#" rev2))
	     (if (> (string-to-int rev2) 0)
		 (progn
		   (p4-noinput-buffer-action
		    "diff2" nil t
		    (append (p4-make-list-from-string
			     p4-default-diff-options)
			    (list fn2 fn1)))
		   (p4-activate-diff-buffer "*P4 diff*"))
	       (error "There is no earlier revision to diff."))))
	  (change (p4-describe-internal
		   (concat p4-default-diff-options " " change)))
	  (user (p4-async-process-command "user" nil
					  (concat
					   "*P4 User: " user "*")
					  "user" (list user)))
	  (client (p4-async-process-command
		   "client" nil (concat "*P4 Client: " client
					"*") "client" (list client)))

	  ;; Check if a "filename link" or an active "diff buffer area" was
	  ;; selected.
	  (t
	   (let ((p4-this-file (get-char-property pnt 'filename))
		 (local-fname (get-char-property pnt 'local-fname))
		 (first-line (get-char-property pnt 'first-line))
		 (start (get-char-property pnt 'start))
		 (p4-unmapped-file (get-char-property pnt 'unmapped-file)))
	     (cond (p4-this-file
		    (find-file-other-window p4-this-file))
		   (local-fname
		    (if first-line
			(let ((c (max 0 (- pnt
					   (save-excursion
					     (goto-char pnt)
					     (beginning-of-line)
					     (point))
					   1)))
			      (r first-line))
			  (save-excursion
			    (goto-char start)
			    (while (re-search-forward "^[ +>].*\n" pnt t)
			      (setq r (1+ r))))
			  (find-file-other-window local-fname)
			  (goto-line r)
			  (beginning-of-line)
			  (goto-char (+ (point) c)))
		      (find-file-other-window local-fname)))
		   (p4-unmapped-file
		    (p4-noinput-buffer-action "print" nil t
					      (list p4-unmapped-file))
		    (p4-activate-print-buffer "*P4 print*"))
		   (t
		    (error "There is no file at that cursor location!"))))))))

(defun p4-find-file-other-window ()
  "Open file"
  (interactive)
  (if (p4-buffer-file-name-2)
      (progn
	(find-file-other-window
	 (p4-buffer-file-name-2))
	(other-window 1))))

(defun p4-filelog-short-format ()
  "Short format"
  (interactive)
  (setq buffer-invisibility-spec t)
  (redraw-display))

(defun p4-filelog-long-format ()
  "Long format"
  (interactive)
  (setq buffer-invisibility-spec (list))
  (redraw-display))

(defun p4-scroll-down-1-line-other-w ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window -1))

(defun p4-scroll-up-1-line-other-w ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))

(defun p4-scroll-down-1-window-other-w ()
  "Scroll other window down one window"
  (interactive)
  (scroll-other-window
   (- next-screen-context-lines (window-height))))

(defun p4-scroll-up-1-window-other-w()
  "Scroll other window up one window"
  (interactive)
  (scroll-other-window
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer-other-w ()
  "Top of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window -1))

(defun p4-bottom-of-buffer-other-w ()
  "Bottom of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun p4-goto-next-change ()
  "Next change"
  (interactive)
  (let ((c (current-column)))
    (forward-line 1)
    (while (get-char-property (point) 'invisible)
      (forward-line 1))
    (move-to-column c)))

(defun p4-goto-prev-change ()
  "Previous change"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (while (get-char-property (point) 'invisible)
      (forward-line -1))
    (move-to-column c)))


;; Activate special handling for a buffer generated with a diff-like command
(make-face 'p4-diff-file-face)
;;(set-face-foreground 'p4-diff-file-face "black")
(set-face-background 'p4-diff-file-face "gray90")

(make-face 'p4-diff-head-face)
;;(set-face-foreground 'p4-diff-head-face "black")
(set-face-background 'p4-diff-head-face "gray95")

(make-face 'p4-diff-ins-face)
(set-face-foreground 'p4-diff-ins-face "blue")

(make-face 'p4-diff-del-face)
(set-face-foreground 'p4-diff-del-face "red")

(make-face 'p4-diff-change-face)
(set-face-foreground 'p4-diff-change-face "dark green")

(defun p4-buffer-set-face-property (regexp face-property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0))
	    (end (match-end 0)))
	(p4-set-extent-property start end
				'face face-property)))))

(defun p4-activate-diff-buffer (buffer-name)
  (p4-make-depot-list-buffer buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (if p4-colorized-diffs
	(progn
	  (p4-buffer-set-face-property "^=.*\n" 'p4-diff-file-face)
	  (p4-buffer-set-face-property "^\\(@\\|\\*\\).*" 'p4-diff-head-face)
	  (p4-buffer-set-face-property "^\\(\\+\\|>\\).*$" 'p4-diff-ins-face)
	  (p4-buffer-set-face-property "^\\(\\-\\|<\\).*$" 'p4-diff-del-face)
	  (p4-buffer-set-face-property "^!.*$" 'p4-diff-change-face)))

    (goto-char (point-min))
    (while (re-search-forward "^\\(==== //\\).*\n\\(\\(\n\\|[^=\n].*\n\\)*\\)"
			      nil t)
      (let ((fname (get-char-property (match-end 1) 'filename))
	    (start (match-beginning 2))
	    (end (match-end 2)))
	(p4-set-extent-property start end 'local-fname fname)))

    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^[@0-9].*\\([cad+]\\)\\([0-9]*\\).*\n"
		    "\\(\\(\n\\|[^@0-9\n].*\n\\)*\\)") nil t)
      (let ((first-line (string-to-int (match-string 2)))
	    (start (match-beginning 3))
	    (end (match-end 3)))
	(p4-set-extent-property start end 'first-line first-line)
	(p4-set-extent-property start end 'start start)))

    (goto-char (point-min))
    (let ((stop
	   (if (re-search-forward "^\\(\\.\\.\\.\\|====\\)" nil t)
	       (match-beginning 0)
	     (point-max))))
      (p4-find-change-numbers buffer-name (point-min) stop))

    (use-local-map p4-diff-map)
    (setq buffer-read-only t)))


;; The p4 describe command
(defun p4-describe ()
  "To get a description for a change number, type \\[p4-describe].

Display a changelist description, including the changelist number,
user, client, date of submission, textual description, list
of affected files and diffs of files updated.  Pending changelists
are flagged as 'pending' and the list of affected files and
file diffs is not displayed.

The -d<flag> passes a flag to the built-in diff routine to
modify the output: -dn (RCS), -dc (context), -du (unified).

The -s flag requests a shortened form of describe that doesn't
include the diffs of files updated."

  (interactive)
  (let ((arg-string (read-string "p4 describe: "
				 (concat p4-default-diff-options " "))))
    (p4-describe-internal arg-string)))

;; Internal version of the p4 describe command
(defun p4-describe-internal (arg-string)
  (get-buffer-create p4-output-buffer-name) ;; We do these two lines
  (kill-buffer p4-output-buffer-name)	    ;; to ensure no duplicates
  (p4-noinput-buffer-action
   "describe" nil t (p4-make-list-from-string arg-string))
  (p4-activate-diff-buffer
   (concat "*P4 describe: " arg-string "*")))

;; The p4 opened command
(defun p4-opened ()
  "To display list of files opened for pending change, type \\[p4-opened].

Optional args [-a] [file ...] are passed as prefix arguments.

Shows files currently opened for pending changes or indicates for the
specified individual files whether they are currently opened.
If no file names are given, all files open on the current client
are listed.  The -a flag lists opened files in all clients."

  (interactive)
  (let ((args '()))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 opened: "
					(p4-buffer-file-name-2)))))
    (p4-opened-internal args)))

(defun p4-opened-internal (args)
  (let ((p4-client (p4-current-client)))
    (get-buffer-create p4-output-buffer-name) ;; We do these two lines
    (kill-buffer p4-output-buffer-name)	      ;; to ensure no duplicates
    (p4-noinput-buffer-action "opened" nil t args)
    (p4-make-depot-list-buffer (concat "*Opened Files: " p4-client "*"))))

(defun p4-update-opened-list ()
  (if (get-buffer-window (concat "*Opened Files: " (p4-current-client) "*"))
      (progn
	(setq current-prefix-arg nil)
	(p4-opened-internal nil))))

;; The p4 users command
(defun p4-users ()
  "To display list of known users, type \\[p4-users].

Optional args [user ...] are passed as prefix arguments.

Reports the list of all users, or those users matching the argument,
currently known to the system.	The report includes the last time
each user accessed the system."

  (interactive)
  (let ((args '()))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 users: " nil "user"))))
    (p4-noinput-buffer-action "users" nil t args))
  (p4-make-basic-buffer "*P4 users*"))

;; The p4 jobs command
(defun p4-jobs ()
  "To display list of jobs, type \\[p4-jobs].

Optional args [ -e jobview -i -l -m max ] [ file[revRange] ... ] are passed
as prefix arguments.

Reports the list of all jobs currently known to the system.  If a
file (pattern) is given, only fixes for changelists affecting that
file (or set of files) are listed.  The file pattern may include
wildcards and/or a revision number range.  See 'p4 help revisions'
for help specifying revisions.

The -e jobview limits the output to jobs satisfying the expression
given as 'jobview'.  See 'p4 help jobview' for a description of
jobview syntax.

The -i flag also includes any fixes made by changelists integrated
into the specified files.

The -l flag produces long output with the full text of the job
descriptions.

The -m max flag limits the output to the first 'max' jobs,
ordered by their job name."
  (interactive)
  (let ((args '()))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 jobs: "))))
    (p4-noinput-buffer-action "jobs" nil t args))
  (p4-make-basic-buffer "*P4 jobs*"))

;; The p4 fix command
(defun p4-fix ()
  "To mark jobs as being fixed by a changelist number, type \\[p4-fix].

'p4 fix' marks each named job as being fixed by the changelist
number given with -c.  The changelist may be either pending or,
submitted and the jobs may be still be opened or already closed
 (fixed by another changelist).

If the changelist has already been submitted and the job is still
open then 'p4 fix' marks the job closed.  If the changelist has not
been submitted and the job is still open, the job will be marked
closed when the changelist is submitted.  If the job is already
closed, it is left alone.

The -d flag causes the specified fixes to be deleted.  This does not
otherwise affect the named changelist or jobs."
  (interactive)
  (let ((args (p4-make-list-from-string (p4-read-arg-string "p4 fix: "
							    nil "job"))))
    (p4-noinput-buffer-action "fix" nil t args)))

;; The p4 fixes command
(defun p4-fixes ()
  "To list what changeslists fix what jobs, type \\[p4-fixes].

p4 fixes [ -i ] [ -j jobName ] [ -c changelist# ] [ file[revRange] ... ]

'p4 fixes' shows all jobs with fix records associated with them,
along with the changelist number of the fix.  Fix records are
created either directly with the 'p4 fix' command or via changelist
creation with the 'p4 change' and 'p4 submit' commands.

The 'p4 fixes' command show fixes regardless of whether the
changelists are submitted or still pending.

By default, 'p4 fixes' lists all fixes.  This list can be limited in
any of three ways.  If -j jobName is given, only fixes for the named
job are listed.  If -c changelist# is given, only fixes from the
numbered changelist are listed.  If a file (pattern) is given, only
fixes for changelists affecting that file (or set of files) are
listed.  The file pattern may include wildcards and/or a revision
number range.  See 'p4 help revisions' for help specifying revisions.

The -i flag also includes any fixes made by changelists integrated
into the specified files."
  (interactive)
  (let ((args 'nil))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 fixes: "))))
    (p4-noinput-buffer-action "fixes" nil t args)
    (p4-make-basic-buffer "*P4 fixes*")))

;; The p4 where command
(defun p4-where ()
  "To show how local file names map into depot names, type \\[p4-where].

Optional args [file ...] are passed as prefix arguments.

Where shows how the named files map through the client map
into the depot.	 If no file is given, the mapping for '...'
\(all files in the current directory and below\) is shown."
  (interactive)
  (let ((args '()))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 where: "
					(p4-buffer-file-name-2)))))
    (get-buffer-create p4-output-buffer-name) ;; We do these two lines
    (kill-buffer p4-output-buffer-name)	      ;; to ensure no duplicates
    (p4-noinput-buffer-action "where" nil 's args)))

;; The following two commands have replaced the deprecated `p4-async-commands'
;; and are much more elegant since they don't depend on external editor
;; clients.

(defun p4-async-process-command (p4-this-command &optional
						 p4-regexp
						 p4-this-buffer
						 p4-out-command
						 p4-in-args
						 p4-out-args)
  "Internal function to call an asynchronous process with a local buffer,
instead of calling an external client editor to run within emacs.

Arguments:
P4-THIS-COMMAND is the command that called this internal function.

P4-REGEXP is the optional regular expression to search for to set the cursor
on.

P4-THIS-BUFFER is the optional buffer to create. (Default is *P4 <command>*).

P4-OUT-COMMAND is the optional command that will be used as the command to
be called when `p4-async-call-process' is called.

P4-IN-ARGS is the optional argument passed that will be used as the list of
arguments to the P4-THIS-COMMAND.

P4-OUT-ARGS is the optional argument passed that will be used as the list of
arguments to P4-OUT-COMMAND."
  (if p4-this-buffer
      (set-buffer (get-buffer-create p4-this-buffer))
    (set-buffer (get-buffer-create (concat "*P4 " p4-this-command "*"))))
  (setq p4-current-command p4-this-command)
  (if (zerop (apply 'call-process-region (point-min) (point-max)
		    p4-executable t t nil
		    p4-current-command "-o"
		    p4-in-args))
      (progn
	(goto-char (point-min))
	(insert (concat "# Created using " (p4-emacs-version) ".\n"
			"# Type C-c C-c to submit changes and exit buffer.\n"
			"# Type C-x k to kill current changes.\n"
			"#\n"))
	(if p4-regexp (re-search-forward p4-regexp))
	(indented-text-mode)
	(setq p4-minor-mode t)
	(setq fill-column 79)
	(p4-push-window-config)
	(switch-to-buffer-other-window (current-buffer))
	(if p4-out-command
	    (setq p4-current-command p4-out-command))
	(setq p4-current-args p4-out-args)

	(define-key p4-minor-map "\C-c\C-c" 'p4-async-call-process)
	(run-hooks 'p4-async-command-hook)
	(message "C-c C-c to finish editing and exit buffer."))
    (error "%s %s -o failed to complete successfully." p4-executable
	   p4-current-command)))

(defun p4-async-call-process ()
  "Internal function called by `p4-async-process-command' to process the
buffer after editing is done using the minor mode key mapped to `C-c C-c'."
  (interactive)
  (message "p4 %s ..." p4-current-command)
  (let ((max (point-max)) msg)
    (goto-char max)
    (if (zerop (apply 'call-process-region (point-min)
		      max p4-executable
		      nil '(t t) nil
		      p4-current-command "-i"
		      p4-current-args))
	(progn
	  (goto-char max)
	  (setq msg (buffer-substring max (point-max)))
	  (delete-region max (point-max))
	  (save-excursion
	    (set-buffer (get-buffer-create p4-output-buffer-name))
	    (delete-region (point-min) (point-max))
	    (insert msg))
	  (kill-buffer nil)
	  (display-buffer p4-output-buffer-name)
	  (p4-partial-cache-cleanup p4-current-command)
	  (message "p4 %s done." p4-current-command)
	  (if (equal p4-current-command "submit")
	      (progn
		(p4-refresh-files-in-buffers)
		(p4-check-mode-all-buffers)))
	  (if (and p4-notify (equal p4-current-command "submit"))
	      (p4-notify p4-notify-list)))
      (error "%s %s -i failed to complete successfully." p4-executable
	     p4-current-command))))

;; The p4 change command
(defun p4-change ()
  "To edit the change specification, type \\[p4-change].

Optional args [-d | -o] [ change# ] passed as prefix arguments.

Creates a new change description with no argument or edit the
text description of an existing change if a change number is
given.	To associate or remove files from a pending change use
the open commands (edit, add, delete) or revert.

The -d flag discards a pending change, but only if it has no
opened files and no pending fixes associated with it.  Use 'opened -a'
to report on opened files and 'reopen' to move them to another
change.	 Use 'fixes -c change#' to report on pending fixes and
'fix -d -c change# jobs...' to delete pending fixes.  The change
can only be deleted by the user and client who created it.

The -o flag causes the change specification to be written
to the standard output.	 The user's editor is not invoked.

The -i flag causes a change specification to be read from the
standard input.	 The user's editor is not invoked."

  (interactive)
  (p4-async-process-command "change" "Description:\n\t" "*P4 New Change*"))

;; The p4 client command
(defun p4-client ()
  "To edit a client specification , type \\[p4-client].

With no argument client creates a new client view specification or
edits an existing client specification. The client name is taken
from the environment variable $P4CLIENT if set, or else from
the current host name.	The specification form is put into a
temporary file and the editor (given by the environment variable
$EDITOR) is invoked.  If a name is given, the specification of
the named client is displayed read-only.

The specification form contains the following fields:

Client:	     The client name (read only.)

Date:	     The date specification was last modified (read only.)

Description: A short description of the client (optional).

Root:	     The root directory of the client file workspace
	     (given in local file system syntax), under which all
	     client files will be placed.  If you change this, you
	     must physically relocate any files as well.

View:	     What files you want to see from the depot and how
	     they map to locations on the client.  The left hand
	     side specifies a depot path, which must begin with
	     //depot/.	The right hand side gives the corresponding
	     client path, given in canonical Perforce file syntax.
	     On expansion to an actual local client file name the
	     initial //client/ is replaced by the Root value, given
	     above.  You may use wildcards:

		 ...		matches any characters including
		 *		matches any character except /
		 %1 to %9	like *, used to associate wild cards

	     Wildcarding must be congruent in both the client and
	     depot paths.  You may have any number of view entries.
	     A new view takes effect on the next 'get'.

Normally, new clients are created with a default view that maps
all depot files onto the client.  The -t flag uses the view from
the named template client as a default instead.

The -d flag causes the named client to be deleted.

The -o flag causes the named client specification to be written
to the standard output.	 The user's editor is not invoked.

The -i flag causes a client specification to be read from the
standard input.	 The user's editor is not invoked."

  (interactive)
  (let ((args nil))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 client: " nil "client"))))
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			 args))
	(p4-noinput-buffer-action "client" nil t args)
      (p4-async-process-command "client" nil nil nil args))))

(defun p4-clients ()
  "To list all clients, type \\[p4-clients].

Reports the list of all clients currently known to the system."
  (interactive)
  (p4-noinput-buffer-action "clients" nil t nil)
  (p4-make-basic-buffer "*P4 clients*"))

(defun p4-branch (args)
  "Edit a P4-BRANCH specification using \\[p4-branch]."
  (interactive (list
		(p4-make-list-from-string
		 (p4-read-arg-string "p4 branch: " nil "branch"))))
  (if (or (null args) (equal args (list "")))
      (error "Branch must be specified!")
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			 args))
	(p4-noinput-buffer-action "branch" nil t args)
      (p4-async-process-command "branch" "Description:\n\t"
				(concat "*P4 Branch: "
					(car (reverse args)) "*")
				"branch" args))))

(defun p4-branches ()
  "To list all branches, type \\[p4-branches].

Reports the list of all branches currently known to the system.
Branches takes no arguments."
  (interactive)
  (p4-noinput-buffer-action "branches" nil t nil)
  (p4-make-basic-buffer "*P4 branches*"))

(defun p4-label (args)
  "Edit a P4-label specification using \\[p4-label]."
  (interactive (list
		(p4-make-list-from-string
		 (p4-read-arg-string "p4 label: " nil "label"))))
  (if (or (null args) (equal args (list "")))
      (error "label must be specified!")
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			 args))
	(p4-noinput-buffer-action "label" nil t args)
      (p4-async-process-command "label" "Description:\n\t"
				(concat "*P4 label: "
					(car (reverse args)) "*")
				"label" args))))

(defun p4-labels ()
  "To display list of defined labels, type \\[p4-labels].

Reports the list of all labels currently known to the system.
Labels takes no arguments."
  (interactive)
  (p4-noinput-buffer-action "labels" nil t nil)
  (p4-make-basic-buffer "*P4 labels*"))

;; The p4 labelsync command
(defun p4-labelsync ()
  "To synchronize a label with the current client contents, type
\\[p4-labelsync]."
  (interactive)
  (let ((args (p4-make-list-from-string
	       (p4-read-arg-string "p4 labelsync: "))))
    (p4-noinput-buffer-action "labelsync" nil t args))
  (p4-make-depot-list-buffer "*P4 labelsync*"))

;; The p4 submit command
(defun p4-submit ()
  "To submit a pending change to the depot, type \\[p4-submit].

Submit commits a pending change with its associated files to the depot.
With no argument submit sends the 'default' change.  With the -c flag
the designated pending change is sent.	Before committing the change
submit locks all associated files not already locked.  If any file
cannot be locked the change is aborted.	 If submit is sending the
default change it first provides the user with a dialog similar to
'p4 change' so the user can compose a change description.  In this
dialog the user is presented with the list of open files in change
'default'.  Files may be deleted from this list but they cannot be
added.	(Use an open command (open, edit, add, delete) to add
additional files to a change or to move files between changes.)

If the submit fails for any reason the files are left open in
a newly created pending change.

Submit is guaranteed to be atomic.  Either all files will be
updated in the depot as a unit or none will be.

The -i flag causes a change specification to be read from the
standard input.	 The user's editor is not invoked."

  (interactive)
  (let ((args nil)
	(submit-buf-name "*P4 Submit*"))
    (if (buffer-live-p (get-buffer submit-buf-name))
	(switch-to-buffer-other-window (get-buffer submit-buf-name))
      (if current-prefix-arg
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 submit: " nil))))
      (save-some-buffers)
      (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			   args))
	  (progn
	    (p4-noinput-buffer-action "submit" nil t args)
	    (p4-refresh-files-in-buffers))
	(if (or (not (p4-empty-diff-p))
		(progn
		  (ding t)
		  (yes-or-no-p
		   "File with empty diff opened for edit. Submit anyway? ")))
	    (p4-async-process-command "change" "Description:\n\t"
				      submit-buf-name "submit" args))))))

;; The p4 user command
(defun p4-user ()
  "To create or edit a user specification, type \\[p4-user].

Create a new user specification or edit an existing user
specification. The specification form is put into a temporary
file and the editor (given by the environment variable $EDITOR)
is invoked.

Normally, a user specification is created automatically the
first time the user invokes any client command that can update
the depot.  The 'user' command is generally used to edit the
user's reviewing subscription list for change review.

The user specification form contains the following fields:

User:	     The user name (read only).

Email:	     The user's email address (user@client default).

Update:	     The date the specification was last modified (read only).

Access:	     The date the user last issued a client command.

FullName:    The user's real name.

Reviews:     The subscription list for change review.  You may
	     use wildcards:
		 ...		matches any characters including

		 *		matches any character except /
	     There may be any number of review lines.

The -d flag deletes the named user, but only if the user is not
the owner of any branches, clients, jobs, labels, or opened files.

The -o flag causes the named user specification to be written
to the standard output.	 The user's editor is not invoked.

The -i flag causes a user specification to be read from the
standard input.	 The user's editor is not invoked."

  (interactive)
  (let ((args nil))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 user: " nil "user"))))
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			 args))
	(p4-noinput-buffer-action "user" nil t args)
      (p4-async-process-command "user" nil nil nil args))))

;; The p4 job command
(defun p4-job ()
  "To create or edit a job, type \\[p4-job].

'p4 job' creates and edits job specifications using an ASCII form.
A job is a defect, enhancement, or other unit of intended work.
The 'p4 fix' command can associate changelists with jobs.

With no arguments, 'p4 job' creates a blank job specification form
and invokes the user's editor.  When the form is saved, a job name
of the form jobNNNNNN is created.  If a jobName is given on the
command line either that named job will be created or, if the job
already exists, the job can be modified.

As jobs are entered or updated, all fields are indexed for
searching by 'p4 jobs'.  Text fields are broken into individual
alphanumeric words (punctuation and whitespace are ignored) and
each word is entered, case folded, into the word index.  Date
fields are converted to an internal representation (seconds
since 1970/01/01 00:00:00) and entered into the date index.

The fields of a job are defined by the 'p4 jobspec' command.
There is a simple default jobspec that is used if no explicit
one has been defined.

The -d flag deletes the named job and any associated fixes.

The -o flag causes the named job specification to be written
to the standard output.  The user's editor is not invoked.

The -i flag causes a job specification to be read from the
standard input.  The user's editor is not invoked.

The -f flag allows otherwise read-only fields to be set."
  (interactive)
  (let ((args nil))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 job: " nil "job"))))
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			 args))
	(p4-noinput-buffer-action "job" nil t args)
      (p4-async-process-command "job" "Description:\n\t" nil nil args))))

;; The p4 jobspec command
(defun p4-jobspec ()
  "To edit the job template, type \\[p4-jobspec]."
  (interactive)
  (p4-async-process-command "jobspec"))

;; A function to get the current P4 client root to be used by various other
;; macros, if needed.

(defun p4-get-client-root (client-name)
  "To get the current value of Client's root type \\[p4-get-client-root].
   This can be used by any other macro that requires this value.
"
  (interactive (list
		(completing-read "Client: " (if p4-my-clients
						p4-my-clients
					      'p4-clients-completion)
				 nil p4-strict-complete (p4-current-client))))
  (if (not client-name)
      nil
    (let (p4-client-root pmin)
      (save-excursion
	(get-buffer-create p4-output-buffer-name)
	(set-buffer p4-output-buffer-name)
	(goto-char (point-max))
	(setq pmin (point))
	(if (zerop (call-process
		    p4-executable nil t nil "client" "-o" client-name))
	    (progn
	      (save-restriction
		(narrow-to-region pmin (point-max))
		(goto-char pmin))
	      (re-search-forward "^Root:[ \t]+\\(.*\\)$")
	      (setq p4-client-root (match-string 1))
	      ;;(message "Root of %s is %s" client-name p4-client-root)
	      (delete-region pmin (point-max)))))
      p4-client-root)))

(defun p4-get-info (key &optional key-list)
  "To get the value associated with KEY, \\[p4-get-info].
This can be used by any other macro that requires this value.

KEY can be any one of

User name       Client name      Client root    Current directory
Client address  Server address   Server root    Server version
Server license  NULL

If KEY is `NULL', and KEY-LIST is defined as a list of KEYS \(strings\),
like:

\(p4-get-info \"NULL\" \(list \"Client name\" \"Client root\"
			 \"Server address\"\)\)

then, a corresponding list of values is returned."
  (interactive "sKey: ")
  (let (p4-this-info pmin)
    (save-excursion
      (get-buffer-create p4-output-buffer-name)
      (set-buffer p4-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (if (zerop (call-process  p4-executable nil t nil "info"))
	  (save-restriction
	    (narrow-to-region pmin (point-max))
	    (let ((all-keys (if (equal key "NULL")
				(if key-list key-list nil)
			      (list key)))
		  all-vals cur-key)
	      (while all-keys
		(setq cur-key (car all-keys))
		(setq all-keys (cdr all-keys))
		(goto-char pmin)
		(if cur-key
		    (progn
		      ;;(message "Looking for %s" cur-key)
		      (re-search-forward (concat "^" cur-key
						 ":[ \t]+\\(.*\\)$") nil t)
		      (add-to-list 'all-vals (match-string 1)))))
	      (setq p4-this-info (if (= (length all-vals) 1)
				     (car all-vals)
				   (reverse all-vals)))
	      (delete-region pmin (point-max)))))
      p4-this-info)))

;; A function to get the current P4 client name
(defun p4-get-client-name ()
  "To get the current value of the environment variable P4CLIENT,
type \\[p4-get-client-name].

This will be the current client that is in use for access through
Emacs P4."

  (interactive)
  (let ((client (if p4-global-config p4-local-client p4-global-clt))
	(global-clt p4-global-clt))
    (message "P4CLIENT [buffer-local: %s], [global: %s]" p4-local-client
	     global-clt)
    client))

;; A function to set the current P4 client name
(defun p4-set-client-name (p4-new-client-name)
  "To set the current value of P4CLIENT, type \\[p4-set-client-name].

This will change the current client from the previous client to the new
given value.

Setting this value to nil would disable P4 Version Checking.

`p4-set-client-name' will complete any client names set using the function
`p4-set-my-clients'. The strictness of completion will depend on the
variable `p4-strict-complete' (default is t).

Argument P4-NEW-CLIENT-NAME The new client to set to. The default value is
the current client."
  (interactive (list
		(completing-read "Change Client to: "
				 (if p4-my-clients
				     p4-my-clients
				   'p4-clients-completion)
				 nil p4-strict-complete (p4-current-client))
		))
  (if (or (null p4-new-client-name) (equal p4-new-client-name "nil"))
      (progn
	(setenv "P4CLIENT"  nil)
	(if (not (getenv "P4CONFIG"))
	    (message
	     "P4 Version check disabled. Set a valid client name to enable."
	     )))
    (progn
      (setenv "P4CLIENT"  p4-new-client-name)
      (setq p4-global-clt p4-new-client-name)
      (message	"P4CLIENT changed to %s" p4-new-client-name)
      (run-hooks 'p4-set-client-hooks))))

(defun p4-get-client-config ()
  "To get the current value of the environment variable P4CONFIG,
type \\[p4-get-client-config].

This will be the current configuration that is in use for access through
Emacs P4."

  (interactive)
  (message "P4CONFIG is %s" p4-global-config))

(defun p4-set-client-config (p4config)
  "To set the P4CONFIG variable, for use with the current versions of the p4
client.

P4CONFIG is a more flexible mechanism wherein p4 will find the current
client automatically by checking the config file found at the root of a
directory \(recursing all the way to the top\).

In this scenario, a P4CLIENT variable need not be explicitly set.
"
  (interactive "sP4 Config: ")
  (if (or (null p4config) (equal p4config ""))
      (message "P4CONFIG not changed.")
    (setenv "P4CONFIG"  p4config)
    (setq p4-global-config p4config)
    (message "P4CONFIG changed to %s" p4-global-config)))

(defun p4-set-my-clients (client-list)
  "To set the client completion list used by `p4-set-client-name', use
this function in your .emacs (or any lisp interaction buffer).

This will change the current client list from the previous list to the new
given value.

Setting this value to nil would disable client completion by
`p4-set-client-name'.

The strictness of completion will depend on the variable
`p4-strict-complete' (default is t).

Argument CLIENT-LIST is the 'list' of clients.

To set your clients using your .emacs, use the following:

\(load-library \"p4\"\)
\(p4-set-my-clients \'(client1 client2 client3)\)"
  (setq p4-my-clients nil)
  (let ((p4-tmp-client-var nil))
    (while client-list
      (setq p4-tmp-client-var (format "%s" (car client-list)))
      (setq client-list (cdr client-list))
      (setq p4-my-clients (append p4-my-clients
				  (list (list p4-tmp-client-var)))))))

;; A function to get the current P4PORT
(defun p4-get-p4-port ()
  "To get the current value of the environment variable P4PORT, type \
\\[p4-get-p4-port].

This will be the current server/port that is in use for access through Emacs
P4."

  (interactive)
  (message "P4PORT is %s" (getenv "P4PORT")))

;; A function to set the current P4PORT
(defun p4-set-p4-port (p4-new-p4-port)
  "To set the current value of P4PORT, type \\[p4-set-p4-port].

This will change the current server from the previous server to the new
given value.

Argument P4-NEW-P4-PORT The new server:port to set to. The default value is
the current value of P4PORT."
  (interactive (list (let
			 ((symbol (read-string "Change server:port to: "
					       p4-global-server-port)))
		       (if (equal symbol "")
			   p4-global-server-port
			 symbol))))
  (if (or (null p4-new-p4-port) (equal p4-new-p4-port "nil"))
      (progn
	(setenv "P4PORT"  nil)
	(message
	 "P4 Version check disabled. Set a valid client name to enable."))
    (progn
      (setenv "P4PORT"	p4-new-p4-port)
      (setq p4-global-server-port p4-new-p4-port)
      (message	"P4PORT changed to %s" p4-new-p4-port))))

;; The find-file hook for p4.
(defun p4-find-file-hook ()
  "To check while loading the file, if it is a P4 version controlled file."
  (if (or p4-global-config p4-global-clt)
      (p4-detect-p4)))

;; The kill-buffer hook for p4.
(defun p4-kill-buffer-hook ()
  "To Remove a file and its associated buffer from out global list of P4
controlled files."
  (if p4-vc-check
      (let ((buffile buffer-file-name)
	    (bufname (buffer-name)))
	(p4-refresh-refresh-list buffile bufname))))

(defun p4-refresh-refresh-list (buffile bufname)
  "Refresh the list of files to be refreshed."
  (if p4-all-buffer-files
      (progn
	(setq p4-all-buffer-files (delete (list buffile bufname)
					  p4-all-buffer-files)))
    (progn
      (if (and p4-running-emacs (timerp p4-file-refresh-timer))
	  (cancel-timer p4-file-refresh-timer))
      (if (and p4-running-xemacs p4-file-refresh-timer)
	  (disable-timeout p4-file-refresh-timer))
      (if p4-file-refresh-timer
	  (setq p4-file-refresh-timer nil)))))

;; A function to check if the file being opened is version controlled by p4.
(defun p4-is-vc ()
  "If a file is controlled by P4 then return version else return nil."
  (let (filename max version)
    (setq filename buffer-file-name)
    (save-excursion
      (get-buffer-create p4-output-buffer-name)
      (set-buffer p4-output-buffer-name)
      (setq max (point-max))
      (goto-char max)
      (if filename
	  (setq default-directory (file-name-directory filename)))
      (if (and filename
	       (zerop (call-process
		       p4-executable
		       nil
		       p4-output-buffer-name
		       nil
		       "have" filename)))
	  (progn
	    (set-buffer p4-output-buffer-name)
	    (goto-char max)
	    (if (re-search-forward "#[0-9]+" (point-max) t)
		(setq version (substring (match-string 0) 1)))))
      (set-buffer p4-output-buffer-name)
      (delete-region max (point-max)))
    (let ((p4-client-serv-info (p4-get-info "NULL" '("Client name"
						     "Server address"))))
      (setq p4-local-client nil p4-local-server-port nil)
      (if version (p4-assign-values '('p4-local-client 'p4-local-server-port)
				    p4-client-serv-info)))
    version))

;; To assign a list of values to a list of variables.
(defmacro p4-assign-value-macro (var val)
  "Macro to set a given value to a given variable."
  (list 'setq var val))

(defun p4-assign-values (vars vals)
  "Given a list of VARS and a corresponding list of VALS, assign the correct
value to the variable."
  (if (= (length vars) (length vals))
      (let (this-var this-val)
	(while vars
	  (setq this-var (car vars))
	  (setq vars (cdr vars))
	  (setq this-val (car vals))
	  (setq vals (cdr vals))
	  (eval (macroexpand
		 (list 'p4-assign-value-macro (eval this-var) this-val)))))
    (error "Lists don't match in length!")))

;; set keymap. We use the C-x p Keymap for all perforce commands

(defvar p4-prefix-map (lookup-key global-map "\C-xp")
  "The Prefix for P4 Library Commands.")
(if (not (keymapp p4-prefix-map))
    (progn
      (setq p4-prefix-map (make-sparse-keymap))
      (define-key global-map "\C-xp" p4-prefix-map)
      (define-key p4-prefix-map "a" 'p4-add)
      (define-key p4-prefix-map "b" 'p4-bug-report)
      (define-key p4-prefix-map "B" 'p4-branch)
      (define-key p4-prefix-map "c" 'p4-client)
      (define-key p4-prefix-map "C" 'p4-changes)
      (define-key p4-prefix-map "d" 'p4-diff2)
      (define-key p4-prefix-map "D" 'p4-describe)
      (define-key p4-prefix-map "e" 'p4-edit)
      (define-key p4-prefix-map "E" 'p4-reopen)
      (define-key p4-prefix-map "\C-f" 'p4-depot-find-file)
      (define-key p4-prefix-map "f" 'p4-filelog)
      (define-key p4-prefix-map "F" 'p4-files)
      (define-key p4-prefix-map "g" 'p4-get-client-name)
      (define-key p4-prefix-map "G" 'p4-get)
      (define-key p4-prefix-map "h" 'p4-help)
      (define-key p4-prefix-map "H" 'p4-have)
      (define-key p4-prefix-map "i" 'p4-info)
      (define-key p4-prefix-map "I" 'p4-integ)
      (define-key p4-prefix-map "j" 'p4-job)
      (define-key p4-prefix-map "J" 'p4-jobs)
      (define-key p4-prefix-map "l" 'p4-label)
      (define-key p4-prefix-map "L" 'p4-labels)
      (define-key p4-prefix-map "\C-l" 'p4-labelsync)
      (define-key p4-prefix-map "m" 'p4-rename)
      (define-key p4-prefix-map "n" 'p4-notify)
      (define-key p4-prefix-map "o" 'p4-opened)
      (define-key p4-prefix-map "p" 'p4-print)
      (define-key p4-prefix-map "P" 'p4-set-p4-port)
      (define-key p4-prefix-map "q" 'p4-pop-window-config)
      (define-key p4-prefix-map "r" 'p4-revert)
      (define-key p4-prefix-map "R" 'p4-refresh)
      (define-key p4-prefix-map "s" 'p4-set-client-name)
      (define-key p4-prefix-map "S" 'p4-submit)
      (define-key p4-prefix-map "t" 'p4-toggle-vc-mode)
      (define-key p4-prefix-map "u" 'p4-user)
      (define-key p4-prefix-map "U" 'p4-users)
      (define-key p4-prefix-map "v" 'p4-emacs-version)
      (define-key p4-prefix-map "V" 'p4-print-with-rev-history)
      (define-key p4-prefix-map "w" 'p4-where)
      (define-key p4-prefix-map "x" 'p4-delete)
      (define-key p4-prefix-map "X" 'p4-fix)
      (define-key p4-prefix-map "=" 'p4-diff)
      (define-key p4-prefix-map "-" 'p4-ediff)
      (define-key p4-prefix-map "?" 'p4-describe-bindings)))

;; For users interested in notifying a change, a notification list can be
;; set up using this function.
(defun p4-set-notify-list (p4-new-notify-list &optional p4-supress-stat)
  "To set the current value of P4NOTIFY, type \\[p4-set-notify-list].

This will change the current notify list from the existing list to the new
given value.

An empty string will disable notification.

Argument P4-NEW-NOTIFY-LIST is new value of the notification list.
Optional argument P4-SUPRESS-STAT when t will suppress display of the status
message. "

  (interactive (list (let
			 ((symbol (read-string
				   "Change Notification List to: "
				   p4-notify-list)))
		       (if (equal symbol "")
			   nil
			 symbol))))
  (setq p4-old-notify-list p4-notify-list)
  (if p4-new-notify-list
      (progn
	(setenv "P4NOTIFY"  p4-new-notify-list)
	(setq p4-notify-list p4-new-notify-list)
	(setq p4-notify t))
    (progn
      (setenv "P4NOTIFY"  nil)
      (setq p4-notify-list nil)
      (setq p4-notify nil)))
  (if (not p4-supress-stat)
      (message	"Notification list changed from '%s' to '%s'"
		p4-old-notify-list p4-notify-list)))

;; To get the current notification list.
(defun p4-get-notify-list ()
  "To get the current value of the environment variable P4NOTIFY,
type \\[p4-get-notify-list].

   This will be the current notification list that is in use for mailing
   change notifications through Emacs P4."

  (interactive)
  (message "P4NOTIFY is %s" p4-notify-list))

(defun p4-notify (users)
  "To notify a list of users of a change submission manually, type
\\[p4-notify].

To do auto-notification, set the notification list with `p4-set-notify-list'
and on each submission, the users in the list will be notified of the
change.

Since this uses the sendmail program, it is mandatory to set the correct
path to the sendmail program in the variable `p4-sendmail-program'.

Also, it is mandatory to set the user's email address in the variable
`p4-user-email'.

Argument USERS The users to notify to. The default value is the notification
list."
  (interactive (list (let
			 ((symbol (read-string "Notify whom? "
					       p4-notify-list)))
		       (if (equal symbol "")
			   nil
			 symbol))))
  (p4-set-notify-list users t)
  (if (not (and (eq p4-sendmail-program nil)
		(eq p4-user-email nil)))
      (p4-do-notify)
    (message "%s"
	     "Please set p4-sendmail-program and p4-user-email variables.")))

(defun p4-do-notify ()
  "This is the internal notification function called by `p4-notify'."
  (save-excursion
    (if (and p4-notify-list (not (equal p4-notify-list "")))
	(progn
	  (save-excursion
	    (set-buffer (get-buffer-create p4-output-buffer-name))
	    (goto-char (point-min))
	    (if (re-search-forward  "[0-9]+.*submitted" (point-max)  t)
		(progn
		  (let ((p4-matched-change 'nil))
		    (setq p4-matched-change (substring (match-string 0) 0 -10))
		    (set-buffer (get-buffer-create "*P4 Notify*"))
		    (delete-region (point-min) (point-max))
		    (call-process-region (point-min) (point-max)
					 p4-executable
					 t t nil "describe" "-s"
					 p4-matched-change)
		    (switch-to-buffer "*P4 Notify*")
		    (goto-char (point-min))
		    (let ((p4-chg-desc 'nil))
		      (if (re-search-forward "^Change.*$" (point-max) t)
			  (setq p4-chg-desc (match-string 0))
			(setq p4-chg-desc (concat
					   "Notification of Change "
					   p4-matched-change)))
		      (goto-char (point-min))
		      (insert
		       "From: " p4-user-email "\n"
		       "To: P4 Notification Recipients:;\n"
		       "Subject: " p4-chg-desc "\n")
		      (call-process-region (point-min) (point-max)
					   p4-sendmail-program t t nil
					   "-odi" "-oi" p4-notify-list)

		      (kill-buffer nil))))
	      (progn
		(save-excursion
		  (set-buffer (get-buffer-create p4-output-buffer-name))
		  (goto-char (point-max))
		  (insert "\np4-do-notify: No Change Submissions found."))))))
      (progn
	(save-excursion
	  (set-buffer (get-buffer-create p4-output-buffer-name))
	  (goto-char (point-max))
	  (insert "\np4-do-notify: Notification list not set."))))))

;; Function to return the current version.
(defun p4-emacs-version ()
  "Return the current Emacs-P4 Integration version."
  (interactive)
  (message (concat (cond (p4-running-xemacs "X")) "Emacs-P4 Integration v%s")
	   p4-emacs-version))

;; To set the path to the p4 executable
(defun p4-set-p4-executable (p4-exe-name)
  "Set the path to the correct P4 Executable.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"p4\"\)
\(p4-set-p4-executable \"/my/path/to/p4\"\)

Argument P4-EXE-NAME The new value of the p4 executable, with full path."
  (interactive "fFull path to your P4 executable: " )
  (setq p4-executable p4-exe-name))

(defun p4-set-sendmail-program (p4-program)
  "Set the path to the correct sendmail.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"p4\"\)
\(p4-set-sendmail-program \"/my/path/to/sendmail\"\)

Argument P4-PROGRAM The full path to sendmail."
  (interactive "fFull path to the sendmail program: " )
  (setq p4-sendmail-program p4-program))

(defun p4-set-user-email (p4-email-address)

  "Set the correct user e-mail address to be used with the notification
system. This must be set for the notification to take place.

The default value is taken from the variable `user-mail-address', if it
exists. Otherwise, the value defaults to nil.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"p4\"\)
\(p4-set-user-email \"joe_user@somewhere.com\"\)

Argument P4-EMAIL-ADDRESS is the complete email address of the current
user."

  (interactive "sEnter your e-mail address: ")
  (setq p4-user-email p4-email-address))

(defun p4-detect-p4 ()
  "Try to recursively go upwards from this directory and see if a file with
the name of the value of P4CONFIG is present. If so, then this is a P4
controlled file. Only check if `p4-use-p4config-exclusively' is non-nil."
  (if (not p4-use-p4config-exclusively)
      ;; no, always call
      (p4-check-mode)
    ;; yes, use it exclusively
    (and (getenv "P4CONFIG")
	 (let ((p4config (getenv "P4CONFIG"))
	       (p4-cfg-dir (cond (buffer-file-name ;; extrapolate from name
				  (file-name-directory
				   (file-truename (buffer-file-name))))
				 (t default-directory) ;; hmm, use default
				 )))
	   (while (not (or (string-equal p4-cfg-dir "/")
			   (file-exists-p (concat p4-cfg-dir p4config))))
	     (setq p4-cfg-dir
		   (substring p4-cfg-dir 0
			      (string-match "[^/]*/?$" p4-cfg-dir))))
	   ;; if we did found a p4config file, this is under P4 control
	   (if (not (string-equal p4-cfg-dir "/"))
	       (p4-check-mode)
	     nil)))))

(defun p4-check-mode (&optional args)
  "Check to see whether we should export the menu map to this buffer.

Optional argument ARGS Used only by `p4-add', the `p4-mode' variable is set
to this instead of the value returned from `p4-is-vc'.

Turning on P4 mode calls the hooks in the variable `p4-mode-hook' with
no args."
  (if p4-do-find-file
      (progn
	(if args
	    (setq p4-vc-check args)
	  (setq p4-vc-check (p4-is-vc)))
	(if p4-vc-check
	    (progn
	      (p4-menu-add)
	      (setq p4-mode (concat " P4:" p4-vc-check)))
	  (setq p4-mode nil))
	(p4-force-mode-line-update)
	(let ((buffile buffer-file-name)
	      (bufname (buffer-name)))
	  (if (and p4-vc-check (not (member (list buffile bufname)
					    p4-all-buffer-files)))
	      (add-to-list 'p4-all-buffer-files (list buffile bufname))))
	(if (not p4-file-refresh-timer)
	    (setq p4-file-refresh-timer
		  (cond (p4-running-emacs
			 (run-at-time nil p4-file-refresh-timer-time
				      'p4-refresh-files-in-buffers))
			(p4-running-xemacs
			 (add-timeout p4-file-refresh-timer-time
				      'p4-refresh-files-in-buffers nil
				      p4-file-refresh-timer-time)))))
	;; run hooks
	(and p4-vc-check (run-hooks 'p4-mode-hook))
	p4-vc-check)))

(defun p4-refresh-files-in-buffers (&optional arg)
  "Check to see if all the files that are under P4 version control are
actually up-to-date, if in buffers, or need refreshing."
  (let ((p4-all-my-files p4-all-buffer-files) buffile bufname thiselt)
    (if (not p4-all-my-files)
	(progn
	  (if p4-file-refresh-timer
	      (cond (p4-running-emacs
		     (cancel-timer p4-file-refresh-timer))
		    (p4-running-xemacs
		     (disable-timeout p4-file-refresh-timer))))
	  (setq p4-file-refresh-timer nil))
      (while p4-all-my-files
	(setq thiselt (car p4-all-my-files))
	(setq p4-all-my-files (cdr p4-all-my-files))
	(setq buffile (car thiselt))
	(setq bufname (cadr thiselt))
	(if (buffer-live-p (get-buffer bufname))
	    (save-excursion
	      (let ((buf (get-buffer bufname)))
		(set-buffer buf)
		(if p4-auto-refresh
		    (if (not (buffer-modified-p buf))
			(if (not (verify-visited-file-modtime buf))
			    (if (file-readable-p buffile)
				(revert-buffer t t)
			      (p4-check-mode))))
		  (if (file-readable-p buffile)
		      (find-file-noselect buffile)
		    (p4-check-mode)))
		(setq buffer-read-only (not (file-writable-p
					     (buffer-file-name))))))
	  (p4-refresh-refresh-list buffile bufname))))))

(defun p4-check-mode-all-buffers ()
  "Call p4-check-mode for all buffers under P4 version control"
  (let ((p4-all-my-files p4-all-buffer-files) buffile bufname thiselt)
    (while p4-all-my-files
      (setq thiselt (car p4-all-my-files))
      (setq p4-all-my-files (cdr p4-all-my-files))
      (setq buffile (car thiselt))
      (setq bufname (cadr thiselt))
      (if (buffer-live-p (get-buffer bufname))
	  (save-excursion
	    (set-buffer (get-buffer bufname))
	    (p4-check-mode))
	(p4-refresh-refresh-list buffile bufname)))))

;; Force mode line updation for different Emacs versions
(defun p4-force-mode-line-update ()
  "To Force the mode line update for different flavors of Emacs."
  (cond (p4-running-xemacs
	 (redraw-modeline))
	(p4-running-emacs
	 (force-mode-line-update))))

;; In case, the P4 server is not available, or when operating off-line, the
;; p4-find-file-hook becomes a pain... this functions toggles the use of the
;; hook when opening files.

(defun p4-toggle-vc-mode ()
  "In case, the P4 server is not available, or when working off-line, toggle
the VC check on/off when opening files."
  (interactive)
  (setq p4-do-find-file (not p4-do-find-file))
  (message (concat "P4 mode check " (if p4-do-find-file
					"enabled."
				      "disabled."))))

;; Wrap C-x C-q to allow p4-edit/revert and also to ensure that
;; we don't stomp on vc-toggle-read-only.

(defun p4-toggle-read-only-verbose ()
  (interactive)
  (p4-toggle-read-only t))

(defun p4-toggle-read-only (&optional verbose)
  "If p4-mode is non-nil, \\[p4-toggle-read-only] toggles between `p4-edit'
and `p4-revert'.

If the current buffer's file is not under p4, then this function passes on
all the parameters to `vc-toggle-read-only'."
  (interactive "P")
  (if (and (boundp 'p4-mode) (not (eq p4-mode nil)))
      (if buffer-read-only
	  (p4-edit verbose)
	(p4-revert verbose))
    (vc-toggle-read-only verbose)))

(defun p4-browse-web-page ()
  "Browse the p4.el web page."
  (interactive)
  (require 'browse-url)
  (browse-url p4-web-page))

;; The menu definition is in the XEmacs format. Emacs parses and converts
;; this definition to its own menu creation commands.

(defalias 'p4-toggle-vc-mode-off 'p4-toggle-vc-mode)
(defalias 'p4-toggle-vc-mode-on 'p4-toggle-vc-mode)

(defvar p4-menu-def
  '(["Add Current to P4" p4-add
     (and buffer-file-name (not p4-mode))]
    ["Check out/Edit"    p4-edit
     (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
    ["Re-open"	       p4-reopen
     (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
    ["Revert File"  p4-revert
     (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
    ["Delete File from Depot"  p4-delete
     (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
    ["Rename Depot File" p4-rename
     (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
    ["Submit Changes"  p4-submit t]
    ["--" nil nil]
    ["Find File using Depot Spec" p4-depot-find-file
     p4-do-find-file]
    ["--" nil nil]
    ["Show Opened Files"	p4-opened t]
    ["Filelog" p4-filelog (p4-buffer-file-name-2)]
    ["Changes" p4-changes t]
    ["Describe change" p4-describe t]
    ["--" nil nil]
    ["Diff 2 Versions" p4-diff2 (p4-buffer-file-name-2)]
    ["Diff Current" p4-diff t]
    ["Diff Current with Ediff"   p4-ediff
     (and buffer-file-name (not buffer-read-only) p4-mode)]
    ["--" nil nil]
    ["Print" p4-print (p4-buffer-file-name-2)]
    ["Print with revision history" p4-print-with-rev-history
     (p4-buffer-file-name-2)]
    ["--" nil nil]
    ["Edit a Branch Specification" p4-branch t]
    ["Edit a Label Specification" p4-label t]
    ["Edit a Client Specification" p4-client t]
    ["Edit a User Specification" p4-user t]
    ["--" nil nil]
    ["Show Version" p4-emacs-version t]
    ["Disable P4 VC Check"  p4-toggle-vc-mode-off
     p4-do-find-file]
    ["Enable P4 VC Check"	 p4-toggle-vc-mode-on
     (not p4-do-find-file)]
    ["--" nil nil]
    ["Set P4 Config"  p4-set-client-config p4-do-find-file]
    ["Get Current P4 Config"  p4-get-client-config
     p4-do-find-file]
    ["--" nil nil]
    ["Set P4 Client"  p4-set-client-name p4-do-find-file]
    ["Get Current P4 Client"  p4-get-client-name
     p4-do-find-file]
    ["--" nil nil]
    ["Set P4 Server/Port"	 p4-set-p4-port p4-do-find-file]
    ["Get Current P4 Server/Port"	 p4-get-p4-port
     p4-do-find-file]
    ["--" nil nil]
    ["Set P4 Notification List"  p4-set-notify-list
     p4-mode]
    ["Get P4 Notification List"  p4-get-notify-list p4-notify]
    ["--" nil nil]
    ["Check for later versions of p4.el" p4-browse-web-page t]
    ["--" nil nil]
    ["Report Bug in p4.el"  p4-bug-report t])
  "The P4 menu definition")

(cond (p4-running-xemacs
       ;; Menu Support for XEmacs
       (require 'easymenu)
       (defun p4-mode-menu (modestr)
	 (cons modestr p4-menu-def)))

      (p4-running-emacs
       ;; Menu support for Emacs
       (or (lookup-key global-map [menu-bar])
	   (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
       (defvar menu-bar-p4-menu (make-sparse-keymap "P4"))
       (setq menu-bar-final-items (cons 'p4-menu menu-bar-final-items))
       (define-key global-map [menu-bar p4-menu]
	 (cons "P4" menu-bar-p4-menu))
       (let ((m (reverse p4-menu-def))
	     (separator-number 0))
	 (while m
	   (let ((menu-text (elt (car m) 0))
		 (menu-action (elt (car m) 1))
		 (menu-pred (elt (car m) 2)))
	     (if menu-action
		 (progn
		   (define-key menu-bar-p4-menu (vector menu-action)
		     (cons menu-text menu-action))
		   (put menu-action 'menu-enable menu-pred))
	       (define-key menu-bar-p4-menu
		 (vector (make-symbol
			  (concat "separator-"
				  (int-to-string separator-number))))
		 '("--"))
	       (setq separator-number (1+ separator-number))))
	   (setq m (cdr m))))))

(defun p4-menu-add ()
  "To add the P4 menu bar button for files that are already not in
the P4 depot or in the current client view.."
  (interactive)
  (cond (p4-running-xemacs
	 (if (not (boundp 'p4-mode))
	     (setq p4-mode nil))
	 (easy-menu-add (p4-mode-menu "P4"))))
  t)

;; issue a message for users trying to use obsolete binding.
(if (not (lookup-key global-map "\C-xP"))
    (define-key global-map "\C-xP"
      `(lambda ()
	 (interactive)
	 (message
	  "Obsolete key binding for P4 commands. use C-x p instead."))))

(defun p4-bug-report ()
  (interactive)
  (if (string-match " 19\\." (emacs-version))
      ;; unfortunately GNU Emacs 19.x doesn't have compose-mail
      (mail nil p4-emacs-maintainer (concat "BUG REPORT: "
					    (p4-emacs-version)))
    (compose-mail p4-emacs-maintainer (concat "BUG REPORT: "
					      (p4-emacs-version))))
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
  ;; Insert warnings for novice users.
  (insert
   "This bug report will be sent to the P4-Emacs Integration Maintainer,\n"
   p4-emacs-maintainer "\n\n")
  (insert (concat (emacs-version) "\n\n"))
  (insert "A brief description of the problem and how to reproduce it:\n")
  (save-excursion
    (let ((message-buf (get-buffer
			(cond (p4-running-xemacs " *Message-Log*")
			      (p4-running-emacs "*Messages*")))))
      (if message-buf
	  (let ((beg-pos nil)
		(end-pos (point-max)))
	    (save-excursion
	      (set-buffer message-buf)
	      (goto-char end-pos)
	      (forward-line -10)
	      (setq beg-pos (point)))
	    (insert "\n\nRecent messages:\n")
	    (insert-buffer-substring message-buf beg-pos end-pos))))))

(defun p4-describe-bindings ()
  "A function to list the key bindings for the p4 prefix map"
  (interactive)
  (save-excursion
    (p4-push-window-config)
    (let ((map (make-sparse-keymap))
	  (p4-bindings-buffer "*P4 key bindings*"))
      (get-buffer-create p4-bindings-buffer)
      (cond
       (p4-running-xemacs
	(set-buffer p4-bindings-buffer)
	(delete-region (point-min) (point-max))
	(insert "Key Bindings for P4 Mode\n------------------------\n")
	(describe-bindings-internal p4-prefix-map))
       (p4-running-emacs
	(kill-buffer p4-bindings-buffer)
	(describe-bindings "\C-xp")
	(set-buffer "*Help*")
	(rename-buffer p4-bindings-buffer)))
      (define-key map "q"  'p4-quit-current-buffer)
      (use-local-map map)
      (display-buffer p4-bindings-buffer))))

;; Break up a string into a list of words
;; (p4-make-list-from-string "ab c de  f") -> ("ab" "c" "de" "f")
(defun p4-make-list-from-string (str)
  (let ((lst '()))
    (while (or (string-match "^ *\"\\([^\"]*\\)\"" str)
	       (string-match "^ *\'\\([^\']*\\)\'" str)
	       (string-match "^ *\\([^ ]+\\)" str))
      (setq lst (append lst (list (substring
				   str
				   (match-beginning 1)
				   (match-end 1)))))
      (setq str (substring str (match-end 0))))
    lst))

;; Return the file name associated with a buffer. If the real buffer file
;; name doesn't exist, try special filename tags set in some of the p4
;; buffers.
(defun p4-buffer-file-name-2 ()
  (cond ((buffer-file-name))
	((get-char-property (point) 'filename))
	((get-char-property (point) 'deleted-filename))
	((get-char-property (point) 'local-fname))
	((boundp 'p4-fname)
	 p4-fname)))

(defvar p4-depot-filespec-history nil
  "History for p4-depot filespecs.")

(defvar p4-depot-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a filespec and
cdr is the list of anwers")

(defvar p4-branches-history nil
  "History for p4 clients.")

(defvar p4-branches-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-clients-history nil
  "History for p4 clients.")

(defvar p4-clients-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-jobs-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-labels-history nil
  "History for p4 clients.")

(defvar p4-labels-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-users-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-arg-string-history nil
  "History for p4 command arguments")

(defun p4-depot-completion-search (filespec cmd)
  "Look into `p4-depot-completion-cache' for filespec.
Filespec is the candidate for completion, so the
exact file specification is \"filespec*\".

If found in cache, return a list whose car is FILESPEC and cdr is the list
of matches.
If not found in cache, return nil.
So the 'no match' answer is different from 'not in cache'."
  (let ((l (cond
	    ((equal cmd "branches") p4-branches-completion-cache)
	    ((equal cmd "clients") p4-clients-completion-cache)
	    ((equal cmd "dirs") p4-depot-completion-cache)
	    ((equal cmd "jobs") p4-jobs-completion-cache)
	    ((equal cmd "labels") p4-labels-completion-cache)
	    ((equal cmd "users") p4-users-completion-cache)))
	dir list)

    (if (and p4-cleanup-cache (not p4-timer))
	(setq p4-timer (cond (p4-running-emacs
			      (run-at-time p4-cleanup-time nil
					   'p4-cache-cleanup))
			     (p4-running-xemacs
			      (add-timeout p4-cleanup-time 'p4-cache-cleanup
					   nil nil)))))

    ;;(message "p4-depot-completion-search '%s'" filespec)
    (while l
      (if (string-match (concat "^" (car (car l)) "[^/]*$") filespec)
	  (progn
	    ;; filespec is included in cache
	    (if (string= (car (car l)) filespec)
		(progn
		  ;;(message "return complete list for %s" filespec)
		  (setq list (cdr (car l))))
	      ;;(message "build list for %s from %s" filespec (car (car l)))
	      (setq dir (cdr (car l)))
	      (while dir
		(if (string-match (concat "^" filespec) (car dir))
		    (setq list (cons (car dir) list)))
		(setq dir (cdr dir))))
	    (setq l nil
		  list (cons filespec list))))
      (setq l (cdr l)))
    list))

(defun p4-cache-cleanup (&optional arg)
  "Cleanup all the completion caches."
  (message "Cleaning up the p4 caches ...")
  (setq p4-branches-completion-cache nil)
  (setq p4-clients-completion-cache nil)
  (setq p4-depot-completion-cache nil)
  (setq p4-jobs-completion-cache nil)
  (setq p4-labels-completion-cache nil)
  (setq p4-users-completion-cache nil)
  (if (and p4-running-emacs (timerp p4-timer)) (cancel-timer p4-timer))
  (if (and p4-running-xemacs p4-timer) (disable-timeout p4-timer))
  (setq p4-timer nil)
  (message "Cleaning up the p4 caches ... done."))

(defun p4-partial-cache-cleanup (type)
  "Cleanup a specific completion cache."
  (cond ((string= type "branch")
	 (setq p4-branches-completion-cache nil))
	((string= type "client")
	 (setq p4-clients-completion-cache nil))
	((or (string= type "submit") (string= type "change"))
	 (setq p4-depot-completion-cache nil))
	((string= type "job")
	 (setq p4-jobs-completion-cache nil))
	((string= type "label")
	 (setq p4-labels-completion-cache nil))
	((string= type "user")
	 (setq p4-users-completion-cache nil))))

(defun p4-depot-output (command &optional args)
  "Executes p4 command inside a buffer.
Returns the buffer."
  (let ((buffer (get-buffer-create p4-output-buffer-name)))
    (p4-exec-p4 buffer (cons command args) t)
    buffer))

(defun p4-read-depot-output (buffer &optional regexp)
  "Reads first line of BUFFER and returns it.
Read lines are deleted from buffer.

If optional REGEXP is passed in, return the substring of the first line that
matched the REGEXP."

  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line)

    (let ((line (buffer-substring (point-min) (point))))
      (if (string= line "")
	  nil
	(delete-region (point-min) (point))
	(if (and regexp (string-match regexp line))
	    (setq line (substring line (match-beginning 1) (match-end 1))))

	;; remove trailing newline
	(if (equal (substring line (1- (length line)) (length line)) "\n")
	    (substring line 0 (1- (length line)))
	  line)))))

(defun p4-depot-completion-build (filespec cmd)
  "Ask Perforce for a list of files and directories beginning with FILESPEC."
  (let (output-buffer line list)

    (cond
     ((equal cmd "branches")
      (message "Making %s completion list..." cmd)

      ;; List branches
      (setq output-buffer (p4-depot-output cmd))

      (while (setq line (p4-read-depot-output
			 output-buffer
			 "^Branch \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$"))
	(if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq p4-branches-completion-cache
	    (cons (cons filespec list) p4-branches-completion-cache)))

     ((equal cmd "clients")
      (message "Making %s completion list..." cmd)

      ;; List clients
      (setq output-buffer (p4-depot-output cmd))

      (while (setq line (p4-read-depot-output
			 output-buffer
			 "^Client \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$"))
	(if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq p4-clients-completion-cache
	    (cons (cons filespec list) p4-clients-completion-cache)))

     ((equal cmd "dirs")
      (message "Making p4 completion list...")

      ;; List dirs
      (setq output-buffer (p4-depot-output cmd
					   (list (concat filespec "*"))))

      (while (setq line (p4-read-depot-output output-buffer))
	(if (not (string-match "no such file" line))
	    (setq list (cons (concat line "/") list))))

      ;; List files
      (setq output-buffer (p4-depot-output "files"
					   (list (concat filespec "*"))))

      (while (setq line (p4-read-depot-output output-buffer))
	(if (string-match "^\\(.+\\)#[0-9]+ - " line)
	    (setq list (cons (match-string 1 line) list))))

      ;; add to cache -  growing and growing ?
      (setq p4-depot-completion-cache
	    (cons (cons filespec list) p4-depot-completion-cache)))

     ((equal cmd "jobs")
      (message "Making %s completion list..." cmd)

      ;; List jobs
      (setq output-buffer (p4-depot-output cmd))
      (while (setq line (p4-read-depot-output
			 output-buffer
			 "\\([^ \n]*\\) on [0-9][0-9][0-9][0-9]/.*$"))
	(if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq p4-jobs-completion-cache
	    (cons (cons filespec list) p4-jobs-completion-cache)))

     ((equal cmd "labels")
      (message "Making %s completion list..." cmd)

      ;; List labels
      (setq output-buffer (p4-depot-output cmd))

      (while (setq line (p4-read-depot-output
			 output-buffer
			 "^Label \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$"))
	(if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq p4-labels-completion-cache
	    (cons (cons filespec list) p4-labels-completion-cache)))

     ((equal cmd "users")
      (message "Making %s completion list..." cmd)

      ;; List users
      (setq output-buffer (p4-depot-output cmd))

      (while (setq line (p4-read-depot-output
			 output-buffer
			 "^\\([^ ]+\\).*$"))
	(if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq p4-users-completion-cache
	    (cons (cons filespec list) p4-users-completion-cache))))

    (message nil)
    (cons filespec list)))

(defun p4-completion-builder (type)
  `(lambda (string predicate action)
     ,(concat "Completion function for Perforce " type ".

Using the mouse in completion buffer on a client will select it
and exit, unlike standard selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection.")

     (let (list)

       ;; First, look in cache
       (setq list (p4-depot-completion-search string ,type))

       ;; If not found in cache, build list.
       (if (not list)
	   (setq list (p4-depot-completion-build string ,type)))

       (cond
	;; try completion
	((null action)
	 (try-completion string (mapcar 'list (cdr list)) predicate))
	;; all completions
	((eq action t)
	 (all-completions string (mapcar 'list (cdr list)) predicate))
	;; Test for an exact match
	(t
	 (= (length list) 2))))))

(defalias 'p4-branches-completion (p4-completion-builder "branches"))

(defalias 'p4-clients-completion (p4-completion-builder "clients"))

(defun p4-depot-completion (string predicate action)
  "Completion function for Perforce files.
return directories and files.

Using the mouse in completion buffer on a directory will select it
and exit, unlike standard file name selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection."

  (if (not (string= (substring string 0 2) "//"))
      (setq string (concat "//" string)))
  (let (list)

    ;; when testing for an exact match, remove trailing /
    (if (eq action 'lambda)
	(if (eq (aref string (1- (length string))) ?/)
	    (setq string (substring string 0 (1- (length string))))))

    ;; First, look in cache
    (setq list (p4-depot-completion-search string "dirs"))

    ;; If not found in cache, build list.
    (if (not list)
	(setq list (p4-depot-completion-build string "dirs")))

    (cond
     ;; try completion
     ((null action)
      (try-completion string (mapcar 'list (cdr list)) predicate))
     ;; all completions
     ((eq action t)
      (all-completions string (mapcar 'list (cdr list)) predicate))
     ;; Test for an exact match
     (t
      (= (length list) 2)))))

(defalias 'p4-jobs-completion (p4-completion-builder "jobs"))

(defalias 'p4-labels-completion (p4-completion-builder "labels"))

(defalias 'p4-users-completion (p4-completion-builder "users"))


(defun p4-read-arg-string (prompt &optional initial type)
  (let ((minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (completing-read prompt
		     (cond ((not type)
			    'p4-arg-string-completion)
			   ((string= type "branch")
			    'p4-branch-string-completion)
			   ((string= type "client")
			    'p4-client-string-completion)
			   ((string= type "label")
			    'p4-label-string-completion)
			   ((string= type "job")
			    'p4-job-string-completion)
			   ((string= type "user")
			    'p4-user-string-completion))
		     nil nil
		     initial 'p4-arg-string-history)))

(defun p4-arg-string-completion (string predicate action)
  (let ((first-part "") completion)
    (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	(progn
	  (setq first-part (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((string-match "-b +$" first-part)
	   (setq completion (p4-branches-completion string predicate action)))
	  ((string-match "-t +$" first-part)
	   (let ((file-types (list "text " "xtext " "binary "
				   "xbinary " "symlink ")))
	     (setq completion (p4-list-completion
			       string file-types predicate action))))
	  ((string-match "-j +$" first-part)
	   (setq completion (p4-jobs-completion string predicate action)))
	  ((string-match "-l +$" first-part)
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "^status=" string)
	   (let ((status-types (list "status=open " "status=closed "
				     "status=suspended ")))
	     (setq completion (p4-list-completion
			       string status-types predicate action))))
	  ((or (string-match "\\(.*@.+,\\)\\(.*\\)" string)
	       (string-match "\\(.*@\\)\\(.*\\)" string))
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "^//" string)
	   (setq completion (p4-depot-completion string predicate action)))
	  ((string-match "^-" string)
	   (setq completion nil))
	  (t
	   (setq completion (p4-file-name-completion string
						     predicate action))))
    (cond ((null action) ;; try-completion
	   (if (stringp completion)
	       (concat first-part completion)
	     completion))
	  ((eq action t) ;; all-completions
	   completion)
	  (t             ;; exact match
	   completion))))

(defun p4-list-completion (string lst predicate action)
  (let ((collection (mapcar 'list lst)))
    (cond ((not action)
	   (try-completion string collection predicate))
	  ((eq action t)
	   (all-completions string collection predicate))
	  (t
	   (eq (try-completion string collection predicate) t)))))

(defun p4-file-name-completion (string predicate action)
  (if (string-match "//\\(.*\\)" string)
      (setq string (concat "/" (match-string 1 string))))
  (setq string (substitute-in-file-name string))
  (setq string (expand-file-name string))
  (let ((dir-path "") completion)
    (if (string-match "^\\(.*[/\\]\\)\\(.*\\)" string)
	(progn
	  (setq dir-path (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((not action)
	   (setq completion (file-name-completion string dir-path))
	   (if (stringp completion)
	       (concat dir-path completion)
	     completion))
	  ((eq action t)
	   (file-name-all-completions string dir-path))
	  (t
	   (eq (file-name-completion string dir-path) t)))))

(defun p4-string-completion-builder (completion-function)
  `(lambda (string predicate action)
     (let ((first-part "") completion)
       (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	   (progn
	     (setq first-part (match-string 1 string))
	     (setq string (match-string 2 string))))
       (cond ((string-match "^-" string)
	      (setq completion nil))
	     (t
	      (setq completion
		    (,completion-function string predicate action))))
       (cond ((null action);; try-completion
	      (if (stringp completion)
		  (concat first-part completion)
		completion))
	     ((eq action t);; all-completions
	      completion)
	     (t;; exact match
	      completion)))))

(defalias 'p4-branch-string-completion (p4-string-completion-builder
					'p4-branches-completion))

(defalias 'p4-client-string-completion (p4-string-completion-builder
					'p4-clients-completion))

(defalias 'p4-job-string-completion (p4-string-completion-builder
				     'p4-jobs-completion))

(defalias 'p4-label-string-completion (p4-string-completion-builder
				       'p4-labels-completion))

(defalias 'p4-user-string-completion (p4-string-completion-builder
				      'p4-users-completion))

(defun p4-get-server-version ()
  "To get the version number of the p4 server."
  (interactive)
  (let (ser-ver pmin)
    (save-excursion
      (get-buffer-create p4-output-buffer-name)
      (set-buffer p4-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (if (zerop (call-process p4-executable nil t nil "info"))
	  (save-restriction
	    (narrow-to-region pmin (point-max))
	    (goto-char pmin)
	    (re-search-forward
	     "^Server version: .*\/.*\/\\(\\([0-9]+\\)\.[0-9]+\\)\/.*(.*)$")
	    (setq ser-ver (string-to-number (match-string 2)))
	    (delete-region pmin (point-max)))))
    ser-ver))

(defun p4-map-depot-file (filespec &optional rmap)
  "Map a file in the depot on the current client.  If RMAP is t then the
depot mapping is returned, else the client mapping is returned."
  (interactive (list (completing-read "Enter filespec: "
				      'p4-depot-completion
				      nil nil
				      "//depot/"
				      'p4-depot-filespec-history)))
  (let (files p4-depot-buffer p4-server-version
	      (p4-client-root (p4-get-client-root (p4-current-client))))
    (if (not p4-client-root)
	nil
      (setq p4-server-version (p4-get-server-version))
      (if (memq system-type '(ms-dos windows-nt))
	  ;; For Windows, since the client root will be terminated with a \ as
	  ;; in c:\ or drive:\foo\bar\, we need to strip the trailing \ .
	  (let ((p4-clt-root-len (length p4-client-root)))
	    (setq p4-clt-root-len (1- p4-clt-root-len))
	    (setq p4-client-root (substring p4-client-root 0 p4-clt-root-len))
	    ))
      (setq p4-depot-buffer p4-output-buffer-name)
      (get-buffer-create p4-depot-buffer) ;; We do these two lines
      (kill-buffer p4-depot-buffer)	;; to ensure no duplicates
      (get-buffer-create p4-depot-buffer)
      (set-buffer p4-output-buffer-name)
      (delete-region (point-min) (point-max))
      (apply 'call-process
	     p4-executable nil t nil "where" (list filespec))
      (goto-char (point-min))
      (if (< p4-server-version 98)
	  (progn
	    (while (re-search-forward
		    (concat "^\\([^ ]+\\) //" (p4-current-client)
			    "\\(.*\\)$") nil t)
	      (setq files (cons
			   (cons
			    (match-string 1)
			    (concat p4-client-root (match-string 2)))
			   files))))
	(progn
	  (while (re-search-forward
		  (concat "^\\([^ ]+\\) //\\([^ ]+\\) \\(.*\\)$") nil t)
	    (setq files (cons
			 (cons
			  (match-string 1)  (match-string 3)) files)))))
      (if files
	  (if rmap
	      (cdr (car files))
	    (car (car files)))))))

(defun p4-depot-find-file (file)
  (interactive (list (completing-read "Enter filespec: "
				      'p4-depot-completion
				      nil nil
				      "//depot/"
				      'p4-depot-filespec-history)))
  (let ((lfile (if file (p4-map-depot-file file t))))
    (if lfile (find-file lfile)
      (if (get-file-buffer file)
	  (switch-to-buffer-other-window file)
	(progn
	  (get-buffer-create file)
	  (set-buffer file)
	  (p4-noinput-buffer-action
	   "print" nil t (p4-make-list-from-string (concat "-q " file)))
	  (p4-activate-print-buffer file))))))

(defun p4-current-client ()
  "Get the current local client, or the global client, if that."
  (if p4-local-client p4-local-client p4-global-clt))

(defun p4-empty-diff-p ()
  "Return t if there exists a file opened for edit with an empty diff"
  (interactive)
  (let ((buffer (get-buffer-create "p4-edp-buf"))
	line opened empty-diff)
    (p4-exec-p4 buffer (list "opened") t)
    (while (setq line (p4-read-depot-output
		       buffer))
      (if (string-match "\\(.*\\)#[0-9]* - edit.*" line)
	  (setq opened (cons (match-string 1 line) opened))))
    (p4-exec-p4 buffer (append (list "diff") opened) t)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (insert "====\n")
      (goto-char (point-min))
      (setq empty-diff (not (null (re-search-forward
				   "====.*\n====" nil t)))))
    (kill-buffer buffer)
    empty-diff))

(defun p4-current-server-port ()
  "Get the current local server:port address, or the global server:port, if
that."
  (if p4-local-server-port p4-local-server-port p4-global-server-port))


;;;###autoload
(if (where-is-internal 'vc-toggle-read-only)
    (substitute-key-definition 'vc-toggle-read-only 'p4-toggle-read-only
			       global-map))

(provide 'p4)

;;; p4.el ends here

