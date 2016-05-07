;; align -- align text to a specific column, by regexp
;;
;; Copyright (C) 1999 John Wiegley
;;
;; Author: John Wiegley <johnw@oneworld.new-era.com>
;; Created: 29 Jun 1998
;; Keywords: text, align, column, format

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.



;;; Commentary:
;;
;; This mode allows you to align regions in a context-sensitive fashion.
;; The classic use is to align assignments:
;;
;;    int a = 1;
;;    short foo = 2;
;;    double blah = 4;
;;
;; becomes
;;
;;    int    a    = 1;
;;    short  foo  = 2;
;;    double blah = 4;
;;
;; To use the aligner, first bind it to a key in your .emacs file:
;;
;;    (require 'align)
;;    (global-set-key "\M-[" 'align)
;;
;; Now mark the region you want to align, and press M-[.  See the
;; documentation for `align-mode-alist' for more info on how to define
;; new alignment rules.



;;; Code:

(provide 'align)

(defconst align-version "2.2"
  "The version of align.")

(defgroup align nil
  "Align text to a specific column, by regexp."
  :group 'fill)



;;; User Variables:

(defcustom align-to-tab-stop nil
  "*If non-nil, alignments will be done to the next tab boundary.
If a prefix argument is passed to `align', it will mean the number of
tab stops to use as padding."
  :type 'boolean
  :group 'align)

(defcustom align-across-paragraphs nil
  "*If non-nil, consider the entire region as a single block for aligning.
Otherwise, each paragraph within the region will be aligned as a separate
group."
  :type 'boolean
  :group 'align)

(defvar align-elisp-list
  '(("\\(\\s-*\\)\\.")                      ; alists
    ("\\(\\s-*\\);[^;]" 1 comment-column))) ; comments

(defvar align-perl-list
  '(("[^=]\\(\\s-*\\)=[^=]")                ; simple assignments
    ("[^\\\\]\\(\\s-*\\)#"
     1 comment-column)))                    ; comments

(defvar align-c/c++-list
  '(("#\\s-*define\\s-+\\w+\\(\\s-+\\)")    ; #define's
    ("typedef.*\\(\\s-+\\)\\w+\\s-*;")      ; typedefs
    ("\\(\\w\\|[*&>]\\)\\(\\s-+\\)[*&]*\\w+\\([(;,]\\|\\s-*=\\)" 2)
    ("[^=]\\(\\s-*\\)=[^=]")                ; simple assignments
    ("\\(\\s-*\\)/\\*.*\\*/\\s-*$"
     1 comment-column)                      ; /*..*/ comments
    ("\\(\\s-*\\)//" 1 comment-column)))    ; // comments

(defvar align-tex-list
  '(("&\\(\\s-*\\)" 1 t)                    ; table elements
    ("\\(\\s-*\\)&" 1 t)                    ; for tables
    ("\\(\\s-*\\)\\\\\\\\\\s-*$")))         ; record breaks

(defcustom align-mode-alist
  '((emacs-lisp-mode . align-elisp-list)
    (c-mode          . align-c/c++-list)
    (c++-mode        . align-c/c++-list)
    (java-mode       . align-c/c++-list) ;jhyde added
    (cperl-mode      . align-perl-list)
    (perl-mode       . align-perl-list)
    (plain-tex-mode  . align-tex-list)
    (latex-mode      . align-tex-list))
  "*Alist for identifying which alignment rules apply to a given mode.
Each CAR should eq to a major-mode; each CDR must be either a list of
lists with the format given below, or a symbol whose value is such a
list of lists.  The format of each sublist is:

   (ALIGN-REGEXP [SUBEXP] [OPTION])

This list is searched in order.  For every line that ALIGN-REGEXP
matches, that line will be aligned according to ALIGN-REGEXP.
ALIGN-REGEXP must contain at least one substring which identifies a
group of characters, typically whitespace.  The first character
immediately after this group is considered the \"alignment character\".
The given substring will be deleted, and an appropriate amount of
whitespace inserted so that all alignment characters within a region (or
paragraph; see `align-across-paragraphs') occur in the same column.  If
`align-to-tab-stop' is non-nil, this column will always be on a tab
boundary.

If a SUBEXP is non-nil, it must be an integer which defines the
substring within ALIGN-REGEXP that will be modified for the sake of
alignment.  The default is 1.

If OPTION is t, it means that the alignment rule will be applied
continuously to later parts of the line (following the previous
alignment character), until ALIGN-REGEXP no longer matches.  (This makes
alignment of column separators possible, for example, which might occur
several times on a single line).  If OPTION is an integer, or symbol or
function whose value is an integer, then the alignment boundary will
fixed at that column for the given rule -- whether or not it's on a tab
boundary."
  :type 'sexp
  :group 'align)



;;; User Functions:

(defun align (begin end spacing &optional alignment-list)
  "Attempt to align a region based on the content of its lines.
BEGIN and END mark the region, and SPACING is the amount of spacing (or
tabs) to be used as extra padding for the alignment.  If ALIGNMENT-LIST
is not specified `align-mode-alist' will be consulted to determine what
the alignments for the current mode are.  ALIGNMENT-LIST may be a
symbol, in which case its value is taken."
  (interactive "*r\np")
  (let ((align-list (or alignment-list
                        (cdr (assq major-mode align-mode-alist))))
        (end-mark (copy-marker end)))
    (if (symbolp align-list)
        (setq align-list (symbol-value align-list)))
    (save-excursion
      (mapcar
       (function
        (lambda (rule)
          (if align-across-paragraphs
              (apply 'align-region begin end-mark spacing rule)
            (goto-char begin)
            (while (< (point) end-mark)
              (let ((bop (point)))
                (forward-paragraph)
                (save-excursion
                  (apply 'align-region bop (min end-mark (point))
                         spacing rule))
                (forward-line 1))))))
       align-list))))

(defun align-region (begin end spacing match &optional substr option)
  "Perform colmunar aligning on a range of lines from BEGIN to END.
SPACING is the amount of extra spaces (or tabs) to add to the calculated
alignment column.  Only the lines within region matching MATCH will be
modified.  MATCH must contain at least one substring, which identifies
the characters to be contracted/expanded for the purposes of alignment.
See the doc for `align-mode-alist' for more information.

SUBSTR, if non-nil, must be an integer identifying the substring within
MATCH which is to be modified.

If OPTION is t, the rule will be applied continuously to the string
until all matching text has been aligned.  If OPTION is an integer, or
symbol evaluating to an integer, then the alignment column will be fixed
there."
  (let* ((repeat (eq option 't))
         (sub (or substr 1))
         (fixed-col
          (if (or repeat (not option))
              nil
            (cond ((symbolp option)
                   (symbol-value option))
                  (t option))))
         (continue t)
         (end-mark (copy-marker end))
         (begcol 0)
         next)

    (while continue
      (let ((align-col (or fixed-col -1))
            pos-list at-end)

        ;; walk through every line in the region, and discover what the
        ;; largest alignment column is; then adjust it to the
        ;; appropriate boundary position
        (goto-char begin)
        (while (< (point) end-mark)
          (and next (move-to-column begcol))
          (if (re-search-forward
               match (save-excursion (end-of-line) (point)) t)
              (and (goto-char (match-beginning sub))
                   (setq pos-list
                         (cons (cons (point) (match-end sub))
                               pos-list))
                   (not fixed-col)
                   (setq align-col (max align-col (current-column)))
                   repeat (not at-end)
                   (setq at-end (= (match-end 0)
                                   (match-end sub)))))
          (forward-line))

        (if (or (< align-col 0) (not pos-list))
            (setq continue nil)
          (setq continue repeat next t)

          ;; if the alignment column is not fixed, adjust it
          (if fixed-col
              t
            (if (not align-to-tab-stop)
                (setq align-col (+ spacing align-col))
              (let ((count spacing)
                    (idx 0) stop)
                (while (and (> count 0)
                            (setq stop (nth idx tab-stop-list)))
                  (if (> stop align-col)
                      (setq count (1- count)))
                  (setq idx (1+ idx)))
                (and stop (setq align-col stop)))))

          ;; if repeating, move the begcol past the match
          (if repeat
              (setq begcol (if at-end
                               align-col
                             (1+ align-col))))

          ;; go through the list of position, fixing them up
          (mapcar (function
                   (lambda (reg)
                     (goto-char (car reg))
                     (delete-region (point) (cdr reg))
                     (indent-to align-col)))
                  pos-list))))))



;;; align.el ends here
