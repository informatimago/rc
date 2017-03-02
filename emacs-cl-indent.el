;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;----------------------------------------------------------------------------
(.EMACS "emacs-cl-indent.el")

(require 'lisp-mode)
(load-library "cl-indent")

(setq lisp-indent-function 'common-lisp-indent-function)

(defun lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation)
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function
which has a non-nil property `lisp-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style
* an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (or (looking-at ":") (not (looking-at "\\sw\\|\\s_"))))
        (progn ; car of form doesn't seem to be a symbol, or is a keyword
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (get (intern-soft function) 'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state)))))))

;; (setq lisp-indent-function 'common-lisp-indent-function)

(defun cl-indent (symbol num-forms)
  "
Put on the SYMBOL and its lower case and upper case variants
a 'lisp-indent-function property set to NUM-FORMS.
"
  (dolist (property '(lisp-indent-function common-lisp-indent-function))
    (put symbol property num-forms)
    (put (intern (string-downcase (symbol-name symbol))) property num-forms)
    (put (intern (string-upcase   (symbol-name symbol))) property num-forms)))


(defun %batch-cl-indent (&rest indent-symbols-list)
  (dolist (item indent-symbols-list)
    (let ((indent (car item)))
      (dolist (sym (cdr item))
        (cl-indent sym indent)
        (let ((p (position (character ":") (symbol-name sym))))
          (when p
            (cl-indent (intern (subseq (symbol-name sym) (1+ p)))
                       indent)))))))


(defmacro* do-directories-up ((var dir-path &optional result) &body body)
  "
DO:     Evaluates body with var bound to dir-path, then dir-path's parent,
        and so on up to the root directory.
RETURN: The evaluation of the result form.
"
  `(do ((,var ,dir-path
              (if (string-match "^\\(.*/\\)[^/]+/$" ,var)
                  (match-string 1 ,var)
                  "")))
       ((string-equal "" ,var) ,result)
     ,@body))


(defun* read* (stream &optional (eof-error-p t) eof-value ignored)
  (handler-case (read stream)
    (end-of-file (err)  (if eof-error-p
                            (error err)
                            eof-value))))


(defun load-lisp-indentations ()
  "Processes a lisp.indentations file,
in the current directory, or in a parent."
  (interactive)
  (do-directories-up (dir default-directory)
    (let ((file (concat dir "lisp.indentations")))
      ;; (message "file = %S" file)
      (when (file-exists-p file)
        (save-excursion
          (let ((count (length (buffer-list)))) ; is there a better way?
            (find-file file)
            (goto-char (point-min))
            (let ((killp (/= count (length (buffer-list)))))
              (unwind-protect
                   (loop
                      for clause = (read* (current-buffer) nil (current-buffer))
                      until (eql clause (current-buffer))
                      do (message "(%%batch-cl-indent '%S)" clause)
                      do (%batch-cl-indent clause))
                (when killp (kill-buffer (current-buffer)))))))))))

;; (defmacro batch-cl-indent (&rest indent-symbols-list)
;;   `(%batch-cl-indent ,@(mapcar (lambda (x) `(quote ,x)) indent-symbols-list)))

(defun batch-cl-indent ()
  (interactive)
  (warn "The new command is load-lisp-indentations")
  (load-lisp-indentations))


(let ((html '(DOCTYPE A ABBR ACRONYM ADDRESS APPLET AREA B BASE
              BASEFONT  BDO BIG BLOCKQUOTE BODY BR BUTTON CAPTION
              CENTER CITE CODE COL COLGROUP DD DEL DFN DIR DIV DL
              DT EM FIELDSET FONT  FORM FRAME FRAMESET H1 H2 H3 H4
              H5 H6 HEAD HR HTML I  IFRAME IMG INPUT INS ISINDEX
              KBD LABEL LEGEND LI LINK MAP MENU  META NOFRAMES
              NOSCRIPT OBJECT OL OPTGROUP OPTION P PARAM PRE Q S
              SAMP SCRIPT SELECT SMALL SPAN STRIKE STRONG STYLE SUB
              SUP TABLE TBODY TD TEXTAREA TFOOT TH THEAD TITLE TR
              TT  U UL VAR)))
  (%batch-cl-indent
   (cons 1 (mapcar (lambda (sym) (intern (concat "HTML:" (symbol-name sym)))) html))
   (cons 0 (mapcar (lambda (sym) (intern (concat "<:"    (symbol-name sym)))) html))
   (cons 2 '(<:div))))



(defun pjb-lisp-remove-end-comment ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\([^;\"]*\\)\n[ \t]*\\())+\\) *;.*" nil t)
    (let ((a (match-string 1))
          (b (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert a b)))
  (goto-char (point-min))
  (while (re-search-forward "^\\([^;\"]*\\)\\())+\\) *;.*" nil t)
    (let ((a (match-string 1))
          (b (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert a b))))
