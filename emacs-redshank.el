;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               emacs-redshank.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    Redshank extensions: transforming CL sources.
;;;;
;;;;    The parsing functions return redshank-source structures
;;;;    refering to the source position and source substring, so that
;;;;    transformation commands may modify the buffer and bring along
;;;;    comments.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-09 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;  
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
;;;;  
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;  
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;  
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(.EMACS "emacs-redshank.el")

(when (require 'redshank-loader "redshank/redshank-loader" t)
  (eval-after-load "redshank-loader"
    `(redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t)))

;;; --------------------------------------------------------------------
;;; Utilities.
;;; --------------------------------------------------------------------

(defun pjb-cl-equal-cl-symbol (cl-symbol item)
  (let ((cl-name (symbol-name cl-symbol)))
    (if (char= ?: (aref cl-name 0))
        (let ((item-name (symbol-name item)))
          (and (char= ?: (aref item-name 0))
               (cl:string-equal item cl-symbol)))
        (or (cl:string-equal item cl-symbol)
            (cl:string-equal item (format "CL:%s"           cl-symbol))
            (cl:string-equal item (format "COMMON-LISP:%s"  cl-symbol))
            (cl:string-equal item (format "CL::%s"          cl-symbol))
            (cl:string-equal item (format "COMMON-LISP::%s" cl-symbol))))))


(defun pjb-cl-equal-cl-keyword (cl-keyword item)
  (and (cl:string-equal cl-keyword item)
       (cl:string-equal "KEYWORD" (cl:symbol-package item))))


(defvar *redshank-lambda-list-keywords*
  '((&optional 0-*)
    (&rest 1)
    (&aux 0-*)
    (&key 0-*)
    (&allow-other-keys 0)
    (&body 0-*)
    (&environment 1)
    (&whole 1)))

(defun redshank-lambda-list-keyword-p (item)
  (let ((fold-case-search t))
    (second (assoc* item *redshank-lambda-list-keywords*
                    :test (lambda (a b) (string= (string-upcase a) (string-upcase b)))))))


;; (remove-if (lambda (x) (member x '(&optional &rest &aux &key &allow-other-keys &body &environment &whole))) lambda-list)

(defun redshank-generalize-lambda-list (specialized-lambda-list)
  ;; quick-and-dirty stuff. We should port com.informatimago.common-lisp.lisp-sexp stuff.
  (let ((end (position '&aux specialized-lambda-list)))
    (mapcar (lambda (item)
                         (if (atom item)
                             item
                             (let ((kv (first item)))
                               (if (atom kv)
                                   kv
                                   (second kv)))))
                       (if end
                           (subseq specialized-lambda-list 0 end)
                           specialized-lambda-list))))

;;; --------------------------------------------------------------------
;;; Generation functions
;;; --------------------------------------------------------------------

(defun redshank-insert-defgeneric (fname gf-lambda-list docstring)
  "Generate a defgeneric form."
  (insert (format "(defgeneric %S %S" fname gf-lambda-list))
  (when docstring (insert (format "\n  (:documentation %S)" docstring)))
  (insert ")"))

(defun redshank-wrap-defgeneric (fname gf-lambda-list docstring)
  "Wrap a defgeneric form around the current :method form."
  (paredit-wrap-sexp)
  (insert (format "defgeneric %S %S" fname gf-lambda-list))
  (when docstring (insert (format "\n  (:documentation %S)" docstring))))




;;; --------------------------------------------------------------------
;;; Parsing functions
;;; --------------------------------------------------------------------

(defun redshank-top-level (&optional arg)
  "
Move forward out of all the levels of parentheses.
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment.
"
  (while (ignore-errors (not (up-list (or arg 1))))))


(defun redshank-looking-at-symbol (sym)
  "Wether the current thing is the symbol `sym'."
  (forward-sexp)
  (backward-sexp)
  (cl:string-equal sym (symbol-at-point)))

(defun redshank-current-sexp ()
  "The sexp just after the point."
  (forward-sexp)
  (backward-sexp)
  (sexp-at-point))

(defun redshank-next-sexp ()
  "The sexp following the sexp just after the point."
  (forward-sexp 2)
  (backward-sexp)
  (sexp-at-point))


(defstruct redshank-source
  sexp
  startpt
  endpt
  text)


(defun redshank-current-source-sexp ()
  "The redshank-source of the sexp just after the point."
  (ignore-errors
   (forward-sexp)
   (let ((endpt (point)))
     (backward-sexp)
     (let ((startpt (point))
           (sexp    (sexp-at-point)))
       (make-redshank-source :sexp sexp
                             :startpt startpt
                             :endpt endpt
                             :text (buffer-substring startpt endpt))))))


(defun redshank-next-source-sexp ()
  "The redshank-source of the sexp following the sexp just after the point."
  (forward-sexp)
  (redshank-current-source-sexp))


(defun redshank-remaining-source-sexps ()
  "The redshank-source of all the sexps following the point, till the end of buffer or the next closing parenthesis."
  (loop
    for sexp = (redshank-current-source-sexp) then (redshank-next-source-sexp)
    while sexp
    collect sexp))


(defun redshank-source-operator (reshank-source)
  "The operator of the `redshank-source', or nil if it's an atom."
  (let ((sexp (redshank-source-sexp reshank-source)))
    (if (atom sexp)
        nil
        (first sexp))))


(defun redshank-source-declarationp (redshank-source)
  "Whether the `redshank-source' is a CL declaration form."
  (let ((operator (redshank-source-operator redshank-source)))
    (and (symbolp operator)
         (pjb-cl-equal-cl-symbol 'declare operator))))

(defun redshank-source-stringp (redshank-source)
  "Whether the `redshank-source' is a CL string literal."
  (stringp (redshank-source-sexp redshank-source)))

(defun redshank-source-symbolp (redshank-source)
  "Whether the `redshank-source' is a symbol (including nil = ())."
  (symbolp (redshank-source-sexp redshank-source)))


(defun redshank-parse-body (kind)
  "
Parses the following sexps as a CL body.

KIND:           (member :lambda :locally :progn) specifies the kind of
                body is found, that is whether it may contains
                docstrings and declarations, or just declarations, or
                none.

RETURN:         A list of three redshank-source values: a docstring
                or nil, a list of declarations, a list of body forms.
"
  (let ((body (redshank-remaining-source-sexps)))
    (flet ((progn-body (body)
             (if (some (function redshank-source-declarationp) body)
                 (error "Found a declaration in the a progn body: ~S" body)
                 body)))
      (ecase kind
        ((:lambda)
         ;; {declaration} [docstring declaration {declaration}] {form}
         ;; {declaration} [docstring] form {form}
         (loop
           with docstring    = nil
           with declarations = '()
           with actual-body  = '()
           with state        = :opt-decl
           for form in body
           do (ecase state
                (:opt-decl
                 (cond
                   ((redshank-source-declarationp form) (push form declarations))
                   ((redshank-source-stringp form)      (setf docstring form
                                                              state :seen-string))
                   (t                                   (push form actual-body)
                                                        (setf state :body))))
                ((:seen-string :after-decl)
                 (if (redshank-source-declarationp form)
                     (progn (push form declarations)
                            (setf state :after-decl))
                     (progn (push form actual-body)
                            (setf state :body))))
                (:body
                    (if (redshank-source-declarationp form)
                        (error "Found a declaration ~S in the body ~S" form body)
                        (push form actual-body))))
           finally (return (ecase state
                             (:opt-decl
                              (list docstring declarations (nreverse actual-body)))
                             (:seen-string
                              (if actual-body
                                  (list docstring declarations (nreverse actual-body))
                                  (list nil declarations (list docstring))))
                             ((:after-decl :body)
                              (list docstring declarations (nreverse actual-body)))))))
        ((:locally)
         ;; {declaration} {form}
         (loop
           for current on body
           for form = (car current)
           while (redshank-source-declarationp form)
           collect form into declarations
           finally (return (list nil
                                 declarations
                                 (progn-body current)))))
        ((:progn)
         ;; {form}
         (values nil
                 nil
                 (progn-body body)))))))


(defun redshank-goto-toplevel-form (&optional redshank-source)
  "Move point to the start of redshank-source or to the toplevel of the current sexp."
  (if redshank-source
      (goto-char (redshank-source-startpt redshank-source))
      (progn
        (redshank-top-level -1)
        (forward-sexp)
        (backward-sexp))))

(defun redshank-parse-defgeneric (&optional defgeneric)
  "Parses a defgeneric form specified as the redshank-source parameter `defgeneric' or at the point.
This doesn't parse the clauses inside the defgeneric.
Call the corresponding parse function on each one.
RETURN: (defgeneric operator fname lambda-list (clause…)),
        each item a redshank-source structure.
"
  (redshank-goto-toplevel-form defgeneric)
  (let ((defgeneric (redshank-current-source-sexp)))
    (when (looking-at "(")
      (forward-char)
      (let ((operator (redshank-current-source-sexp)))
        (when (pjb-cl-equal-cl-symbol 'defgeneric (redshank-source-sexp operator))
          (let ((fname       (redshank-next-source-sexp))
                (lambda-list (redshank-next-source-sexp))
                (clauses     (progn (forward-sexp)
                                    (redshank-remaining-source-sexps))))
            (list defgeneric operator fname lambda-list clauses)))))))


(defun redshank-parse-defgeneric-method (method)
  "Parses the :method defgeneric clause specied by the redshank-source parameter `method'.
RETURN: (method operator lambda-list docstring (declaration…) (body…)),
        each item a redshank-source structure."
  (goto-char (redshank-source-startpt method))
  (when (looking-at "(")
    (forward-char)
    (let ((operator (redshank-current-source-sexp)))
      (when (pjb-cl-equal-cl-symbol ':method (redshank-source-sexp operator))
        (let ((lambda-list (redshank-next-source-sexp))
              (body        (progn (forward-sexp)
                                  (redshank-parse-body :lambda))))
          (list* method operator lambda-list body))))))



(defun redshank-parse-defmethod (&optional defmethod)
  "Parses a defmethod form specified as the redshank-source parameter `defmethod' or at the point.
RETURN: (defmethod operator fname qualifier lambda-list docstring (declaration…) (body…)),
        each item a redshank-source structure."
  (redshank-goto-toplevel-form defmethod)
  (let ((defmethod (redshank-current-source-sexp)))
    (when (looking-at "(")
      (forward-char)
      (let ((operator (redshank-current-source-sexp)))
        (when (pjb-cl-equal-cl-symbol 'defmethod (redshank-source-sexp operator))
          (let* ((fname       (redshank-next-source-sexp))
                 (qorll       (redshank-next-source-sexp))
                 (qualifier   (when (redshank-source-symbolp qorll)
                                qorll))
                 (lambda-list (if (redshank-source-symbolp qorll)
                                  (redshank-next-source-sexp)
                                  qorll))
                 (body        (progn (forward-sexp)
                                     (redshank-parse-body :lambda))))
            (list* defmethod operator fname qualifier lambda-list body)))))))


;;; --------------------------------------------------------------------
;;; Transformation commands.
;;; --------------------------------------------------------------------

(defun redshank-source-delete (redshank-source)
  "Delete from the buffer the text of the `redshank-source'."
  (when redshank-source
    (delete-region (redshank-source-startpt redshank-source)
                   (redshank-source-endpt   redshank-source))))


(defun redshank-make-defgeneric-for-defmethod ()
  "
The point must be before the defmethod form.
A new defgeneric form is inserted before the defmethod.

   (defmethod name ((pname class))
     body)
   ----
   (defgeneric name (pname))
   (defmethod name ((pname class))
     body)
"
  (interactive)
  (destructuring-bind (defmethod operator fname qualifier lambda-list docstring declarations body)
      (redshank-parse-defmethod)
    (let ((gf-lambda-list (redshank-generalize-lambda-list (redshank-source-sexp lambda-list))))
      ;; insert the defgeneric
      (goto-char (redshank-source-startpt defmethod))
      (redshank-insert-defgeneric (redshank-source-sexp fname)
                                  gf-lambda-list
                                  nil)
      (insert "\n")
      (paredit-reindent-defun))))


(defun redshank-make-defgeneric-from-defmethod ()
  "
The point must be before the defmethod form.
The method is then wrapped in a defgeneric form.
If there's a docstring, it's moved to the :documentation option of the
defgeneric.

   (defmethod name ((pname class))
     \"docstring\"
     body)
   ----
   (defgeneric name (pname)
      (:documentation \"docstring\")
      (:method ((pname class))
        body))
"
  (interactive)
  (destructuring-bind (defmethod operator fname qualifier lambda-list docstring declarations body)
      (redshank-parse-defmethod)
    (let ((gf-lambda-list (redshank-generalize-lambda-list (redshank-source-sexp lambda-list))))
      ;; first delete the method docstring
      (redshank-source-delete docstring)
      ;; then delete defmethod and fname
      (redshank-source-delete fname)
      (redshank-source-delete operator)
      ;; and insert :method instead
      (goto-char (redshank-source-startpt operator))
      (insert ":method ")
      ;; finally wrap the defgeneric
      (goto-char (redshank-source-startpt defmethod))
      (redshank-wrap-defgeneric (redshank-source-sexp fname)
                                gf-lambda-list
                                (when docstring (redshank-source-sexp docstring)))
      (insert "\n")
      (paredit-reindent-defun)
      (up-list))))

(defun redshank-marker (pt)
  (let ((marker (make-marker)))
    (set-marker marker pt)
    marker))

(defun redshank-split-defgeneric-to-defmethod ()
  "A defgeneric form with :method clauses is split into defmethod forms.

   (defgeneric name (pname)
      (:documentation \"docstring\")
      (:method ((pname class))
        body))
   ----
   (defgeneric name (pname))
   (defmethod name ((pname class))
     \"docstring\"
     body)
"
  (interactive)
  (destructuring-bind (defgeneric operator fname lambda-list clauses)
      (redshank-parse-defgeneric)
    (let* ((docstring (find :documentation clauses
                            :key (function redshank-source-operator)
                            :test (function pjb-cl-equal-cl-symbol)))
           (dsstart   (when docstring (redshank-marker (redshank-source-startpt docstring))))
           (dsend     (when docstring (redshank-marker (redshank-source-endpt   docstring))))
           (methods   (remove* :method clauses
                               :key (function redshank-source-operator)
                               :test-not (function pjb-cl-equal-cl-symbol))))
      (dolist (method (reverse methods))
        (destructuring-bind (method operator lambda-list docstring declarations body)
            (redshank-parse-defgeneric-method method)
          (goto-char (redshank-source-endpt operator))
          (insert " " (redshank-source-text fname))
          (goto-char (redshank-source-startpt operator))
          (delete-char 1) (insert "def")))
      (when docstring
        (delete-region dsstart dsend)
        (set-marker dsstart nil)
        (set-marker dsend   nil))
      (goto-char (redshank-source-endpt lambda-list))
      (when docstring
        (insert (format "\n%s" (redshank-source-text docstring))))
      (paredit-split-sexp)
      (down-list)
      (paredit-splice-sexp-killing-backward))))


'(defgeneric fname (a b c)
  (:method ((a a) (b b) (c c))
    (declare (ignore c))
    "docstring"
    (declare (ignore a))
    b)
  (:method ((a a) (b b) (c c))
    (declare (ignore c))
    (declare (ignore a))
    "docstring"
    b)
  (:documentation "gf docstring")
  (:method ((a a) (b b) (c c))
    (declare (ignore c))
    (declare (ignore a))
    "result")
  (:method ((a a) (b b) (c c))
    (declare (ignore c))
    (declare (ignore a))
    b)
  (:method ((a a) (b b) (c c))
    "docstring"
    "result")
  (:method ((a a) (b b) (c c))
    "result")
  (:method ((a e) (b f) (c g))))



;;; --------------------------------------------------------------------
;;; extract to defun or defmethod
;;; --------------------------------------------------------------------

(defun pjb-redshank-extract-to-defun (start end name &optional package)
  "Extracts region from START to END as new defun NAME.
The marked region is replaced with a call, the actual function
definition is placed on the toplevel before the current form.

A best effort is made to determine free variables in the marked
region and make them parameters of the extracted function.  This
involves macro-expanding code, and as such might have side effects."
  (interactive "*r\nsName for extracted function: ")
  (flet ((princ-to-string (o)
             (with-output-to-string
               (princ (if (null o) "()" o)))))
    (let* ((form-string (buffer-substring-no-properties start end))
           (free-vars (slime-eval `(redshank:free-vars-for-emacs
                                    ,(concat "(locally " form-string ")")
                                    ,(or package (slime-pretty-package-name
                                                  (slime-current-package))))
                                  package))
           (defun (with-temp-buffer
                      (lisp-mode)              ; for proper indentation
                    (insert "(defun " name " " (princ-to-string free-vars) "\n")
                    (insert form-string ")\n")
                    (goto-char (point-min))
                    (indent-sexp)
                    (buffer-substring  (point-min) (point-max)))))
      (delete-region start end)
      (princ (list* name free-vars) (current-buffer))
      (save-excursion
       (beginning-of-defun)
       (insert defun "\n")))))

;;; --------------------------------------------------------------------
;;; defpackage: updating export lists
;;; --------------------------------------------------------------------

(defun pjb-cl-find-defpackage-form (package-name)
  "Find the defpackage form for the given `package-name' in the current buffer.
RETURN:  The point at the start of the defpackage sexp, or NIL if not found.
NOTE:    Excursion is saved.
"
  (save-excursion
    (goto-char (point-min))
    (forward-sexp)
    (loop
       do (let ((form (progn (backward-sexp) (redshank-current-sexp))))
            (when (and (listp form)
                       (atom (first form))
                       (pjb-cl-equal-cl-symbol 'defpackage (first form))
                       (atom (second form))
                       (cl:string-equal (second form) package-name))
              (return  (point)))
            (forward-sexp 2))
       while (< (point) (point-max))
       finally (return nil))))


(defun pjb-cl-package-files ()
  "RETURN: A list of files named *package*.lisp and the current buffer file."
  (let ((current-file (buffer-file-name)))
    (append
     (when current-file (list current-file))
     (file-expand-wildcards
      (replace-regexp-in-string "//" "/"
                                (format "%s/*package*.lisp" default-directory))))))


(defvar pjb-cl-package-files 'pjb-cl-package-files
  "The function used to get a list of files where there are defpackage forms.
The default function only searches in the current file and in
\"*package*.lisp\" in the same directory.")


(defun* pjb-cl-find-package-file (package-name &key (if-does-not-exist nil))
  "Find the file where the current package is defined.
Search the current buffer and files named *package*.lisp in the default directory.

IF-DOES-NOT-EXIST:  can be :error, :file or another value.

RETURN: If a defpackage form is found for the current package (path point).
NOTE:   The searched files are left open.  Excursion is saved.
"
  (let ((pos (pjb-cl-find-defpackage-form package-name)))
    (if pos
        (list (buffer-file-name) pos)
        (save-excursion
          (loop
             with files = (funcall pjb-cl-package-files)
             for file in files
             do (progn
                  (find-file file)
                  (let ((pos (pjb-cl-find-defpackage-form package-name)))
                    (when pos
                      (return (list file pos)))))
             finally ; doesn't exist
               (return (case if-does-not-exist
                         (:error (error "No file with (defpackage %S) found." package-name))
                         (:file  (or (first files) (buffer-file-name)))
                         (otherwise if-does-not-exist))))))))


(defun pjb-cl-package-designator (name)
  (funcall redshank-canonical-package-designator-function
           (etypecase name
             (symbol (symbol-name name))
             (string name))))


(defun* pjb-cl-insert-defpackage (name &key
                                       (nicknames '())
                                       (documentation nil)
                                       (use '("COMMON-LISP"))
                                       (shadow '())
                                       (shadowing-import-from '())
                                       (import-from '())
                                       (export '())
                                       (intern '())
                                       (size   nil))
  (flet ((insert-option (option items)
           (insert (format "\n  (%s" option))
           (when (listp items)
             (dolist (name items)
               (insert (format  " %s" (pjb-cl-package-designator name)))))
           (insert ")")))
    (insert (format  "(defpackage %s" (pjb-cl-package-designator name)))
    (when nicknames             (insert-option :nicknames nicknames))
    (when documentation         (insert (format "\n  (:documentation %S)" documentation)))
    (insert-option :use use)
    (when shadow                (insert-option :shadow shadow))
    (when shadowing-import-from (insert-option :shadowing-import-from shadowing-import-from))
    (when import-from           (insert-option :import-from import-from))
    (when export                (insert-option :export export))
    (when intern                (insert-option :intern intern))
    (when size                  (insert (format "\n  (:size %s)")))
    (insert ")\n")))


(defun pjb-cl-find-export-point ()
  "Find the file where the current package is defined, and in it, the
point where one can insert an exported symbol.  If there's no :export
clause, add one in the defpackage form.  If there's no defpackage
form, then error out.
RETURN: (path point)
"
  (let* ((package-name   (first (read-from-string (slime-current-package))))
         (file-defpackpt (pjb-cl-find-package-file package-name :if-does-not-exist :file)))
    (when file-defpackpt
      (save-excursion ; in case it's in the same file.
        (destructuring-bind (file defpackpt)
            (if (stringp file-defpackpt)
                (progn ; a new defpackage form is needed in that file.
                  (find-file file-defpackpt)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-sexp)
                    (backward-sexp)
                    (prog1 (list file-defpackpt (point))
                      (pjb-cl-insert-defpackage package-name
                                                :documentation "\nUndocumented yet.\n"
                                                :export t)
                      (insert "\n"))))
                file-defpackpt)
          ;; we can insert into that defpackage form.
          (find-file file)
          (let ((pt (point)))
            (goto-char defpackpt) ; looking at the defpackage form.
            (let ((defpack (redshank-current-sexp)))
              (unless (ignore-errors (find :export (cddr defpack) :key (function first)))
                ;; no export
                (forward-char) (forward-sexp 2)
                (insert "\n(:export)"))
              ;; there's an export
              (goto-char defpackpt)
              (forward-char)
              (forward-sexp)
              (loop
                 for sexp = (redshank-next-sexp)
                 until (cl:string-equal (car sexp) :export))
              (let ((start (prog1 (point) (forward-sexp)))
                    (end   (prog1 (point) (backward-sexp))))
                (forward-char)
                (forward-sexp) 
                (loop
                   with target = (if (and (< start pt) (< pt end))
                                     pt ; current point inside the export.
                                     (1- end)) ; current point ouside the export.
                   for lastpt = (point)
                   while (and (ignore-errors (progn (forward-sexp) t))
                              (< (point) target))
                   finally (return (list file lastpt)))))))))))


(defun pjb-cl-export-symbols (symbol-list)
  (destructuring-bind (file point) (pjb-cl-find-export-point)
    (find-file file)
    (goto-char point)
    (dolist (sym symbol-list)
      (insert (format "\n   %s" (pjb-cl-package-designator sym))))))


(defun pjb-cl-export-symbol-at-point ()
  "Insert into the defpackage form an export of the symbol following the point."
  (interactive)
  (save-window-excursion
    (save-excursion
     (forward-sexp) (backward-sexp)
     (pjb-cl-export-symbols (list (symbol-at-point))))))


(defun pjb-cl-function-name-symbol (name)
  "RETURN: the symbol of a function name (either itself or the second element of (setf name))."
  (cond ((and (listp name)
              (<= 2 (length name))
              (pjb-cl-equal-cl-symbol 'setf (first name))
              (symbolp (second name)))
         (second name))
        ((symbolp name)
         name)
        (t
         (error "~S is not a function name" name))))


(defun pjb-cl-defstruct-symbols (form)
  "Return a list of symbol names defined by the defstruct FORM."
  (let* ((name         (second form))
         (uname        (string-upcase (if (listp name)
                                          (first name)
                                          name)))
         (conc-name    (format "%s-" uname))
         (constructors (list (format "MAKE-%s" uname)))
         (copier       (format "COPY-%s" uname))
         (predicate    (format "%s-P" uname)))
    (when (listp name)
      (loop
         for option in (rest name)
         do (if (atom option)
                (case option
                  (:conc-name   (setf conc-name    ""))
                  (:constructor (setf constructors (pushnew (format "MAKE-%s" uname) constructors
                                                            :test (function string=))))
                  (:copier      (setf copier       nil))
                  (:predicate   (setf predicate    nil)))
                (case (first option)
                  (:conc-name   (setf conc-name (or (and (second option)
                                                         (string-upcase (second option)))
                                                    "")))
                  (:constructor (cond
                                  ((null (rest option))
                                   (pushnew (format "MAKE-%s" uname) constructors
                                            :test (function string=)))
                                  ((null (second option))
                                   (setf constructors '()))
                                  (t
                                   (pushnew (string-upcase (second option)) constructors
                                            :test (function string=)))))
                  (:copier      (setf copier       (and (second option)
                                                        (string-upcase (second option)))))
                  (:predicate   (setf predicate    (and (second option)
                                                        (string-upcase (second option)))))))))
    (append (list uname)
            constructors
            (when predicate (list predicate))
            (when copier    (list copier))
            (mapcar (lambda (field)
                      (format "%s%s"
                              conc-name
                              (string-upcase
                               (if (listp field)
                                   (first field)
                                   field))))
                    (cddr form)))))


(defun pjb-cl-defclass-symbols (form)
  "Return a list of symbol names defined by the defclass or define-condition FORM."
  (cons (second form)
        (mapcan (lambda (slot)
                  (when (listp slot)
                    (loop
                       for (key name) on (cdr slot) by (function cddr)
                       when (or (pjb-cl-equal-cl-keyword :reader   key)
                                (pjb-cl-equal-cl-keyword :writer   key)
                                (pjb-cl-equal-cl-keyword :accessor key))
                       collect (pjb-cl-function-name-symbol name))))
                (fourth form))))


(defun pjb-cl-export-definition-at-point ()
  "Insert into the defpackage form an export of the symbols defined by the form the point."
  (interactive)
  (let* ((pt     (point))
         (marker (make-marker)))
    (set-marker marker pt)
    (save-window-excursion
      (forward-sexp)
      (setf pt (point))
      (set-marker marker pt)
      (backward-sexp)
      (let ((form (sexp-at-point)))
        (cond
          ((null form)    (error "Cannot find a sexp at point (possibly because of a reader macro in it)."))
          ((symbolp form) (pjb-cl-export-symbols (list form)))
          ((atom form)    (error "Cannot export a %S" (type-of form)))
          (t (cond
               ((and (pjb-cl-equal-cl-symbol 'defstruct (first form))
                     (<= 2 (length form)))
                (pjb-cl-export-symbols (pjb-cl-defstruct-symbols form)))
               ((and (or (pjb-cl-equal-cl-symbol 'defclass         (first form))
                         (pjb-cl-equal-cl-symbol 'define-condition (first form)))
                     (<= 4 (length form)))
                (pjb-cl-export-symbols (pjb-cl-defclass-symbols form)))
               ((and (or (pjb-cl-equal-cl-symbol 'defun      (first form))
                         (pjb-cl-equal-cl-symbol 'defmacro   (first form))
                         (pjb-cl-equal-cl-symbol 'defmethod  (first form))
                         (pjb-cl-equal-cl-symbol 'defgeneric (first form)))
                     (<= 2 (length form)))
                (pjb-cl-export-symbols (list (pjb-cl-function-name-symbol (second form)))))
               ((and (cl:string-equal "def" (first form)
                                    :end2 (min 3 (length (prin1-to-string (first form)))))
                     (<= 2 (length form))
                     (symbolp (second form)))
                (pjb-cl-export-symbols (list (second form))))
               (t
                (error "No recognized form.")))))))
    (goto-char marker)))


;;; --------------------------------------------------------------------
;;; Miscellaneous
;;; --------------------------------------------------------------------

(defun pjb-insert-niy-method ()
  (interactive)
  (let ((pt (point)))
    (destructuring-bind ((outerpt defmethodpt fnamept qualifierpt lambda-list-pt docstringpt bodypt endpt)
                         defmethod fname qualifier lambda-list docstring body)
        (redshank-parse-defmethod)
      (let ((gf-lambda-list (redshank-generalize-lambda-list lambda-list)))
        (goto-char pt)
        (insert (format "%S\n" (append (list 'niy fname) gf-lambda-list)))
        (paredit-reindent-defun)
        (up-list)))))

;; (global-set-key (kbd "<f13>")'pjb-insert-niy-method)

;;;; THE END ;;;;
