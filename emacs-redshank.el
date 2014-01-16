
(.EMACS "emacs-redshank.el")

(when (require 'redshank-loader "redshank/redshank-loader" t)
  (eval-after-load "redshank-loader"
    `(redshank-setup '(lisp-mode-hook
                       slime-repl-mode-hook) t)))


(defun redshank-looking-at-symbol (sym)
  (forward-sexp)
  (backward-sexp)
  (string-equal* sym (symbol-at-point)))

(defun redshank-wrap-defgeneric (fname gf-lambda-list docstring)
  (paredit-wrap-sexp)
  (insert (format "defgeneric %S %S" fname gf-lambda-list))
  (when docstring (insert (format "\n  (:documentation %S)" docstring))))

(defun redshank-generalize-lambda-list (specialized-lambda-list)
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

(defun redshank-current-sexp ()
  (forward-sexp)
  (backward-sexp)
  (sexp-at-point))

(defun redshank-next-sexp ()
  (forward-sexp 2)
  (backward-sexp)
  (sexp-at-point))


(defun pjb-cl-equal-cl-symbol (cl-symbol item)
  (and  (char/= ?: (aref (prin1-to-string item) 0))
   (or (string-equal* item cl-symbol)
       (string-equal* item (format "CL:%s"           cl-symbol))
       (string-equal* item (format "COMMON-LISP:%s"  cl-symbol))
       (string-equal* item (format "CL::%s"          cl-symbol))
       (string-equal* item (format "COMMON-LISP::%s" cl-symbol)))))


(defun pjb-cl-equal-cl-keyword (cl-keyword item)
  (and (string-equal* cl-keyword item)
       (string-equal* "KEYWORD" (symbol-package item))))



(defun parse-body (where body)
  "
WHERE:          (member :lambda :locally :progn) specifies where the
                body is found, that is whether it may contains
                docstrings and declarations, or just declarations, or
                none.

BODY:           A list of forms.

RETURN:         Three values: a docstring or nil, a list of declarations, a list of forms.
"
  (flet ((progn-body (body)
           (if (some (lambda (form) (and (consp form) (eq 'declare (first form))))
                     body)
             (error "Found a declaration in the a progn body: ~S" body)
             body)))
    (ecase where
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
                  ((declarationp form) (push form declarations))
                  ((stringp form)      (setf docstring form
                                             state :seen-string))
                  (t                   (push form actual-body)
                                       (setf state :body))))
               ((:seen-string :after-decl)
                (if (declarationp form)
                  (progn (push form declarations)
                         (setf state :after-decl))
                  (progn (push form actual-body)
                         (setf state :body))))
               (:body
                 (if (declarationp form)
                   (error "Found a declaration ~S in the body ~S" form body)
                   (push form actual-body))))
          finally (return (ecase state
                            (:opt-decl
                             (values docstring declarations (nreverse actual-body)))
                            (:seen-string
                             (if actual-body
                               (values docstring declarations (nreverse actual-body))
                               (values nil declarations (list docstring))))
                            ((:after-decl :body)
                             (values docstring declarations (nreverse actual-body)))))))
      ((:locally)
       ;; {declaration} {form}
       (loop
          for current on body
          for form = (car current)
          while (declarationp form)
          collect form into declarations
          finally (return  (values nil
                                   declarations
                                   (progn-body current)))))
      ((:progn)
       ;; {form}
       (values nil
               nil
               (progn-body body))))))


(defun redshank-make-defgeneric-from-defmethod ()
  "
The point must be before the defmethod form.
The method is then wrapped in a defgeneric form.
If there's a docstring, it's moved to the :documentation option of the
defgeneric.
"
  (interactive)
  (forward-sexp) (backward-sexp)
  (let ((outerpt (point)))
    (when (looking-at "(")
      (forward-char)
      (let ((startpt (point)))
        (when (pjb-cl-equal-cl-symbol 'defmethod (redshank-current-sexp))
          (let* ((fname          (redshank-next-sexp))
                 (qualifier      (redshank-next-sexp))
                 (endpt          (point))
                 (gf-lambda-list (redshank-generalize-lambda-list
                                  (if (symbolp qualifier)
                                      (redshank-next-sexp)
                                      qualifier)))
                 ;; Note: this docstring stuff is bad. We should
                 ;; implement the algorithm for CL bodies. See
                 ;; parse-body above.
                 (docstring      (let ((str (redshank-next-sexp)))
                                   (when (stringp str)
                                     str)))
                 (doc-start      (when docstring
                                   (point)))
                 (doc-end        (when docstring
                                   (redshank-next-sexp)
                                   (point))))
            (when doc-end
              ;; check if there's something after the docstring. If
              ;; not, it's not a docstring.
              (goto-char doc-end)
              (ignore-errors (forward-sexp))
              (when (= (point) doc-end)
                (setf docstring nil
                      doc-start nil
                      doc-end nil)))
            ;; first delete the method docstring
            (when (and doc-start doc-end)
              (delete-region doc-start doc-end))
            ;; then delete defmethod and fname
            (delete-region startpt endpt)
            ;; and insert :method instead
            (goto-char startpt)
            (insert ":method ")
            ;; finally wrap the defgeneric
            (goto-char outerpt)
            (redshank-wrap-defgeneric fname
                                      gf-lambda-list
                                      docstring)
            (insert "\n")
            (paredit-reindent-defun)))))))




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
                       (pjb-cl-equal-cl-symbol 'defpackage (car form))
                       (string-equal* (second form) package-name))
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
                 until (string-equal* (car sexp) :export))
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
               ((and (string-equal* "def" (first form)
                                    :end2 (min 3 (length (prin1-to-string (first form)))))
                     (<= 2 (length form))
                     (symbolp (second form)))
                (pjb-cl-export-symbols (list (second form))))
               (t
                (error "No recognized form.")))))))
    (goto-char marker)))


