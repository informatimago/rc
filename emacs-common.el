;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file.
;;;;
;;;; We only run GNU emacs.
;;;;

;; Emacs   Makes All Computing Simple.
;; Eine    Is Not Emacs.
;; Zwei    Was Eine Initially.
;; Drei    Ressembled Emacs Intelligently.
;; Vier    Integrates Emacs Regexps.
;; Vier    Is Emacs Rewritten.
;; Vier    Improves Eine's Revisions.
;; Fünf    Überly New Framework.
;; Sechs   Sechs Emacs Can Handle Strings.
;; Sieben  Is Even Better Emacs Now
;; Acht    Can Handle Text.
;; Neun    Emacs Usually .
;; Zehn    Emacs Handles News.
;; Hemlock Emacs Made Laughically Overly Capably Kidding.
;; Climacs Common Lisp Interface Manager Application Creating Sources.
;; Mince   Is Not Complete Emacs.
;; FRED    Resembles Emacs Deliberately

;;(when (= (user-uid) 0)
;;  (load "/root/.emacs" pjb:*load-noerror* pjb:*load-silent*)
;;  (error "~/.emacs: Cannot load ~/.emacs under root account."))


;; tramp hops: /ssh:bird@bastion|ssh:you@remotehost:/path
;; tramp hops: /ssh:you@remotehost:/path


;;;----------------------------------------------------------------------------
;;; Message Log
;;;----------------------------------------------------------------------------
(setq-default lexical-binding t)
(setq byte-compile-warnings '(not obsolete))
(defvar *emacs-start-time*       (current-time) "For (emacs-uptime).")

(defvar *pjb-load-noerror*       t)
(defvar *pjb-load-silent*        nil)
(defvar *pjb-light-emacs*        nil "pjb-loader will load the minimum.")
(defvar *pjb-pvs-is-running*     (and (boundp 'x-resource-name)
                                      (string-equal x-resource-name "pvs")))
(defvar *pjb-save-log-file-p*    nil "Whether .EMACS must save logs to /tmp/messages.txt")

(if (string= emacs-version "25.0.50.1")
    (setq source-directory
          ;; "/usr/local/src/emacs/src"
          "~/works/emacs/src")
    (setq source-directory (format "/usr/local/src/emacs-%s/src" emacs-version)))


(defvar *lac-functions* '()
  "A list of functions to be called after elpa is loaded.
please, use `add-lac' and `remove-lac' instead of accessing this list directly.")
(defun add-lac (&rest lac-functions)
  (setf *lac-functions* (union lac-functions *lac-functions*)))
(defun remove-lac (&rest lac-functions)
  (setf *lac-functions* (set-difference *lac-functions* lac-functions)))
(defun run-lac-functions ()
  "Runs each lac function in *lac-functions*."
  (interactive)
  (dolist (lac *lac-functions*)
    (ignore-errors (funcall lac))))

(defvar shell-file-name "/bin/bash")
(defvar *tempdir*  (format "/tmp/emacs%d" (user-uid)))

(defvar *hostname*
  (or (and (boundp 'system-name) system-name)
      (and (fboundp 'system-name) (system-name))
      (ignore-errors
        (shell-command-to-string
         "echo -n $( (hostname -f 2>/dev/null) || (hostname 2>/dev/null) )")
        "localhost")))



(defun .EMACS (fctl &rest args)
  (let ((text (apply (function format) (concat ".EMACS: " fctl) args)))
    (when *pjb-save-log-file-p*
      (with-current-buffer (get-buffer-create " .EMACS temporary buffer")
        (erase-buffer)
        (insert text "\n")
        (append-to-file (point-min) (point-max) (format "%s/messages.txt" *tempdir*))))
    (message text)))

(.EMACS "~/rc/emacs-common.el %s" "Pascal J. Bourguignon's emacs startup file.")


;;;----------------------------------------------------------------------------
;;; Life saver
;;;----------------------------------------------------------------------------
(.EMACS "REQUIRE CL...")
(require 'cl)
(require 'parse-time)
(require 'tramp nil t)
(require 'cc-mode)
(require 'bytecomp)
(byte-compile-disable-warning 'cl-functions)

(when (boundp 'byte-compile-warning-types)
  (setq byte-compile-warning-types (remove 'cl-functions byte-compile-warning-types)))

;; byte-compile-warning-types
;; (redefine callargs free-vars unresolved obsolete noruntime cl-functions interactive-only make-local mapcar constants suspicious lexical)
;; byte-compile-warnings
;; (not cl-functions)


(.EMACS "STARTING...")
(mapc (lambda (f) (when (fboundp (car f)) (apply (function funcall) f)))
      '((scroll-bar-mode     -1)
        (menu-bar-mode       -1)
        (tool-bar-mode       -1)
        (transient-mark-mode +1)))

;; (progn (scroll-bar-mode -1) (menu-bar-mode -1) (tool-bar-mode -1) (transient-mark-mode +1))

(defun mac-vnc-keys ()
  (interactive)
  (setf mac-command-modifier    'alt ; emacsformacosx
        mac-option-modifier     'meta
        one-buffer-one-frame    nil)
  (setf mac-command-key-is-meta nil  ; which emacs?
        mac-reverse-ctrl-meta   nil))

(defun mac-adjust-full-screen ()
  (interactive)
  (tool-bar-mode +1)
  (tool-bar-mode -1)
  (ff -1))

;; (defun mac-vanilla-keys (&optional prefix)
;;   (interactive "P")
;;   (if prefix
;;    (setf mac-command-key-is-meta t     ; which emacs?
;;          mac-reverse-ctrl-meta   nil)
;;    (setf mac-command-modifier    'meta  ; emacsformacosx
;;          mac-option-modifier     'alt
;;          one-buffer-one-frame    nil)))

;; MacOSX Modifiers:
;; C-
;; S-                     S-
;; C- A- M- SPC M- A- C-p C-

;; (global-set-key (kbd "C") 'self-insert-command)
;; (local-set-key (kbd "C") 'self-insert-command)

(defun mac-vanilla-keys ()
  (interactive)
  (setf mac-command-modifier    'meta ; emacsformacosx
        mac-option-modifier     'alt
        one-buffer-one-frame    nil)
  (setf mac-command-key-is-meta t     ; which emacs?
        mac-reverse-ctrl-meta   nil))


(when (or (boundp 'aquamacs-version) (eq window-system 'ns))
  (mac-vanilla-keys)
  ;; (if 'thru-vnc
  ;;     (mac-vnc-keys)
  ;;     (mac-vanilla-keys))
  (cua-mode 0))

(when (boundp 'x-toolkit-scroll-bars)
  (setf x-toolkit-scroll-bars nil))


;;;----------------------------------------------------------------------------
(when t
  (.EMACS "emacs-major-version  = %S" emacs-major-version)
  (.EMACS "emacs-minor-version  = %S" emacs-minor-version)
  (.EMACS "emacs-version        = %S" emacs-version)
  (.EMACS "system-type          = %S" system-type)
  (.EMACS "system-name          = %S" system-name)
  (.EMACS "system-configuration = %S" system-configuration)
  (.EMACS "window-system        = %S" window-system)
  (when (boundp 'aquamacs-version)
    (.EMACS "aquamacs-version     = %S" aquamacs-version)))

;; (window-system system-type system-configuration system-name)
;; (emacs-major-version emacs-minor-version emacs-version)
;; system-type          darwin   gnu/linux  cygwin windows-nt
;; system-name          "naiad.informatimago.com" "hermes.afaa.asso.fr"
;; system-configuration "i686-pc-linux-gnu" "i686-pc-cygwin" "i386-apple-darwin9.8.0"
;; window-system        nil x mac ns w32
;; emacs-major-version  18 19 20 21 23
;; emacs-minor-version  0 1 2 3
;; emacs-version        "20.7.2" "21.2.1" 
;; NO emacs-type! We won't run on anything else than GNU emacs.

;; window-system ==> (display-multi-frame-p)
;; xterm-mouse-mode ;; To use the mouse inside xterm!

(mouse-avoidance-mode 'cat-and-mouse)

(require 'rst nil t)
(require 'rst-mode nil t)




;; Unfortunately, custom only updates toplevel forms, so we need to do the same.
(defun reset-faces ()
  "Search in ~/.emacs for a custom-set-faces toplevel form, and evaluates it."
  (interactive)
  (when (or custom-file init-file-user)
    (save-window-excursion
      (find-file (or custom-file user-init-file))
      (goto-char (point-min))
      (forward-sexp) 
      (while (and (< (point) (point-max))
                  (not
                   (let ((form (progn (backward-sexp) (sexp-at-point))))
                     (when (and (listp form)
                                (eq 'custom-set-faces (first form)))
                       (eval form)
                       t))))
        (forward-sexp 2)))))


(.EMACS "enabling features")
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'mh-rmail         'disabled t)
(put 'scroll-left      'disabled nil)
(put 'set-goal-column  'disabled t)
(put 'erase-buffer     'disabled nil)

(display-time-mode 1)

(setf tetris-score-file "~/.tetris-scores")

(setf fancy-splash-text
      '(
        (:face (variable-pitch :weight bold) "
WELCOME TO EMACS!
"
         ;; :face variable-pitch "Text"
         ;; :face (variable-pitch :weight bold :slant oblique) "Text"
         ;; :face variable-pitch function
         )
        (:face (variable-pitch :weight bold) "
-%- WELCOME TO EMACS -%-
")))
(setf fancy-splash-text nil)

(case system-type
  ((darwin)
   (setf browse-url-netscape-program "/sw/bin/mozilla"
         browse-url-firefox-program  "/opt/local/bin/firefox"))
  ((gnu/linux)
   (setf browse-url-netscape-program "/usr/local/apps/netscape-7.02/netscape"
         browse-url-firefox-program  "/usr/bin/firefox")))



;;;----------------------------------------------------------------------------
;; (spam-initialize)
;; (gnus-registry-initialize)
;; (setq spam-split-group "mail.spamgate"
;;       spam-use-spamassassin-headers t
;;       spam-use-bogofilter t
;; 
;;       gnus-spam-process-newsgroups
;;       '(("mail\\.*" ((spam spam-use-bogofilter))))
;; 
;;       gnus-spam-newsgroup-contents
;;       '(("mail.spamgate" gnus-group-spam-classification-spam))
;; 
;;       spam-mark-only-unseen-as-spam t
;;       spam-mark-ham-unread-before-move-spam-from-group t
;;       gnus-ham-process-destinations '(("mail\\.spamgate"
;; "mail.inbox"))
;;       gnus-spam-process-destinations '(("mail\\..*"
;; "mail.spam.expired"))
;;       spam-log-to-registry t
;;       gnus-registry-max-entries 4000)


;; (setf spam-use-stat t
;;       spam-use-spamoracle nil
;;       spam-split-group "mail.junk"
;;       spam-log-to-registry nil
;;       gnus-registry-max-entries 4000
;;       gnus-spam-process-newsgroups '(("mail\\.*" ((spam spam-use-stat)))))
;; (spam-initialize)


;;;----------------------------------------------------------------------------
;;; emacs lisp functions
;;;----------------------------------------------------------------------------

(defun symbol-value-in-buffer (symbol buffer)
  (save-excursion
    (set-buffer buffer)
    (when (boundp symbol)
      (symbol-value symbol))))

(defun set-symbol-value-in-buffer (symbol buffer value)
  (save-excursion
    (set-buffer buffer)
    (make-local-variable symbol)
    (setf (symbol-value symbol) value)))

(defsetf symbol-value-in-buffer set-symbol-value-in-buffer)

;;;----------------------------------------------------------------------------
;;; cl-like functions
;;;----------------------------------------------------------------------------
;;; TODO: we should just load pjb-cl!

(define-modify-macro appendf (&rest args) append "Append onto list")

(defun delete-from-sequence (sequence-place item &rest keywords)
  (apply (function delete*) item sequence-place keywords))
(define-modify-macro deletef (&rest args) delete-from-sequence "Delete from sequence")


(defmacro defconstant (symbol initvalue &optional docstring)
  `(defconst ,symbol ,initvalue ,docstring))

(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))





(defun symbol-name* (sym)
  (let* ((name (symbol-name sym))
         (colon (position (character ":") name)))
    (cond 
      ((and colon (char= (character ":") (char name (1+ colon))))
       (subseq name (+ 2 colon)))
      (colon
       (subseq name (+ 1 colon)))
      (t name))))

(defun string* (x)
  "Common-Lisp: If X is a string, then X, else if it's a symbol, then (symbol-name* X)
X---a string, a symbol, or a character.

Returns a string described by x; specifically:

    * If X is a string, it is returned.
    * If X is a symbol, its name is returned.
    * If X is a character, then a string containing that one character is returned.
    * string might perform additional, implementation-defined conversions.
"
  (cond
    ((stringp x) x)
    ((symbolp x) (symbol-name* x))
    ((characterp x) (make-string* 1 :initial-element x))
    (t (signal 'type-error "Expected a string, a symbol or a character."))))
(defun string-downcase (x) (downcase (string* x)))
(defun string-upcase   (x) (upcase   (string* x)))

(unless (fboundp 'string<=)
  (defun string>  (a b) (string< b a))
  (defun string<= (a b) (not (string> a b)))
  (defun string>= (a b) (not (string< a b)))
  (defun string/= (a b) (not (string= a b))))

(defun character (x) (etypecase x
                       (integer x)
                       (string (aref x 0))
                       (symbol (aref (symbol-name x) 0))))
(defun char= (x y) (= x y))
(defun* string-equal* (str1 str2 &key (start1 0) (end1) (start2 0) (end2))
  (string= (string-upcase
            (if (and (= 0 start1) (or (null end1) (= end1 (length str1))))
                str1
                (subseq str1 start1 (or end1 (length str1)))))
           (string-upcase
            (if (and (= 0 start2) (or (null end2) (= end2 (length str2))))
                str2
                (subseq str2 start2 (or end2 (length str2)))))))


(defun string-right-trim (character-bag string-designator)
  "Common-Lisp: returns a substring of string, with all characters in \
`character-bag' stripped off the end.

"
  (unless (sequencep character-bag)
    (signal 'type-error  "Expected a sequence for `character-bag'."))
  (let* ((string (string* string-designator))
         (margin (format "[%s]*" (regexp-quote
                                  (if (stringp character-bag)
                                      character-bag
                                      (map 'string 'identity character-bag)))))
         (trimer (format "\\`\\(\\(.\\|\n\\)*?\\)%s\\'" margin)))
    (replace-regexp-in-string  trimer "\\1" string)))


(defun user-homedir-pathname ()
  (if user-init-file
      (dirname user-init-file)
      (dirname (first (file-expand-wildcards "~/.emacs")))))
(defun namestring (path) path)
(defun pathname-name (path)
  (let ((path (basename path)))
    (if (string-match "^\\(.*\\)\\.[^.]*$" path)
        (match-string 1 path)
        path)))
;; (mapcar (lambda (x) (list (dirname x) (basename x) (pathname-name x)))
;;         '("abc" "abc.def" "abc.def.ghi"
;;           "/abc" "/abc.def" "/abc.def.ghi"
;;           "./abc" "./abc.def" "./abc.def.ghi"
;;           "ddd/abc" "ddd/abc.def" "ddd/abc.def.ghi"
;;           "eee/ddd/abc" "eee/ddd/abc.def" "eee/ddd/abc.def.ghi"
;;           "/eee/ddd/abc" "/eee/ddd/abc.def" "/eee/ddd/abc.def.ghi"))



;;; Some other utility.

(defun ensure-list (x) (if (listp x) x (list x)))

(defmacro* string-case (string-expression &body clauses)
  "Like case, but for strings, compared with string-equal*"
  (let ((value (gensym)))
    `(let ((,value ,string-expression))
       (cond
         ,@(mapcar (lambda (clause)
                     (destructuring-bind (constants &rest body) clause
                       (if (member* constants '(t otherwise) :test (function eql))
                           `(t ,@body)
                           `((member* ,value ',(ensure-list constants)
                                      :test (function string-equal*))
                             ,@body))))
                   clauses)))))


(defun dirname (path)
  (if (string-match "^\\(.*/\\)\\([^/]*\\)$" path)
      (match-string 1 path)
      "./"))
(defun basename (path &optional extension)
  (let ((extension (or extension "")))
    (if (string-match (format "^\\(.*/\\)\\([^/]*\\)%s$" (regexp-quote extension)) path)
        (match-string 2 path)
        path)))




;;;----------------------------------------------------------------------------
;;; File access rights
;;;----------------------------------------------------------------------------

(defun* cl:digit-char-p (char &optional (radix 10))
  (let ((value (position (upcase char) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (and value (< value radix) value)))

(defun* cl:parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  (let ((end    (or end (length string)))
        (n      0)
        (sign   1)
        (plus   (character "+"))
        (minus  (character "-"))
        (space  (character " ")))
    (labels ((parse-integer-error ()
               (error "Not an integer string %S (:start %d :end %d :radix %d)"
                      string start end radix))
             (check-range ()
               (unless (< start end)
                 (parse-integer-error)))
             (eat-spaces (i)
               (loop
                 while (and (< i end) (char= space (aref string i)))
                 do (incf i)
                 finally (return i))))
      (setf start (eat-spaces start))
      (check-range)
      (cond
        ((char= plus  (aref string start)) (setf sign +1) (incf start) (check-range))
        ((char= minus (aref string start)) (setf sign -1) (incf start) (check-range)))
      (loop
        for i from start below end
        for digit = (cl:digit-char-p (aref string i) radix)
        while digit
        do (setf n (+ (* n radix) digit))
        finally (when (< i end)
                  (setf i (eat-spaces i)))
                (when (and (not junk-allowed) (< i end))
                  (parse-integer-error))
                (return (values (* sign n) i))))))


(defun octal (n)
  "N is a decimal numbers whose digits are taken as octal digits
and converted as such."
  (let ((digits (format "%d" n))
        (r 0))
    (dotimes (i (length digits))
      (setf r (+ (* 8 r) (cl:digit-char-p (aref digits i)))))
    r))


(defun chmod (file mode)
  (interactive "fFile path: \nXMode: ")
  (set-file-modes file mode))

(set-default-file-modes (octal 755))


;;;----------------------------------------------------------------------------
;;; File and directory stuff
;;;----------------------------------------------------------------------------

(defun first-existing-file (list-of-files)
  "Finds the first file in LIST-OF-FILES that exists.
"
  (find-if (lambda (file) (and file (file-exists-p file))) list-of-files))

(defun map-existing-files (function list-of-files)
  "Calls FUNCTION on each file in LIST-OF-FILES that exists, and returns the list of results.
"
  (let ((result '()))
    (dolist (file list-of-files (nreverse result))
      (when (file-exists-p file)
        (push (funcall function file) result)))))


(defun remove-non-existing-files (list-of-files)
  "Returns the LIST-OF-FILES with non-existing files removed.
"
  (remove-if-not (function file-exists-p) list-of-files))


(defmacro* with-file (file-and-options &body body)
  "Processes BODY with a buffer on the given file.
DO:              find-file or find-file-literally, process body, and
                 optionally save the buffer and kill it.
                 save is not done if body exits exceptionnaly.
                 kill is always done as specified.
FILE-AND-OPTION: either an atom evaluated to a path,
                 or (path &key (save t) (kill t) (literal nil))
"
  (if (atom file-and-options)
      `(with-file (,file-and-options) ,@body)
      ;; destructuring-bind is broken, we cannot give anything else than nil
      ;; as default values:
      (destructuring-bind (path &key (save nil savep) (kill nil killp)
                                (literal nil literalp))
          file-and-options
        (unless savep (setf save t))
        (unless killp (setf kill t))
        `(unwind-protect
              (progn
                (,(if literal 'find-file-literally 'find-file) ,path)
                (prog1 (save-excursion ,@body)
                  ,(when save `(save-buffer 1))))
           ,(when kill
                  `(kill-buffer (current-buffer)))))))


(defvar *directories* '() "A cache for the ~/directories.txt dictionary.")
;; (setf  *directories* '())


(defun load-directories (&optional directories-file)
  "Loads ~/directories.txt (or the given DIRECTORIES-FILE),
and stores it in `*directories*'.
"
  (let ((directories-file (or directories-file "~/directories.txt")))
    (setf *directories*
          (progn
            (find-file directories-file)
            (prog1
                (loop
                   for (k v)
                   on (split-string (buffer-substring-no-properties
                                     (point-min) (point-max)))
                   by (function cddr)
                   nconc (list (intern (format ":%s" (substitute ?- ?_ (downcase k))))
                               v))
              (kill-buffer (current-buffer)))))))


(defun get-directory (key &optional subpath)
  "
RETURN: The directory in ~/directories.txt for the key, concatenated with the subpath.
NOTE:   ~/directories.txt is cached in *directories*.
"
  (unless *directories*
    (load-directories))
  (when  (getf *directories* key)
    (let ((dir (getf *directories* key)))
      (if (or (null subpath) (string= "" subpath))
          dir
          (flet ((lastchar (str) (and (< 0 (length str)) (aref str (1- (length str)))))
                 (firstchar (str) (and (< 0 (length str)) (aref str 0)))
                 (xor (a b) (or (and a (not b)) (and (not a) b))))
            (if (xor (eql ?/ (lastchar dir)) (eql ?/ (firstchar subpath)))
                (concat dir subpath)
                (concat dir "/" subpath)))))))


;;;----------------------------------------------------------------------------
;;; Setting up load-path
;;;----------------------------------------------------------------------------
;;
;;
;; When we start, emacs has already filled load-path with
;; installation-local directories.
;;
;; So we only need to add the directories of specific packages (that
;; could be used in the various emacs installations).  It also means
;; that installing an emacs package must occur either in an emacs
;; specific installation (notably if .elc are compiled for this
;; specific version), or in  the package specific directory.
;;
;; If any of these directories contain one of the site or subdir el
;; files, then it is loaded too.
;;



(defun print-load-path ()
  "Insert the paths in load-path one per line."
  (interactive)
  (dolist (x load-path)
    (princ x)
    (terpri)))
(defalias 'dump-load-path 'print-load-path)


(defun clean-load-path ()
  "Remove slashes at the end of the path in load-path."
  (setf load-path
        (remove-duplicates
         (mapcar (lambda (path) (string-right-trim "/" path)) load-path)
         :test (function string=))))


(defun load-pathname (file &optional nosuffix must-suffix)
  "Return the pathname of the file that would be loaded by (load file)."
  (let* ((file (substitute-in-file-name file))
         (size (length file)))
    (unless (zerop size)
      (when (and must-suffix
                 (or (and (< 3 size) (string= ".el" (substring file (- size 3))))
                     (and (< 4 size) (string= ".elc" (substring file (- size 4))))
                     (file-name-directory file)))
        (setf must-suffix nil))
      (if (fboundp 'locate-file-internal)
          (locate-file-internal file
                                load-path
                                (cond (nosuffix    '())
                                      (must-suffix (get-load-suffixes))
                                      (t           (append (get-load-suffixes)
                                                           load-file-rep-suffixes)))
                                nil)
          (error "Missing locate-file-internal in version %s" emacs-version)
          ;; (let ((suffixes (append (get-load-suffixes) "")))
          ;;   (dolist (dir load-path)
          ;;     (dolist (suffix suffixes)
          ;;       (format "%s/%s%s" dir file suffix))))
          ))))



(defun setup-load-path ()
  "Set up my load-path."
  (let ((new-paths '())
        (base-load-path (copy-list load-path)))
    (flet ((add-if-good (site-lisp)
             (let ((site-lisp (expand-file-name site-lisp)))
               (when (file-exists-p site-lisp)
                 (pushnew site-lisp new-paths)
                 (mapc (lambda (file)
                         (let ((file (concat site-lisp "/" file)))
                           (when (file-exists-p file)
                             (let ((default-directory site-lisp))
                               (.EMACS "%s FOUND" file)
                               (load file)))))
                       '("site-start.el" "site-gentoo.el" "subdirs.el"))
                 t))))
      (dolist (directories (append
                            ;; When several directories are listed in a sublist, only
                            ;; the first found directory will be added.
                            (case emacs-major-version
                              ((20 21 22)
                               (append '("/opt/lisp/emacs"
                                         "/opt/local/share/emacs/site-lisp"
                                         "/usr/local/share/emacs/site-lisp")
                                       '("/opt/clisp-2.48/share/emacs/site-lisp"
                                         "/opt/clisp-2.48-newclx/share/emacs/site-lisp"
                                         "/opt/clisp-2.48-mitclx/share/emacs/site-lisp"
                                         "/opt/clisp-2.47/share/emacs/site-lisp"
                                         "/opt/clisp-2.46/share/emacs/site-lisp"
                                         "/opt/clisp-2.41-pjb1-regexp/share/emacs/site-lisp")
                                       '("/opt/smalltalk-3.0.4/share/emacs/site-lisp")
                                       ))
                              ((23)
                               '("/usr/local/share/emacs/site-lisp"))
                              ((24)
                               '("/opt/share/emacs/site-lisp/w3m/"))
                              ((25)
                               '())
                              (otherwise
                               (.EMACS "WARNING: No load-paths for emacs version %d"
                                       emacs-major-version)
                               '()))
                            (list
                             ;; -----------------
                             ;; PJB emacs sources
                             ;; -----------------
                             ;; Since we may have several emacs version running
                             ;; on the same system, for now we will avoid
                             ;; compiling pjb sources, and we will load them
                             ;; directly from ~/src/public/emacs/.  Later we
                             ;; will see how we can install elc in version
                             ;; specific directories, but keeping references to
                             ;; the same source directory.
                             ;; (get-directory :share-lisp "packages/com/informatimago/emacs")
                             '("~/src/public/emacs")
                             '("~/emacs"))))
        (if (listp directories)
            (find-if (function add-if-good) directories)
            (add-if-good directories)))
      
      (setf load-path (append new-paths
                              (set-difference load-path base-load-path :test (function equal))
                              base-load-path)))))

(message "old load-path = %S" (with-output-to-string (dump-load-path)))
(setup-load-path)
(message "new load-path = %S" (with-output-to-string (dump-load-path)))


(map-existing-files (lambda (dir) (pushnew dir exec-path))
                    (cons (expand-file-name "~/bin/")
                          '("/sw/sbin/" "/sw/bin/" "/usr/local/sbin" "/usr/local/bin" "/opt/local/sbin" "/opt/local/bin")))

(setf (getenv "PATH") (mapconcat (function identity) exec-path ":"))

(require 'highlight-flet nil t)

;;;----------------------------------------------------------------------------
;;; PAREDIT: essential!
;;;----------------------------------------------------------------------------

(load "paredit")

(defun pjb-paredit-space-for-delimiter-p/sharp-plus-minus (endp delimiter)
  (not (and (not endp)
            (let ((two-before  (- (point) 2)))
              (and (<= (point-min) two-before)
                   (let ((previous (buffer-substring two-before (point))))
                     (and (or (string= previous "#+")  (string= previous "#-"))
                          (or (= (point-min) two-before)
                              (not (memq (char-syntax (aref (buffer-substring two-before (1+ two-before)) 0))
                                         (list ?w ?_)))))))))))

(push 'pjb-paredit-space-for-delimiter-p/sharp-plus-minus paredit-space-for-delimiter-predicates)

;;;----------------------------------------------------------------------------
;;; CEDET / EIEIO
;;;----------------------------------------------------------------------------
(when (and (require 'eieio nil t)
           (require 'cedet nil t))
  ;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
  ;; Select one of the following:

  ;; * This enables the database and idle reparse engines
  ;;(semantic-load-enable-minimum-features)

  ;; * This enables some tools useful for coding, such as summary mode
  ;;   imenu support, and the semantic navigator
  ;; (semantic-load-enable-code-helpers)

  ;; * This enables even more coding tools such as the nascent intellisense mode
  ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
  ;; (semantic-load-enable-guady-code-helpers)

  ;; * This turns on which-func support (Plus all other code helpers)
  ;; (semantic-load-enable-excessive-code-helpers)

  ;; This turns on modes that aid in grammar writing and semantic tool
  ;; development.  It does not enable any other features such as code
  ;; helpers above.
  ;; (semantic-load-enable-semantic-debugging-helpers)
  (setf eieio-skip-typecheck t))


;;;----------------------------------------------------------------------------
;;; Emacs-CL
;;;----------------------------------------------------------------------------
(defvar *pjb-emacs-cl-present-p* nil)


;; We cannot load old emacs-cl in emacs-24 with lexical-binding, since
;; it overrides new emacs functions.

;; (when (load "load-cl" t)
;;   (setf *pjb-emacs-cl-present-p* t)
;;   (message "emacs-cl streams = %S" (list  *STANDARD-INPUT*  
;;                                           *STANDARD-OUTPUT* 
;;                                           *TERMINAL-IO*))
;;   (let ((stream (make-buffer-output-stream "*scratch*")))
;;     (setf *STANDARD-INPUT*  stream
;;           *STANDARD-OUTPUT* stream
;;           *TERMINAL-IO*     stream)))


;;;----------------------------------------------------------------------------
(defparameter *milliways* '())


(defun milliways-run ()
  (interactive)
  (while *milliways*
    (ignore-errors (funcall (pop *milliways*)))))

(defun milliways-activate (&optional delay)
  "Called at the end of ~/.emacs"
  (run-at-time (or delay 5)
               1
               'milliways-run))

(defun milliways-schedule (function)
  "Schedule the function to be called after emacs started."
  (push function *milliways*))

(milliways-schedule
 (lambda ()
   (speak   "Welcome to the Restaurant at the End of the Universe!")
   (message "Welcome to the Restaurant at the End of the Universe!")))



(defun emacs-time-to-universal-time (emacs-time)
  (+ (* (first emacs-time) 65536.0)
     (second emacs-time)
     (/ (third emacs-time) 1000000.0)))

(defun timer-emacs-time (timer)
  (list (timer--high-seconds timer)
        (timer--low-seconds timer)
        (timer--usecs timer)))


(defun timer-delete-function (function)
  (cancel-timer (find function (append timer-list timer-idle-list)
                      :key (function timer--function))))

;; (timer-delete-function 'milliways-run)
;; (milliways-activate)


;; (mapcar (lambda (timer)
;;           (list
;;            (timer--function timer)
;;            (- (emacs-time-to-universal-time (timer-emacs-time timer))
;;               (emacs-time-to-universal-time (current-time)))))
;;         timer-list)






;;;----------------------------------------------------------------------------
;;; Language & character encoding stuff:
;;;----------------------------------------------------------------------------

;; (standard-display-european 1) ;;is semi-obsolete, but it works better than:
;; (standard-display-8bit 128 255)

(set-input-mode nil nil t nil) ;; INTERRUPT FLOW META [QUIT]
;; (setq meta-prefix-char nil) ; to split ESC from M- 

(when (fboundp 'unify-8859-on-encoding-mode)
  (unify-8859-on-encoding-mode 1))
(when (fboundp 'unify-8859-on-decoding-mode)
  (unify-8859-on-decoding-mode 1))

(setq default-enable-multibyte-characters      t
      unibyte-display-via-language-environment nil)

;; (setq my-latin (if (assoc-ignore-case "Latin-9" language-info-alist) 9 1))
;; ;; For coding-system we don't specify *-unix to allow it to load DOS files.
;; (cond
;;   ((= my-latin 1) (setq my-lenv     "Latin-1"
;;                         my-encoding 'iso-8859-1
;;                         x-encoding  "iso8859-1"))
;;   ((= my-latin 9) (setq my-lenv     "Latin-9"
;;                         my-encoding 'iso-8859-15
;;                         x-encoding  "iso8859-15"))
;;   (t (error "Invalid value for my-latin variable.")))

(progn
  (case system-type
    (darwin 

     (set-language-environment                "utf-8")
     (prefer-coding-system                    'utf-8-unix)

     ;; (set-buffer-file-coding-system           'utf-8-unix)
     ;; (set-buffer-process-coding-system        'utf-8-unix)
     (set-clipboard-coding-system             'utf-8-unix)
     (set-file-name-coding-system             'utf-8-unix)
     (set-keyboard-coding-system              'utf-8-unix)
     (set-next-selection-coding-system        'utf-8-unix)
     (set-selection-coding-system             'utf-8-unix)
     (set-terminal-coding-system              'utf-8-unix)


     (setq default-buffer-file-coding-system  'utf-8-unix
           default-file-name-coding-system    'utf-8-unix
           default-terminal-coding-system     'utf-8-unix
           default-keyboard-coding-system     'utf-8-unix
           default-sendmail-coding-system     'utf-8-unix
           default-process-coding-system      '(utf-8-unix . utf-8-unix))
     (modify-coding-system-alist 'process ".*shell\\'"     'utf-8-unix)
     (modify-coding-system-alist 'process "\\*shell\\*\\'" 'utf-8-unix)
     (modify-coding-system-alist 'process ".*lisp\\'"      'utf-8-unix))
    (otherwise
     (set-language-environment                "utf-8")
     (prefer-coding-system                    'utf-8-unix)
     (set-default-coding-systems              'utf-8-unix)
     (set-keyboard-coding-system              'utf-8-unix) ; 'iso-8859-1-unix)
     (set-terminal-coding-system              'utf-8-unix) ; 'iso-8859-1-unix)
     (set-clipboard-coding-system             'utf-8-unix)
     (set-selection-coding-system             'utf-8-unix)
     (setq default-buffer-file-coding-system  'utf-8-unix
           default-file-name-coding-system    'utf-8-unix
           default-terminal-coding-system     'utf-8-unix ; 'iso-8859-1-unix
           default-keyboard-coding-system     'utf-8-unix ; 'iso-8859-1-unix
           default-sendmail-coding-system     'utf-8-unix
           default-process-coding-system      '(utf-8-unix . utf-8-unix))
     (modify-coding-system-alist 'process ".*shell\\'"     'utf-8-unix)
     (modify-coding-system-alist 'process "\\*shell\\*\\'" 'utf-8-unix)
     (modify-coding-system-alist 'process ".*lisp\\'"      'utf-8-unix)))

  (dolist (cs coding-system-list nil)
    (modify-coding-system-alist 'file (format "\\.%s\\'" cs) cs)))

;; and/or set locales like LC_ALL, LC_CTYPE, LANG to contain UTF-8 as for
;; example: LANG=de_DE.UTF-8. Modern Emacsen, I think 21.3 at least,
;; derive their mode of operation from this.

;; You can start your text files à la: ;;; -*- mode: Text; coding: utf-8 -*-
;; Once you've done that you can C-x RET r:
;; revert-buffer-with-coding-system.


;; 	(set-language-environment		'German)
;; 	(setq default-file-name-coding-system	'utf-8)
;; 	(setq file-name-coding-system		'utf-8)
;; 	(setq default-buffer-file-coding-system 'iso-latin-9-unix))
;; 	(set-default-coding-systems		'mac-roman-unix)
;; 	;(setq mac-keyboard-text-encoding	 kTextEncodingISOLatin1)
;; 	(set-keyboard-coding-system		'sjis-mac)
;; 	(set-clipboard-coding-system		'sjis-mac)
;; 	(prefer-coding-system			'mac-roman-unix)
;; 	(modify-coding-system-alist	 'file "\\.tex\\'" 'iso-latin-9-unix)
;; 	(modify-coding-system-alist	 'process
;; "\\*[Ss][Hh][Ee][Ll][Ll].*\\'"  'utf-8-unix)
;; 	;(set-buffer-process-coding-system	'utf-8 'utf8)



;;;----------------------------------------------------------------------------
;;; On Macintosh
;;;----------------------------------------------------------------------------

(when (or (boundp 'aquamacs-version)
          (eq window-system 'ns))
  (setf initial-frame-alist '((background-color . "#ddffee")
                              (left . 76)
                              (top . 20)
                              (width . 80)
                              (height . 60))
        default-frame-alist (append initial-frame-alist default-frame-alist)
        cursor-type 'box)
  (when (fboundp 'smart-frame-positioning-mode)
    (smart-frame-positioning-mode nil))
  (when (load "scroll-bar" nil t)
    (defun scroll-bar-columns (side)
      "Return the width, measured in columns, of the vertical scrollbar on SIDE.
SIDE must be the symbol `left' or `right'."
      (let* ((wsb   (window-scroll-bars))
             (vtype (nth 2 wsb))
             (cols  (nth 1 wsb)))
        (cond
          ((not (memq side '(left right nil)))
           (error "`left' or `right' expected instead of %S" side))
          ((and (eq vtype side) cols))
          ((eq (frame-parameter nil 'vertical-scroll-bars) side)
           ;; nil means it's a non-toolkit scroll bar, and its width in
           ;; columns is 14 pixels rounded up.
           (ceiling (or (frame-parameter nil 'scroll-bar-width) 14)
                    (frame-char-width)))
          (0))))))




;;;----------------------------------------------------------------------------
;;; Global key map
;;;----------------------------------------------------------------------------
(.EMACS "Setting global key map")


;; feature simple is not provided on emacs < 22, so we use load-library:
(load-library "simple") (define-key ctl-x-map              "." nil)
(require 'iso-transl)   (define-key iso-transl-ctl-x-8-map "E" [342604])



;; For control, there's no distinction between shift and plain!
;; The only control keys are: @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
;; \C-^ is free.
;; Cannot use \C-i because it's TAB
;; Cannot use \C-m because it's CR
;; Cannot use \C-[ because it's ESC!
;; Cannot use \C-{ or \C-}  (no distinction between shift and plain)
;;
;; [?\C-c f5]
;; [(control c) f5]
;; (kbd "C-c <f5>")
;;
;; C-z is used now by elscreen.



(setf visible-bell t)
(when (eq window-system 'x)
  (setf ring-bell-function 
        (lambda ()
          (call-process-shell-command "xset led;sleep 0.1;xset -led;sleep 0.05;xset led;sleep 0.1;xset -led;sleep 0.05;xset led;sleep 0.2;xset -led" nil 0 nil))))

(defun disabled ()
  (interactive)
  (beep))

(defun insert-sharp-brace ()
  (interactive)
  (insert "#[]")
  (forward-char -1))

(defun reset-movement-keypad ()
  "Locally set the keys <insert>, <suppr>, <home>, <end>, <prior> and <next>."
  (interactive)
  (local-set-key (kbd "<home>")        'beginning-of-buffer)
  (local-set-key (kbd "<end>")         'end-of-buffer)
  (local-set-key (kbd "<prior>")       'scroll-down)
  (local-set-key (kbd "<next>")        'scroll-up)
  (global-set-key (kbd "<home>")        'beginning-of-buffer)
  (global-set-key (kbd "<end>")         'end-of-buffer)
  (global-set-key (kbd "<prior>")       'scroll-down)
  (global-set-key (kbd "<next>")        'scroll-up))

;; http://paste.lisp.org/display/10157

(defun swap-brackets-parens ()
  (interactive)
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\] ?\)))

(defun normal-brackets-parens ()
  (interactive)
  (keyboard-translate ?\( ?\()
  (keyboard-translate ?\) ?\))
  (keyboard-translate ?\[ ?\[)
  (keyboard-translate ?\] ?\]))

(defun translate-powerbook-keyboard ()
  (interactive)
  (keyboard-translate ?\§ ?\`)
  (keyboard-translate ?\± ?\~))


(defmacro define-force-justification (direction)
  `(defun ,(intern (format "force-set-justification-%s" direction)) (start end)
     (interactive "r")
     (let ((mode major-mode))
       (text-mode)
       (,(intern (format "set-justification-%s" direction))  start end)
       (funcall mode))))



(defun pjb-terminal-key-bindings ()
  (interactive)
  ;; http://paste.lisp.org/display/131216
  (global-set-key "OF"    'end-of-buffer)
  (global-set-key "OH"    'beginning-of-buffer)
  (global-unset-key "[")
  (global-set-key "[15~"  'set-justification-left) ; <f5>
  (global-set-key "[17~"  'set-justification-center) ; <f6>
  (global-set-key "[18~"  'set-justification-right) ; <f7>
  (global-set-key "[19~"  'disabled)  ; <f8>
  (global-set-key "[20~"  'disabled)  ; <f9>
  (global-set-key "[21~"  'disabled)  ; <f10>
  (global-set-key "[23~"  'disabled)  ; <f11>
  (global-set-key "[24~"  'disabled)  ; <f12>

  (define-key input-decode-map "\M-[1;5A" [C-up])
  (define-key input-decode-map "\M-[1;5B" [C-down])
  (define-key input-decode-map "\M-[1;5C" [C-right])
  (define-key input-decode-map "\M-[1;5D" [C-left])

  (define-key input-decode-map "\M-[1;3A" [M-up])
  (define-key input-decode-map "\M-[1;3B" [M-down])
  (define-key input-decode-map "\M-[1;3C" [M-right])
  (define-key input-decode-map "\M-[1;3D" [M-left])

  (define-key input-decode-map "\M-[1;9A" [M-up])
  (define-key input-decode-map "\M-[1;9B" [M-down])
  (define-key input-decode-map "\M-[1;9C" [M-right])
  (define-key input-decode-map "\M-[1;9D" [M-left])
  
  nil)


(defun pjb-global-key-bindings ()
  (interactive)

  (define-force-justification left)
  (define-force-justification center)
  (define-force-justification right)
  (define-force-justification full)

  ;; Advance key map setting: get a sane keyboard when loading this file fails.
  (global-set-key (kbd "C-x RET C-h")   'describe-prefix-bindings)
  ;; (global-set-key (kbd "C-x 5 o")     'other-frame-non-excluded)

  (global-set-key (kbd "<home>")        'beginning-of-buffer)
  (global-set-key (kbd "<end>")         'end-of-buffer)
  (global-set-key (kbd "<prior>")       'scroll-down)
  (global-set-key (kbd "<next>")        'scroll-up)

  (global-set-key (kbd "C-c C-s")       'search-forward-regexp)
  (global-set-key (kbd "C-c C-r")       'search-backward-regexp)

  (global-set-key (kbd "<f5>")          'set-justification-left)
  (global-set-key (kbd "<f6>")          'set-justification-full)
  (global-set-key (kbd "<f7>")          'set-justification-right)
  (global-set-key (kbd "C-c <f5>")      'force-set-justification-left)
  (global-set-key (kbd "C-c <f6>")      'force-set-justification-full)
  (global-set-key (kbd "C-c <f7>")      'force-set-justification-right)

  (global-set-key (kbd "M-g g")         'goto-char)
  
  (global-set-key (kbd "<f8>")          'pjb-show-lisp-repl)
  (global-set-key (kbd "C-<f8>")         (lambda () (interactive) (pjb-show-lisp-repl t)))

  (global-set-key (kbd "C-c .")         'forward-sexp)
  (global-set-key (kbd "C-c ,")         'backward-sexp)
  (global-set-key (kbd "C-x DEL")       'disabled)
  (global-set-key (kbd "C-<delete>")    'disabled)
  (global-set-key (kbd "C-<backspace>") 'disabled)
  (global-set-key (kbd "A-x")           'insert-sharp-brace)
  (global-set-key (kbd "M-[")           'insert-parentheses)
  (global-set-key (kbd "M-]")           'move-past-close-and-reindent)
  ;; (global-set-key "\M-["                'insert-parentheses)
  ;; (global-set-key "\M-]"                'move-past-close-and-reindent)

  (global-set-key (kbd "C-<f9>")  (lambda()(interactive)(set-input-method 'chinese-py-b5)))
  (global-set-key (kbd "C-<f10>") (lambda()(interactive)(set-input-method 'cyrillic-yawerty)))
  (global-set-key (kbd "C-<f11>") (lambda()(interactive)(set-input-method 'greek)))
  (global-set-key (kbd "C-<f12>") (lambda()(interactive)(set-input-method 'hebrew)))
  ;; (autoload 'hebr-switch  "hebwork"  "Toggle Hebrew mode.")
  ;; (global-set-key (kbd "C-<f12>") 'hebr-switch)

  (when (and (fboundp 'pjb-search-backward-region)
             (fboundp 'pjb-search-forward-region))
    (global-set-key (kbd "<f9>")   'pjb-search-backward-region)
    (global-set-key (kbd "<f10>")  'pjb-search-forward-region))

  (when (and (fboundp 's2p-calculette)
             (fboundp 's2p-calculette-to-lisp))
    (global-set-key (kbd "C-c =")  's2p-calculette)
    (global-set-key (kbd "C-c +")  's2p-calculette-to-lisp))

  (global-set-key (kbd "C-c _")    'google-search-region)
  (when (fboundp 'invoke-ding-dictionary)
    (global-set-key (kbd "C-c -")   'invoke-ding-dictionary))

  ;; (delete-selection-mode t)
  (if (fboundp 'delete-region-and-yank)
      (global-set-key (kbd "C-y")  'delete-region-and-yank) 
      (global-set-key (kbd "C-y")  'yank))

  ;; A strange configuration with a narrow frame...
  (when (< (frame-parameter (selected-frame) 'width) 42)
    (global-set-key (kbd "C-h") 'backward-delete-char-untabify))

  (case window-system
    ((nil)
     (.EMACS "Setting terminal keyboard")
     (pjb-terminal-key-bindings)
     (set-keyboard-coding-system 'iso-8859-15)
     (normal-erase-is-backspace-mode 0)
     (.EMACS "C-h = %S" (key-binding "\C-h"))
     (.EMACS "DEL = %S" (key-binding "\C-?")))
    ((x)
     (.EMACS "Setting X keyboard")
     (define-key global-map [(delete)]    "\C-d")
     (make-face-bold 'bold-italic))
    ((mac ns)
     (.EMACS "Setting Macintosh keyboard")
     (setq *window-manager-y-offset* (+ 24 24))
     (set-keyboard-coding-system 'mac-roman)
     (translate-powerbook-keyboard)))
  nil)


(pjb-global-key-bindings)


;;;----------------------------------------------------------------------------
(.EMACS "PJB FUNCTION KEYS")

(defun pjb-undefined-function-key ()
  "Default command for undefined dynamic function keys."
  (interactive)
  (message (format "Undefined key %S" (this-command-keys))))

(defvar *pjb-function-key-commands*
  '()
  "An a-list mapping dynamic function keys to either a command (symbol) or a register (integer).")

(defun pjb-function-key-command ()
  "The command bound to dynamic function keys: it redirects to the command or register mapped to in `*pjb-function-key-command*'."
  (interactive)
  (let* ((key   (this-command-keys))
         (entry (assoc (aref key 0) *pjb-function-key-commands*)))
    (if entry
        (cond
          ((integerp (cdr entry)) (jump-to-register (cdr entry)))
          ((symbolp (cdr entry))  (call-interactively (cdr entry)))
          (t (error "Unexpected binding for pjb-function-key: %S" entry)))
        (pjb-undefined-function-key))))

(defun pjb-define-function-key (ask-for-a-command)
  "Defines a dynamic function key as a command if a prefix is given, or as a register."
  (interactive "P")
  (let* ((c-key  (this-command-keys))
         (name   (symbol-name (aref c-key (if ask-for-a-command 1 0))))
         (key    (intern (subseq name 2)))
         (number (cl:parse-integer name :start (- (length name) 2)))
         (entry  (assoc key *pjb-function-key-commands*)))
    (unless entry
      (push (setf entry (cons key nil)) *pjb-function-key-commands*))
    (setf (cdr entry) (if ask-for-a-command
                          (read-command (format "Command to bind to %S:" key))
                          (progn
                            (point-to-register number)
                            number)))
    (global-set-key (vector key) 'pjb-function-key-command)
    (message (format "Defined key %S as %s"
                     (vector key)
                     (if (symbolp (cdr entry))
                         (cdr entry)
                         (format "register %c" (cdr entry)))))))

(defun pjb-function-keys ()
  "Binds function keys to allow for their dynamic binding to a jump register or a command.
Function keys from f13 to f35 and M-f13 to M-f35 can be bound by
typing C-f13 to C-f35 and C-M-f13 to C-M-f35.
"
  (interactive)
  (loop
     ;; There are function keys from 1 to 35.  f1 to f12 are on the
     ;; toprow of the pc keyboards and used and bound specifically.
     ;; f13 to f35 are mapped to the numeric keypad, and free.
     for i from 13 to 35
     for f   = (format "<f%d>" i)
     for cf  = (format "C-<f%d>" i)
     for mf  = (format "M-<f%d>" i)
     for cmf = (format "C-M-<f%d>" i)
     do
       (global-set-key (read-kbd-macro f)   'pjb-undefined-function-key)
       (global-set-key (read-kbd-macro mf)  'pjb-undefined-function-key)
       (global-set-key (read-kbd-macro cf)  'pjb-define-function-key)
       (global-set-key (read-kbd-macro cmf) 'pjb-define-function-key)))

(pjb-function-keys)

;;------------------------------------------------------------------------


(defvar scroll-page-delimiter "")
(make-local-variable 'scroll-page-delimiter)
(setf scroll-page-delimiter "Software Design Notes")

(defun scroll-page-up ()
  (interactive)
  (if (re-search-forward scroll-page-delimiter nil t)
      (progn
        (goto-char (match-beginning 0))
        (recenter 0)
        (forward-line 1))
      (message ".EMACS: Last page")))

(defun scroll-page-down ()
  (interactive)
  (if (re-search-backward scroll-page-delimiter nil t 2)
      (progn
        (goto-char (match-beginning 0))
        (recenter 0)
        (forward-line 1))
      (message ".EMACS: First page")))

(defvar scroll-page-mode nil)
(make-local-variable 'scroll-page-mode)

(defun scroll-page-mode ()
  (interactive)
  (if scroll-page-mode
      (progn
        (local-set-key (kbd "<next>")  'scroll-up)
        (local-set-key (kbd "<prior>") 'scroll-down)
        (setf scroll-page-mode nil))
      (progn
        (local-set-key (kbd "<next>")  'scroll-page-up)
        (local-set-key (kbd "<prior>") 'scroll-page-down)
        (setf scroll-page-mode t))))




(defun indentation ()
  "returns the indentation of the line at point."
  (back-to-indentation)
  (let ((indentation (current-column)))
    (if (= indentation (save-excursion (end-of-line) (current-column)))
        0
        indentation)))

(defun forward-same-indent ()
  (interactive)
  (let ((current (point))
        (indentation (indentation)))
    (while (and (< (point) (point-max))
                (progn
                  (forward-line)
                  (/= indentation (indentation)))))
    (unless (= indentation (indentation))
      (goto-char current))))

(defun backward-same-indent ()
  (interactive)
  (let ((current (point))
        (indentation (indentation)))
    (while (and (< (point-min) (point))
                (progn
                  (forward-line -1)
                  (/= indentation (indentation)))))
    (unless (= indentation (indentation))
      (goto-char current))))


(standard-display-ascii #o200 (vector (decode-char 'ucs #x253c)))
(standard-display-ascii #o201 (vector (decode-char 'ucs #x251c)))
(standard-display-ascii #o202 (vector (decode-char 'ucs #x252c)))
(standard-display-ascii #o203 (vector (decode-char 'ucs #x250c)))
(standard-display-ascii #o204 (vector (decode-char 'ucs #x2524)))
(standard-display-ascii #o205 (vector (decode-char 'ucs #x2502)))
(standard-display-ascii #o206 (vector (decode-char 'ucs #x2510)))
(standard-display-ascii #o210 (vector (decode-char 'ucs #x2534)))
(standard-display-ascii #o211 (vector (decode-char 'ucs #x2514)))
(standard-display-ascii #o212 (vector (decode-char 'ucs #x2500)))
(standard-display-ascii #o214 (vector (decode-char 'ucs #x2518)))
(standard-display-ascii #o220 [? ])
(standard-display-ascii #o221 [?\` ])
(standard-display-ascii #o222 [?\'])
(standard-display-ascii #o223 [?\"])
(standard-display-ascii #o224 [?\"])
(standard-display-ascii #o225 "* ")
(standard-display-ascii #o226 "--")
(standard-display-ascii #o227 " -- ")


;; some more global key map are defined after loading my personal files below.


(global-set-key (kbd "H-<up>")    'backward-same-indent)
(global-set-key (kbd "H-<down>")  'forward-same-indent)
(global-set-key (kbd "H-`")       'next-error)

;;;----------------------------------------------------------------------------
(.EMACS "Loading my personal files -- My own stuff.")
(unless (load "pjb-loader.el" t)
  (.EMACS "WARNING WARNING WARNING: Could not find and load 'My own stuff'!"))
;;;----------------------------------------------------------------------------

(when (and (boundp 'elscreen-display-tab) elscreen-display-tab)
  (elscreen-toggle-display-tab))

;;------------------------------
(.EMACS "Miscellaneous patches")

(when (< emacs-major-version 22)
  (unless (fboundp 'called-interactively-p)
    (defun called-interactively-p () (interactive-p))))


(when (string= emacs-version "24.3.1")
  (require 'minibuffer)
  
  (defun completion--twq-all (string ustring completions boundary
                              unquote requote)
    (when completions
      (pcase-let*
          ((prefix
            (let ((completion-regexp-list nil))
              (try-completion "" (cons (substring ustring boundary)
                                       completions))))
           (`(,qfullpos . ,qfun)
             (funcall requote (+ boundary (length prefix)) string))
           (qfullprefix (substring string 0 qfullpos))
           ;; FIXME: This assertion can be wrong, e.g. in Cygwin, where
           ;; (unquote "c:\bin") => "/usr/bin" but (unquote "c:\") => "/".
           ;;(cl-assert (completion--string-equal-p
           ;;            (funcall unquote qfullprefix)
           ;;            (concat (substring ustring 0 boundary) prefix))
           ;;           t))
           (qboundary (car (funcall requote boundary string)))
           ;; (_ (cl-assert (<= qboundary qfullpos)))
           ;; FIXME: this split/quote/concat business messes up the carefully
           ;; placed completions-common-part and completions-first-difference
           ;; faces.  We could try within the mapcar loop to search for the
           ;; boundaries of those faces, pass them to `requote' to find their
           ;; equivalent positions in the quoted output and re-add the faces:
           ;; this might actually lead to correct results but would be
           ;; pretty expensive.
           ;; The better solution is to not quote the *Completions* display,
           ;; which nicely circumvents the problem.  The solution I used here
           ;; instead is to hope that `qfun' preserves the text-properties and
           ;; presume that the `first-difference' is not within the `prefix';
           ;; this presumption is not always true, but at least in practice it is
           ;; true in most cases.
           (qprefix (propertize (substring qfullprefix qboundary)
                                'face 'completions-common-part)))

        ;; Here we choose to quote all elements returned, but a better option
        ;; would be to return unquoted elements together with a function to
        ;; requote them, so that *Completions* can show nicer unquoted values
        ;; which only get quoted when needed by choose-completion.
        (nconc
         (mapcar (lambda (completion)
                   ;; (cl-assert (string-prefix-p prefix completion 'ignore-case) t)
                   (let* ((new (substring completion (length prefix)))
                          (qnew (funcall qfun new))
                          (qcompletion (concat qprefix qnew)))
                     ;; FIXME: Similarly here, Cygwin's mapping trips this
                     ;; assertion.
                     ;;(cl-assert
                     ;; (completion--string-equal-p
                     ;;  (funcall unquote
                     ;;           (concat (substring string 0 qboundary)
                     ;;                   qcompletion))
                     ;;  (concat (substring ustring 0 boundary)
                     ;;          completion))
                     ;; t)
                     qcompletion))
                 completions)
         qboundary)))))


;;;----------------------------------------------------------------------------
(.EMACS "debug")
(require 'debug)
(define-key debugger-mode-map "\C-r" 'debugger-record-expression)
(define-key debugger-mode-map "\C-m" 'help-follow)
(define-key debugger-mode-map "B" 'debugger-frame)
(define-key debugger-mode-map "C" 'debugger-continue)
(define-key debugger-mode-map "J" 'debugger-jump)
(define-key debugger-mode-map "R" 'debugger-return-value)
(define-key debugger-mode-map "U" 'debugger-frame-clear)
(define-key debugger-mode-map "D" 'debugger-step-through)
(define-key debugger-mode-map "L" 'debugger-list-functions)
(define-key debugger-mode-map "H" 'describe-mode)
(define-key debugger-mode-map "Q" 'top-level)
(define-key debugger-mode-map "E" 'debugger-eval-expression)
(define-key debugger-mode-map "\C-R" 'debugger-record-expression)
(define-key debugger-mode-map "\C-M" 'help-follow)


;;;----------------------------------------------------------------------------
(.EMACS "shell")
(add-hook 'ielm-mode-hook (lambda () (setf standard-output (current-buffer))))

;;;----------------------------------------------------------------------------
;; (.EMACS "eshell")
;; (unless (featurep 'eshell-auto)
;;   (load "eshell-auto" *pjb-load-noerror* *pjb-load-silent*))
;; (defun pjb-eshell-load-meat ()
;;   (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
;;   ;; (defun string (&rest chars)
;;   ;;   (do ((s (make-string (length chars) 0))
;;   ;;        (ch chars (cdr ch))
;;   ;;        (i 0 (1+ i)))
;;   ;;       ((null ch) s)
;;   ;;     (setf (aref s i) (car ch))))
;;   )
;; (add-hook 'eshell-load-hook (function pjb-eshell-load-meat))



;;;----------------------------------------------------------------------------
(.EMACS "shell")

(defun colorize-buffer ()
  "Interprete les codes ASCII de couleur dans ce buffer."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (ansi-color-apply-on-region (point-min) (point-max))
    (set-buffer-modified-p modified)))

(defun pjb-shell-mode-meat ()
  (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (set-variable 'tab-width 8)
  (setf comint-process-echoes nil)
  (when (fboundp 'auto-complete-mode)
    (auto-complete-mode -1))
  (when (fboundp 'ansi-color-for-comint-mode-on)
    (ansi-color-for-comint-mode-on))
  ;; (cond
  ;;   ((let ((shell (getenv "ESHELL")))
  ;;       (or (null shell)
  ;;        (not (or (search "clash" shell)
  ;;              (search "scsh" shell)))))
  ;; Moved to ~/.emacs-bash:
  ;;    (process-send-string (get-buffer-process (current-buffer))
  ;;                      "alias less=cat ; alias more=cat ; ")))
  )
(add-hook 'shell-mode-hook (function pjb-shell-mode-meat))


;; (setf (getenv "ESHELL") (concatenate 'string  (USER-HOMEDIR-PATHNAME)
;;                          "bin/clash"))
;; (setf (getenv "ESHELL") "/bin/bash")


;;;----------------------------------------------------------------------------
;; (.EMACS "ido-mode")
;; (ido-mode 'both) ; 'file 'buffer  -1
;; See also smex for M-x enhancements.

;;;----------------------------------------------------------------------------
(.EMACS "caps-mode")
;;;(autoload 'caps-mode "caps-mode" "Toggle caps mode." t)

(defun caps-mode-self-insert-command (&optional n)
  "Like `self-insert-command', but uppercase the the typed character."
  (interactive "p")
  (insert-char (upcase last-command-event) n))

(defvar caps-mode-map nil)

(when (fboundp 'define-minor-mode)
  (define-minor-mode caps-mode
      "Toggle caps mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When caps mode is enabled, all letters are inserted in their
capitalized form."
    :init-value nil
    :lighter " Caps"
    (setq caps-mode-map
          (let ((map (make-sparse-keymap)))
            (substitute-key-definition 'self-insert-command
                                       'caps-mode-self-insert-command
                                       map global-map)
            map))
    (if caps-mode
        (add-to-list 'minor-mode-map-alist (cons 'caps-mode caps-mode-map))
        (setq minor-mode-map-alist
              (delete (assoc 'caps-mode minor-mode-map-alist)
                      minor-mode-map-alist)))))

;;;----------------------------------------------------------------------------
(.EMACS "AUTO-COMPLETE-MODE")
(require 'auto-complete nil t)
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (ac-config-default))

;;;----------------------------------------------------------------------------
(.EMACS "ORG-MODE")
(require 'org)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(setf org-agenda-files '("~/notes.txt"
                         ;; "~/notes-kuiper.txt"
                         ;; ;; (file-expand-wildcards "~/firms/*/notes.txt")
                         ;; "~/firms/wizards/notes.txt"
                         ;; "~/firms/willcom/notes.txt"
                         ;; "~/firms/secur.net/notes.txt"
                         ;; "~/firms/ravenpack/notes.txt"
                         ;; "~/firms/osii/notes.txt"
                         ;; "~/firms/medicalis/notes.txt"
                         ;; "~/firms/mappy/notes.txt"
                         ;; "~/firms/joellegymtonic/notes.txt"
                         ;; "~/firms/jem/notes.txt"
                         ;; "~/firms/intergruas/notes.txt"
                         ;; "~/firms/hf/notes.txt"
                         ;; "~/firms/hbedv/notes.txt"
                         ;; "~/firms/hamster-s-fabric-inc/notes.txt"
                         ;; "~/firms/camille/notes.txt"
                         ;; "~/firms/afaa/notes.txt"
                         )
      org-enforce-todo-dependencies t
      org-log-done 'note)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)



;;;----------------------------------------------------------------------------


(appendf auto-mode-alist '(("\\.jmf$"    . java-mode)
                           ("\\.j$"      . java-mode)))

(appendf auto-mode-alist '(("\\.pl1$"    . pl1-mode)))

(appendf auto-mode-alist '(("\\.html\\.in$"  . html-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               insert-image.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A patch to emacs to be able to insert images in a comint buffer
;;;;    such as inferior-lisp REPL.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-04-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(require 'cl)

(defun splice (new-list old list)
  "Like substitute but replace the old by the elements in the new-list."
  (loop
     with result = '()
     for item in list
     do (if (eql old item)
            (loop
               for item in new-list
               do (push item result))
            (push item result))
     finally (return (nreverse result))))


(defun comint-output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    ;; First check for killed buffer or no input.
    (when (and string oprocbuf (buffer-name oprocbuf))
      (with-current-buffer oprocbuf
        ;; Run preoutput filters
        (let ((functions (splice (default-value 'comint-preoutput-filter-functions)
                                 t
                                 comint-preoutput-filter-functions))
              (strings (list string)))
          
          (while (and functions strings)
            (setf strings (loop
                             with result = ()
                             for string in strings
                             do (setf result (revappend (ensure-list (funcall (car functions) string)) result))
                             finally (return (nreverse result))))
            (setq functions (cdr functions)))
          (setf string strings))
        
        ;; Insert STRING
        (let ((inhibit-read-only t)
              ;; The point should float after any insertion we do.
              (saved-point (copy-marker (point) t)))

          ;; We temporarly remove any buffer narrowing, in case the
          ;; process mark is outside of the restriction
          (save-restriction
            (widen)

            (goto-char (process-mark process))
            (set-marker comint-last-output-start (point))

            ;; insert-before-markers is a bad thing. XXX
            ;; Luckily we don't have to use it any more, we use
            ;; window-point-insertion-type instead.
            (loop
               for item in string
               do (cond
                    ((stringp item) (insert item))
                    ((consp   item) (insert-image (first item) (second item)))
                    (t (error "Unexpected kind of insert %S" item))))

            
            ;; Advance process-mark
            (set-marker (process-mark process) (point))
            (setf string (buffer-substring comint-last-output-start (point)))
            (unless comint-inhibit-carriage-motion
              ;; Interpret any carriage motion characters (newline, backspace)
              (comint-carriage-motion comint-last-output-start (point)))

            ;; Run these hooks with point where the user had it.
            (goto-char saved-point)
            (run-hook-with-args 'comint-output-filter-functions string)
            (set-marker saved-point (point))

            (goto-char (process-mark process)) ; in case a filter moved it

            (unless comint-use-prompt-regexp
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (add-text-properties comint-last-output-start (point)
                                     '(front-sticky
                                       (field inhibit-line-move-field-capture)
                                       rear-nonsticky t
                                       field output
                                       inhibit-line-move-field-capture t))))

            ;; Highlight the prompt, where we define `prompt' to mean
            ;; the most recent output that doesn't end with a newline.
            (let ((prompt-start (save-excursion (forward-line 0) (point)))
                  (inhibit-read-only t)
                  (inhibit-modification-hooks t))
              (when comint-prompt-read-only
                (or (= (point-min) prompt-start)
                    (get-text-property (1- prompt-start) 'read-only)
                    (put-text-property
                     (1- prompt-start) prompt-start 'read-only 'fence))
                (add-text-properties
                 prompt-start (point)
                 '(read-only t rear-nonsticky t front-sticky (read-only))))
              (unless (and (bolp) (null comint-last-prompt-overlay))
                ;; Need to create or move the prompt overlay (in the case
                ;; where there is no prompt ((bolp) == t), we still do
                ;; this if there's already an existing overlay).
                (if comint-last-prompt-overlay
                    ;; Just move an existing overlay
                    (move-overlay comint-last-prompt-overlay
                                  prompt-start (point))
                    ;; Need to create the overlay
                    (setq comint-last-prompt-overlay
                          (make-overlay prompt-start (point)))
                    (overlay-put comint-last-prompt-overlay
                                 'font-lock-face 'comint-highlight-prompt))))
            (goto-char saved-point)))))))


(defun pjb-comint-preoutput-insert-image (string)
  (let ((case-fold-search t))
    (loop
       with result = '()
       while (and (plusp (length string))
                  (string-match "\\(.*\\)(EMACS:INSERT-IMAGE[ \t\n]+#P\"\\(\\([^\\\"]\\|\\.\\)*\\)\")\\(.*\\)"
                                string))
       do (let ((before (match-string 1 string))
                (path   (match-string 2 string))
                (after  (match-string 4 string)))
            (push before result)
            (push (list (create-image path) " ") result)
            (setf string after))
       finally (push string result) (return (nreverse result)))))

;;;; THE END ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; (defun find-definition-site-of-preloaded-function (function)
;;   ;; Find the real def site of the preloaded function.
;;   ;; This is necessary only for defaliases.
;;   (let ((location
;;          (condition-case nil
;;              (find-function-search-for-symbol function nil "loaddefs.el")
;;            (error nil))))
;;     (when location
;;       (with-current-buffer (car location)
;;         (goto-char (cdr location))
;;         (when (re-search-backward
;;                "^;;; Generated autoloads from \\(.*\\)" nil t)
;;           (match-string 1))))))
;; 
;; 
;; (defun find-definition-site-of-subr-function (function)
;;   ;; Find the C source file name.
;;   nil)
;; 
;; 
;; (defun find-function-source (function)
;;   "Find the source of the emacs lisp FUNCTION (a symbol)."
;;   (interactive
;;    (let ((fn (function-called-at-point))
;;          (enable-recursive-minibuffers t)
;;          val)
;;      (setq val (completing-read
;;                 (if fn
;;                     (format "Find source of function  (default %s): " fn)
;;                     "Find source of function: ")
;;                 obarray 'fboundp t nil nil
;;                 (and fn (symbol-name fn))))
;;      (list (if (equal val "")
;;                fn (intern val)))))
;;   (if (null function)
;;       (.EMACS "You didn't specify a function")
;;       (let* ((def (if (symbolp function)
;;                       (symbol-function function)
;;                       function))
;;              file-name string
;;              (beg (if (commandp def) "an interactive " "a ")))
;;         (setq file-name (if (eq (car-safe def) 'autoload)
;;                             (nth 1 def)
;;                             (symbol-file function 'defun)))
;;         (when (and file-name
;;                    (equal (describe-simplify-lib-file-name file-name)
;;                           "loaddefs.el")) 
;;           (setf file-name (find-definition-site-of-preloaded-function function)))
;;         (when (and (null file-name) (subrp def))
;;           (setq file-name (find-definition-site-of-subr-function function)))
;;         (if file-name
;;             (progn
;;               (find-file file-name)
;;               (re-search-forward (format "^ *(def.* %s" function) nil t))
;;             (error "No source file for %s" function)))))


(defun hide-brackets ()
  "Show brackets as parens.
From: Jorgen Schaefer <forcer@forcix.cx>
Message-ID: <87irohiw7u.fsf@forcix.kollektiv-hamburg.de>
"
  (interactive)
  (font-lock-add-keywords
   nil '(("\\["
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    "(")
                    nil)))
         ("\\]"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ")")
                    nil)))))
  (font-lock-fontify-buffer))

(defun fix-brackets ()
  "Replace brackets outside of strings and comments with parens.
From: Jorgen Schaefer <forcer@forcix.cx>
Message-ID: <87irohiw7u.fsf@forcix.kollektiv-hamburg.de>
"
  (interactive)
  ;; This can be called in a hook before font lock mode has a chance
  ;; to run, but we need its information. So we enforce a font lock
  ;; run.
  (font-lock-fontify-buffer)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[][]" nil t)
      (let* ((bracket (match-string 0))
             (face (get-text-property 0 'face bracket)))
        (cond
          ((or (looking-back "#\\\\[][]") ; character literal
               (eq face 'font-lock-comment-face)
               (eq face 'font-lock-string-face))
           ;; Do nothing
           nil)
          ((string= "[" bracket)
           (replace-match "("))
          ((string= "]" bracket)
           (replace-match ")"))
          (t
           (error "Bad token: %s (This is a CAN'T-HAPPEN type of error)"
                  bracket)))))))






;;;----------------------------------------------------------------------------
;;; W3M and Web Browsers
;;;----------------------------------------------------------------------------



(defun browse-url-firefox2 (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-firefox-arguments' are also passed to
Firefox.

When called interactively, if variable
`browse-url-new-window-flag' is non-nil, load the document in a
new Firefox window, otherwise use a random existing one.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

When called non-interactively, optional second argument
NEW-WINDOW is used instead of `browse-url-new-window-flag'.

On MS-Windows systems the optional `new-window' parameter is
ignored.  Firefox for Windows does not support the \"-remote\"
command line parameter.  Therefore, the
`browse-url-new-window-flag' and `browse-url-firefox-new-window-is-tab'
are ignored as well.  Firefox on Windows will always open the requested
URL in a new window."
  (interactive (browse-url-interactive-arg "URL: "))
  ;; URL encode any `confusing' characters in the URL.  This needs to
  ;; include at least commas; presumably also close parens.
  (while (string-match "[[-` -$&-,;->{-~]" url)
    (setq url (replace-match
               (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply 'start-process
                         (concat "firefox " url)
                         " *start-browser*"
                         browse-url-firefox-program
                         (append
                          browse-url-firefox-arguments
                          (list url)))))
    (set-process-sentinel process
                          `(lambda (process change)
                             (browse-url-firefox-sentinel process ,url)))))

;; (setf common-lisp-hyperspec-browser (function browse-url-firefox2))


(defun pjb-browse-url (url &optional new-session)
  (interactive "sUrl: ")
  (let ((w3m-pop-up-windows t))
    (when (one-window-p)
      (split-window))
    (other-window 1)
    (w3m-browse-url url new-session)))

(defun pjb-w3m-browse-url-in-another-frame (url &rest args)
  (save-excursion
    (raise-frame
     (select-frame
      (or (find-if (lambda (frame) (equalp (frame-name frame) *browse-frame-name*))
                   (frame-list))
          (make-frame (list (cons 'name *browse-frame-name*))))))
    (w3m-goto-url url)))

(when (and (or (<= 23 emacs-major-version) (require 'mime-parse nil t))
           (ignore-errors (require 'w3m        nil t))
           (or (<= 23 emacs-major-version) (require 'mime-w3m   nil t)))
  (.EMACS "w3m mode")

  (global-set-key (kbd "H-w") 'w3m-browse-url)
  (global-set-key (kbd "H-b") 'w3m-select-buffer)
  
  (defvar *browse-frame-name* "*w3m*")
  (pushnew *browse-frame-name* special-display-buffer-names :test (function equal))

  ;; (setf common-lisp-hyperspec-browser (function pjb-w3m-browse-url-in-another-frame))
  (setf common-lisp-hyperspec-browser (function w3m-browse-url))
  ;; (push '("."  .  w3m-browse-url) browse-url-browser-function)
  
  ) ;;when








;;;----------------------------------------------------------------------------
(defstruct location buffer file-name line-number line-contents)

(defmacro ignore-errors* (&rest body)
  `(handler-case
       (progn ,@body)
     (error (err)
       (message "ignore-errors* %S" err)
       (values nil err))))


(defun %find-tag-locations-in-order ()
  (message "%%find-tag-locations-in-order enters tagname=%S next-p=%S regexp-p=%S"
           tagname next-p regexp-p)
  (loop
     with locations = '()
     for (buffer err) = (ignore-errors* (find-tag-noselect tagname nil regexp-p))
     then (ignore-errors* (find-tag-noselect tagname t regexp-p))
     initially  (message "%%find-tag-locations-in-order initially")
     do (message "buffer = %S" buffer)
     while buffer
     collect (with-current-buffer buffer
               (make-location
                :buffer (current-buffer)
                :file-name (buffer-file-name (current-buffer))
                :line-number (count-lines (point-min) (point-at-bol))
                :line-contents (buffer-substring-no-properties
                                (point-at-bol) (point-at-eol))))
     finally (message "%%find-tag-locations-in-order exists %S" locations) (return (values locations err))))


(defun pjb-find-tag-meat ()
  (message "pjb-find-tag-meat enters")
  (unless next-p
    (message "pjb-find-tag-meat 1")
    (multiple-value-bind (locations error) (%find-tag-locations-in-order)
      (message  " locations (2) = %S" locations)
      (if locations
          (progn
            (message "pjb-find-tag-meat 2")
            (save-excursion
              (message "pjb-find-tag-meat 3")
              (switch-to-buffer-other-window (get-buffer-create
                                              (format "*tags on %s*" tagname))) 
              (erase-buffer)
              (compilation-mode 1)
              (message "pjb-find-tag-meat 4")
              (dolist (loc locations)
                (insert (format "%s:%s %s\n"
                                (location-file-name loc)
                                (location-line-number loc)
                                (location-line-contents loc))))
              (message "pjb-find-tag-meat 5"))
            (message "pjb-find-tag-meat 6")
            (message "pjb-find-tag-meat exits %S" (location-buffer (first locations)))
            (location-buffer (first locations)))
          (when error
            (signal (car error) (cdr error)))))
    (error (err)
           (message "pjb-find-tag-meat 7")
           (message "%s" err))))


;; (add-hook 'find-tag-hook (function pjb-find-tag-meat))
;; (setq find-tag-hook nil)


;;;----------------------------------------------------------------------------
(.EMACS "bee for bigloo")
(when (file-exists-p "/usr/local/share/emacs/bigloo/")
  (setf load-path (cons (expand-file-name "/usr/local/share/emacs/bigloo/") load-path))
  (when (require 'bmacs nil t)
    (push '("\\.scm\\'" . bee-mode) auto-mode-alist)
    (push '("\\.sch\\'" . bee-mode) auto-mode-alist)))

;;;----------------------------------------------------------------------------
(.EMACS "matlab/scilab")

(autoload 'matlab-mode "matlab.el" "Enter Matlab mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab.el" "Interactive Matlab mode." t)



;;;----------------------------------------------------------------------------
(.EMACS "ocaml")

(autoload 'caml-mode "ocaml" (interactive)
          "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

(appendf auto-mode-alist '(("\\.ml[iyl]?$" .  caml-mode)))


;;;----------------------------------------------------------------------------
(.EMACS "otter")
(autoload 'otter-mode "otter-mode" "Mode for editing Otter files." t)
(autoload 'run-otter  "otter-mode" "Running an inferior Otter process." t)
(appendf auto-mode-alist '(("\\.ot$" . otter-mode)
                           ("\\.in$" . otter-mode)))

(defvar otter-program "/usr/local/src/otter-3.2/source/otter"
  "The absolute path to Otter")

(defvar otter-out-extension ".oout"
  "*The extension used to generate output files from Otter.")
(defvar otter-in-extension1 "\\.ot"
  "*The standard extension to recognize input-files for Otter.
 Note that the \\ before the period is a must !")
(defvar otter-in-extension2 "\\.in"
  "*An other extension to recognize input-files for Otter.
  Note that the \\ before the period is a must !")


;;;----------------------------------------------------------------------------
(.EMACS "COBOL mode")
(autoload 'cobol-mode "cobol")
(appendf auto-mode-alist '(("\\.cbl\\'" . cobol-mode)
                           ("\\.cob\\'" . cobol-mode)
                           ("\\.cobol\\'" . cobol-mode)))

(when (fboundp 'speedbar-add-supported-extension)
  (speedbar-add-supported-extension "CBL"))


;;;----------------------------------------------------------------------------
(.EMACS "PHP mode")
(autoload 'php-mode "php-mode" "Major mode for editing PHP" t)
(appendf auto-mode-alist '(("\\.php\\'" . php-mode)))

;;;----------------------------------------------------------------------------
(.EMACS "lua mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\(\\.lua\\)\\|\\(/mission\\)$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;----------------------------------------------------------------------------
(.EMACS "css mode")
(autoload 'css-mode "css-mode" "Major mode for editing CSS" t)
(appendf auto-mode-alist '(("\\.css\\'" . css-mode)))
(setf cssm-indent-function (function cssm-c-style-indenter))

;;;----------------------------------------------------------------------------
(.EMACS "Postscript mode")
(autoload 'ps-mode "ps-mode" "Major mode for editing PostScript" t)
(appendf auto-mode-alist '(("\\.[eE]?[pP][sS]$" . ps-mode)))

;; Use lazy-lock for large PostScript files
(add-hook 'ps-mode-hook
          '(lambda ()
            (make-local-variable 'font-lock-support-mode)
            (make-local-variable 'lazy-lock-defer-on-scrolling)
            (setq font-lock-support-mode 'lazy-lock-mode
             lazy-lock-defer-on-scrolling t)
            (turn-on-font-lock)))
(add-hook 'ps-run-mode-hook
          '(lambda ()
            (turn-on-font-lock)))

(unless (fboundp 'run-mode-hooks) (defun run-mode-hooks (&rest args)))



;;;----------------------------------------------------------------------------
;; (.EMACS "remem")
;; (when (require 'remem nil t)
;;   (setq remem-scopes-list
;;         (file-expand-wildcards "~/RA-indexes/*")
;;         '(("my-email" 6 5 500)
;;           ("my-notes" 2 10 500))))


;;;----------------------------------------------------------------------------
(.EMACS "psgml mode")
(when (require 'psgml nil t)
  ;;(define-key sgml-mode-map "\M-\C-f"   'set-justification-full)
  (appendf auto-mode-alist '(("\\.html$"   . html-mode)
                             ("\\.htm$"    . html-mode))))


(global-set-key (kbd "H-h a")
                (lambda (start end)
                  (interactive "r")
                  (let ((url (buffer-substring start end)))
                    (delete-region start end)
                    (insert (format (if prefix-arg
                                        "<a href=\"%s\">%s</a>"
                                        "<a href=\\\"%s\\\">%s</a>") url url)))))

;; ------------------------------------------------------------------------
;; AIM client for emacs:
;; (load"tnt") ;; Doesn't work good.

;; ------------------------------------------------------------------------
(defun lac-emms ()
  "Load and configure emms"
 (when (require 'emms-setup nil t)
   (require 'emms-player-simple)
   (require 'emms-source-file)
   (require 'emms-source-playlist)
   ;; save playlist and load at emacs start
   (require 'emms-history)
   (emms-history-load)
   (require 'emms-volume)
   (global-set-key (kbd "C-c =") 'emms-volume-mode-plus)
   (global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
   (global-set-key (kbd "C-c -") 'emms-volume-mode-minus)
   (global-set-key (kbd "<f9>") 'emms-pause)
   (global-set-key (kbd "<f10>") 'emms-seek-backward)
   ;; (setq emms-repeat-playlist 1)
   ;; (emms-standard)

   (when (require 'emms-info-id3v2 nil t)
     (add-to-list 'emms-info-functions 'emms-info-id3v2))


   (emms-all)
   (emms-default-players)
   (setq emms-playlist-default-major-mode 'emms-playlist-mode)


   (defvar emms-browser-mode-hook '()
     "Hook for meat called after emms-browser-mode is activated.")
  
   (defadvice emms-browser-mode (after pjb-emms-browse-mode-hook-advice activate)
     "Add an emms-browser-mode-hook feature."
     (interactive)
     (run-mode-hooks 'emms-browser-mode-hook)
     (when (null delay-mode-hooks)
       (run-mode-hooks 'after-change-major-mode-hook)))

   (defun pjb-emms-save-file-path ()
     (interactive)
     (let* ((bdata (emms-browser-bdata-at-point))
            (data  (cdr (assoc 'data    bdata)))
            (track (cdr (assoc '*track* data)))
            (name  (cdr (assoc 'name    track))))
       (kill-new name nil nil)
       (message "%s" name)))
  
   (defun pjb-emms-browser-mode-meat ()
     (interactive)
     (local-set-key (kbd "u") 'pjb-emms-save-file-path)
     (local-set-key (kbd "U") 'pjb-emms-save-file-path))

   (add-hook 'emms-browser-mode-hook 'pjb-emms-browser-mode-meat)
  
   (defalias 'np 'emms-show)
   'lac-emms))

(add-lac 'lac-emms)
(lac-emms)




;;;----------------------------------------------------------------------------
(when (require 'column-marker nil t)
  (.EMACS "columnmarker")
  (column-marker-1 80))


;;;----------------------------------------------------------------------------
(.EMACS "wiki")
(autoload 'wiki-remote-get "wiki-remote" "Edit a wiki page." t)


;; Need modifications to use french mediapedias:
(when (load "~/src/wikipedia-el/wikipedia.el" t)
  (.EMACS "wikipedia")
  ;; (.EMACS "wikipedia-mode")
  ;; (autoload 'wikipedia-mode "wikipedia-mode.el"
  ;;   "Major mode for editing documents in Wikipedia markup." t)
  )

;;;----------------------------------------------------------------------------
;; Rmime:
;;(load "rmime"  *pjb-load-noerror* *pjb-load-silent*)
;;(when (fboundp 'rmime-format)
;; (add-hook 'rmail-show-message-hook 'rmime-format))
;;(when (fboundp 'rmime-cancel)
;;  (add-hook 'rmail-edit-mode-hook    'rmime-cancel))
;;(remove-hook 'rmail-show-message-hook 'rmime-format)
;;(remove-hook 'rmail-edit-mode-hook    'rmime-cancel)

;;;----------------------------------------------------------------------------
;; (.EMACS "mailcrypt")
;; ;; (load-library "mailcrypt")
;; ;; (mc-setversion "gpg")
;; (autoload 'mc-install-write-mode "mailcrypt" nil t)
;; (autoload 'mc-install-read-mode  "mailcrypt" nil t)
;; (add-hook 'mail-mode-hook 'mc-install-write-mode)
;;
;; (setf mc-default-scheme 'mc-scheme-gpg
;;       mc-gpg-user-id     "E9350DE9")

;; (when (string-lessp "23" emacs-version)
;;   (require 'epa-file)
;;   (epa-file-enable)
;;   (setf epa-armor t))

;; Distribution installs crypt++...
(setf find-file-hook (remove 'crypt-find-file-hook find-file-hook))

(defun find-file-read-args (prompt mustmatch)
  (list (read-file-name prompt nil default-directory mustmatch nil
                        (lambda (name) (not (string-match "~$" name))))
	t))

;;;----------------------------------------------------------------------------
(.EMACS "mew")

(autoload 'mew      "mew" "Start Mew." t)
(autoload 'mew-send "mew" "Compose a new message." t)
;;(defalias 'mail 'mew-send)

(setq mew-mailbox-type 'mbox)
(setq mew-mbox-command "incm")
(setq mew-mbox-command-arg "-d /var/spool/mail/pjb")


;; (autoload 'x-face-decode-message-header "x-face-e21")
;; (autoload 'x-face-insert "x-face-e21" nil t)
;; (autoload 'x-face-save "x-face-e21" nil t)
;; (autoload 'x-face-show "x-face-e21" nil t)
;; (autoload 'x-face-turn-off "x-face-e21")
;; (setq x-face-auto-image t)



;;;----------------------------------------------------------------------------
;; (require 'nnmail)
;; (defadvice nnmail-process-babyl-mail-format
;;     (before nnmail-process-babyl-mail-format/log (func artnum-func) activate)
;;   (message "Found a BABYL mailbox!"))
;; (defadvice nnmail-process-mmdf-mail-format
;;     (before nnmail-process-mmdf-mail-format/log (func artnum-func) activate)
;;   (message "Found a MMDF mailbox!"))
;; (defadvice nnmail-process-maildir-mail-format
;;     (before nnmail-process-maildir-mail-format/log (func artnum-func) activate)
;;   (message "Found a MAILDIR mailbox!"))
;; (defadvice nnmail-process-unix-mail-format
;;     (before nnmail-process-unix-mail-format/log (func artnum-func) activate)
;;   (message "Found a UNIX mailbox!"))

;;;----------------------------------------------------------------------------

(defun pjb-mail-mode-meat ()
  (message "mail-mode-meat")
  (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (set-buffer-file-coding-system   'utf-8)
  ;; (setf buffer-file-coding-system  'utf-8)
  ;; (inactivate-input-method)
  (auto-complete-mode -1)
  (local-set-key (kbd "TAB") (quote expand-mail-aliases)))




;;;----------------------------------------------------------------------------
;;; GNUS

(require 'gnus)

;; (defadvice gnus-summary-mark-as-expirable
;;     (after gnus-summary-mark-as-expirable+next-line activate)
;;   (next-line))
;; (ad-disable-advice 'gnus-summary-mark-as-expirable 'after 'gnus-summary-mark-as-expirable+next-line)


;; (local-set-key (kbd "e") 'gnus-summary-mark-as-expirable)

(setf *pjb-gnus-trash-mailbox* "nnimap+hubble.informatimago.com:INBOX.Trash")
(setf *pjb-gnus-junk-mailbox*  "nnimap+hubble.informatimago.com:INBOX.Junk")



(when (boundp 'gnus-summary-mode-map)
 (define-key gnus-summary-mode-map (kbd "v DEL") 'pjb-gnus-summary-move-article-to-trash)
 (define-key gnus-article-mode-map (kbd "v DEL") 'pjb-gnus-summary-move-article-to-trash)

 (define-key gnus-summary-mode-map (kbd "v j")   'pjb-gnus-summary-move-article-to-junk)
 (define-key gnus-article-mode-map (kbd "v j")   'pjb-gnus-summary-move-article-to-junk))

;; (define-key gnus-group-mode-map   (kbd "v j d")
;;   (lambda ()
;;     (interactive)
;;     (gnus-group-jump-to-group "nndraft:drafts")))


(defun pjb-gnus-message-setup-meat ()
  ;;   (save-excursion
  ;;     (beginning-of-buffer)
  ;;     (while (re-search-forward "anevia.com" (point-max) t)
  ;;       (delete-region (match-beginning 0) (match-end 0))
  ;;       (insert "informatimago.com")))
  )



;;;----------------------------------------------------------------------------
;;; pjb

(setq pgp-command 'pgp-gpg-command)
(setq pgp-signer  "0xEF5E9966") ;; "pjb@informatimago.com"
(setq *pjb-sources-initials* "PJB")

(require 'message)
(defalias 'rot13-region 'message-caesar-region)

(defalias 'scratch      'pjb-scratch)
(defalias 'eurotunnel   'pjb-eurotunnel)
(defalias 'address      'pjb-address)
(defalias 'attach-file  'pjb-mail-attach-file)
(defalias 'ff           'full-frame)
(defalias 'make         'compile)

(remove-hook 'mail-send-hook 'mime-edit-maybe-translate)


;; Server for emacsclient:
(setf mm-content-transfer-encoding-defaults '(("text/.*" 8bit)
                                              ("message/rfc822" 8bit)
                                              ("application/emacs-lisp" 8bit)
                                              ("application/x-emacs-lisp" 8bit)
                                              ("application/x-patch" 8bit)
                                              (".*" base64))
      mm-body-charset-encoding-alist '((iso-8859-1  . 8bit)
                                       (iso-8859-15 . 8bit)))
;;(add-to-list 'mm-charset-synonym-alist '(iso8859-15 . iso-8859-15))
;;(add-to-list 'mm-charset-synonym-alist '(iso885915 . iso-8859-15))


;;;---------------------------------------------------------------------
;;; erc
;;;---------------------------------------------------------------------

(when (require 'erc-highlight-nicknames nil t)
  (erc-highlight-nicknames-enable))

(require 'erc-eval nil t)


;;(erc-select :server "localhost" :nick "pjb")
;;(erc-send-command  (format "PRIVMSG &bitlbee :identify %s" "popo"))

(defun pjb/erc-insert-timestamp-left (string)
  "Insert timestamps at the beginning of the line."
  (goto-char (point-min))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
                        (string-equal string erc-timestamp-last-inserted)))
         (len (length string))
         (s (if ignore-p "" string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (unless (string= s "")
      (erc-put-text-property 0 len 'field 'erc-timestamp s)
      (erc-put-text-property 0 len 'invisible 'timestamp s))
    (insert s)))


(defun pjb/erc-fill-static ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (save-match-data
    (goto-char (point-min))
    (when (looking-at "^\\(\\S-+\\)")
      (let ((nick (match-string 1)))
        (let ((fill-column (- erc-fill-column (erc-timestamp-offset)))
              (fill-prefix (make-string erc-fill-static-center 32)))
          (insert (make-string (max 0 (- erc-fill-static-center
                                         (length nick) 1))
                               32))
          (erc-fill-regarding-timestamp))
        (erc-restore-text-properties)))))



;; (add-hook 'erc-join-hook 'pjb-erc-join-meat)  
;; (pjb-set-erc-nickserv-passwords)
;; (setf erc-timestamp-format "%Y-%m-%d %H:%M\n")
;; (erc-match-mode 1)


;;(erc-select :server "localhost" :nick "pjb")
;;(erc-send-command  (format "PRIVMSG &bitlbee :identify %s" "popo"))

;; (setf erc-hide-list '())
;; (setf erc-hide-list  (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))


;;;----------------------------------------------------------------------------

(defparameter *digits-for-letters*
  '(("0" . "o")
    ("1" . "il")
    ("3" . "e")
    ("4" . "a")
    ("5" . "s")
    ("6" . "b")
    ("7" . "t")
    ("8" . "b")
    ("9" . "gq")))

(defun vowelp (ch) (find ch "aeiouy"))

;; (setf word "qu1j0t3")
;; 
;; (defun used-digits-for-letters-p (word)
;;   (let ((d (map 'vector 'cl:digit-char-p word))
;;         (v (map 'vector 'vowelp word)))
;;     (and (some 'identity d)
;;          (loop
;;             with m = (1- (length d))
;;             for i below (length d)
;;             always (or (not (aref d i))
;;                        (and (< 0 i) (not (aref v (1- i))))
;;                        (and (< i m) (not (aref v (1+ i)))))))))
;; 
;; (mapcar 'used-digits-for-letters-p '("qu1j0t3"
;;                                      "tali713"
;;                                      "elf"))
;; (t t nil)


;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
(require 'pjb-speak)
(require 'pjb-erc-speak)

;;;----------------------------------------------------------------------------

(defparameter *pjb-erc-answers*
  '((geb
     . "the most important book of the XX century: \"Gödel, Escher and Bach: An Eternal Golden Braid\" http://www.amazon.com/G%C3%B6del-Escher-Bach-Eternal-Golden/dp/0465026567")
    (lisp-1-vs-lisp-2-technical-issues  
     . "Please read: http://www.nhplace.com/kent/Papers/Technical-Issues.html")
    (equal         
     . "Please read: http://www.nhplace.com/kent/PS/EQUAL.html")
    (ambitious-eval
     . "Please read: http://www.nhplace.com/kent/PS/Ambitious.html")
    (what-implementation  
     . "To get help choosing a CL implementation, connect to telnet://hubble.informatimago.com:8101 ; have a look at http://www.cliki.net/Common%20Lisp%20implementation")
    (clhs          
     . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
    (intersection  
     . "Have a look at (intersection common-lisp emacs-lisp scheme) http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/intersection-r5rs-common-lisp-emacs-lisp/")
    (scheme-or-cl  
     . "CL vs. Scheme http://irreal.org/blog/?p=813")
    (cliki         
     . "Have a look at http://cliki.net/ ; start with http://www.cliki.net/Getting%20Started")
    (newbie
     . "http://cliki.net/Getting%20Started or http://articulate-lisp.com/ ")
    (getting-started 
     . "Start with http://www.cliki.net/Getting%20Started  or  http://articulate-lisp.com/" )
    (emacs-lisp-intro
     . "An Introduction to Programming in Emacs Lisp  http://www.gnu.org/software/emacs/emacs-lisp-intro/  or  M-: (info \"(eintr)Top\") RET (for non-programmers)")
    (emacs-lisp      
     . "Emacs Lisp Manual http://www.gnu.org/software/emacs/manual/elisp.html  or  M-: (info \"(elisp)Top\") RET")
    (emacs-manual    
     . "Emacs Manual http://www.gnu.org/software/emacs/manual/   or  M-: (info \"(emacs)Top\") RET")
    (the-art-of-unix-programming    
     . "The Art of Unix Programming http://www.faqs.org/docs/artu/")
    (hacker-howto    
     . "http://www.catb.org/~esr/faqs/hacker-howto.html")
    (the-craft-of-text-editing    
     . "The Craft of Text Editing   http://www.finseth.com/craft/")
    (essentials-of-programming-languages     
     . "Essentials of Programming Languages, 3rd ed.   Daniel P. Friedman and Mitchell Wand   ISBN: 978-0-262-06279-4   http://MITPress.MIT.Edu/0262062798/  http://WWW.EoPL3.Com/")
    (practical-common-lisp      
     . "Practical Common Lisp http://www.gigamonkeys.com/book/")
    (common-lisp-a-gentle-introduction-to-symbolic-computation   
     . "Common Lisp: A Gentle Introduction to Symbolic Computation  http://www.cs.cmu.edu/~dst/LispBook/  http://www-cgi.cs.cmu.edu/afs/cs.cmu.edu/user/dst/www/LispBook/index.html")
    (common-lisp-programming-for-artificial-intelligence   
     . "Common Lisp Programming for Artificial Intelligence  Tony Hasemer & John Domingue - 1989  International Computer Science Series  Addison & Wesley  ISBN 0-201-17579-7")
    (common-lisp-an-interactive-approach    
     . "Common Lisp: An Interactive Approach  by Stuart C. Shapiro   http://www.cse.buffalo.edu/~shapiro/Commonlisp/")
    (paradigms-of-artificial-intellgience     
     . "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp")
    (artifical-intelligence-a-modern-approach     
     . "Artificial Intelligence: A Modern Approach  http://aima.cs.berkeley.edu")
    (sicp     
     . "Structure and Interpretation of Computer Programs  http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html  http://swiss.csail.mit.edu/classes/6.001/abelson-sussman-lectures/")
    (sicp-mit 
     . "http://web.mit.edu/alexmv/6.S184/")
    (6.S184   
     . "http://web.mit.edu/alexmv/6.S184/")
    ;; http://www.codepoetics.com/wiki/index.php?title=Topics:SICP_in_other_languages
    ;; http://eli.thegreenplace.net/category/programming/lisp/sicp/
    ;; http://www.neilvandyke.org/sicp-plt/
    ;; http://www.youtube.com/watch?v=rdj6deraQ6k
    (r5rs     
     . "http://www.schemers.org/Documents/Standards/R5RS/HTML/")
    (how-to-design-programs     
     . "How to Design Programs -- An Introduction to Computing and Programming  http://www.htdp.org/2003-09-26/Book/  ")
    (concrete-abstraction       
     . "Concrete Abstractions -- An Introduction to Computer Science Using Scheme  http://www.gustavus.edu/+max/concrete-abstractions.html")
    (lisp-in-small-pieces     
     . "Lisp in Small Pieces   http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html  http://pagesperso-systeme.lip6.fr/Christian.Queinnec/Books/LiSP-2ndEdition-2006Dec11.tgz")
    (on-lisp   
     . "On Lisp  Paul Graham   http://www.paulgraham.com/onlisptext.html  http://www.bookshelf.jp/texi/onlisp/onlisp.html  http://www.bookshelf.jp/texi/onlisp/onlisp.tar.gz")
    (compiler-principle-techniques-and-tools     
     . "Compiler Principles Techniques and Tools, Aho et al. http://dragonbook.stanford.edu/")
    (the-art-of-computer-programming    
     . "The Art of Computer Programming  Donald E. Knuth  Addison & Wesley")
    (goedel-escher-bach      
     . "Gödel, Escher, Bach: An Eternal Golden Braid  Douglas Hofstadter")
    (basic-lisp-technique      
     . "Basic Lisp Techniques  Cooper - 2003 Franz, Inc. - 100 pages.  http://www.franz.com/resources/educational_resources/cooper.book.pdf")
    (casting-speels-in-lisp  
     . "Casting Spels in Lisp  Conrad Barski, M.D.  http://www.lisperati.com/casting.html")
    (floating-point
     . "What Every Computer Scientist Should Know About Floating-Point Arithmetic http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html   and   What Every Programmer Should Know About Floating-Point Arithmetic http://floating-point-gui.de/") 
    ;; --
    (gitorious-lisp 
     . "https://gitorious.org/com-informatimago/com-informatimago/trees/master")
    (gitorious-emacs
     . "https://gitorious.org/com-informatimago/emacs/trees/master")
    (rc       
     . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/rc")
    (bin      
     . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/bin")
    (idiots   
     . "There, there, we know there are idiots on the Internet.  Lisp will make it all better.")
    (maintained-illustration
     . "http://tinyurl.com/last-commit-six-month-ago http://tinyurl.com/monthly-commits http://tinyurl.com/last-commit-yesterday http://tinyurl.com/last-commit-before-VCS-existed")
    (ibcl
     . "Image Based Development http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/index.html")
    ;; --
    (see-defpackage
     . ";;;;    See defpackage documentation string.\n")
    (agpl3         
     . "
License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 1994 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <a href=\\\"http://www.gnu.org/licenses/\\\">http://www.gnu.org/licenses/</a>.

")))



(defun pjb-erc-get-answers ()
  (mapcar (function car) *pjb-erc-answers*))

(defvar *pjb-erc-last-answer* nil)

(defun pjb-erc-answer (key)
  (interactive (list 
                (intern (completing-read 
                         "What? " (mapcar (lambda (x) (cons x nil)) (pjb-erc-get-answers))
                         (lambda (answer) (setq *pjb-erc-last-answer* (car answer)))
                         t))))
  (insert (format "%s" (cdr (assoc key *pjb-erc-answers*)))))

;; (add-hook 'erc-join-hook (lambda () (local-set-key (kbd "H-a") 'pjb-erc-answer)))
(global-set-key (kbd "H-a") 'pjb-erc-answer)

;;;----------------------------------------------------------------------------
(.EMACS "server")

(setf server-socket-dir *tempdir*
      server-name       (format "server-%d" (emacs-pid)))

;;;----------------------------------------------------------------------------


;; c modes

(when (fboundp 'pjb-c-todo-hook)
  (mapc (lambda (hook) (add-hook hook 'pjb-c-todo-hook))
        '(c-mode-hook c++-mode-hook objc-mode-hook )))

(when (fboundp 'pjb-objc-edit-meat)
  (add-hook 'objc-mode-hook 'pjb-objc-edit-meat))


(appendf auto-mode-alist  '(("\\.mc\\'" . c++-mode)))


;; (push  '(c . pjb-lineup-C-comments)              c-offsets-alist)
;; (push  '(comment-intro . pjb-lineup-C-comments)  c-offsets-alist)

(defun c-indent-or-tab ()
  (interactive)
  (if (string-match "^[ \t]*$"
                    (buffer-substring-no-properties
                     (save-excursion (beginning-of-line) (point))
                     (point)))
      (c-indent-command)
      (let ((indent-line-function 'indent-relative))
        (indent-for-tab-command))))


;; (setf c-mode-hook nil c++-mode-hook nil objc-mode-hook nil )

(defun c-mode-meat ()
  (interactive)
  (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (define-key c-mode-map (kbd "C-c p") 'pjb-ide-insert-tag-comment)
  (local-set-key  (kbd "C-c p") 'pjb-ide-insert-tag-comment)
  (define-key c-mode-map "{" 'self-insert-command)
  (local-set-key (kbd "TAB") (quote c-indent-or-tab)))

(add-hook 'c-mode-hook 'c-mode-meat)

;;(add-hook 'c++-mode-hook (function pjb-c++-mode-hook))
;;(setf c++-mode-hook (delete (function pjb-c++-mode-hook) c++-mode-hook))




;; To correct some rebound problem I have with my keyboard, disable
;; two spaces in a row, unless it's preceded by a punctuation, or
;; explicitely requested.

(defun pjb-electric-space-rebound (p)
  (interactive "P")
  ;; (message "p=%S" p)
  (cond
    ((null p)
     (let ((recent (recent-keys)))
       ;; (message "recent=%S"(equal (subseq recent (1- (length recent))) [32]))
       (if (equal (subseq recent (- (length recent) 2)) [32 32])
           (when (let ((pt (point)))
                   (when (< (+ (point-min) 2) pt)
                     (unwind-protect
                          (progn
                            (forward-char -2)
                            (looking-at "[.;!?] "))
                       (goto-char pt))))
             (insert " "))
           (insert " "))))
    ((eq p '-))
    ((integerp p)                       (insert (make-string p 32)))
    ((and (listp p) (integerp (car p))) (insert (make-string (car p) 32)))
    (t (error "Unknown raw prefix argument %S" p))))

;; (global-set-key (kbd "SPC") 'pjb-electric-space-rebound)



(defun pjb-electric-ellipsis (p)
  (interactive "P")
  (cond
    ((null p)
     (let ((recent (recent-keys)))
       (if (and (equal (subseq recent (- (length recent) 2)) [?. ?.])
                (equal (buffer-substring (- (point) 2) (point)) ".."))
           (progn
             (delete-region (- (point) 2) (point))
             (insert "…"))
           (insert "."))))
    ((eq p '-))
    ((integerp p)                       (insert (make-string p ?.)))
    ((and (listp p) (integerp (car p))) (insert (make-string (car p) ?.)))
    (t (error "Unknown raw prefix argument %S" p))))

(global-set-key (kbd ".") 'pjb-electric-ellipsis)




;; lisp modes

;; (setq emacs-lisp-mode-hook nil lisp-mode-hook nil)
(when (fboundp 'common-lisp-font-lock-hook)
  (add-hook 'common-lisp-mode-hook 'common-lisp-font-lock-hook))

(when (and (< emacs-major-version 23) (and (eq window-system 'x) (fboundp 'pretty-greek)))
  (add-hook 'emacs-lisp-mode-hook 'pretty-greek))
;;(add-hook mode 'show-paren-mode)))


(when (fboundp 'common-lisp-font-lock-hook)
  (add-hook 'lisp-mode-hook 'common-lisp-font-lock-hook))


;;(setq emacs-lisp-mode-hook nil lisp-mode-hook nil)

(setq open-paren-in-column-0-is-defun-start nil)
(setq minibuffer-max-depth nil)
(setq print-circle t)


(autoload 'd-mode "/usr/local/src/languages/clisp/clisp-cvs/clisp/emacs/d-mode"
  "Mode to edit clisp sources." t)

;; (setq auto-mode-alist (append '(("\\.c\\'" . c-mode)) auto-mode-alist))
(appendf auto-mode-alist  '(("\\.pp\\'"                     . pascal-mode)
                            ("\\.\\(m[id]\\|mod\\|def\\)$"  . modula-2-mode)
                            ("-MIB$\\|-SMI$"                . snmp-mode)
                            ("\\.bison\\'"                  . c-mode)
                            ("\\.lex\\'"                    . c-mode)
                            ("\\.d\\'"                      . d-mode)))


;;;----------------------------------------------------------------------------
;; X-GPG-Key-ID: 0xAC23A821
;; X-GPG-fingerprint: CA53 7C90 2052 3484 51A2  8B3A E10A 8A44 AC23 A821
;; Disposition-Notification-To: <pjb@informatimago.com>
;; Return-Receipt-To:           <pjb@informatimago.com>
;; X-PGP-Key-URL:     http://www.informatimago.com/pgpkey
;;;----------------------------------------------------------------------------
;; Generate vc-annotate-color-map:
;; (let ((x 240))
;;     (while (< 0 x)
;;         (insert (format "(%f . \"#0000%02x\")\n" (* (/ 86400.0 65536.0 4.0)
;;                                                     (- 255 x)) x))
;;         (setq x (- x 16))))
;;;----------------------------------------------------------------------------

;; blank-mode.el to examine tabs and spaces in interior positions in the text
;; http://www.cpqd.com.br/~vinicius/emacs/Emacs.html



;;;----------------------------------------------------------------------------
(.EMACS "Info-directory-list")

(defun find-subdirs-with-info (base-path)
  (let ((result '())
        (add-base nil)
        (items (DIRECTORY (concatenate 'string base-path "/*"))))
    (unless (find-if (lambda (item) (string-match "/\\.nosearch$" item)) items)
      (dolist (item items)
        (cond
          ((string-match "\\.info\\(.gz\\)?$" item)
           (setq add-base t))
          ((file-directory-p item)
           (setq result (append result (find-subdirs-with-info item))))))
      (when add-base
        (push base-path result)))
    result))


(defun find-subdirs-with-dir (base-path)
  (when (file-exists-p base-path)
    (mapcar
     (function NAMESTRING)
     (mapcar (lambda (path)  (MAKE-PATHNAME :NAME nil :TYPE nil :VERSION nil
                                            :DEFAULTS path))
             (remove-if-not (function NULL)
                            (DIRECTORY (CONCATENATE 'STRING base-path "**/dir"))
                            :key (function PATHNAME-TYPE))))))


(defun find-subdirs-with-dir (base-path)
  (let ((result '())
        (add-base nil)
        (items (file-expand-wildcards (concat base-path "/*") t)))
    (unless (find-if (lambda (item) (string-match "/\\.nosearch$" item)) items)
      (dolist (item items)
        (cond
          ((string-match "/dir$" item)
           (setq add-base t))
          ((file-directory-p item)
           (setq result (append result (find-subdirs-with-dir item))))))
      (when add-base
        (push base-path result))
      result)))


(setq Info-default-directory-list
      (remove
       "/usr/share/info/emacs-21"
       (labels ((flatten
                    (tree)
                  "collect in a simple list all the non-nil atoms in the tree."
                  (cond
                    ((null tree) nil)
                    ((atom tree) (list tree))
                    (t (nconc (flatten (car tree))
                              (and (cdr tree) (flatten (cdr tree))))))))
         (flatten (mapcar (function find-subdirs-with-dir)
                          '("/usr/local/share/emacs/"
                            "/usr/local/share/info/"
                            "/usr/local/info/"
                            "/usr/share/info/"
                            "/usr/share/gcc-data/i486-pc-linux-gnu/4.1.2/info/")))))
      Info-directory-list Info-default-directory-list)



;;;----------------------------------------------------------------------------
(.EMACS "Miscellaneous commands")


(defun* notes ()
  (interactive)
  (do-directories-up (dir default-directory)
    (dolist (file '("NOTES.txt" "notes.txt" "NOTES.*[a-z]" "notes.*[a-z]"
                    ".notes.utf-8" ".notes*[a-z]"))
      (let ((files (file-expand-wildcards (concat dir file) t)))
        (when files
          (find-file (first files))
          (return-from notes))))))


(defun afaire ()
  "Jump to my TODO list."
  (interactive)
  (unless (zerop (user-uid))
    (notes)
    (goto-char (point-min))
    (search-forward "AFAIRE:" nil t 2)
    (recenter 1)
    (when (string-match "^thalassa" system-name)
      (vm-visit-folder "~/mail/todo.mbox"))))

(defun acheter ()
  "Jump to my TODO list."
  (interactive)
  (notes)
  (goto-char (point-min))
  (search-forward "ACHETER:" nil t 2)
  (recenter 1))


(when t
  (milliways-schedule (lambda ()
			(unless (intersection
				 '("-f" "-funcall" "--funcall" "-e" "-eval" "--eval" "-execute"
				   "--execute" "-insert" "--insert") command-line-args
				   :test (function string=))
			  (afaire)))))


(defun doing (what)
  (interactive "sWhat are you doing? ")
  (find-file "~/doing.txt")
  (goto-char (point-max))
  (insert (shell-command-to-string "date")  what "\n#\n")
  (save-buffer)
  (bury-buffer))


;;;----------------------------------------------------------------------------

(defun informatimago ()
  "Browse http://www.informatimago.com"
  (interactive)
  (browse-url "http://www.informatimago.com/toc.html"))


;;;----------------------------------------------------------------------------
(.EMACS "Web Searches")

;; (require 'google)
;; (setq google-license-key "dF18sc1QFHLPxvBVqwv/WxCbYR18GHbp")
;; ;; Then M-x google-search RET
;; ;; or M-x google-search-region RET
;; (defalias 'url-retrieve-synchronously 'url-retrieve)

(defun %search-region (start end thing search-function)
  (when start
    (cond
      ((null end)
       (let ((bounds (bounds-of-thing-at-point thing)))
         (if bounds
             (%search-region (car bounds) (cdr bounds) thing search-function)
             (call-interactively search-function))))
      ((= start end)
       (call-interactively search-function))
      (t
       (funcall search-function (buffer-substring-no-properties start end))))))

;; (if (or (not mark-active) (eql (point) (mark)))
;;     "string"
;;     (buffer-substring-no-properties (min (point) (mark))
;;                                     (max (point) (mark))))


(defparameter *whitespaces* '(32 9 10 13))

(defmacro with-browser-for-apple-documentation (&rest body)
  `(let ((browse-url-browser-function (if (and (eq window-system 'ns)
                                              (eq system-type 'darwin))
                                         'browse-url-generic
                                         'browse-url-firefox2)))
    ,@body))

(defun osx-search (search-string)
  "Search a string with Apple."
  (interactive "sApple Developer Documentation Search: ")
  (with-browser-for-apple-documentation
   (browse-url
    (format "https://developer.apple.com/library/mac/search/?q=%s"
            (browse-url-url-encode-chars
             (string-trim *whitespaces* search-string)
             "[^A-Za-z0-9]")))))

(defun osx-search-region (start end)
  "Search the text in the region with Apple."
  (interactive "r")
  (%search-region start end 'symbol 'osx-search))

;; (debug-on-entry 'browse-url)
(defun ios-search (search-string)
  "Search a string with Apple."
  (interactive "sApple Developer Documentation Search: ")
  (with-browser-for-apple-documentation
      (browse-url
       (format "https://developer.apple.com/library/ios/search/?q=%s"
               (browse-url-url-encode-chars
                (string-trim *whitespaces* search-string)
                "[^A-Za-z0-9]")))))

(defun ios-search-region (start end)
  "Search the text in the region with Apple."
  (interactive "r")
  (%search-region start end 'symbol 'ios-search))


(defun android-search (search-string)
  "Search a string with Android."
  (interactive "sAndroid Developer Documentation Search: ")
  (browse-url (or (when (and (search "." search-string) (not (search ".." search-string)))
                    (let ((words (split-string search-string "\\.")))
                      (when (and (<= 3 (length words))
                                 (every (lambda (word)
                                          (and (alpha-char-p (aref word 0))
                                               (every (function alphanumericp) word)))
                                        
                                        words))
                        (format "http://developer.android.com/reference/%s.html"
                                (mapconcat (function identity) words "/")))))
                  (format "http://developer.android.com/reference/index.html?q=%s"
                          (browse-url-url-encode-chars
                           (string-trim *whitespaces* search-string)
                           "[^A-Za-z0-9]")))))

(defun android-search-region (start end)
  "Search the text in the region with Android."
  (interactive "r")
  (%search-region start end 'symbol 'android-search))

(defun project-search (search-string)
  "Search a regex in the current project (with `find-grep' and `grep-find-command')."
  (interactive "sSearch Project Regexp: ")
  (find-grep (concat grep-find-command " " (shell-quote-argument search-string))))

(defun project-search-region (start end)
  "Search the text in the region in the current project (with `find-grep' and `grep-find-command')."
  (interactive "r")
  (%search-region start end 'symbol 'project-search))


(defun google-search (search-string)
  "Search a string with Google."
  (interactive "sGoogle Search: ")
  (browse-url
   (format "http://www.google.com/search?as_q=%s&num=50&hl=en&ie=ISO8869-1&btnG=Google+Search&as_epq=&as_oq=&as_eq=&lr=&as_ft=i&as_filetype=&as_qdr=all&as_nlo=&as_nhi=&as_occt=any&as_dt=i&as_s
itesearch=&safe=images"
	   (browse-url-url-encode-chars
	    (string-trim *whitespaces* search-string)
	    "[^A-Za-z0-9]")))) 

(defun google-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'google-search))


(defparameter *acronym-search-url* "http://www.acronymfinder.com/%s.html")
;;  "http://www.cygwin.com/acronyms/#%s"
(defun acronym-search (acronym-string)
  (interactive "sAcronym Search: ")
  (browse-url (format *acronym-search-url* acronym-string)))

(defun acronym-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'acronym-search))



(defun includes-search (string)
  (interactive "sIncludes Search: ")
  (find-grep (format "find /usr/include/ /usr/local/include/ -type f -exec grep -n -i %s {} /dev/null \\; #" (shell-quote-argument string))))

(defun includes-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'includes-search))

(defalias 'grep-includes 'includes-search)


(defun hyperspec-search (string)
  (interactive "sHyperspec Search: ")
  (find-grep (format "find '%s' -type f -print|while read f ; do lynx -dump -nolist \"$f\" | grep -i '%s' && echo \"$f:1:-\" ; done #" (shell-quote-argument *hyperspec-path*) string)))

(defun hyperspec-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'hyperspec-search))

(defalias 'grep-hyperspec 'hyperspec-search)


(defun here-search (pattern)
  "Does an egrep  in the current directory just asking for a pattern."
  (interactive (list (read-from-minibuffer (format "In %s egrep pattern: " (shell-quote-argument default-directory)))))
  (check-type pattern string)
  (if (string-equal "" pattern)
      (error "The empty string matches everything. Are you happy?")
      (grep (format "egrep -n -e '%s' `find . -type f -print` /dev/null" pattern))))

(defun here-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'here-search))


(global-set-key (kbd "C-h 0")
                (lambda ()
                  (interactive)
                  (message (format "C-h 1 %s  C-h 2 google  C-h 3 acronym  C-h 4 project  C-h 5 includes  C-h 6 hyperspec  C-h 7 this directory"
                                   (let* ((search (format "%s" (local-key-binding (kbd "C-h 1") t)))
                                          (dash   (search "-" search)))
                                     (if dash
                                         (subseq search 0 dash)
                                         search))))))

;;(global-set-key (kbd "C-h 1") 'android-search-region)
;;(global-set-key (kbd "C-h 1") 'osx-search-region)
(global-set-key (kbd "C-h 1") 'ios-search-region)
(global-set-key (kbd "C-h 2") 'google-search-region)
(global-set-key (kbd "C-h 3") 'acronym-search-region)
(global-set-key (kbd "C-h 4") 'project-search-region)
(global-set-key (kbd "C-h 5") 'includes-search-region)
(global-set-key (kbd "C-h 6") 'hyperspec-search-region)
(global-set-key (kbd "C-h 7") 'here-search-region)
(global-set-key (kbd "C-h 0") 'android-browse-documentation-of-class-at-point)

(defun set-osx-search-region-function ()
  (interactive)
  (local-set-key (kbd "C-h 1") 'osx-search-region))
(defun set-ios-search-region-function ()
  (interactive)
  (local-set-key (kbd "C-h 1") 'ios-search-region))
(defun set-android-search-region-function ()
  (interactive)
  (local-set-key (kbd "C-h 1") 'android-search-region)
  (local-set-key (kbd "C-h 0") 'android-browse-documentation-of-class-at-point))


(add-hook 'objc-mode-hook 'set-osx-search-region-function)
(add-hook 'objc-mode-hook 'set-ios-search-region-function)
(add-hook 'java-mode-hook 'set-android-search-region-function)


;;;----------------------------------------------------------------------------





;;;----------------------------------------------------------------------------
(.EMACS "emacs-uptime")

;;;----------------------------------------------------------------------------
;;; emacs-uptime.el
;;;
;;; Copyright (C) 1998, 2000, 2002, 2004, 2007, 2008 Thien-Thi Nguyen
;;;
;;; This file is part of ttn's personal elisp library, released under
;;; the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.  There is NO WARRANTY.  See file COPYING for details.

;;; Description: Give Emacs' uptime and some other stats in the modeline.

(defvar *emacs-start-time*   (current-time) "For (emacs-uptime);")

;;;###autoload
(defun emacs-uptime ()
  "Gives Emacs' uptime, based on global var `*emacs-start-time*'."
  (interactive)
  (let* ((st *emacs-start-time*)                ; set in do-it-now.el
         (cur (current-time))
         (hi-diff (- (car cur) (car st)))
         (tot-sec (+ (ash hi-diff 16) (- (cadr cur) (cadr st))))
         (days (/ tot-sec (* 60 60 24)))
         (hrs  (/ (- tot-sec (* days 60 60 24)) (* 60 60)))
         (mins (/ (- tot-sec (* days 60 60 24) (* hrs 60 60)) 60))
         (secs (/ (- tot-sec (* days 60 60 24) (* hrs 60 60) (* mins 60)) 1)))
    (message "Up %dd %dh %dm %ds (%s), %d buffers, %d files"
             days hrs mins secs
             (format-time-string "%a %Y-%m-%d %T" st)
             (length (buffer-list))
             (count t (buffer-list)
                    :test-not
                    (lambda (ignore buf)
                      (null (cdr (assoc 'buffer-file-truename
                                        (buffer-local-variables buf)))))))))

(provide 'emacs-uptime)

;;; emacs-uptime.el ends here
;;;----------------------------------------------------------------------------

(defalias 'uptime 'emacs-uptime)

(when (require 'uptimes nil t)
  (defun uptimes-read-uptimes ()
    "Read the uptimes database into `uptimes-last-n' and `uptimes-top-n'."
    (when (file-exists-p uptimes-database) ; doesn't mean the file contains anything
      (with-temp-buffer
        (let ((inhibit-clash-detection t)) ; For the benefit of XEmacs.
          ;; we don't want to visit the file, to avoid locking the file.
          (insert-file-contents uptimes-database nil))
        (setq uptimes-last-n 
              (ignore-errors            ; eat end-of-file errors
                (read (current-buffer))))
        (setq uptimes-top-n
              (or (ignore-errors        ; eat end-of-file errors
                    (read (current-buffer)))
                  uptimes-last-n))))))


;;;----------------------------------------------------------------------------
(.EMACS "Other patches")
(ignore-errors
  (require 'newcomment)
  (defun comment-region-internal (beg end cs ce
                                  &optional ccs cce block lines indent)
    "Comment region BEG..END.
CS and CE are the comment start resp end string.
CCS and CCE are the comment continuation strings for the start resp end
of lines (default to CS and CE).
BLOCK indicates that end of lines should be marked with either CCE, CE or CS
\(if CE is empty) and that those markers should be aligned.
LINES indicates that an extra lines will be used at the beginning and end
of the region for CE and CS.
INDENT indicates to put CS and CCS at the current indentation of the region
rather than at left margin."
    ;;(assert (< beg end))
    (let ((no-empty nil ; PJB: always no-empty.
            ;;  (not (or (eq comment-empty-lines t)
            ;;           (and comment-empty-lines (zerop (length ce)))))
            ))
      ;; Sanitize CE and CCE.
      (if (and (stringp ce) (string= "" ce)) (setq ce nil))
      (if (and (stringp cce) (string= "" cce)) (setq cce nil))
      ;; If CE is empty, multiline cannot be used.
      (unless ce (setq ccs nil cce nil))
      ;; Should we mark empty lines as well ?
      (if (or ccs block lines) (setq no-empty nil))
      ;; Make sure we have end-markers for BLOCK mode.
      (when block (unless ce (setq ce (comment-string-reverse cs))))
      ;; If BLOCK is not requested, we don't need CCE.
      (unless block (setq cce nil))
      ;; Continuation defaults to the same as CS and CE.
      (unless ccs (setq ccs cs cce ce))

      (save-excursion
        (goto-char end)
        ;; If the end is not at the end of a line and the comment-end
        ;; is implicit (i.e. a newline), explicitly insert a newline.
        (unless (or ce (eolp)) (insert "\n") (indent-according-to-mode))
        (comment-with-narrowing
            beg end
          (let ((min-indent (point-max))
                (max-indent 0))
            (goto-char (point-min))
            ;; Quote any nested comment marker
            (comment-quote-nested comment-start comment-end nil)
            
            ;; Loop over all lines to find the needed indentations.
            (goto-char (point-min))
            (while
                (progn
                  (unless (looking-at "[ \t]*$")
                    (setq min-indent (min min-indent (current-indentation))))
                  (end-of-line)
                  (setq max-indent (max max-indent (current-column)))
                  (not (or (eobp) (progn (forward-line) nil)))))

            (setq max-indent
                  (+ max-indent (max (length cs) (length ccs))
                     ;; Inserting ccs can change max-indent by (1- tab-width)
                     ;; but only if there are TABs in the boxed text, of course.
                     (if (save-excursion (goto-char beg)
                                         (search-forward "\t" end t))
                         (1- tab-width) 0)))
            ;; ;; Inserting ccs can change max-indent by (1- tab-width).
            ;; (setq max-indent
            ;;   (+ max-indent (max (length cs) (length ccs)) tab-width -1))
            (unless indent (setq min-indent 0))
            
            ;; make the leading and trailing lines if requested
            (when lines
              (let ((csce
                     (comment-make-extra-lines
                      cs ce ccs cce min-indent max-indent block)))
                (setq cs (car csce))
                (setq ce (cdr csce))))

            (goto-char (point-min))
            ;; Loop over all lines from BEG to END.
            (while
                (progn
                  (unless (and no-empty (looking-at "[ \t]*$"))
                    (move-to-column min-indent t)
                    (insert cs) (setq cs ccs) ;switch to CCS after the first line
                    (end-of-line)
                    (if (eobp) (setq cce ce))
                    (when cce
                      (when block (move-to-column max-indent t))
                      (insert cce)))
                  (end-of-line)
                  (not (or (eobp) (progn (forward-line) nil))))))))))
  );;patch


;;;----------------------------------------------------------------------------
;; (.EMACS "indent buffer on find-file")
(defun pjb-indent-meat ()
  ;; If pjb-indent-meat is not in last position,
  ;; then move it over to last position.
  (let ((p (position (function pjb-indent-meat) find-file-hook)))
    (when (and p (< (1+ p) (length find-file-hook)))
      (setf find-file-hook
            (append (remove (function pjb-indent-meat) find-file-hook)
                    (list (function pjb-indent-meat))))))
  ;; Work only on some modes:
  (when (member major-mode '(lisp-mode common-lisp-mode emacs-lisp-mode
                             perl-mode shell-script-mode
                             scheme-mode c-mode c++-mode objective-c-mode))
    (.EMACS "indenting %S" (buffer-name))
    (if buffer-read-only
        (unwind-protect
             (progn (toggle-read-only)
                    (indent-region (point-min) (point-max))
                    (pop-mark))
          (toggle-read-only))
        (progn (indent-region (point-min) (point-max))
               (pop-mark)))
    (.EMACS "indenting %S done" (buffer-name))
    (set-buffer-modified-p nil)))
;; (setf find-file-hook
;;       (append (remove (function pjb-indent-meat) find-file-hook)
;;               (list (function pjb-indent-meat))))

;; (setf find-file-hook  (remove (function pjb-indent-meat) find-file-hook))


;;;----------------------------------------------------------------------------
(when (require 'vc-fossil nil t)
  (.EMACS "vc")
  (require 'vc-hooks)
  (defadvice vc-registered (around vc-registered/bug-on-empty-string-filename
				   first (file) activate)
    (unless (and (stringp file) (string= "" file))
      ad-do-it))
  (add-to-list 'vc-handled-backends 'Fossil))

;;;----------------------------------------------------------------------------
(.EMACS "darcs")
(load "vc-darcs" t nil)

(defun jump-to-real-file-from-darcs ()
  (interactive)
  (let* ((f (buffer-file-name (current-buffer)))
         (match (string-match "_darcs/current" f)))
    (and f match
         (find-alternate-file
          (concat (substring f 0 (match-beginning 0))
                  (substring f (match-end 0)))))))

(defun warn-if-darcs-file ()
  (let ((f (buffer-file-name (current-buffer))))
    (and f (string-match "_darcs" f)
         (if (y-or-n-p "This is a _darcs file, open the real file? ")
             (jump-to-real-file-from-darcs)
             (push '(:propertize "_DARCS-FILE:" face font-lock-warning-face)
                   mode-line-buffer-identification)))))

(add-hook 'find-file-hooks 'warn-if-darcs-file)


;;;----------------------------------------------------------------------------
(.EMACS "balance windows")

(defun horizontal-offset ()
  "Number of columns taken by the fringe and vertical scroll bar"
  ;; TODO: Implement in function of the effective fringe and vertical scroll bar.
  5)

(defun pjb-balance-windows-vertically ()
  "Make all visible windows the same width (approximately)."
  (interactive)
  (let ((count -1) levels newsizes level-size
        (last-window (previous-window (frame-first-window (selected-frame))))
        ;; Don't count the columns that are past the lowest main window.
        total)
    ;; Rightmost edge of last window determines what size we have to work with.
    (setq total
          (+ (window-width last-window) (horizontal-offset)
             (nth 0 (window-edges last-window))))
    ;; Find all the different hpos's at which windows start,
    ;; then count them.  But ignore levels that differ by only 1.
    (let (lefts (prev-left -2))
      (walk-windows (function (lambda (w)
                      (setq lefts (cons (nth 0 (window-edges w))
                                        lefts))))
                    'nomini)
      (setq lefts (sort lefts '<))
      (while lefts
        (if (> (car lefts) (1+ prev-left))
            (setq prev-left (car lefts)
                  count (1+ count)))
        (setq levels (cons (cons (car lefts) count) levels))
        (setq lefts (cdr lefts)))
      (setq count (1+ count)))
    ;; Subdivide the frame into desired number of vertical levels.
    (setq level-size (/ total count))
    (.EMACS "levels=%S" levels)
    (.EMACS "level-size=%S" level-size)
    (save-selected-window
      ;; Set up NEWSIZES to map windows to their desired sizes.
      ;; If a window ends at the rightmost level, don't include
      ;; it in NEWSIZES.  Those windows get the right sizes
      ;; by adjusting the ones above them.
      (walk-windows (function
                     (lambda (w)
                      (let ((newleft (cdr (assq (nth 0 (window-edges w))
                                                levels)))
                            (newright (cdr (assq (+ (window-width w)
                                                    (horizontal-offset)
                                                    (nth 0 (window-edges w)))
                                                 levels))))
                        (message ".EMACS: newleft=%S newright=%S"
                                 newleft newright)
                        (if newright
                            (setq newsizes
                                  (cons (cons w (* level-size
                                                   (- newright newleft)))
                                        newsizes))))))
                    'nomini)
      (.EMACS "newsizes=%S" newsizes)
      ;; Make walk-windows start with the leftmost window.
      (select-window (previous-window (frame-first-window (selected-frame))))
      (let (done (count 0))
        ;; Give each window its precomputed size, or at least try.
        ;; Keep trying until they all get the intended sizes,
        ;; but not more than 3 times (to prevent infinite loop).
        (while (and (not done) (< count 3))
          (setq done t)
          (setq count (1+ count))
          (walk-windows (function (lambda (w)
                          (select-window w)
                          (let ((newsize (cdr (assq w newsizes))))
                            (when newsize
                              (apply (function enlarge-window)
                                     (- newsize
                                        (horizontal-offset)
                                        (window-width))
                                     t
                                     (if (= 2 (cdr (function-argument-counts
                                                    (function enlarge-window))))
                                         '()
                                         '(preserve)))
                              (unless (= (window-width)
                                         (- newsize (horizontal-offset)))
                                (setq done nil))))))
                        'nomini))))))



(defun pjb-balance-windows (&optional horizontally)
  "Make all visible windows on the current frame the same size (approximately).
If optional prefix arg is not given, \"same size\" is same height.
When prefix arg is given,  \"same size\" is same width."
  (interactive "P")
  (let* (count size w cmjr resize
               (edge (if horizontally 0 1)) ;; Minor field to sort by 0=LEFT, 1=TOP
               (mjr (- 1 edge))             ;; Major field to sort
               (far (+ 2 edge)) ;; far edge (right/bottom) - for current size
               (windows nil)    ;; list of windows
               (ix 0)
               nwin                   ;; number of windows
               (curw (selected-window)) ;; selected window (to return to)
               )
    ;; Build and sort list of all windows on frame
    (save-window-excursion
      (walk-windows (function (lambda (w)
                      (let ((ltrb (window-edges w)))
                        (setq windows (cons (list
                                             (nth mjr  ltrb)
                                             (nth edge ltrb)
                                             (nth far  ltrb)
                                             w) windows)))))
                    'nomini)
      (setq windows (sort windows (lambda (e1 e2)
                                    (if (< (nth 0 e1) (nth 0 e2))
                                        t
                                        (if (= (nth 0 e1) (nth 0 e2))
                                            (if (< (nth 1 e1) (nth 1 e2))
                                                t)))))))
    (setq nwin (length windows))
    ;; add 1 extra entry (for while check)
    (appendf windows '((-1 -1 -1 nil)))

    (while (< ix nwin)                  ; walk on all (sorted) windows
      (setq count ix)         ; count the windows in 1 column (or row)
      (setq cmjr (car (nth ix windows))) ; column / raw identification
      (while (= cmjr (car (nth ix windows)))   ; same column / row
        (setq ix (1+ ix)))                     ; next window
      (setq count (- ix count))
      (if (/= count 1) ; do only if more than one window in this column/row
          (let ((gix (- ix count)))
            (setq size (- (nth far (window-edges (nth 3 (nth (1- ix) windows))))
                          (nth edge (window-edges
                                     (nth 3 (nth (- ix count) windows))))))
            (setq size (/ (+ size count -1) count)) ; average window size

            ;; (.EMACS "Size=%d" size)

            (while (< gix ix)
              (setq w (nth 3 (nth gix windows)))
              (setq resize (- size (- (nth far (window-edges w))
                                      (nth edge (window-edges w)))))

              ;; (.EMACS "Window=%s  resize=%d" w resize)
                                        ; don't resize by 1 character/line
              (if (or (> resize 1)
                      (< resize -1))
                  (progn

                    ;; (sit-for 2)

                    (select-window w)   ; window to work on
                    (apply (function enlarge-window)
                           resize horizontally
                           (if (= 2 (cdr (function-argument-counts
                                          (function enlarge-window))))
                               '()
                               '(preserve)))
                    ;; (sit-for 2)
                    ))
              (setq gix (1+ gix))))))

    ;; (.EMACS "")
    (select-window curw)))


(defun align-cols (start end max-cols)
  "Align text between point and mark as columns.
Columns are separated by whitespace characters.
Prefix arg means align that many columns. (default is all)
Attribution: ?"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
          pos
          end-of-line
          word
          count
          (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
          (pos-list nil)
          (ref-list nil))
      ;; find the positions
      (goto-char start)
      (while (< p end)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (setq pos (current-column))     ;start of first word
        (if (null (car ref-list))
            (setq pos-list (list pos))
            (setq pos-list (list (max pos (car ref-list))))
            (setq ref-list (cdr ref-list)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq word (- (current-column) pos))
          ;; length of next word including following whitespaces
          (setq pos (current-column))
          (if (null (car ref-list))
              (setq pos-list (cons word pos-list))
              (setq pos-list (cons (max word (car ref-list)) pos-list))
              (setq ref-list (cdr ref-list))))
        (while ref-list
          (setq pos-list (cons (car ref-list) pos-list))
          (setq ref-list (cdr ref-list)))
        (setq ref-list (nreverse pos-list))
        (forward-line)
        (setq p (point)))
      ;; align the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start
            (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (goto-char (match-end 0))
        (setq pos (nth count pos-list))
        (while (< (current-column) pos)
          (insert-char ?\040 1))
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq pos   (+  pos (nth count pos-list)))
          (goto-char (match-end 0))
          (while (< (current-column) pos)
            (insert-char ?\040 1))
          (setq end-of-line (save-excursion (end-of-line) (point))))
        (forward-line -1)
        (if (= p (point-min)) (setq p (1- p))
            (setq p (point))))))) ;;align-cols



;;;----------------------------------------------------------------------------
(defun remove-meat-from-all-hook (meat)
  (let ((c 0))
    (do-symbols (s)
      (when (and (boundp s)
                 (listp (symbol-value s))
                 (< 5 (length (symbol-name s)))
                 (string= (subseq (symbol-name s) (- (length (symbol-name s)) 5))
                          "-hook"))
        (set s (remove meat (symbol-value s)))
        (incf c)))
    c))
(remove-meat-from-all-hook 'semantic-default-elisp-setup)
(remove-meat-from-all-hook 'semantic-default-c-setup)
(remove-meat-from-all-hook 'semantic-make)

;;;----------------------------------------------------------------------------
;; (load "/opt/smalltalk-3.0.4/share/emacs/site-lisp/gst-mode.el") 
;; (load "/opt/smalltalk-3.0.4/share/emacs/site-lisp/smalltalk-mode.el") 

;;;----------------------------------------------------------------------------

(defun pjb-compilation-meat ()
  (toggle-truncate-lines +1))

(add-hook 'compilation-mode-hook 'pjb-compilation-meat)

;;;----------------------------------------------------------------------------
(defvar *compile-and-run-cflags*
  (let ((prefix "."))
    (format  "-I%s -L%s" prefix prefix)))

(defun compile-and-run (mode)
  (interactive "p")
  (flet ((name (path)
           (when (string-match "^.*/\\([^./]*\\)\\.[^/.]*$" path)
             (match-string 1 path)))
         (type (path)
           (when (string-match "^.*/[^./]*\\.\\([^/.]*\\)$" path)
             (match-string 1 path))))
    (let* ((src (buffer-file-name (current-buffer)))
           (compiler (or (cdr (assoc* (type src)
                                      '(("c++" . "g++")
                                        ("cpp" . "g++")
                                        ("cxx" . "g++")
                                        ("C" . "g++"))
                                      :test (function string=)))
                         "gcc")))
      ;; (message "src=%S" src)
      ;; (message "exe=%S"  (name src))
      ;; (message "mode=%S" mode)
      (compile
       (format ;; "SRC=%S ; EXE=%S ; cat $SRC ; echo '/*' ; %s %s -g3 -ggdb3 -o ${EXE} ${SRC} && %s ./${EXE} && echo status = $? ; echo '*/'"
        "SRC=%S ; EXE=%S ; %s %s -g3 -ggdb3 -o ${EXE} ${SRC} && %s ./${EXE} && echo status = $?"
        src (name src) compiler *compile-and-run-cflags*
        (cond
          ((equal '(4) mode) "valgrind")
          ((equal '-1  mode) "xterm -hold -e")
          ((equal '-4  mode) "xterm -hold -e valgrind")
          (t                 "")))))))

;;;----------------------------------------------------------------------------
(when (require 'psql-mode nil t)
  (modify-syntax-entry ?/   "<14>" psql-mode-syntax-table)
  (modify-syntax-entry ?*   "<23>" psql-mode-syntax-table)
  
  (modify-syntax-entry ?-   "<12"  psql-mode-syntax-table)
  (modify-syntax-entry ?\n  ">"    psql-mode-syntax-table))

;; A little patch:
(require 'canlock)
(defun canlock-sha1 (message)
  "Make a SHA-1 digest of MESSAGE as a unibyte string of length 20 bytes."
  (let (sha1-maximum-internal-length)
    (sha1 message nil nil )))


(defun alert ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*compilation*"))

(setf (getenv "ANT_ARGS") "")



;; MacOSX / Xcode

(when (require 'gnuserv-compat nil t)
  (autoload 'gnuserv-start "gnuserv-compat"
    "Allow this Emacs process to be a server for client processes." t)
  (gnuserv-start))

(when (eq system-type 'darwin)
  ;; (shell-command "defaults write com.apple.Xcode PBXEmacsPath /opt/local/bin/emacsclient")
  (shell-command "defaults write com.apple.Xcode PBXEmacsPath /usr/bin/true")
  (defun bh-compile ()
    (interactive)
    (let ((df (directory-files "."))
          (has-proj-file nil))
      (while (and df (not has-proj-file))
        (let ((fn (car df)))
          (if (> (length fn) 10)
              (if (string-equal (substring fn -10) ".xcodeproj")
                  (setq has-proj-file t))))
        (setq df (cdr df)))
      (if has-proj-file
          (compile "xcodebuild -configuration Debug")
          (compile "make")))))

;; Delete matlab auto-mode:
(setf auto-mode-alist
      (set-difference auto-mode-alist '(("\\.m$" . matlab-mode)
                                        ("\\.m\\'" . matlab-mode)
                                        ("\\.ml[iyl]?$" . caml-mode)
                                        ("\\.m[mes]\\'" . nroff-mode)
                                        ("\\.m[4c]\\'" . m4-mode))
                      :test (function equalp)))

(appendf auto-mode-alist '(("\\.m$"  . objc-mode)
                           ("\\.mm$" . objc-mode)))


;; find-grep customization:
(setf grep-find-command "find $HOME/src/manager2/trunk \\( -name release -prune \\) -o -type f  \\(  -name \\*.h -o -name \\*.c -name \\*.hh -o -name \\*.hxx -o -name \\*.cc  -o -name \\*.cxx -o -name \\*.lisp -o -name \\*.rb -o -name \\*.logs \\) -print0 | xargs -0 -e grep -niH -e "
      grep-host-defaults-alist nil)
(setf grep-find-command "find $HOME/firms/medicalis/src/amd/subprojects/incident-tracker/sources/siam \\( \\( -name release -o -name .git \\) -prune \\) -o -type f  \\( -name \\*.php -o -name \\*.inc -o -name \\*.txt \\) -print0 | xargs -0  grep -niH -e "
      grep-host-defaults-alist nil)
(setf grep-find-command "find . \\( \\( -name release -o -name .git \\) -prune \\) -o -type f  -print0 | xargs -0  grep -niH -e "
      grep-host-defaults-alist nil)
(setf grep-find-command "find . -name \\*.lisp -print0 | xargs -0  grep -niH -e "
      grep-host-defaults-alist nil)



(defun pwfind (start end)
  (interactive "r")
  (message (format "%S" (list start end)))
  (let ((what (if (and (region-active-p) (< start end))
                  (buffer-substring-no-properties start end)
                  (progn
                    (forward-sexp) (backward-sexp)
                    (thing-at-point-no-properties 'symbol)))))
   (find-grep
    (format "find ~/works/patchwork/src/patchwork/ ~/works/patchwork/src/mcl-unix -name \\*.lisp -print0 | xargs -0  grep -niH -e %S" what)))) 
(global-set-key (kbd "H-/") 'pwfind)

(defun next-day (date)
  "Returns the next day.
DATE: (YYYY MM DD [DOW])
RETURN: (YYYY MM DD DOW)  next day."
  (destructuring-bind (y m d &rest ignored) date
    (declare (ignore ignored))
    (let ((next-cursor-day (calendar-gregorian-from-absolute
                            (1+ (calendar-absolute-from-gregorian  (list m d y))))))
      (destructuring-bind (m d y) next-cursor-day
        
        (list y m d (aref [Do Lu Ma Mi Ju Vi Sa]
                          (calendar-day-of-week next-cursor-day)))))))

(defun* insert-calendar (start-date &optional (count 30))
  "Inserts a calendar from start-date up to count days."
  (destructuring-bind (year month day &rest ignored) start-date
    (declare (ignore ignored))
    (let ((date (next-day (list year month (1- day)))))
      (destructuring-bind (year month day dow) date
        (insert (format "%04d-%02d-%02d %2s : \n" year month day dow))
        (when (eq 'Do dow) (insert "\n"))
        (when (plusp count)
          (insert-calendar (next-day date) (1- count)))))))



;;;----------------------------------------------------------------------------
(defvar *from-regexp*
  "^From [^ ]*@[^ ]*  \\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) [ 0-3][0-9] [ 0-2][0-9]:[0-5][0-9]:[0-5][0-9] [0-9][0-9][0-9][0-9]$")


(defun sfn (&optional hostnamep)
  "Set the name of all frames as \"EMACS\" with a prefix,
or as \"emacs at <hostname>\"."
  (interactive "P")
  (let ((c   (selected-frame))
        (use (if (zerop (user-uid))
                 "root"
                 (or (getenv "EMACS_USE") "emacs"))))
    (unwind-protect
         (dolist (f (frame-list))
           (select-frame f)
           (set-frame-name (if hostnamep
                               (format "%6s at %s" use *hostname*)
                               (string-upcase use))))
      (select-frame c))))

(defun after-make-frame/set-frame-name (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (sfn t)))

(pushnew (quote after-make-frame/set-frame-name) after-make-frame-functions)
(milliways-schedule (lambda () (sfn t)))




(defun current-minor-modes (&optional buffer)
  "The list of the minor modes currently active in the buffer (or current buffer)."
  (let ((result '()))
    (with-current-buffer (or buffer (current-buffer))
      (dolist (mode minor-mode-list result)
        (when (and (boundp mode) (symbol-value mode))
          (push mode result))))))



;;;----------------------------------------------------------------------------
;;; Google Maps
;;;----------------------------------------------------------------------------
(let ((gomapel  (get-directory :share-lisp  "packages/org/naquadah/google-maps/google-maps.el")))
  (when (and gomapel (file-exists-p gomapel))
    (push (get-directory :share-lisp  "packages/org/naquadah/google-maps/") load-path)
    (require 'google-maps)))
;; (google-maps-static-show :center "Valencia"
;;                          :maptype 'hybrid)
;; (google-maps-static-show
;;  :center "Cimetière du Montparnasse"
;;  :maptype 'hybrid
;;  ;; :zoom 5
;;  :markers '((("Place Saint-Michel, Paris") . (:label ?M :color "blue"))
;;             (("Jardin du Luxembourg, Paris" "Parc Montsouris, Paris") . (:label ?P :color "green")))
;;  :visible '("44 rue de l'Ouest, Paris" "Montrouge")
;;  :paths '((("Tour Eiffel, Paris" "Arc de triomphe, Paris" "Panthéon, Paris")
;;            . (:weight 3 :color "black" :fillcolor "yellow"))))


(let ((gowel (get-directory :share-lisp  "packages/org/naquadah/google-weather-el/google-weather.el")))
  (when (and gowel (file-exists-p gowel))
    (push (get-directory :share-lisp  "packages/org/naquadah/google-weather-el/") load-path)
    (require 'google-weather)
    (require 'org-google-weather)))


;;;----------------------------------------------------------------------------

(defun unwrap-google-url (&optional start end)
  (interactive "r")
  (let ((start (or start (min (or (mark)  (point-min)))))
        (end   (let ((m (make-marker)))
                 (set-marker m (or end   (max (or (point) (point-max)))))
                 m)))
    (unwind-protect
         (progn
           (goto-char start)
           (when (search-forward "http://www.google.com/url?url=")
             (delete-region start (match-end 0)))
           (goto-char start)
           (when (search-forward "&")
             (delete-region (match-beginning 0) end))
           (goto-char start)
           (when (search-forward "%3A%2F%2F" end t)
             (delete-region (match-beginning 0) (match-end 0))
             (insert "://")
             (while (search-forward "%2F" end t)
               (delete-region (match-beginning 0) (match-end 0))
               (insert "/"))))
      (set-marker end nil))))


;;;----------------------------------------------------------------------------

(defvar *screen-dump-number* 0)

(defun screen-dump (&optional screen-dump-file)
  (interactive)
  (cond
    (screen-dump-file
     (shell-command (format "xwd | xwdtopnm | pnmtopng > %S"
                            (expand-file-name screen-dump-file))))
    (current-prefix-arg
     (screen-dump (read-from-minibuffer "Screen dump file: "
                                        (format "~/screen-dump-%d.png" (incf *screen-dump-number*)))))
    (t
     (screen-dump (format "~/screen-dump-%d.png" (incf *screen-dump-number*))))))

(global-set-key (kbd "<print>") 'screen-dump)


;;;----------------------------------------------------------------------------

(defun gradient (start end start-color end-color)
  (destructuring-bind (red green blue) start-color
    (destructuring-bind (ered egreen eblue) end-color
      (let* ((count     (coerce (- end    start) 'float))
             (ired   (/ (- ered   red)   count))
             (igreen (/ (- egreen green) count))
             (iblue  (/ (- eblue  blue)  count)))
        (while (< 0 count)
          (add-text-properties start (incf start)
                               `(face (:foreground ,(format "#%02x%02x%02x"
                                                            red green blue))))
          (incf red   ired)
          (incf green igreen)
          (incf blue  iblue)
          (decf       count))))))


(defun rgb (name)
  (let ((entry (assoc name color-name-rgb-alist)))
    (if entry
        (mapcar (lambda (x) (/ x 256.0)) (rest entry))
        '(0 0 0))))

(defun rainbow (start end)
  (interactive "r")
  (let ((range (truncate (- end start) 5)))
    (loop
       for (from to) on (list (rgb "red")
                              (rgb "orange")
                              (rgb "yellow")
                              (rgb "green")
                              (rgb "blue")
                              (rgb "violet"))
       while to
       for start from start           by range
       for next  from (+ start range) by range
       do (gradient start (if to next end) from to))))

(defun rainbow-buffer ()
  (interactive)
  (if buffer-read-only
      (unwind-protect
           (progn
             (toggle-read-only -1)
             (font-lock-mode -1)
             (rainbow (point-min) (point-max)))

        (toggle-read-only +1))
      (progn
        (font-lock-mode -1)
        (rainbow (point-min) (point-max)))))

(defun get-random-color ()
  (first (elt color-name-rgb-alist (random (length color-name-rgb-alist)))))


(defun v+ (a b) (mapcar* (function +) a b))
(defun v- (a b) (mapcar* (function -) a b))
(defun v. (a b) (reduce (function +) (mapcar* (function *) a b)))
(defun *v (n a) (mapcar (lambda (x) (* n x)) a))

(defun get-pair-of-random-colors ()
  (let* ((fn   (get-random-color))
         (bn   (get-random-color))
         (f    (color-name-to-rgb fn))
         (b    (color-name-to-rgb bn)))
    (if (equal f b)
        (get-pair-of-random-colors)
        (mapcar (lambda (rgb)
                  (apply (function color-rgb-to-hex)
                         (mapcar (lambda (x) (min (max 0.0 x) 1.0)) rgb)))
                (let* ((fh (apply (function color-rgb-to-hsl) f))
                       (bh (apply (function color-rgb-to-hsl) b)))
                  (flet ((spread (l d)
                           (list (color-lighten-hsl (first l) (second l) (third l) 80)
                                 (color-darken-hsl  (first d) (second d) (third d) 50))))
                    (if (< (third fh) (third bh))
                        (reverse (spread bh fh))
                        (spread fh bh))))))))


(defun set-random-colors ()
  (interactive)
  (let ((pair (get-pair-of-random-colors)))
    (message "newcolors = %S" pair)
    (mapcar* (function funcall)
             (list (function set-background-color)
                   (function set-foreground-color))
             pair)))

(global-set-key (kbd "<f12>") 'set-random-colors)



(loop for key in (list (kbd "<mouse-5>") (kbd "C-<mouse-5>") (kbd "S-<mouse-5>")
                       (kbd "<mouse-4>") (kbd "C-<mouse-4>") (kbd "S-<mouse-4>"))
     do (global-set-key key 'ignore)) 


(defun toggle-read-only-region (start end)
  (interactive "r")
  (let ((inhibit-read-only t)
        (ro    (not (getf (text-properties-at start) 'read-only))))
    (set-text-properties start end (list 'read-only ro))))


;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
(require 'semantic)
(semantic-mode 1)
(push '(objc-mode . semantic-default-c-setup) semantic-new-buffer-setup-functions)
;;;----------------------------------------------------------------------------

;; (setf inhibit-splash-screen t)
;; (switch-to-buffer (get-buffer-create "emtpy"))
;; (delete-other-windows)

;; To assign windows to specific roles: C-h v split-window-preferred-function

;; (let ((progress-reporter
;;        (make-progress-reporter "Collecting mana for Emacs..."
;;                                0 500)))
;;   (dotimes (k 500)
;;     (sit-for 0.01)
;;     (progress-reporter-update progress-reporter k))
;;   (progress-reporter-done progress-reporter))


;; For long lines:
;; (progn
;;   (fundamental-mode)
;;   (toggle-truncate-lines 1)
;;   (setq-default cache-long-line-scans t))

(defun eval-in-shell-last-command ()
  (interactive "*")
  (let* ((end   (point))
         (start (save-excursion (beginning-of-line) (point)))
         (start (if (re-search-backward shell-prompt-pattern start t)
                    (match-end 0)
                    start)))
    (goto-char end)
    (insert (format "\n| %s\n" (mapconcat (function identity) (split-string (shell-command-to-string (buffer-substring start end))) "\n| ")))
    (set-mark (point))
    (goto-char end)))


(defun len ()
  "Displays the length of the object at point.
For a string or a symbol, the length of the designated string; for a
list or vector, the length of the sequence."
  (interactive)
  (let ((thing (thing-at-point 'sexp)))
    (when thing
      (let* ((object (car (read-from-string thing)))
             (length (typecase object
                       (string (length object))
                       (symbol (length (symbol-name object)))
                       (list   (length object))
                       (vector (length object))
                       (t      -1))))
        (unless (minusp length)
          (message "length: %d" length))))))


;; (pushnew '("/midishare/libraries/.*\\.[hc]$" . iso-8859-1) auto-coding-alist :test (function equal))

;;;; THE END ;;;;
