;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file.
;;;;
;;;; We only run GNU emacs.
;;;;

;; Emacs Makes All Computing Simple.
;; Eine Is Not Emacs.
;; Zwei Was Eine Initially.
;; Drei Ressembled Emacs Intelligently.
;; Vier Integrates Emacs Regexps.
;; Fünf
;; Sechs 
;; Sieben Is Even Better Emacs Now
;; Acht
;; Neun
;; Zehn
;; Hemlock 
;; Climacs Common Lisp Interface Manager Application Creating Sources 
;; Mince Is Not Complete Emacs

;;(when (= (user-uid) 0)
;;  (load "/root/.emacs" pjb:*load-noerror* pjb:*load-silent*)
;;  (error "~/.emacs: Cannot load ~/.emacs under root account."))



;;;----------------------------------------------------------------------------
;;; Message Log
;;;----------------------------------------------------------------------------

(defvar *emacs-start-time*       (current-time) "For (emacs-uptime).")

(defvar *pjb-load-noerror*       t)
(defvar *pjb-load-silent*        nil)
(defvar *pjb-light-emacs*        nil "pjb-loader will load the minimum.")
(defvar *pjb-pvs-is-running*     (and (boundp 'x-resource-name)
                                      (string-equal x-resource-name "pvs")))
(defvar *pjb-save-log-file-p*    nil "Whether .EMACS must save logs to /tmp/messages.txt")

(warn "~/rc/emacs-common.el: Please set the right source-directory.")
(setq source-directory "/usr/src/emacs-23.3/src/")
;; emacs-version "23.4.1"

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


;;;----------------------------------------------------------------------------
;;; Life saver
;;;----------------------------------------------------------------------------
(.EMACS "REQUIRE CL...")
(require 'cl)
(require 'parse-time)
(require 'tramp nil t)
(require 'cc-mode)

(when (boundp 'byte-compile-warning-types)
  (setq byte-compile-warning-types (remove 'cl-functions byte-compile-warning-types)))


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

(defun octal (n)
  "N is a decimal numbers whose digits are taken as octal digits
and converted as such."
  (loop
     for d across (format "%d" n)
     for r = (digit-char-p d) then (+ (* 8 r) (digit-char-p d))
     finally (return r)))

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
  (find-if (function file-exists-p) list-of-files))

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
    (flet ((add-if-good
               (site-lisp)
             (let ((site-lisp (expand-file-name site-lisp)))
               (when (file-exists-p site-lisp)
                 (pushnew site-lisp new-paths)
                 (mapc (lambda (file)
                         (let ((file (concat site-lisp "/" file)))
                           (when (file-exists-p file)
                             (let ((default-directory site-lisp))
                               (.EMACS "%s FOUND" file)
                               (.EMACS "load file = %s " (load file))
                               (.EMACS "load pjb file = %s " (load file *pjb-load-noerror*  *pjb-load-silent*))
                               ))))
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
                             '("~/emacs"))
                            ;; (unless (fboundp 'mdi)
                            ;;   '("~/opt/share/emacs/site-lisp"))
                            ;; (when (string= "mdi-development-1" *hostname*)
                            ;;   '(("~/opt/share/emacs/site-lisp/slime/contribs/")))
                            ;; (when (string= "mdi-development-1" *hostname*)
                            ;;   '(("~/opt/share/emacs/site-lisp/slime/")))
                            ;; (unless (string= "mdi-development-1" *hostname*)
                            ;;   (list
                            ;;    ;; (list (get-directory :share-lisp  "packages/net/common-lisp/projects/slime/slime/"))
                            ;;    ;; (list "/home/pjb/quicklisp/dists/quicklisp/software/slime-20111105-cvs/")
                            ;;    (list (get-directory :share-lisp  "packages/net/mumble/campbell/emacs/"))))
                            ))
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
                    '("/sw/sbin/" "/sw/bin/" "/opt/local/sbin" "/opt/local/bin"))


(load (expand-file-name "~/quicklisp/slime-helper.el") t)
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
     (set-keyboard-coding-system              'iso-8859-1-unix)
     (set-terminal-coding-system              'iso-8859-1-unix)
     (set-clipboard-coding-system             'utf-8-unix)
     (set-selection-coding-system             'utf-8-unix)
     (setq default-buffer-file-coding-system  'utf-8-unix
           default-file-name-coding-system    'utf-8-unix
           default-terminal-coding-system     'iso-8859-1-unix
           default-keyboard-coding-system     'iso-8859-1-unix
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
         (number (parse-integer name (- (length name) 2)))
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


;;;----------------------------------------------------------------------------
(.EMACS "Loading my personal files -- My own stuff.")
(unless (load "pjb-loader.el" t)
  (.EMACS "WARNING WARNING WARNING: Could not find and load 'My own stuff'!"))


;;;----------------------------------------------------------------------------
(.EMACS "setting up fonts")
;; See also:
;; (info "(emacs)Defining Fontsets")

(when (< emacs-major-version 22)
  (require 'font nil t)

  (defun font-spatial-to-canonical (spec &optional device)
    "Convert SPEC (in inches, millimeters, points, or picas) into points"
    ;; 1 in = 6 pa = 25.4 mm = 72 pt
    (cond
      ((numberp spec)
       spec)
      ((null spec)
       nil)
      (t
       (let ((num nil)
             (type nil)
             ;; If for any reason we get null for any of this, default
             ;; to 1024x768 resolution on a 17" screen
             (pix-width (float (or (device-pixel-width device) 1024)))
             (mm-width (float (or (device-mm-width device) 293)))
             (retval nil))
         (cond
           ((string-match "^ *\\([-+*/]\\) *" spec) ; math!  whee!
            (let ((math-func (intern (match-string 1 spec)))
                  (other (font-spatial-to-canonical
                          (substring spec (match-end 0) nil)))
                  (default (font-spatial-to-canonical
                            (font-default-size-for-device device))))
              (if (and default (fboundp math-func))
                  (setq type "px"
                        spec (int-to-string (funcall math-func default other)))
                  (setq type "px"
                        spec (int-to-string other)))))
           ((string-match "[^0-9.]+$" spec)
            (setq type (substring spec (match-beginning 0))
                  spec (substring spec 0 (match-beginning 0))))
           (t
            (setq type "px"
                  spec spec)))
         (setq num (string-to-number spec))
         (cond
           ((member type '("pixel" "px" "pix"))
            (setq retval (* num (/ pix-width mm-width) (/ 25.4 72.0))))
           ((member type '("point" "pt"))
            (setq retval num))
           ((member type '("pica" "pa"))
            (setq retval (* num 12.0)))
           ((member type '("inch" "in"))
            (setq retval (* num 72.0)))
           ((string= type "mm")
            (setq retval (* num (/ 72.0 25.4))))
           ((string= type "cm")
            (setq retval (* num 10 (/ 72.0 25.4))))
           (t
            (setq retval num)))
         retval))))


  (when  (boundp 'x-font-alist)
    ;; Correct the font menu.
    (setf x-font-alist
          (let ((monop (find "monospaced fonts" (rest x-font-alist)
                             :test (function string=)
                             :key (function first))))
            (cons (first x-font-alist)
                  (loop for (a b) on (rest x-font-alist)
                     unless (equalp a b)
                     collect (cond
                               (monop a)
                               ((string= (first a) "proportional fonts")
                                '("monospaced fonts"   nil))
                               ((string= (first a) "non-proportional fonts")
                                '("proportional fonts" nil))
                               (t a)))))))
  );; when emacs-major-version < 23



(defparameter *pjb-font-list*
  '(
    "-sony-fixed-medium-r-normal--16-120-100-100-c-80-iso8859-1"
    
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-17-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-21-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-25-*-*-*-m-0-*-*"
    "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-29-*-*-*-m-0-*-*"

    "-bitstream-terminal-medium-r-normal--18-140-100-100-c-110-iso8859-1"

    "-b&h-lucidatypewriter-medium-r-normal-sans-8-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-10-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-11-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-12-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-bold-r-normal-sans-12-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-14-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-bold-r-normal-sans-14-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-15-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-17-*-*-*-m-*-*-*"

    "-bitstream-courier 10 pitch-medium-r-normal--*-*-*-*-m-*-*-*"
    "-bitstream-courier 10 pitch-medium-r-normal--11-130-*-*-m-*-*-*"
    "-bitstream-courier 10 pitch-medium-r-normal--12-130-*-*-m-*-*-*"
    "-bitstream-courier 10 pitch-medium-r-normal--13-130-*-*-m-*-*-*"
    "-bitstream-courier 10 pitch-medium-r-normal--14-130-*-*-m-*-*-*"
    "-bitstream-courier 10 pitch-medium-r-normal--15-150-*-*-m-*-*-*"
    "-bitstream-courier 10 pitch-medium-r-normal--17-170-*-*-m-*-*-*"
    "-bitstream-courier 10 pitch-medium-r-normal--19-170-*-*-m-*-*-*"


    "-LFP-Bright-normal-normal-normal-*-9-*-*-*-c-60-*-*"
    "-LFP-Smooth-normal-normal-normal-*-9-*-*-*-c-60-*-*"
    "-LFP-LucidaTerminal-normal-normal-normal-*-9-*-*-*-c-90-*-*"
    
    "-LFP-Computer-normal-normal-normal-*-11-*-*-*-c-90-*-*"
    "-LFP-Computer Alt-normal-normal-normal-*-9-*-*-*-c-90-iso10646-1"


    "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-9-*-*-*-m-0-*-*"
    "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-11-*-*-*-m-0-*-*"
    "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-13-*-*-*-m-0-*-*"
    "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-15-*-*-*-m-0-*-*"
    "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-17-*-*-*-m-0-*-*"

    
    "-adobe-courier-medium-r-normal--*-*-*-*-m-*-*-*"
    "-b&h-luxi mono-medium-r-normal--*-*-*-*-m-*-*-*"
    "-ibm-courier-medium-r-normal--*-*-*-*-m-*-*-*"
    "-monotype-courier new-medium-r-normal--*-*-*-*-m-*-*-*"
    "-urw-courier-medium-r-normal--*-*-*-*-m-*-*-*"
    "-urw-nimbus mono l-medium-r-normal--*-*-*-*-m-*-*-*"

    "-Schumacher-Clean-normal-normal-normal-*-12-*-*-*-c-60-*-*"
    
    "-urw-Nimbus Mono L-normal-normal-normal-*-15-*-*-*-m-0-fontset-auto25"
    "-KC-Fixed-normal-normal-normal-*-15-*-*-*-c-80-fontset-auto1"
    "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"


    "-unknown-ArnoldBoecklin-extra-bold-normal-normal-*-16-*-*-*-*-0-*-*"
    "-unknown-Becker-normal-normal-normal-*-16-*-*-*-*-0-*-*"
    "-unknown-Caligula-normal-normal-normal-*-19-*-*-*-*-0-*-*"

    
    "-unknown-Bandal-normal-normal-normal-*-16-*-*-*-*-0-*-*"
    "-unknown-Penguin Attack-normal-normal-normal-*-19-*-*-*-*-0-*-*"
    "-artwiz-glisp-medium-r-normal--11-110-75-75-p-90-*-*"

    "-adobe-courier-medium-r-normal--*-*-*-*-m-*-*-*"
    "-b&h-luxi mono-medium-r-normal--*-*-*-*-m-*-*-*"
    "-ibm-courier-medium-r-normal--*-*-*-*-m-*-*-*"
    "-monotype-courier new-medium-r-normal--*-*-*-*-m-*-*-*"
    "-urw-courier-medium-r-normal--*-*-*-*-m-*-*-*"
    "-urw-nimbus mono l-medium-r-normal--*-*-*-*-m-*-*-*"

    "-urw-Nimbus Mono L-normal-normal-normal-*-15-*-*-*-m-0-fontset-auto25"
    "-KC-Fixed-normal-normal-normal-*-15-*-*-*-c-80-fontset-auto1"
    "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"

    ))

(defvar *pjb-current-font-index* 0)

(defun sign (number)
  (cond ((< number 0) -1)
        ((> number 0) +1)
        (t             0)))

(defun* forward-font (&optional (increment 1))
  (interactive "p")
  (typecase increment 
    (integer
     (let ((increment (if (zerop increment) 1 increment)))
       (setf *pjb-current-font-index* (mod (+ *pjb-current-font-index* increment)
                                           (length *pjb-font-list*)))))
    (string
     (let ((new-index (or (position increment *pjb-font-list*
                                    :test (function string=))
                          0)))
       (setf increment (- new-index *pjb-current-font-index*)
             *pjb-current-font-index* new-index))))
  (loop
     for try below (length *pjb-font-list*)
     do (ignore-errors
          (return
            (progn (set-frame-font (elt *pjb-font-list* *pjb-current-font-index*))
                   (message "Set frame font %S" (elt *pjb-font-list* *pjb-current-font-index*)))))
     do (message "Failed to set frame font %S" (elt *pjb-font-list* *pjb-current-font-index*))
     do (setf *pjb-current-font-index* (mod (+ *pjb-current-font-index* (sign increment))
                                            (length *pjb-font-list*)))))


(global-set-key (kbd "H-<right>") (lambda () (interactive) (forward-font +1)))
(global-set-key (kbd "H-<left>")  (lambda () (interactive) (forward-font -1)))

(global-set-key (kbd "H-<up>")    'backward-same-indent)
(global-set-key (kbd "H-<down>")  'forward-same-indent)

(global-set-key (kbd "H-`")  'next-error)

(defvar *default-font* "fixed")
(ignore-errors (set-frame-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*"))

;; *** Which font backends to use can be specified by the X resource
;; "FontBackend".  For instance, to use both X core fonts and Xft fonts:
;; 
;; Emacs.FontBackend: x,xft
;; 
;; If this resource is not set, Emacs tries to use all font backends
;; available on your graphic device.
;; 
;; *** New frame parameter `font-backend' specifies a list of
;; font-backends supported by the frame's graphic device.  On X, they are
;; currently `x' and `xft'.


;; (when (eq window-system 'x)
;;   (set-frame-font 
;;    (if (fboundp 'font-exists-p)
;;      (cond
;;       ((font-exists-p  "7x13") "7x13")
;;       ((font-exists-p (make-font-pattern :foundry "lispm" :family "fixed"))
;;        (create-fontset-from-fontset-spec
;;         "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-fontset-lispm,
;; ascii:,
;; latin-iso8859-1:-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*,
;; latin-iso8859-15:-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*")
;;        ;; once the fontset has been defined, it can be invoked :
;;        "fontset-lispm")
;;       ((font-exists-p  "lucidasanstypewriter-12") "lucidasanstypewriter-12")
;;       (t *default-font*))
;;      *default-font*))
;;   (when (fboundp 'single-frame) (single-frame)))

;;;----------------------------------------------------------------------------


;;;----------------------------------------------------------------------------
(when (and (not *pjb-pvs-is-running*) (member window-system '(x mac ns)))
  ;; By default turn on colorization.

  ;; ----------------------------------------
  (.EMACS "defining palettes")
  
  (defvar *palettes* '())
  (defvar *current-palette* nil)


  (defstruct palette
    name foreground background cursor region mouse)


  (defmacro defpalette (name foreground background cursor region mouse)
    `(progn
       (defparameter ,name (make-palette :name ',name
                                         :foreground ,foreground
                                         :background ,background
                                         :cursor ,cursor
                                         :region ,region
                                         :mouse ,mouse))
       (pushnew ',name *palettes*)
       (when (eq ',name *current-palette*)
         (set-palette ',name))
       ',name))
  

  (defun set-palette (palette)
    (interactive
     (list (completing-read
            "Palette: "
            (mapcar (lambda (pal) (cons (symbol-name pal) pal)) *palettes*)
            nil  t  nil nil *current-palette*)))
    (typecase palette
      (string (set-palette (intern palette)))
      (symbol (if (boundp palette)
                  (let ((palval (symbol-value palette)))
                    (if (and (palette-p palval) (eq palette (palette-name palval)))
                        (set-palette palval)
                        (error "%S is not a palette name." palette)))
                  (error "%S is not a palette name." palette)))
      (palette
       (setf *current-palette* (palette-name palette))
       (set-default-frame-parameter 'foreground-color (palette-foreground palette))
       (set-default-frame-parameter 'background-color (palette-background palette))
       (set-default-frame-parameter 'cursor-color     (palette-cursor palette))
       (set-default-frame-parameter 'mouse-color      (palette-mouse palette))
       (set-face-background 'region (palette-region palette))
       (when (getenv "EMACS_WM")
         (set-face-background 'border (palette-background palette)))
       (set-foreground-color (palette-foreground palette))
       (set-background-color (palette-background palette))
       (set-face-background 'fringe (palette-background palette))
       (set-cursor-color     (palette-cursor palette))
       (when (fboundp 'set-mouse-color)
         (set-mouse-color     (palette-mouse palette))))
      (otherwise (error "%S is not a palette" palette))))


  (defparameter *turquoise*      "#1abc9c")
  (defparameter *green-sea*      "#16a085")

  (defparameter *emerland*       "#2ecc71")
  (defparameter *nephritis*      "#27ae60")

  (defparameter *peter-river*    "#3498db")
  (defparameter *belize-hole*    "#2980b9")

  (defparameter *amethyst*       "#9b59b6")
  (defparameter *wisteria*       "#8e44ad")

  (defparameter *wet-asphalt*    "#34495e")
  (defparameter *midnight-blue*  "#2c3e50")

  (defparameter *sun-flower*     "#f1c40f")
  (defparameter *orange*         "#f39c12")

  (defparameter *carrot*         "#e67e22")
  (defparameter *pumpkin*        "#d35400")

  (defparameter *alizarin*       "#e74c3c")
  (defparameter *pomegranate*    "#c0392b")

  (defparameter *clouds*         "#ecf0f1")
  (defparameter *silver*         "#bdc3c7")

  (defparameter *concrete*       "#95a5a6")
  (defparameter *asbestos*       "#7f8c8d")

  ;; (apply 'format "#%02x%02x%02x" (mapcar (lambda (x) (* 0.199219 x)) '( 42 203 243)))
  
  ;;          name              foreground     background      cursor   region           mouse
  (defpalette pal-tg            "Black"        *turquoise*     "Red"     *green-sea*     "#444444")
  (defpalette pal-en            "Black"        *emerland*      "Red"     *nephritis*     "#444444")
  (defpalette pal-pb            "Black"        *peter-river*   "Red"     *belize-hole*   "#444444")
  (defpalette pal-aw            "Black"        *amethyst*      "Red"     *wisteria*      "#444444")
  (defpalette pal-wm            "Black"        *wet-asphalt*   "Red"     *midnight-blue* "#444444")
  (defpalette pal-so            "Black"        *sun-flower*    "Red"     *orange*        "#444444")
  (defpalette pal-cp            "Black"        *carrot*        "Red"     *pumpkin*       "#444444")
  (defpalette pal-ap            "Black"        *alizarin*      "Red"     *pomegranate*   "#444444")
  (defpalette pal-cs            "Black"        *clouds*        "Red"     *silver*        "#444444")
  (defpalette pal-ca            "Black"        *concrete*      "Red"     *asbestos*      "#444444")

  (defpalette pal-default       "White"        "Black"         "Red"     "blue3"         "#444444")
  (defpalette pal-white         "#000000"      "#ffffff"       "#555555" "#aaaaaa"       "#444444")
  (defpalette pal-ltgray        "#000000"      "#aaaaaa"       "#ffffff" "#555555"       "#444444")
  (defpalette pal-dkgray        "#ffffff"      "#555555"       "#000000" "#aaaaaa"       "#444444")
  (defpalette pal-black         "#ffffff"      "#000000"       "#aaaaaa" "#555555"       "#444444")
  (defpalette pal-lukhas        "#fff8dc"      "#537182"       "Red"     "#ddd"          "#444444")
  (defpalette pal-thalassa      "MidnightBlue" "#e0f8ff"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-larissa       "DarkOrchid4"  "#f8e8ff"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-lassell       "green yellow" "#08350F"       "yellow"  "#0f0835"       "#444444")
  (defpalette pal-triton        "#929982"      "#2d4e4e"       "cyan"    "#336666"       "#444444")
  (defpalette pal-naiad         "MidnightBlue" "DarkSeaGreen1" "Pink3"   "orchid1"       "#444444")
  (defpalette pal-galatea       "#3080ff"      "#030828"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-galatea-light "#60c0ff"      "#030828"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-green         "green"        "black"         "yellow"  "grey50"        "#444444")
  (defpalette pal-dark          "White"        "#055045"       "yellow"  "grey40"        "#444444")
  (defpalette pal-dark-cyan     "#11eef2"      "black"         "yellow"  "grey80"        "#444444")
  (defpalette pal-dark-blue     "#1199f2"      "black"         "yellow"  "grey80"        "#444444")
  (defpalette pal-dark-amber    "#e0d010"      "black"         "cyan"    "grey40"        "#444444")
  (defpalette pal-dark-galatea  "#60f0c0"      "#0c2040"       "green"   "gray60"        "#444444")
  (defpalette pal-irc           "MidnightBlue" "light yellow"  "blue"    "light green"   "#444444")
  (defpalette pal-stripe        "#a7feff"      "#0a171b"       "Cyan"    "#082830"       "#446688")
  (defpalette pal-stripe1       "#a7feff"      "#0a171b"       "Cyan"    "#105060"       "#446688")
  (defpalette pal-anevia        "white"        "#081040"       "green"   "cadetblue4"    "yellow")
  (defpalette pal-blueprint     "white"        "#392b8d"       "yellow"  "cadetblue4"    "yellow")
  (defpalette pal-blueprint2    "white"        "#06104d"       "yellow"  "cadetblue4"    "yellow")
  (defpalette pal-blueprint3    "white"        "#080635"       "yellow"  "cadetblue4"    "yellow")
  
  (set-palette  pal-default)



  ;; ----------------------------------------
  (.EMACS "set-default-frame-alist")


  (defun set-default-frame-alist (&optional font)
    "Sets default-frame-alist depending on the current environment (host, display, etc)."
    (interactive)
    (let* (
           ;; ---------------------
           (display  (let* ((display (getenv "DISPLAY"))
                            (colon   (and display (string-match ":" display))))
                       (if (or (not display) (zerop colon))
                           system-name
                           (substring display 0 colon))))
           ;; --- default values ---
           (font                 (or font (frame-font)))
           (width                (frame-width))
           (height               (frame-height))
           (top                  1)
           (left                 1)
           (cursor-type            'box)
           (horizontal-scroll-bars 'nil)
           (vertical-scroll-bars   'nil) ; or left or right
           (palette              pal-default)
           (hname                (subseq *hostname* 0 (position (character ".") *hostname*)))
           ;; (name (format "emacs: %s@%s" (user-real-login-name) host-name))
           (name "EMACS")
           ;; ---------------------
           (fringe-background nil))

      (setf default-cursor-type cursor-type)
      (string-case hname

                   (("thalassa" "despina" "kuiper")
                    (forward-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*")
                    (setq palette            pal-thalassa
                          width              81
                          height             70))

                   (("triton" "proteus")
                    (forward-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*")
                    (setq palette            pal-galatea
                          width              86
                          height             52))

                   (("galatea")
                    (forward-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*")
                    (setq palette            pal-blueprint3
                          width              81
                          height             54
                          font   (let ((fixed (make-font-pattern :foundry "Misc"
                                                                 :family "Fixed"
                                                                 :weight "Medium"
                                                                 :slant "R"
                                                                 :width "SemiCondensed"
                                                                 :style ""
                                                                 :pixel-size "13"
                                                                 :point-size "120"
                                                                 :resolution-x "75"
                                                                 :resolution-y "75"
                                                                 :spacing "C"
                                                                 :average-width "60"
                                                                 :registry "ISO8859"
                                                                 :encoding "1")))
                                   (if (and (eq window-system 'x)
                                            (font-exists-p fixed))
                                       fixed
                                       font))))
                   
                   (("larissa") 
                    (setq palette            pal-larissa
                          Width              81
                          height             70))

                   (("naiad")
                    (setq palette            pal-naiad
                          width              81
                          height             54))

                   (("lassell")
                    (setq palette            pal-lassell
                          width              81
                          height             54))

                   (("mini")
                    (setq palette            pal-white
                          width              86
                          height             52))

                   (("mdi-development-1" "mdi-development-2")
                    (setf fringe-background "yellow"))

                   (("simias")
                    (setq palette            pal-anevia)))

      (if (getenv "EMACS_WM")
          (progn
            (setq
             width    140
             height   58
             top      2
             left     2
             font     (make-font-pattern :foundry "Adobe"
                                         :family "Courier"
                                         :weight "Medium"
                                         :slant "R"
                                         :width "Normal"
                                         :style ""
                                         :pixel-size "12"
                                         :point-size "120"
                                         :resolution-x "75"
                                         :resolution-y "75"
                                         :spacing "M"
                                         :average-width "70"
                                         :registry "ISO8859"
                                         :encoding "1"))
            (set-face-background 'border (palette-background palette))
            (shell-command (format "xsetroot -solid %s" (palette-background palette))))
          (setq initial-frame-alist  `((left  . -64))))

      (when (getenv "EMACS_OLD")
        (setq palette            pal-green)
        (setq font               (make-font-pattern :foundry "Adobe"
                                                    :family "Courier"
                                                    :weight "Bold"
                                                    :slant "R"
                                                    :width "Normal"
                                                    :style ""
                                                    :pixel-size "12"
                                                    :point-size "120"
                                                    :resolution-x "75"
                                                    :resolution-y "75"
                                                    :spacing "M"
                                                    :average-width "70"
                                                    :registry "ISO8859")
              background-color "black"
              foreground-color "green"
              region-color     "navyblue"
              cursor-color     "yellow"))

      (when (getenv "EMACS_BG")
        (setq palette (copy-palette palette))
        (setf (palette-background palette) (getenv "EMACS_BG")))

      (when (zerop (user-uid))
        (setq palette (copy-palette palette))
        (setf (palette-foreground palette) "Red"))

      (when (fboundp 'max-frame-line-number)
        (setf height (- (max-frame-line-number (car (frame-list))) 2)))

      (setq default-frame-alist
            `(
              (tool-bar-lines       . 0)
              (menu-bar-lines       . 0) ;; window-system 'mac
              (font                 . ,font)
              ,@(unless (getenv "RATPOISON")
                        `((width                . ,width)
                          (height               . ,height)
                          (top                  . ,top)
                          (left                 . ,left)))
              (cursor-type          . ,cursor-type)
              (cursor-color         . ,(palette-cursor palette))
              (mouse-color          . ,(palette-mouse palette))
              (foreground-color     . ,(palette-foreground palette))
              (background-color     . ,(palette-background palette))
              (vertical-scroll-bars . ,vertical-scroll-bars)
              (name                 . ,name)))

      (when (and (string= "21.3.1" emacs-version)
                 (not (getenv "EMACS_WM"))
                 (not (getenv "RATPOISON")))
        (set-frame-position (car (frame-list)) -64 top)
        (set-frame-size     (car (frame-list)) width height)
        (setq frame-initial-frame nil))

      (set-face-background 'region (palette-region palette))
      (set-palette palette)
      (unless (fboundp 'mdi)
        (when (facep 'fringe)
          (if fringe-background
              (set-face-background 'fringe fringe-background)
              (set-face-background 'fringe (palette-background palette)))))
      (set-frame-name name)
      (when (zerop (user-uid))
        (set-foreground-color "Red"))))

  
  ;; (set-default-frame-alist *default-font*)
  ;; (.EMACS "set-default-frame-alist done")
  )


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
(.EMACS "eshell")
(unless (featurep 'eshell-auto)
  (load "eshell-auto" *pjb-load-noerror* *pjb-load-silent*))
(defun pjb-eshell-load-meat ()
  (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  ;; (defun string (&rest chars)
  ;;   (do ((s (make-string (length chars) 0))
  ;;        (ch chars (cdr ch))
  ;;        (i 0 (1+ i)))
  ;;       ((null ch) s)
  ;;     (setf (aref s i) (car ch))))
  )
(add-hook 'eshell-load-hook (function pjb-eshell-load-meat))




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
  (insert-char (upcase last-command-char) n))

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
(.EMACS "INFERIOR LISP")

(when (require 'slime nil t)
  
  (defun slime-eval-print (string)
    "Eval STRING in Lisp; insert any output and the result at point."
    (message "current-prefix-arg = %S" current-prefix-arg)
    (let ((commentp (and (listp current-prefix-arg)
                         (integerp (first current-prefix-arg))
                         (< 4 (first current-prefix-arg)))))
      (slime-eval-async `(swank:eval-and-grab-output ,string)
        `(lambda (result)
           (destructuring-bind (output value) result
             (push-mark)
             (if ,commentp
                 (progn
                   (insert output)
                   (let ((lines (split-string value "\n")))
                     (insert "\n;; --> " (pop lines) "\n")
                     (dolist (line lines)
                       (insert ";;     " line "\n"))))
                 (insert output value)))))))

  (or (ignore-errors
        (progn (slime-setup '(slime-fancy
                              slime-xref-browser
                              slime-asdf
                              slime-banner
                              slime-repl
                              slime-indentation
                              slime-fuzzy
                              slime-autodoc
                              slime-presentations
                              slime-presentation-streams))
               (setf slime-complete-symbol*-fancy   t
                     slime-complete-symbol-function 'slime-fuzzy-complete-symbol
                     slime-load-failed-fasl         'never)
               t))
      (ignore-errors
        (progn (slime-setup '(slime-fancy slime-indentation))
               t))
      (ignore-errors
        (progn (slime-setup :autodoc t :typeout-frame t :highlight-edits t)
               t))
      (ignore-errors
        (progn (slime-setup)
               t))
      (error ".EMACS: Cannot setup slime :-("))

  (setf slime-net-coding-system 'utf-8-unix)
  (setf slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
  (pushnew 'paredit-mode slime-repl-mode-hook)



  (.EMACS " define-lisp-implementation")
  (defvar *lisp-implementations* '() "List of defined lisp implementations.")
  (defvar *default-lisp-implementation* nil "The default lisp implementation.")
  (defvar slime-lisp-implementations    nil)
  (defvar lisp-implementation nil
    "Buffer local variable indicating what lisp-implementation is used here.")


  (defstruct lisp-implementation
    name command prompt coding
    (function-documentation-command
     "(let ((fn '%s))
     (format t \"Documentation for ~a:~&~a\"
	     fn (documentation fn 'function))
     (values))\n")
    (variable-documentation-command
     "(let ((v '%s))
     (format t \"Documentation for ~a:~&~a\"
	     v (documentation v 'variable))
     (values))\n")
    (argument-list-command
     "(let ((fn '%s))
     (format t \"Arglist for ~a: ~a\" fn (arglist fn))
     (values))\n")
    (describe-symbol-command "(describe '%s)\n")
    (init 'slime-init-command))


  (defmacro define-lisp-implementation (name commands-expression prompt coding &rest rest)
    `(let ((command (first-existing-file (mapcar (lambda (cmd)
                                                   (if (listp cmd)
                                                       (first cmd)
                                                       cmd))
                                                 ,commands-expression))))
       (if command
           (let* ((command (ensure-list command))
                  (li (make-lisp-implementation
                       :name     ',name
                       :command  (mapconcat (function identity) command " ")
                       :prompt   ,prompt
                       :coding  ',coding
                       ,@rest))
                  (sli (assoc ',name slime-lisp-implementations)))
             (setf (get ',name :lisp-implementation) li)
             (pushnew ',name *lisp-implementations*)
             (if (null sli)
                 (push (list ',name command
                             :coding-system  (intern (format "%s-unix" ',coding))
                             :init (lisp-implementation-init li))
                       slime-lisp-implementations)
                 (setf (cdr sli)
                       (list command
                             :coding-system (intern (format "%s-unix" ',coding))
                             :init (lisp-implementation-init li))))
             ',name)
           (warn "No executable for lisp implementation: %s" ',name))))


  (define-lisp-implementation scheme
      '("mzscheme")
    "^> "
    iso-8859-1)


  (define-lisp-implementation mzscheme
      '("mzscheme")
    "^> "
    iso-8859-1)

  (define-lisp-implementation mit-scheme
      '("/usr/local/languages/mit-scheme/bin/scheme")
    "^\[[0-9]*\]> "
    iso-8859-1)

  (define-lisp-implementation umb-scheme
      '("/usr/bin/scheme")
    "^==> "
    iso-8859-1)


  
  (define-lisp-implementation abcl
      '("/data/languages/abcl/abcl")
    "^.*([0-9]+): "
    iso-8859-1)
  
  (define-lisp-implementation allegro
      '("/data/languages/acl82express/alisp")
    "^\[[0-9]*\]> "
    iso-8859-1)

  (define-lisp-implementation ccl
      '("/data/languages/ccl/bin/ccl"
        "/usr/local/bin/ccl"
        "/opt/local/bin/ccl"
        "/usr/bin/ccl")
    "^? "
    utf-8)



  (defun windoize-pathname (path)
    ;; "/home/pjb/quicklisp/dists/quicklisp/software/slime-20120208-cvs/swank-loader.lisp"
    (let ((home (expand-file-name "~/")))
      (if (prefixp home path)
          (format "HOME:%s"      (substitute (character ";") (character "/") (subseq path (length home))))
          (format "C:\\cygwin%s" (substitute (character "\\") (character "/") path)))))
  
  (defun slime-init-ccl-win-cygwin (port-filename coding-system)
    "Return a string to initialize Lisp."
    (let ((loader (if (file-name-absolute-p slime-backend)
                      slime-backend
                      (concat slime-path slime-backend))))
      ;; Return a single form to avoid problems with buffered input.
      (format "%S\n\n"
              `(progn
                 (load ,(windoize-pathname (expand-file-name loader)) 
                       :verbose t)
                 (funcall (read-from-string "swank-loader:init"))
                 (funcall (read-from-string "swank:start-server")
                          ,(windoize-pathname port-filename))))))

  (define-lisp-implementation ccl-win-cygwin
      ;; This is a Windows CCL run thru cygwin emacs…
      '("/usr/local/bin/ccl")
    "^? "
    utf-8
    :init  'slime-init-ccl-win-cygwin)


  
  (define-lisp-implementation openmcl
      '("/usr/local/bin/openmcl")
    "^\[[0-9]*\]> "
    iso-8859-1)


  (define-lisp-implementation clisp
      (list* (cond
               ((eq system-type 'cygwin)  "/usr/bin/clisp")
               (t  (first-existing-file '("/data/languages/clisp/bin/clisp"
                                          "/opt/local/bin/clisp"
                                          "/usr/local/bin/clisp"
                                          "/opt/clisp-2.41-pjb1-regexp/bin/clisp"
                                          "/usr/bin/clisp"))))
             "-ansi""-q";"-m""32M""-I""-K""full"
             (cond
               ((eq system-type 'darwin)
                (list "-Efile"     "UTF-8"
                      "-Epathname" "UTF-8"
                      "-Eterminal" "UTF-8"
                      "-Emisc"     "UTF-8" ; better be same as terminal
                      "-Eforeign"  "ISO-8859-1")) ; must be 1-1.
               (t
                (list "-Efile"     "UTF-8"
                      "-Epathname" "ISO-8859-1"
                      "-Eterminal" "UTF-8"
                      "-Emisc"     "UTF-8" ; better be same as terminal
                      "-Eforeign"  "ISO-8859-1")))) ; must be 1-1.
    "^\[[0-9]*\]> "
    utf-8
    :argument-list-command
    "(let ((fn '%s))
     (cond
       ((not (fboundp fn))      (format t \"~A is not a function\" fn))
       ((special-operator-p fn) (format t \"~A is a special operator\" fn))
       ((macro-function fn)     (format t \"~A is a macro\" fn))
       (t  (format t \"Arglist for ~a: ~a\" fn (ext:arglist fn))))
     (values))\n")

  ;; (lisp-implementation-coding(get 'clisp :lisp-implementation))
  ;; utf-8
  ;; slime-net-coding-system

  (define-lisp-implementation cmucl
      '("/data/languages/cmucl/bin/lisp"
        "/usr/local/bin/lisp"
        "/opt/local/bin/lisp"
        "/usr/bin/lisp")
    "^\* "
    utf-8)

  
  (define-lisp-implementation sbcl
      (mapcar (lambda (cmd) (list cmd "--noinform"))
              '("/data/languages/sbcl/bin/sbcl" 
                "/usr/local/bin/sbcl" 
                "/opt/local/bin/sbcl"
                "/usr/bin/sbcl"))
    "^\[[0-9]*\]> "
    utf-8)


  (define-lisp-implementation ecl
      '("/data/languages/ecl/bin/ecl"
        "/usr/local/bin/ecl"
        "/opt/local/bin/ecl"
        "/usr/bin/ecl")
    "^> "
    utf-8)

  
  (define-lisp-implementation gcl
      '("/data/languages/gcl/bin/gcl"
        "/usr/local/bin/gcl"
        "/opt/local/bin/gcl"
        "/usr/bin/gcl")
    "^> "
    utf-8)

  

  


  (defun set-inferior-lisp-implementation (impl)
    "Set the default lisp implementation used by inferior-lisp and slime."
    (interactive "SImplementation: ")
    (when (member impl *lisp-implementations*)
      (let ((limpl (get impl :lisp-implementation)))
        (if limpl
            (progn
              (message ".EMACS: inferior-lisp implementation: %s"
                       (lisp-implementation-name limpl))
              (let ((coding (lisp-implementation-coding limpl)))
                (setf *default-lisp-implementation* limpl
                      inferior-lisp-program         (lisp-implementation-command limpl)
                      inferior-lisp-prompt          (lisp-implementation-prompt limpl)
                      lisp-function-doc-command     (lisp-implementation-function-documentation-command limpl)
                      lisp-var-doc-command          (lisp-implementation-variable-documentation-command limpl)
                      lisp-arglist-command          (lisp-implementation-argument-list-command limpl)
                      lisp-describe-sym-command     (lisp-implementation-describe-symbol-command limpl)
                      default-process-coding-system (cons coding coding)
                      slime-net-coding-system       (intern (format "%s-unix" coding))
                      slime-default-lisp            impl)))
            (error "%S not a lisp implementation." impl))
        impl)))

  (defalias 'set-default-lisp-implementation 'set-inferior-lisp-implementation)

  (defun set-inferior-lisp-buffer (buffer-name)
    (interactive "bInferior Lisp Buffer: ")
    (make-local-variable 'inferior-lisp-buffer)
    (make-local-variable 'lisp-function-doc-command)
    (make-local-variable 'lisp-var-doc-command)
    (make-local-variable 'lisp-arglist-command)
    (make-local-variable 'lisp-describe-sym-command)
    (make-local-variable 'lisp-implementation)
    (setf inferior-lisp-buffer buffer-name
          lisp-implementation  (or (buffer-local-value 'lisp-implementation
                                                       (get-buffer inferior-lisp-buffer))
                                   (lisp-implementation-name *default-lisp-implementation*)))
    (let ((limpl (get lisp-implementation :lisp-implementation)))
      (when limpl
        (setf lisp-function-doc-command (lisp-implementation-function-documentation-command limpl)
              lisp-var-doc-command      (lisp-implementation-variable-documentation-command limpl)
              lisp-arglist-command      (lisp-implementation-argument-list-command limpl)
              lisp-describe-sym-command (lisp-implementation-describe-symbol-command limpl)))))


  ;; Used both by ilisp and slime:
  ;; (case system-type
  ;;   ((darwin)     (set-default-lisp-implementation 'clisp)) ; openmcl))
  ;;   ((gnu/linux)  (set-default-lisp-implementation 'clisp)) ; sbcl))
  ;;   ((cygwin)     (set-default-lisp-implementation 'clisp))
  ;;   (otherwise    (warn "unexpected system-type for inferior-lisp-program")
  ;;                 (set-default-lisp-implementation 'clisp)))

  (loop
     for impl in '(ccl clisp sbcl ecl abcl)
     when (set-default-lisp-implementation impl)
     do (progn
          (message "Default Lisp implementations is %s" impl)
          (return impl)))
  
  (defun %lisp-buffer-name (n impl) (format "%dlisp-%s" n impl))
  (defun %lisp-buffer-name-match-p (buffer-name &optional number)
    (string-match (if number (format "^%dlisp" number) "^[0-9]+lisp") buffer-name))
  (defun %lisp-buffer-name-number (buffer-name)
    (when (string-match "^\\([0-9]+\\)lisp" buffer-name)
      (parse-integer (match-string 1 buffer-name))))
  (defun inferior-lisp-buffers-list ()
    "RETURN: a list of the inferior-lisp buffers."
    (delete-if (lambda (name) (not (%lisp-buffer-name-match-p name)))
               (mapcar (function buffer-name) (buffer-list))))
  (defun %lisp-buffer-next-number ()
    (loop
       with i = 0
       with numbers = (sort (mapcar (function  %lisp-buffer-name-number)
                                    (inferior-lisp-buffers-list))
                            (function <=))
       while numbers
       do (if (= i (car numbers))
              (progn (incf i) (pop numbers))
              (return i))
       finally (return i)))

  (defvar *lisp-command-history* '())

  (defun inferior-lisp-other-window (cmd)
    "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
    (interactive (list (if current-prefix-arg
                           (read-string "Run lisp: " inferior-lisp-program)
                           inferior-lisp-program)))
    (if (not (comint-check-proc "*inferior-lisp*"))
        (let ((cmdlist (split-string cmd)))
          (set-buffer (apply (function make-comint)
                             "inferior-lisp" (car cmdlist) nil (cdr cmdlist)))
          (inferior-lisp-mode)))
    (setq inferior-lisp-buffer "*inferior-lisp*")
    (pop-to-buffer "*inferior-lisp*" t))


  (defun nlisp (&optional ask-command)
    "Create a new inferior-lisp buffer."
    (interactive "P")
    (let* ((impl-or-cmd
            (if ask-command
                (read-from-minibuffer
                 "Lisp implementation or command: "
                 (format "%s" (lisp-implementation-name
                               *default-lisp-implementation*))
                 nil nil '*lisp-command-history*)
                (format "%s" (lisp-implementation-name
                              *default-lisp-implementation*))))
           (impl  (unless (position (character " ") impl-or-cmd
                                    :test (function char=))
                    (intern-soft impl-or-cmd)))
           (limpl (and impl (get impl :lisp-implementation)))
           (cmd   (if limpl (lisp-implementation-command limpl) impl-or-cmd)))
      (inferior-lisp-other-window cmd)
      (make-local-variable 'lisp-implementation)
      (setf lisp-implementation
            (or impl (lisp-implementation-name *default-lisp-implementation*)))
      (rename-buffer
       (setf inferior-lisp-buffer
             (%lisp-buffer-name
              (%lisp-buffer-next-number)
              (cond
                (impl)
                ((string= cmd (lisp-implementation-command
                               *default-lisp-implementation*))
                 (lisp-implementation-name *default-lisp-implementation*))
                ('custom)))))))


  (defun lisp (&optional ask-command)
    "Create a new inferior-lisp when none exist,
   or switch to the last created one."
    (interactive "P")
    (if (and (boundp 'inferior-lisp-buffer) inferior-lisp-buffer
             (get-buffer inferior-lisp-buffer))
        (switch-to-buffer inferior-lisp-buffer)
        (let ((lisp-buffers (inferior-lisp-buffers-list)))
          (if lisp-buffers
              (switch-to-buffer
               (setf inferior-lisp-buffer (first lisp-buffers)))
              (nlisp ask-command))))))


(defvar package 'common-lisp-user)

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

;; (symbol-value-in-buffer 'inferior-lisp-buffer "a.lisp")
;; (local-variable-p 'package)
;; (inferior-lisp-package)
;; (local-variable-p 'package)

;; Interfers with slime:
;;
;; (defun inferior-lisp-buffer (&optional process)
;;   (if (boundp 'inferior-lisp-buffer)
;;       inferior-lisp-buffer
;;       (process-buffer (or process (inferior-lisp-proc)))))
;; 
;; (defun inferior-lisp-package (&optional process)
;;   (symbol-value-in-buffer 'package (inferior-lisp-buffer process)))
;; 
;; ;; (defun lisp-eval-region (start end &optional and-go)
;; ;;   "Send the current region to the inferior Lisp process.
;; ;; Prefix argument means switch to the Lisp buffer afterwards."
;; ;;   (interactive "r\nP")
;; ;;   (comint-send-region (inferior-lisp-proc) start end)
;; ;;   (comint-send-string (inferior-lisp-proc) "\n")
;; ;;   (if and-go (switch-to-lisp t)))
;; 
;; (defadvice lisp-eval-region (before ler-in-package activate) 
;;   (when (and (boundp 'package) (not (eq package (inferior-lisp-package))))
;;     (comint-send-string (inferior-lisp-proc)
;;                         (upcase (format "(CL:IN-PACKAGE #:%s)\n" package)))
;;     (setf (symbol-value-in-buffer 'package (inferior-lisp-buffer)) package)))


(defun lisp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))


(appendf interpreter-mode-alist '(("sbcl" . lisp-mode)
                                  ("abcl" . lisp-mode)
                                  ("gcl" . lisp-mode)
                                  ("ecl" . lisp-mode)
                                  ("cmucl" . lisp-mode)
                                  ("alisp" . lisp-mode)))

(appendf auto-mode-alist '(("\\.lisp$" . lisp-mode)
                           ("\\.fas$"  . lisp-mode)
                           ("\\.lsp$"  . lisp-mode)
                           ("\\.cl$"   . lisp-mode)
                           ("\\.acl2$" . lisp-mode)
                           ("\\.LISP$" . lisp-mode)
                           ("\\.FAS$"  . lisp-mode)
                           ("\\.LSP$"  . lisp-mode)
                           ("\\.CL$"   . lisp-mode)
                           ("\\.ACL2$" . lisp-mode)))

(appendf auto-mode-alist '(("\\.scm$"    . scheme-mode)
                           ("\\.ss$"     . scheme-mode)
                           ("\\.stk$"    . scheme-mode)
                           ("\\.stklos$" . scheme-mode)))

(appendf auto-mode-alist '(("\\.jmf$"    . java-mode)
                           ("\\.j$"      . java-mode)))

(appendf auto-mode-alist '(("\\.pl1$"    . pl1-mode)))

(appendf auto-mode-alist '(("\\.html\\.in$"  . html-mode)))


(defun pjb-show-lisp-repl (jump-in)
  "Switches to a repl buffer, depending on the major mode and what's available.
If `jump-in' is true (ie. a prefix is given), we switch to the repl too."
  (interactive "P")
  (labels ((show-buffer (buffer)
             (delete-other-windows)
             (split-window-horizontally)
             (other-window 1)
             (etypecase buffer
               (buffer (switch-to-buffer buffer))
               (function (funcall buffer)))
             (unless jump-in (other-window 1)))
           (inferior-lisp-repl ()
             (when inferior-lisp-buffer
               (let ((lisp-buffer (get-buffer inferior-lisp-buffer)))
                 (when lisp-buffer
                   (show-buffer lisp-buffer)
                   (return-from inferior-lisp-repl))))
             ;; No inferior-lisp buffer, let's start a lisp.
             (if (fboundp 'slime)
                 (show-buffer (function slime))
                 (show-buffer (function inferior-lisp)))))
    (case major-mode
      ((emacs-lisp-mode)
       (show-buffer (function ielm)))
      ((lisp-mode)
       (if (and (boundp 'slime-mode) slime-mode)
           (show-buffer (function slime-switch-to-output-buffer))
           (inferior-lisp-repl)))
      ((slime-repl-mode inferior-emacs-lisp-mode)
       (message "Already there."))
      (t
       (inferior-lisp-repl)))))

(defun indent-defun ()
  (interactive)
  (save-excursion
    (indent-region (progn (beginning-of-defun) (point))
                   (progn (end-of-defun) (point)))))

(defun pjb-lisp-comment-region (beg end &optional arg)
  (let* ((numarg (prefix-numeric-value arg))
         (style (cdr (assoc comment-style comment-styles)))
         (lines (nth 2 style))
         (block (nth 1 style))
         (multi (nth 0 style)))
    ;; we use `chars' instead of `syntax' because `\n' might be
    ;; of end-comment syntax rather than of whitespace syntax.
    ;; sanitize BEG and END
    (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
    (setq beg (max beg (point)))
    (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
    (setq end (min end (point)))
    (if (>= beg end) (error "Nothing to comment"))

    ;; sanitize LINES
    (setq lines
          (and
           lines ;; multi
           (progn (goto-char beg) (beginning-of-line)
                  (skip-syntax-forward " ")
                  (>= (point) beg))
           (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
                  (<= (point) end))
           (or block (not (string= "" comment-end)))
           (or block (progn (goto-char beg) (search-forward "\n" end t)))))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
      ((consp arg) (uncomment-region beg end))
      ((< numarg 0) (uncomment-region beg end (- numarg)))
      (t
       (setq numarg (comment-add arg))
       (comment-region-internal
        beg end
        (let ((s (comment-padright comment-start numarg)))
          (if (string-match comment-start-skip s) s
              (comment-padright comment-start)))
        (let ((s (comment-padleft comment-end numarg)))
          (and s (if (string-match comment-end-skip s) s
                     (comment-padright comment-end))))
        (if multi (comment-padright comment-continue numarg))
        (if multi
            (comment-padleft (comment-string-reverse comment-continue) numarg))
        block
        lines
        (nth 3 style))))))


(defun paredit-beginning-of-toplevel-form ()
  (interactive)
  (ignore-errors (paredit-backward-up 1000)))

(defun paredit-end-of-toplevel-form ()
  (interactive)
  (ignore-errors (paredit-forward-up 1000)))


(defun pjb-lisp-meat ()
  (interactive)
  (.EMACS "pjb-lisp-meat on %S starts" (buffer-name))
  (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (local-set-key (kbd "RET")  'newline-and-indent)
  ;; (local-set-key (kbd "RET") 'indent-defun)
  ;; (setq blink-matching-paren t)
  (setf skeleton-pair         nil
        comint-process-echoes nil)
  (setf comment-style 'indent)
  ;; (setf comment-region-function 'pjb-lisp-comment-region)
  (local-set-key (kbd "<A-up>")      'backward-up-list)
  (local-set-key (kbd "<A-down>")    'down-list)
  (paredit-mode +1)
  (local-set-key (kbd "s-A-<left>")  'paredit-backward-barf-sexp)
  (local-set-key (kbd "s-A-<right>") 'paredit-backward-slurp-sexp)
  (local-set-key (kbd "A-<right>")   'paredit-forward-slurp-sexp)
  (local-set-key (kbd "A-<left>")    'paredit-forward-barf-sexp)
  (local-set-key (kbd "A-s")         'paredit-backward-barf-sexp)
  (local-set-key (kbd "A-d")         'paredit-backward-slurp-sexp)
  (local-set-key (kbd "A-f")         'paredit-forward-slurp-sexp)
  (local-set-key (kbd "A-g")         'paredit-forward-barf-sexp)
  (local-set-key (kbd "C-M-U")       'paredit-beginning-of-toplevel-form)
  (local-set-key (kbd "C-M-N")       'paredit-end-of-toplevel-form)
  (local-set-key (kbd "C-M-<backspace>") 'backwards-kill-sexp)
  (local-set-key (kbd "C-x C-r g")   'redshank-make-defgeneric-from-defmethod)
  (local-set-key (kbd "H-e")         'pjb-cl-export-definition-at-point)
  (local-set-key (kbd "H-s")         'pjb-cl-export-symbol-at-point)
  ;;   (setq skeleton-pair t)
  ;;   (local-set-key "("  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "["  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "{"  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "|"  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "\"" 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "M-[") 'paredit-wrap-square)
  (local-set-key (kbd "M-{") 'paredit-wrap-curly)
  (modify-syntax-entry ?\[ "()" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")(" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "()" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} ")(" lisp-mode-syntax-table)
  (when (fboundp 'column-marker-1) (column-marker-1 80))
  (add-hook 'comint-preoutput-filter-functions (function pjb-comint-preoutput-insert-image))
  (font-lock-add-keywords nil '(("\\<[Rr][Kk]:\\sw\\sw+\\>" 0 font-lock-builtin-face)
                                ("(\\(\\<[-A-Za-z0-9]+-define-[-A-Za-z0-9]+\\>\\)" 1 font-lock-keyword)))
  (.EMACS "pjb-lisp-meat on %S done" (buffer-name))
  (values))




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

(defun ensure-list (x) (if (listp x) x (list x)))
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




;; (load-library "cl")
;; (setq indent-region-function (function lisp-indent-function))
;; (setq lisp-indent-function   (function common-lisp-indent-function))
;; (setq lisp-indent-function   (function lisp-indent-function))
;; (put 'let  'lisp-indent-function        '(&lambda &body))
;; (put 'let* 'lisp-indent-function        '(&lambda &body))
;; (put 'let  'common-lisp-indent-function '(&lambda &body))
;; (put 'let* 'common-lisp-indent-function '(&lambda &body))


;; (defun test (var var var
;;                  var var var)
;;   body)

;; (let (var var var
;;           var
;;           var)
;;   var)

;; (TRACE lisp-indent-function common-lisp-indent-function
;;        common-lisp-indent-function-1)

;; (setq lisp-simple-loop-indentation   1
;;       lisp-loop-keyword-indentation  6
;;       lisp-loop-forms-indentation    6)


;; M-( insert-parentheses
;; M-) move-past-close-and-reindent

(defun sexp-diff (s1 s2)
  (cond
    ((and (atom s1) (atom s2))
     (if (equal s1 s2)
         s1
         (list :DIFFERENCE s1 s2)))
    ((and (consp s1) (consp s2))
     (cons (sexp-diff (car s1) (car s2))
           (sexp-diff (cdr s1) (cdr s2))))
    (t
     (list :DIFFERENCE s1 s2))))

(defun collect-sexps (count)
  (when (< count 0)
    (setf count (- count))
    (backward-sexp count))
  (forward-sexp)
  (backward-sexp)
  (let ((b (point))
        (e (point))
        (l '()))
    (dotimes (n count)
      (forward-sexp)
      (backward-sexp)
      (push (sexp-at-point) l)
      (forward-sexp)
      (setf e (point)))
    (list b e (nreverse l))))

(defun ex-parenthese (&optional arg)
  (interactive "p")
  (let ((bel (collect-sexps arg)))
    (when bel
      (goto-char (first bel))
      (dolist (sexp (third bel))
        (forward-sexp)
        (when (listp sexp)
          (let ((e (point)))
            (backward-sexp)
            (delete-char 1)
            (goto-char (- e 1))
            (backward-delete-char 1))))
      (goto-char (if (< 0 arg) (first bel) (- (second bel) 2))))))

(defun in-parenthese (&optional arg)
  (interactive "p")
  (let ((bel (collect-sexps arg)))
    (when bel
      (goto-char (second bel))
      (insert ")")
      (goto-char (first bel))
      (insert "(")
      (goto-char (1+ (first bel))))))

(global-set-key (kbd "A-[")   'in-parenthese)
(global-set-key (kbd "A-]")   'ex-parenthese)


(require 'inf-lisp)
(defun sexp-movement ()
  "Binds locally some keys to sexp movement commands."
  (interactive)
  (define-key inferior-lisp-mode-map  (kbd "C-c .") 'forward-sexp)
  (define-key inferior-lisp-mode-map  (kbd "C-c ,") 'backward-sexp)
  (local-set-key (kbd "C-c .") 'forward-sexp)
  (local-set-key (kbd "C-c ,") 'backward-sexp)
  (define-key inferior-lisp-mode-map  (kbd "A-.") 'forward-sexp)
  (define-key inferior-lisp-mode-map  (kbd "A-,") 'backward-sexp)
  (local-set-key (kbd "A-.") 'forward-sexp)
  (local-set-key (kbd "A-,") 'backward-sexp)
  (local-set-key [M-up]        'up-list)
  (local-set-key [M-down]      'down-list)
  (local-set-key [M-right]     'forward-sexp)
  (local-set-key [M-left]      'backward-sexp)
  (values))


(require 'bytecomp)
(byte-compile-disable-warning 'cl-functions)
;; byte-compile-warning-types
;; (redefine callargs free-vars unresolved obsolete noruntime cl-functions interactive-only make-local mapcar constants suspicious lexical)
;; byte-compile-warnings
;; (not cl-functions)

(defun make-lisp-command-sender (string)
  (byte-compile `(lambda ()
                   (interactive)
                   (cond
                     ((and (boundp 'slime-inferior-process:connlocal)
                           slime-inferior-process:connlocal)
                      (slime-repl-send-string ,(format "%s\n" string)))
                     ((and inferior-lisp-buffer
                           (inferior-lisp-proc))
                      (comint-send-string (inferior-lisp-proc)
                                          ,(format "%s\n" string)))
                     ((get-buffer-process (current-buffer))
                      (comint-send-string (get-buffer-process (current-buffer))
                                          ,(format "%s\n" string)))
                     (t (error "No process to send debugging command to."))))))

(defun clisp-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender ":s"))
  (local-set-key (kbd "<f6>") (make-lisp-command-sender ":n"))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender ":o"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender ":c"))
  (message "<f5> step into  <f6> next       <f7> step over  <f8> continue"))

(defun ecl-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender ""))
  (local-set-key (kbd "<f6>") (make-lisp-command-sender ""))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender ":skip"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender ":exit"))
  (message "<f5> step into  <f6> next       <f7> step over  <f8> continue"))

(defun sbcl-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender "step"))
  (local-set-key (kbd "<f6>") (make-lisp-command-sender "next"))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender "over"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender "out"))
  (message "<f5> step into  <f6> next       <f7> step over  <f8> continue"))

(defun allegro-debug-keys ()
  "Binds locally some keys to send allegro debugger commands to the inferior-lisp
<f5> step into
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender ":scont 1"))
  ;; (local-set-key (kbd "<f6>") (make-lisp-command-sender ))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender ":sover"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender ":continue"))
  (message "<f5> step into                  <f7> step over  <f8> continue"))



(loop for x in '(setf common-lisp-mode-hook      nil
                 inferior-lisp-load-hook    nil
                 inferior-lisp-mode-hook    nil
                 lisp-interaction-mode-hook nil
                 lisp-mode-hook             nil
                 comint-mode-hook           nil
                 comint-exec-hook           nil
                 ilisp-mode-hook            nil
                 scheme-mode-hook           nil)
   for i from 0
   when (oddp i)
   collect x)

(message (format  "hooks=%S" 
                  (mapcar (lambda (h)  (if (boundp h) (list h (symbol-value h)) (list h 'unbound)))
                          '(common-lisp-mode-hook inferior-lisp-load-hook inferior-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook comint-mode-hook comint-exec-hook ilisp-mode-hook scheme-mode-hook))))


(setf common-lisp-mode-hook      nil
      inferior-lisp-load-hook    nil
      inferior-lisp-mode-hook    nil
      lisp-interaction-mode-hook nil
      lisp-mode-hook             nil
      comint-mode-hook           nil
      comint-exec-hook           nil
      ilisp-mode-hook            nil
      scheme-mode-hook           nil)

(add-hook 'scheme-mode-hook      'pjb-lisp-meat)
(add-hook 'scheme-mode-hook
          (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
(add-hook 'lisp-mode-hook        'pjb-lisp-meat)
(add-hook 'common-lisp-mode-hook 'pjb-lisp-meat)
(add-hook 'emacs-lisp-mode-hook  'pjb-lisp-meat)
(add-hook 'emacs-lisp-mode-hook  'eldoc-mode)





;; slime-net-valid-coding-systems
;; (map nil (lambda (n)
;;            (format t "(~A-unix~VA~:[nil~;t  ~]  ~4:*:~A-unix)~%"
;;                    n (- 32 (length n)) ""
;;                    (ignore-errors (/= 1 (length (ext:convert-string-to-bytes
;;                                   "A" (ext:make-encoding :charset n)))))))
;;      (let ((l '()))
;;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))


;; swank-clisp.lisp find-encoding
;; (map nil (lambda (n) (format t "(:~A-unix~VA\"~A\")~%" n (- 32 (length n)) "" n))
;;      (let ((l '()))
;;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))



;; This is about the easiest profiling I've seen in any language. In  
;; fact, I think it's the only time I been able to make significant  
;; improvements based on the report.  
;; 
;;     
;;     M-x slime-toggle-profile-fdefinition
;; 
;; on all the functions you want to  
;; profile, 
;;     
;;     M-x slime-profile-reset
;; 
;; to clear any existing data, and  
;; 
;;     
;;     M-x slime-profile-report
;; 
;; to see the report after running.  

(when (fboundp 'slime-repl-bol)
  (defvar *slime-repl-bol* (symbol-function 'slime-repl-bol))
  (defun slime-repl-bol ()
    (interactive)
    (if (eql 'home last-input-event)
        (beginning-of-buffer) 
        (funcall *slime-repl-bol*))))


;; (message (format ".EMACS:  Environment EMACS_INFERIOR_LISP = %S"
;;            (getenv "EMACS_INFERIOR_LISP")))
;; (setf (getenv "EMACS_INFERIOR_LISP")
;;       (or
;;        "inferior-lisp"
;;        (getenv "EMACS_INFERIOR_LISP")
;;        "inferior-lisp"
;;        "minimum-slime"
;;        "slime"
;;        "allegro-fi"
;;        "ILISP"))
;; ;; (setf (getenv "EMACS_INFERIOR_LISP") "slime")
;; (message (format ".EMACS:  Selected EMACS_INFERIOR_LISP = %S"
;;            (getenv "EMACS_INFERIOR_LISP")))

;; (progn
;;   (add-to-list 'load-path (get-directory :share-lisp "packages/net/common-lisp/slime/slime/"))
;;   (setf common-lisp-mode-hook      nil
;;         inferior-lisp-load-hook    nil
;;         inferior-lisp-mode-hook    nil
;;         lisp-interaction-mode-hook nil
;;         lisp-mode-hook             nil
;;         comint-mode-hook           nil
;;         comint-exec-hook           nil
;;         ilisp-mode-hook            nil
;;         scheme-mode-hook           nil)
;; 
;;   (add-hook 'scheme-mode-hook      (function pjb-lisp-meat))
;;   (add-hook 'scheme-mode-hook
;;             (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;   (add-hook 'lisp-mode-hook        (function pjb-lisp-meat))
;;   (add-hook 'common-lisp-mode-hook (function pjb-lisp-meat))
;;   (add-hook 'emacs-lisp-mode-hook  (function pjb-lisp-meat))
;;   
;;   ;; (list scheme-mode-hook lisp-mode-hook common-lisp-mode-hook emacs-lisp-mode-hook)
;; 
;;   (cond
;; 
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "allegro-fi")
;; ;;;----------------------------------------------------------------------------
;; ;;; ALLEGRO FI interface.
;;      (load "/usr/local/languages/acl80/eli/fi-site-init.el")
;;      (add-hook 'lisp-mode-hook
;;                (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;      (defun ficl-meat ()
;;        (sexp-movement))
;;      (add-hook 'fi:common-lisp-mode-hook 'ficl-meat))
;; 
;; 
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "inferior-lisp")
;;      (.EMACS "inferior-lisp")
;; ;;;----------------------------------------------------------------------------
;; ;;; INFERIOR-LISP
;;      (add-hook 'lisp-mode-hook
;;                (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;      (setf inferior-lisp-mode-hook nil)
;; 
;; 
;;      (add-hook 'inferior-lisp-mode-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "inferior-lisp-mode-hook done.")))
;;      (add-hook 'comint-mode-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "comint-mode-hook done.")))
;;      (add-hook 'comint-exec-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "comint-mode-hook done.")))
;;      );; inferior-lisp
;; 
;; 
;;     ((and (string-equal (getenv "EMACS_INFERIOR_LISP") "minimum-slime")
;;           (require 'slime nil t))
;;      (.EMACS "minimum-slime")
;; ;;;----------------------------------------------------------------------------
;; ;;; MINIMUM SLIME
;; ;;; site-lisp configuration for slime-cvs
;;      
;;      (slime-setup '(slime-repl))
;;      (setf slime-net-coding-system 'utf-8-unix)
;;      (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;      (setf slime-space-information-p t)
;;      (global-set-key (kbd "C-c s") 'slime-selector)
;;      ;; this prevents us from requiring the user get dev-lisp/hyperspec
;;      ;; (which is non-free) as a hard dependency
;; 
;;      
;;      (defun newline-and-lisp-indent (&rest rest)
;;        (interactive)
;;        (newline)
;;        (lisp-indent-line))
;;      (define-key slime-mode-map (kbd "RET") 'newline-and-lisp-indent)
;; 
;; 
;; 
;;      (defun inferior-lisp-buffer-name (name index)
;;        (format "*slime inferior-lisp %d%s*" index name))
;; 
;;      (defun slime-buffer-name (name index)
;;        (format "%d%s (slime)" index name))
;; 
;;      (defun get-next-buffer-name (name bnf)
;;        (let ((i 0))
;;          (while (get-buffer (funcall bnf name i)) (incf i))
;;          (funcall bnf name i)))
;; 
;;      (defun slime-repl-buffer (&optional create connection)
;;        "Get the REPL buffer for the current connection; optionally create."
;;        (funcall (if create
;;                     (function get-buffer-create)
;;                     (function get-buffer))
;;                 ;; (format "*slime-repl %s*" (slime-connection-name connection))
;;                 (get-next-buffer-name (slime-lisp-implementation-name connection)
;;                                       (function slime-buffer-name))))
;; 
;; 
;;      (defun slime (&optional command coding-system)
;;        "Start an inferior^_superior Lisp and connect to its Swank server."
;;        (interactive)
;;        (let* ((args (slime-read-interactive-args))
;;               (impl (get (getf args :name) :lisp-implementation)))
;;          (if impl
;;              (apply (function slime-start)
;;                     :buffer (get-next-buffer-name
;;                              (lisp-implementation-name impl)
;;                              (function inferior-lisp-buffer-name))
;;                     args)
;;              (apply (function slime-start) args))))
;;      ) ;; minimum-slime
;;     
;; 
;;     ((and (string-equal (getenv "EMACS_INFERIOR_LISP") "slime")
;;           (require 'slime nil t))
;;      (.EMACS "slime")
;; ;;;----------------------------------------------------------------------------
;; ;;; SLIME
;;      
;;      ;;(add-to-list 'load-path "/home/luke/slime")
;;      (require 'slime)
;;      (slime-setup '(slime-fancy slime-asdf slime-banner slime-repl))
;; 
     (add-hook 'lisp-mode-hook
               (lambda () (slime-mode t) (slime-autodoc-mode t)))
;;      ;;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;      ;; (modify-syntax-entry ?$ "'" lisp-mode-syntax-table)
;; 
;;      (define-key slime-mode-map (kbd "[") 'insert-parentheses)
;;      (define-key slime-mode-map (kbd "]") 'move-past-close-and-reindent)
;;      ;;(define-key slime-mode-map (kbd "(") (lambda () (interactive) (insert "[")))
;;      ;;(define-key slime-mode-map (kbd ")") (lambda () (interactive) (insert "]")))
;;      (define-key slime-mode-map (kbd "(") (function self-insert-command))
;;      (define-key slime-mode-map (kbd ")") (function self-insert-command))
;;      (define-key slime-mode-map (kbd "\e\[") (lambda () (interactive) (insert "(")))
;;      (define-key slime-mode-map (kbd "\e\]") (lambda () (interactive) (insert ")")))
;; 
;;           
;;      (defun slime-send-dwim (arg)
;;        "Send the appropriate forms to CL to be evaluated.
;; http://bc.tech.coop/blog/070424.html
;; "
;;        (interactive "P")
;;        (save-excursion
;;          (cond 
;;            ;;Region selected - evaluate region
;;            ((not (equal mark-active nil))
;;             (copy-region-as-kill-nomark (mark) (point)))
;;            ;; At/before sexp - evaluate next sexp
;;            ((or (looking-at "\s(")
;;                 (save-excursion
;;                   (ignore-errors (forward-char 1))
;;                   (looking-at "\s(")))
;;             (forward-list 1)
;;             (let ((end (point))
;;                   (beg (save-excursion
;;                          (backward-list 1)
;;                          (point))))
;;               (copy-region-as-kill-nomark beg end)))
;;            ;; At/after sexp - evaluate last sexp
;;            ((or (looking-at "\s)")
;;                 (save-excursion
;;                   (backward-char 1)
;;                   (looking-at "\s)")))
;;             (if (looking-at "\s)")
;;                 (forward-char 1))
;;             (let ((end (point))
;;                   (beg (save-excursion
;;                          (backward-list 1)
;;                          (point))))
;;               (copy-region-as-kill-nomark beg end)))
;;            ;; Default - evaluate enclosing top-level sexp
;;            (t (progn
;;                 (while (ignore-errors (progn
;;                                         (backward-up-list)
;;                                         t)))
;;                 (forward-list 1)
;;                 (let ((end (point))
;;                       (beg (save-excursion
;;                              (backward-list 1)
;;                              (point))))
;;                   (copy-region-as-kill-nomark beg end)))))
;;          (set-buffer (slime-output-buffer))
;;          (unless (eq (current-buffer) (window-buffer))
;;            (pop-to-buffer (current-buffer) t))
;;          (goto-char (point-max))
;;          (yank)
;;          (if arg (progn
;;                    (slime-repl-return)
;;                    (other-window 1)))))
;; 
;; 
;;      ;; (define-key lisp-mode-map [f7] 'slime-send-dwim)
;;      ;; (define-key lisp-mode-map [f8] (lambda ()
;;      ;;                                  (interactive)
;;      ;;                                  (slime-send-dwim 1)))
;; 
;;      
;;      (defun slime-version ()
;;        (interactive)
;;        (eval-in-cl "(swank-loader::slime-version-string)"
;;                    (lambda (values)
;;                      (if (null (cdr values))
;;                          (message (format "%s" v))
;;                          (dolist (v values)
;;                            (message (format "%s\n" v)))))))
;;   
;; 
;; 
;;      (defvar *pm* '() "process-marker alist")
;; 
;;      (defun pjb-slime-net-filter (process string)
;;        "Accept output from the socket and input all complete messages."
;;        (with-current-buffer (process-buffer process)
;;          (save-excursion
;;            (let ((pma (assoc process *pm*)))
;;              (when pma (goto-char (marker-position (cdr pma)))))
;;            (insert string))
;;          (slime-process-available-input)))
;; 
;; 
;;      (defun pjb-slime-eval-with-transcript (form &optional fn wait)
;;        "Send FROM and PACKAGE to Lisp and pass the result to FN.
;; Display the result in the message area, if FN is nil."
;;        (let* ((proc (slime-connection))
;;               (spb (process-buffer proc))
;;               (spf (process-filter proc)))
;;          (let ((pma (assoc proc *pm*))
;;                (m (let ((m (make-marker)))
;;                     (set-marker m (point) (current-buffer))
;;                     m)))
;;            (if pma
;;                (setf (cdr pma) m)
;;                (push (cons proc m) *pm*)))
;;          (set-process-buffer proc (current-buffer))
;;          (set-process-filter proc 'pjb-slime-net-filter)
;;          (unwind-protect (with-lexical-bindings (fn)
;;                            (slime-eval-async  form
;;                                               (lambda (value)
;;                                                 (cond (fn (funcall fn value))
;;                                                       (t (.EMACS "%s" value)))
;;                                                 (slime-show-last-output))))
;;            (set-process-buffer proc spb)
;;            (set-process-filter proc spf)
;;            (setf *pm* (delete (assoc proc *pm*) *pm*)))))
;; 
;; 
;;      ;;   (defun pjb-slime-eval-last-expression ()
;;      ;;     "Evaluate the expression preceding point."
;;      ;;     (interactive)
;;      ;;     (let* ((str  (slime-last-expression))
;;      ;;            (sexp (read-from-string str)))
;;      ;;       (if (and (listp sexp)
;;      ;;                (symbolp (fisrt sexp))
;;      ;;                (< 3 (LENGTH (SYMBOL-NAME (first sexp))))
;;      ;;                (STRING-EQUAL "DEF"  (SYMBOL-NAME (first sexp)) :end2 3))
;;      ;;         (slime-eval-last-expression str)
;;      ;;         (slime-eval-print-last-expression str))))
;; 
;; 
;;      (defun pjb-slime-eval-last-expression ()
;;        "Evaluate the expression preceding point."
;;        (interactive)
;;        (if buffer-read-only
;;            (slime-eval-last-expression)
;;            (let ((str  (slime-last-expression)))
;;              ;; (.EMACS "A DEF? %S" (STRING-EQUAL "(DEF"  str :end2 4))
;;              (if (string-equal* "(DEF"  str :end2 4)
;;                  (slime-interactive-eval str)
;;                  (slime-eval-print-last-expression str)))))
;; 
;; 
;;      (defun slime-restart-lisp-image ()
;;        (interactive)
;;        (when (slime-connected-p)
;;          (dolist (buf (buffer-list))
;;            (when (or (string= (buffer-name buf) slime-event-buffer-name)
;;                      (string-match "^\\*inferior-lisp*" (buffer-name buf)))
;;              (kill-buffer buf))))
;;        (call-interactively 'slime)) ;;slime-restart-lisp-image
;; 
;; 
;;      (defun pjb-slime-erase-buffer ()
;;        "Reset the slime output buffer to initial state."
;;        (interactive)
;;        (with-current-buffer (slime-output-buffer)
;;          (let ((inhibit-read-only t))
;;            (erase-buffer)
;;            (slime-repl-update-banner)))) ;;pjb-slime-erase-buffer
;; 
;; 
;;      (defun slime-kill ()
;;        (interactive)
;;        (map nil (lambda (x) (when (buffer-named x) (kill-buffer x)))
;;             '("*slime-repl[1]*" "*slime-events*" "*inferior-lisp*")))
;; 
;; 
;;      (defun slime-relaunch ()
;;        (interactive)
;;        (slime-kill)
;;        (sit-for 1)
;;        (slime)) ;;slime-relaunch
;; 
;;      (defalias 'slime-reload 'slime-relaunch)
;; 
;; 
;;      (defun pjb-slime-reset-minor-mode ()
;;        (dolist (slime-mode-vars '( slime-repl-read-mode
;;                                   slime-temp-buffer-mode
;;                                   inferior-slime-mode slime-mode))
;;          (setf minor-mode-map-alist (delete-if (lambda (x) (eq (car x) slime-mode-vars))
;;                                                minor-mode-map-alist)))
;;        ) ;;pjb-slime-reset-minor-mode
;; 
;; 
;;      (defvar *pjb-slime-keys-dynamic* nil)
;;      (defun pjb-slime-substitute-command (key command &rest keys)
;;        (unless  *pjb-slime-keys-dynamic*
;;          (setf slime-keys (mapcar (function copy-seq) (copy-seq slime-keys))
;;                *pjb-slime-keys-dynamic* t))
;;        (let ((prefixedp (cadr (member :prefixed keys)))
;;              (skeys slime-keys))
;;          (while skeys
;;            (when (and (string= key (first (car skeys)))
;;                       (equiv prefixedp (cadr (member :prefixed (car skeys)))))
;;              (setf (second (car skeys)) command
;;                    skeys nil))
;;            (pop skeys)))
;;        (pjb-slime-reset-minor-mode)
;;        (load "slime" *pjb-load-noerror* *pjb-load-silent*))
;; 
;; 
;;      ;; (pjb-slime-substitute-command "\M-." 'slime-edit-definition-other-window)
;; 
;;      ;; (pjb-slime-substitute-command "\C-e" 'pjb-slime-eval-last-expression
;;      ;;                               :prefixed t)
;; 
;; 
;;      (progn
;;        (define-key sldb-mode-map  "\M-."     'slime-edit-definition-other-window)
;;        (define-key slime-mode-map          "\C-ch"    'slime-hyperspec-lookup)
;;        (define-key inferior-slime-mode-map "\C-ch"    'slime-hyperspec-lookup)
;;        (define-key slime-mode-map  "\C-c\C-e" 'pjb-slime-eval-last-expression)
;;        (define-key slime-mode-map  "\C-x\C-e" 'pjb-slime-eval-last-expression)
;;        (define-key slime-mode-map          "\C-c\C-t" 'pjb-slime-erase-buffer)
;;        (define-key slime-mode-map          " "        'slime-space) ;'cl-magic-space)
;;        (define-key inferior-slime-mode-map "\C-c\C-t" 'pjb-slime-erase-buffer)
;;        )
;; 
;;      (defun slime-symbol-name-at-point ()
;;        "Return the name of the symbol at point, otherwise nil."
;;        (save-restriction
;;          ;; Don't be tricked into grabbing the REPL prompt.
;;          (when (and (eq major-mode 'slime-repl-mode)
;;                     (>= (point) slime-repl-input-start-mark))
;;            (narrow-to-region slime-repl-input-start-mark (point-max)))
;;          (save-excursion
;;            (skip-syntax-forward "w_")
;;            (skip-syntax-backward "-")
;;            (let ((string (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;                            (when bounds
;;                              (buffer-substring (car bounds)
;;                                                (progn
;;                                                  (goto-char (1- (cdr bounds)))
;;                                                  (if (looking-at "\\.\"")
;;                                                      (1- (cdr bounds))
;;                                                      (cdr bounds))))))))
;;              (and string
;;                   ;; In Emacs20 (thing-at-point 'symbol) returns "" instead
;;                   ;; of nil when called from an empty (or
;;                   ;; narrowed-to-empty) buffer.
;;                   (not (equal string ""))
;;                   (substring-no-properties   string)))))) ;;slime-symbol-name-at-point
;; 
;; 
;; 
;;      ;; (trace slime-init-keymaps  slime-init-keymaps  slime-define-key)
;;      ;; (trace pjb-slime-eval-last-expression)
;;      ;; (show (assoc "" slime-keys))
;; 
;;      (defun slime-hyperspec-lookup (symbol-name)
;;        "A wrapper for `hyperspec-lookup'"
;;        (interactive (list (let ((completion-ignore-case t)
;;                                 (symbol-at-point (slime-symbol-name-at-point)))
;;                             (if (and symbol-at-point
;;                                      (intern-soft (downcase symbol-at-point)
;;                                                   common-lisp-hyperspec-symbols))
;;                                 symbol-at-point
;;                                 (completing-read
;;                                  "Look up symbol in Common Lisp HyperSpec: "
;;                                  common-lisp-hyperspec-symbols #'boundp
;;                                  t symbol-at-point
;;                                  'common-lisp-hyperspec-history)))))
;;        (hyperspec-lookup symbol-name)) ;;slime-hyperspec-lookup
;; 
;;      ;; (setf sldb-hook nil)
;;      (add-hook 'sldb-hook (lambda () (toggle-truncate-lines 1)))
;; 
;; 
;;      (defun slime-macroexpand-in-place (&optional string)
;;        (interactive)
;;        (unless string
;;          (setf string (slime-sexp-at-point-or-error)))
;;        (lexical-let ((package (slime-current-package)))
;;          (insert (slime-eval `(swank:swank-macroexpand-1 ,string)))))
;;      
;;      ) ;;slime
;; 
;;     
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "ILISP")
;;      (.EMACS "ilisp")
;; ;;;----------------------------------------------------------------------------
;; ;;; ILISP
;;      (require 'ilisp)
;; 
;;      (setq ilisp-*use-fsf-compliant-keybindings*  t)
;;      (setq ilisp-*use-frame-for-arglist-output-p* nil)
;;      (setq ilisp-*arglist-message-lisp-space-p*   nil)
;;      (setq ilisp-arglist-output                   nil)
;;      (setq ilisp-motd                             nil)
;;      (setq ilisp-defpackage-command-string
;;            "([Dd][Ee][Ff][-A-Za-z]*[Pp][Aa][Cc][Kk][Aa][Gg][Ee]  *\\([^ ][^ ]*\\)")
;;      ;; ;; (setq ilisp-hash-form-regexp "\\(^[ \t]*#[+-].\\)\\|\\(^[ \t]*(\\(.*::?\\)?\\(defpackage\\|define-package\\)[ \t\n]\\)\\|\\(^[ \t]*(\\(.*::?\\)?in-package[ \t\n]*\\)")
;; 
;; 
;;      ;; (setf ilisp-mode-hook nil lisp-mode-hook nil scheme-mode-hook nil clisp-hs-hook)
;;      (let ((hook  (lambda () (require 'ilisp))))
;;        (add-hook 'lisp-mode-hook   hook)
;;        (add-hook 'ilisp-mode-hook  hook)
;;        (add-hook 'scheme-mode-hook hook))
;; 
;;      ;;(lambda () (set-buffer-process-coding-system 'mule-utf-8 'mule-utf-8)))
;;      ;;(setf common-lisp-hook nil clisp-hs-hook nil)
;;      (add-hook 'ilisp-init-hook
;;                (lambda () (set-buffer-process-coding-system 'mule-utf-8 'mule-utf-8)))
;; 
;;      (defun ilisp-eval-region (start end)
;;        (interactive "r")
;;        (let* ((form (lisp-defun-region-and-name))
;;               (result
;;                (eval-region-lisp start end  'result
;;                                  (format "Evaluating %s" (car (cdr (cdr form)))))))
;;          (goto-char end)
;;          (lisp-display-output result))) ;;ilisp-eval-region
;; 
;; 
;;      (defun pjb-output-to-current-buffer (output ilisp-output-sink)
;;        "
;; This function is used to display the output from ilisp.
;; It's hooked by `ilisp-display-output-function'.
;; "
;;        (end-of-line)
;;        (insert (if (string-match "\n" output) "\n" "\n;;"))
;;        (insert output)
;;        (unless (string-match "\n" output) (goto-char 0)))
;; 
;;      (setq ilisp-display-output-function 'pjb-output-to-current-buffer)
;; 
;; 
;;      (defadvice  ilisp-display-output-adaptively
;;          (around pjb-ilisp-display-output-adaptively last
;;                  (output ilisp-output-sink) activate)
;;        "Always display output to the echo area: 21.2 can do with multiline strings"
;;        (ilisp-display-output-in-echo-area output ilisp-output-sink)
;;        ) ;;ilisp-display-output-adaptively
;;      (ad-activate 'ilisp-display-output-adaptively)
;;      ;;end ilisp
;;      )
;;     ))




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
(.EMACS "emacs<->Common Lisp RPC with slime/swank")

;;; In emacs, we can execute Common Lisp expressions:

;; (require 'slime)
;; (slime)

(setf slime-enable-evaluate-in-emacs t) 

(defun eval-in-cl (cl-expression-string process-result-values)
  (slime-eval-with-transcript
   `(swank:eval-and-grab-output ,cl-expression-string)
   (lexical-let  ((here (current-buffer))
                  (process-result-values process-result-values))
     (lambda (result-values)
       (set-buffer here)
       (funcall process-result-values result-values)))))

;; (eval-in-cl "(values 1 * (ext:! 20) (package-name *package*))"
;;             (lambda (values)
;;               (dolist (v values)
;;                 (insert (format "%s\n" v)))))
;; Returns:
;;
;; nil
;;
;; then later inserts:
;;
;; 1
;; (42 (EMACS-UNREADABLE |buffer| |*scratch*|))
;; 2432902008176640000
;; "COMMON-LISP-USER"


;; ;;; In Common Lisp, we can execute emacs lisp expressions:
;; 
;; (defparameter *emacs-readtable* (copy-readtable))
;; (setf (readtable-case *emacs-readtable*) :preserve)
;; (set-syntax-from-char #\> #\) *emacs-readtable*)
;; (set-dispatch-macro-character
;;  #\# #\<
;;  (lambda (stream subchar dispchar)
;;    `(emacs-unreadable ,@(read-delimited-list #\> stream t)))
;;  *emacs-readtable*)
;; 
;; ;; Probably more readtable patching would be in order.
;; ;;
;; ;; We could define CLOS proxies for emacs objects for a more seamless
;; ;; integration. swank::eval-in-emacs process the CL form to make it
;; ;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; ;; to emacs lisp forms returning the corresponding emacs object.
;; 
;; (defun eval-in-emacs (form &optional nowait)
;;   (let ((result (SWANK::EVAL-IN-EMACS `(format "%S" ,form) nowait))
;;         (*readtable* *emacs-readtable*))
;;     (with-input-from-string (in result)
;;       (let ((result (read in nil in)))
;;         result))))
;; 
;; 
;; (eval-in-emacs `(progn
;;                   (switch-to-buffer (buffer-named "*scratch*"))
;;                   (goto-char (point-max))
;;                   (insert ,(format nil "~%Hello~%"))
;;                   (list 42 (current-buffer))))
;; 
;; ;; Switch to the *scratch* buffer,
;; ;; goto the last position, and
;; ;; inserts \nHello\n
;; ;; then returns:
;; ;; (42 (EMACS-UNREADABLE |buffer| |*scratch*|))


(.EMACS "Redshank")
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



;;;----------------------------------------------------------------------------
(.EMACS "Common Lisp indenting")

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

(defun eval-last-sexp-lisp ()
  (interactive)
  (forward-sexp -1)
  (let ((current-prefix-arg '-))
    (eval-next-sexp-lisp)))


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


;;;----------------------------------------------------------------------------
(.EMACS "Common-Lisp Hyperspec")
;; common-lisp-hyperspec-symbols


(defun probe-url (url)
  (cond
    ((string= "file://" (subseq url 0 (min (length url) 7)))
     (file-readable-p (subseq url 7)))
    ((file-readable-p "/tmp/no-internet")
     nil)
    (t
     (zerop (parse-integer
             (shell-command-to-string
              (format "wget -O /dev/null %S >/dev/null 2>&1 ; echo -n $?"
                      url)))))))


(defun thing-at-point-no-properties (thing)
  "Return the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING."
  (if (get thing 'thing-at-point)
      (funcall (get thing 'thing-at-point))
      (let ((bounds (bounds-of-thing-at-point thing)))
        (if bounds
            (buffer-substring-no-properties (car bounds) (cdr bounds))))))



(when (require 'hyperspec nil t)

  (load "extra/hyperspec" *pjb-load-noerror* *pjb-load-silent*)

  (defparameter *lw-clhs* "www.lispworks.com/documentation/HyperSpec/")
  (defparameter *hyperspec-path*  (or (get-directory :hyperspec)
				      (concat "/usr/local/html/local/lisp/" *lw-clhs*)))
  (setf common-lisp-hyperspec-root
	(dolist
	    (url (list
		  (concat "file://" *hyperspec-path*)
		  "file:///usr/share/doc/hyperspec/HyperSpec/"
		  ;; (concat "http://thalassa.lan.informatimago.com/lisp/" *lw-clhs*)
		  (concat "http://" *lw-clhs*)))
	  (when (probe-url url)
	    (return url))))

  (defparameter common-lisp-hyperspec-browser (function ignore))
  (defparameter common-lisp-hyperspec-frame   (selected-frame))

  ;; (setf common-lisp-hyperspec-browser 'w3m-browse-url 
  ;; (push '("."  .  w3m-browse-url) browse-url-browser-function)






  (when (or t  (boundp 'common-lisp-hyperspec-symbols))

    (defun common-lisp-hyperspec-complete (string predicate allp)
      (if allp
	  (let ((result '()))
	    (mapatoms
	     (lambda (symbol)
	       (let ((name (symbol-name symbol)))
		 (when (or (and (<= (length string) (length name))
				(string-equal* string name :end2 (length string)))
			   (search (concat "-" string) name :test (function equalp)))
		   (push name result))))
	     common-lisp-hyperspec-symbols)
	    result)
	  (try-completion string common-lisp-hyperspec-symbols predicate)))

    
    (defun clhs-entry (symbol-designator)
      (let ((symbol (intern-soft (downcase (etypecase symbol-designator
					     (symbol (symbol-name symbol-designator))
					     (string  symbol-designator)))
				 common-lisp-hyperspec-symbols)))
	(if (and symbol (boundp symbol))
	    (symbol-value symbol)
	    nil)))

    
    (defun common-lisp-hyperspec (symbol-name)
      "View the documentation on SYMBOL-NAME from the Common Lisp HyperSpec.
If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions.
The Common Lisp HyperSpec is the full ANSI Standard Common Lisp, provided
by Kent Pitman and Xanalys Inc.  By default, the Xanalys Web site is
visited to retrieve the information.  Xanalys Inc. allows you to transfer
the entire Common Lisp HyperSpec to your own site under certain conditions.
Visit http://www.xanalys.com/software_tools/reference/HyperSpec/ for more
information.  If you copy the HyperSpec to another location, customize the
variable `common-lisp-hyperspec-root' to point to that location."
      (interactive
       (list (let ((completion-ignore-case t)
		   (symbol-at-point (thing-at-point-no-properties 'symbol)))
	       (completing-read
		"Look up symbol in Common Lisp HyperSpec: "
		(function common-lisp-hyperspec-complete) #'boundp
		t symbol-at-point
		'common-lisp-hyperspec-history))))
      (maplist
       (lambda (entry)
	 (case system-type
	   ((darwin)
	    (case window-system
	      ((x)
	       (browse-url (concat common-lisp-hyperspec-root
				   "Body/" (car entry))))
	      ((mac ns nil)
	       (let ((browse-url-browser-function (cons '("." . browse-url-generic) browse-url-browser-function))
		     (browse-url-generic-program "/usr/bin/open"))
		 (browse-url (concat common-lisp-hyperspec-root "Body/" (car entry)))))
	      (otherwise
	       (error "Unknown window-system"))))
	   ((gnu/linux)
	    (browse-url (concat common-lisp-hyperspec-root "Body/" (car entry))))
	   (otherwise
	    (error "Unknown system-type.")))
	 (if (cdr entry)
	     (sleep-for 1.5)))
       (delete-duplicates
	(or (clhs-entry symbol-name)
	    (error "The symbol `%s' is not defined in Common Lisp"
		   symbol-name))
	:test (function equal))))
    

    (defun gcl-hyperspec (symbol-name)
      (interactive
       (list (let ((completion-ignore-case t)
		   (symbol-at-point (thing-at-point-no-properties 'symbol)))
	       (completing-read
		"Look up symbol in Common Lisp HyperSpec: "
		common-lisp-hyperspec-symbols #'boundp
		t symbol-at-point
		'common-lisp-hyperspec-history))))
      (maplist
       (lambda (entry)
	 (info (format "(gcl)%s" (car entry)))
	 (if (cdr entry)
	     (sleep-for 1.5)))
       (delete-duplicates
	(let ((symbol (intern-soft (downcase symbol-name)
				   common-lisp-hyperspec-symbols)))
	  (if (and symbol (boundp symbol))
	      (list symbol)
	      (error "The symbol `%s' is not defined in Common Lisp"
		     symbol-name)))
	:test (function equal))))


    (defalias 'clhs               'common-lisp-hyperspec)
    (defalias 'hyperspec-lookup   'common-lisp-hyperspec) ; 'gcl-hyperspec)
    (global-set-key (kbd "C-h y") 'hyperspec-lookup)

    ) ;;(boundp 'common-lisp-hyperspec-symbols)


  (defun random-hyperspec ()
    (interactive)
    (let* ((random-hyperspec-symbol
	    (let ((syms '()))
	      (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
	      (nth (random (length syms)) syms)))
	   (random-page (let ((pages (symbol-value random-hyperspec-symbol)))
			  (nth (random (length pages)) pages))))
      (browse-url (concat common-lisp-hyperspec-root "Body/" random-page)))))




;;   (defun send-url-to-safari (url)
;;     "Sends URL to Safari, using Apple's Open Scripting Architecture."
;;     (with-temp-buffer
;;       (insert "tell application \"Safari\"\n")
;;       (insert "  activate\n")
;;       (insert "  make new document at the beginning of documents\n")
;;       (insert (format "  set the URL of the front document to \"%s\"\n" url))
;;       (insert "end tell\n")
;;       (call-process-region (point-min) (point-max) "/usr/bin/osascript")))

;;; (setq common-lisp-hyperspec-root
;;;       "file://Users/ayank/Documents/text/computer/lisp/HyperSpec/")
;;; (setq common-lisp-hyperspec-symbol-table
;;;       "file://Users/ayank/Documents/text/computer/lisp/HyperSpec/Data/Map_Sym.txt")

;;; (load-library
;;;  "file://Users/ayank/Documents/text/computer/lisp/ilisp/extra/hyperspec")

;;; (global-set-key [(shift f1)]
;;;                 '(lambda ()
;;;                    (interactive)
;;;                    (common-lisp-hyperspec
;;;                     (thing-at-point 'symbol))))


;;; or:
;;;       (push browse-url-browser-function
;;; 	    '("."  . (lambda (url &optional new-win)
;;; 	       (do-applescript (concat "open location \""
;;; 				       url "\"")))))




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
  
  (defalias 'np 'emms-show))






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
  (local-set-key (kbd "TAB") (quote expand-mail-aliases)))

(when (require 'vm nil t)
  (.EMACS "vm")
  (require 'vm-vars)
  (ignore-errors (load-library "vm-w3m"))



  ;; (add-hook 'vm-mode-hook              'mc-install-read-mode)
  ;; (add-hook 'vm-summary-mode-hook      'mc-install-read-mode)
  ;; (add-hook 'vm-virtual-mode-hook      'mc-install-read-mode)
  ;; (add-hook 'vm-mail-mode-hook         'mc-install-write-mode)
  ;; (add-hook 'vm-presentation-mode-hook 'mc-install-write-mode)
  (defun pjb-vm-summary-meat         () (when nil (set-frame-name "MAIL")))
  (defun pjb-vm-mail-meat            () (when nil (set-frame-name "COMPOSE")))
  (defun pjb-vm-reply-meat           () (inactivate-input-method))
  (defun pjb-vm-arrived-message-meat () (pjb-vm-kill-subject-regexp "\\[SPAM\\]"))
  (add-hook 'vm-summary-mode-hook    'pjb-vm-summary-meat)
  (add-hook 'vm-mail-mode-hook       'pjb-vm-mail-meat)
  (add-hook 'vm-reply-hook           'pjb-vm-reply-meat)
  (add-hook 'vm-arrived-message-hook 'pjb-vm-arrived-message-meat)


  (defun pjb-vm-delete-spam (count)
    (interactive "p")
    (vm-save-message "~/mail/spam.mbox" count)
    (pjb-vm-delete-message count))

  (defun pjb-vm-delete-message (count)
    (interactive "p")
    (vm-delete-message count)
    (vm-next-message))

  (defun pjb-vm-visit-folder-meat ()
    (define-key vm-mode-map (kbd "d")     'pjb-vm-delete-message)
    (define-key vm-mode-map (kbd "M-d")   'pjb-vm-delete-spam)
    (define-key vm-mode-map (kbd "o")     'vm-save-message)
    (define-key vm-mode-map (kbd "r")     'vm-followup-include-text)
    (define-key vm-mode-map (kbd "s")     'vm-save-folder)
    (local-set-key          (kbd "c")     'vm-save-message))

  (add-hook 'vm-visit-folder-hook 'pjb-vm-visit-folder-meat)

  (unless (<= 23 emacs-major-version)
    (keyboard-translate (aref (kbd "M-S-d") 0) (aref (kbd "M-S-d") 0))
    (keyboard-translate (aref (kbd "M-D")   0) (aref (kbd "M-D")   0)))

  ;; (defun vm-from-biff ()
  ;;   (interactive)
  ;;   (select-frame (make-frame))
  ;;   (vm-register-frame (vm-selected-frame))
  ;;   (when vm-warp-mouse-to-new-frame
  ;;     (vm-warp-mouse-to-frame-maybe (vm-selected-frame)))
  ;;   (vm))

  (when (load "vm-sort" *pjb-load-noerror* *pjb-load-silent*)
    (defun vm-sort-compare-author (m1 m2)
      "Let's sort by domain first"
      (let ((s1 (vm-su-from m1))
            (s2 (vm-su-from m2))
            l1 d1 l2 d2)
        (let ((@-pos (position (character "@") s1)))
          (if @-pos
              (setf d1 (subseq s1 (1+ @-pos))
                    l1 (subseq s1 0 @-pos))
              (setf d1 ""
                    l1 s1)))
        (let ((@-pos (position (character "@") s2)))
          (if @-pos
              (setf d2 (subseq s2 (1+ @-pos))
                    l2 (subseq s2 0 @-pos))
              (setf d2 ""
                    l2 s2)))
        (cond ((string-equal s1 s2) '=)
              ((string-equal d1 d2)
               (cond ((string-lessp l1 l2) t)
                     ((string-equal l1 l2)
                      (let ((f1 (vm-su-full-name m1))
                            (f2 (vm-su-full-name m2)))
                        (cond ((string-lessp f1 f2) t)
                              ((string-lessp f1 f2) '=)
                              (t nil))))
                     (t nil)))
              ((string-lessp d1 d2) t)
              (t nil))))
    ) ;;when vm-sort

  

  ;; (catch :found
  ;;   (let ((version emacs-version)
  ;;         (next
  ;;          (lambda ()
  ;;            (cond
  ;;              ((null version)          (throw :found :default))
  ;;              ((= 0 (length version))
  ;;               (setf version nil)
  ;;               (concatenate 'string (NAMESTRING (USER-HOMEDIR-PATHNAME))
  ;;                            "bin/movemail"))
  ;;              (t (prog1
  ;;                     (format "/usr/local/libexec/emacs/%s/%s/movemail"
  ;;                       version system-configuration)
  ;;                   (string-match "^\\(\\([0-9][.0-9]*\\)\\.\\)?[0-9]+$" version)
  ;;                   (setq version (or (match-string 2 version) ""))))))))
  ;;     (do ((path (funcall next) (funcall next)))
  ;;         (nil)
  ;;       (when (file-exists-p path)
  ;;         (setq vm-movemail-program  path)
  ;;         (throw :found :one)))))

  ;; ;; movemail: No locks available for /larissa//var/spool/mail/pjb
  ;; ;; /usr/local/libexec/emacs/21.3/i686-pc-linux-gnu/movemail exited with code 1
  ;; (setq vm-movemail-program
  ;;       (concatenate 'string  (NAMESTRING (USER-HOMEDIR-PATHNAME)) "bin/movemail"))

;;; '(vm-imap-server-list (quote ("imap:imap.afaa.asso.fr:143:inbox:login:pjb:pari-fle")))



  ;; rmail -> vm
  ;;(defalias 'rmail 'vm)
  ;;(defalias 'rmail-input 'vm-visit-folder)
  ;;(defun rmail       () (interactive) (error "Use mail in a shell!"))
  ;;(defun vm          () (interactive) (error "Use mail in a shell!"))
  ;;(defun rmail-input () (interactive) (error "Use mail in a shell!"))

  ;; (defmacro advise-replace (fname parameters body)
  ;;   (let ((aname (intern (format "pjb-adrep-%s" fname))))
  ;;     `(progn
  ;;        (defadvice ,fname
  ;;            (around ,aname  first  ,parameters  activate)
  ;;          ,body)
  ;;        (ad-activate (quote ,fname)))
  ;;     )) ;;advise-replace
  ;; (put 'advise-replace      'lisp-indent-function 2)
  ;; 
  ;; 
  ;; (advise-replace rmail-sort-by-correspondent (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-author" "author")))
  ;; 
  ;; (advise-replace rmail-sort-by-date          (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-date" "date")))
  ;; 
  ;; (advise-replace rmail-sort-by-labels        (reverse)
  ;;   (error "Not implemented with VM."))
  ;; 
  ;; (advise-replace rmail-sort-by-lines         (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-line-count" "line-count")))
  ;; 
  ;; (advise-replace rmail-sort-by-recipient     (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-recipients" "recipients")))
  ;; 
  ;; (advise-replace rmail-sort-by-subject       (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-subject" "subject")))


  ;; (defadvice vm-mime-attach-object
  ;;     (before pjb-removemime-vm-mime-attach-object nil activate)
  ;;   (save-restriction
  ;;     (pjb-mail-narrow-to-headers)
  ;;     (pjb-mail-remove-header "^\\(MIME-Version:\\|Content-\\)" t))
  ;;   )
  ;; (ad-activate 'vm-mime-attach-object)



  (when (require 'vm-pop nil t)
    (defun vm-pop-cleanup-region (start end)
      (setq end (vm-marker end))
      (save-excursion
        (goto-char start)
        ;; CRLF -> LF
        (while (and (< (point) end) (search-forward "\r\n"  end t))
          (replace-match "\n" t t))
        (goto-char start)
        (while (and (< (point) end) (search-forward "^\\(From .*\\)" end t))
          (message "inserting a new line before %S" (buffer-substring (match-beginning 0) (match-end 0)))
          (goto-char (match-beginning 0))
          (insert "\n\n")
          (forward-line))
        ;; (goto-char start)
        ;; chop leading dots
        ;; (while (and (< (point) end) (re-search-forward "^\\."  end t))
        ;;   (replace-match "" t t)
        ;;   (forward-char))
        )
      (set-marker end nil)))


  (defun vm (&optional folder read-only access-method)
    "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, message additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox normally causes any contents of the system mailbox to
be moved and appended to the resulting buffer.  You can disable this automatic fetching of mail by setting `vm-auto-get-new-mail' to nil.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' saves the buffered folder to disk, but does not expunge
deleted messages.  Use `###' to expunge deleted messages.

See the documentation for vm-mode for more information."
    (interactive (list nil current-prefix-arg))
    (vm-session-initialization)
    ;; set inhibit-local-variables non-nil to protect
    ;; against letter bombs.
    ;; set enable-local-variables to nil for newer Emacses
    (catch 'done
      ;; deduce the access method if none specified
      (if (null access-method)
          (let ((f (or folder vm-primary-inbox)))
            (cond ((and vm-recognize-imap-maildrops
                        ;; f could be a buffer
                        (stringp f)
                        (string-match vm-recognize-imap-maildrops f))
                   (setq access-method 'imap
                         folder f))
                  ((and vm-recognize-pop-maildrops
                        ;; f could be a buffer
                        (stringp f)
                        (string-match vm-recognize-pop-maildrops f))
                   (setq access-method 'pop
                         folder f)))))
      (let ((full-startup (not (bufferp folder)))
            (did-read-index-file nil)
            folder-buffer first-time totals-blurb
            folder-name remote-spec
            preserve-auto-save-file)
        (cond ((eq access-method 'pop)
               (setq remote-spec (vm-pop-find-spec-for-name folder))
               (if (null remote-spec)
                   (error "No such POP folder: %s" folder))
               (setq folder-name folder)
               ;; Prior to VM 7.11, we computed the cache filename
               ;; based on the full POP spec including the password
               ;; if it was in the spec.  This meant that every
               ;; time the user changed his password, we'd start
               ;; visiting the wrong (and probably nonexistent)
               ;; cache file.
               ;;
               ;; To fix this we do two things.  First, migrate the
               ;; user's caches to the filenames based in the POP
               ;; sepc without the password.  Second, we visit the
               ;; old password based filename if it still exists
               ;; after trying to migrate it.
               ;;
               ;; For VM 7.16 we apply the same logic to the access
               ;; methods, pop, pop-ssh and pop-ssl and to
               ;; authentication method and service port, which can
               ;; also change and lead us to visit a nonexistent
               ;; cache file.  The assumption is that these
               ;; properties of the connection can change and we'll
               ;; still be accessing the same mailbox on the
               ;; server.
               (let ((f-pass (vm-pop-make-filename-for-spec remote-spec))
                     (f-nopass (vm-pop-make-filename-for-spec remote-spec t))
                     (f-nospec (vm-pop-make-filename-for-spec remote-spec t t)))
                 (cond ((or (string= f-pass f-nospec)
                            (file-exists-p f-nospec))
                        nil )
                       ((file-exists-p f-pass)
                        ;; try to migrate
                        (condition-case nil
                            (rename-file f-pass f-nospec)
                          (error nil)))
                       ((file-exists-p f-nopass)
                        ;; try to migrate
                        (condition-case nil
                            (rename-file f-nopass f-nospec)
                          (error nil))))
                 ;; choose the one that exists, password version,
                 ;; nopass version and finally nopass+nospec
                 ;; version.
                 (cond ((file-exists-p f-pass)
                        (setq folder f-pass))
                       ((file-exists-p f-nopass)
                        (setq folder f-nopass))
                       (t
                        (setq folder f-nospec)))))
              ((eq access-method 'imap)
               (setq remote-spec folder
                     folder-name (or (nth 3 (vm-imap-parse-spec-to-list
                                             remote-spec))
                                     folder)
                     folder (vm-imap-make-filename-for-spec remote-spec))))
        (setq folder-buffer
              (if (bufferp folder)
                  folder
                  (let ((file (or folder (expand-file-name vm-primary-inbox
                                                           vm-folder-directory))))
                    (if (file-directory-p file)
                        ;; MH code perhaps... ?
                        (error "%s is a directory" file)
                        (or (vm-get-file-buffer file)
                            (let ((default-directory
                                   (or (and vm-folder-directory
                                            (expand-file-name vm-folder-directory))
                                       default-directory))
                                  (inhibit-local-variables t)
                                  (enable-local-variables nil)
                                  (enable-local-eval nil)
                                  ;; for Emacs/MULE
                                  (default-enable-multibyte-characters nil)
                                  ;; for XEmacs/Mule
                                  (coding-system-for-read
                                   (vm-line-ending-coding-system)))
                              (message "Reading %s..." file)
                              (prog1 (find-file-noselect file)
                                ;; update folder history
                                (let ((item (or remote-spec folder
                                                vm-primary-inbox)))
                                  (if (not (equal item (car vm-folder-history)))
                                      (setq vm-folder-history
                                            (cons item vm-folder-history))))
                                (message "Reading %s... done" file))))))))
        (set-buffer folder-buffer)
        (cond ((memq access-method '(pop imap))
               (if (not (equal folder-name (buffer-name)))
                   (rename-buffer folder-name t))))
        (if (and vm-fsfemacs-mule-p enable-multibyte-characters)
            (set-buffer-multibyte nil))
        ;; for MULE
        ;;
        ;; If the file coding system is not a no-conversion variant,
        ;; make it so by encoding all the text, then setting the
        ;; file coding system and decoding it.  This situation is
        ;; only possible if a file is visited and then vm-mode is
        ;; run on it afterwards.
        ;;
        ;; There are separate code blocks for FSF Emacs and XEmacs
        ;; because the coding systems have different names.
        (defvar buffer-file-coding-system)
        (if (and (or vm-xemacs-mule-p vm-xemacs-file-coding-p)
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-unix)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-dos)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-mac)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'binary))))
            (let ((buffer-read-only nil)
                  (omodified (buffer-modified-p)))
              (unwind-protect
                   (progn
                     (encode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system)
                     (set-buffer-file-coding-system 'no-conversion nil)
                     (decode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system))
                (set-buffer-modified-p omodified))))
        (if (and vm-fsfemacs-mule-p (null buffer-file-coding-system))
            (set-buffer-file-coding-system 'raw-text nil))
        (if (and vm-fsfemacs-mule-p
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-unix)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-mac)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-dos)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'no-conversion))))
            (let ((buffer-read-only nil)
                  (omodified (buffer-modified-p)))
              (unwind-protect
                   (progn
                     (encode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system)
                     (set-buffer-file-coding-system 'raw-text nil)
                     (decode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system))
                (set-buffer-modified-p omodified))))
        (vm-check-for-killed-summary)
        (vm-check-for-killed-presentation)
        ;; If the buffer's not modified then we know that there can be no
        ;; messages in the folder that are not on disk.
        (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
        (setq first-time (not (eq major-mode 'vm-mode))
              preserve-auto-save-file (and buffer-file-name
                                           (not (buffer-modified-p))
                                           (file-newer-than-file-p
                                            (make-auto-save-file-name)
                                            buffer-file-name)))
        ;; Force the folder to be read only if the auto
        ;; save file contains information the user might not
        ;; want overwritten, i.e. recover-file might be
        ;; desired.  What we want to avoid is an auto-save.
        ;; Making the folder read only will keep
        ;; subsequent actions from modifying the buffer in a
        ;; way that triggers an auto save.
        ;;
        ;; Also force the folder read-only if it was read only and
        ;; not already in vm-mode, since there's probably a good
        ;; reason for this.
        (setq vm-folder-read-only (or preserve-auto-save-file read-only
                                      (default-value 'vm-folder-read-only)
                                      (and first-time buffer-read-only)))
        ;; If this is not a VM mode buffer then some initialization
        ;; needs to be done 
        (if first-time
            (progn
              (buffer-disable-undo (current-buffer))
              (abbrev-mode 0)
              (auto-fill-mode 0)
              ;; If an 8-bit message arrives undeclared the 8-bit
              ;; characters in it should be displayed using the
              ;; user's default face charset, rather than as octal
              ;; escapes.
              (vm-fsfemacs-nonmule-display-8bit-chars)
              (vm-mode-internal access-method)
              (cond ((eq access-method 'pop)
                     (vm-set-folder-pop-maildrop-spec remote-spec))
                    ((eq access-method 'imap)
                     (vm-set-folder-imap-maildrop-spec remote-spec)))
              ;; If the buffer is modified we don't know if the
              ;; folder format has been changed to be different
              ;; from index file, so don't read the index file in
              ;; that case.
              (if (not (buffer-modified-p))
                  (setq did-read-index-file (vm-read-index-file-maybe)))))

        ;; builds message list, reads attributes if they weren't
        ;; read from an index file.
        (vm-assimilate-new-messages nil (not did-read-index-file) nil t)

        (if (and first-time (not did-read-index-file))
            (progn
              (vm-gobble-visible-header-variables)
              (vm-gobble-bookmark)
              (vm-gobble-pop-retrieved)
              (vm-gobble-imap-retrieved)
              (vm-gobble-summary)
              (vm-gobble-labels)))

        (if first-time
            (vm-start-itimers-if-needed))

        ;; make a new frame if the user wants one.  reuse an
        ;; existing frame that is showing this folder.
        (if (and full-startup
                 ;; this so that "emacs -f vm" doesn't create a frame.
                 this-command)
            (apply 'vm-goto-new-folder-frame-maybe
                   (if folder '(folder) '(primary-folder folder))))

        ;; raise frame if requested and apply startup window
        ;; configuration.
        (if full-startup
            (let ((buffer-to-display (or vm-summary-buffer
                                         vm-presentation-buffer
                                         (current-buffer))))
              (vm-display buffer-to-display buffer-to-display
                          (list this-command)
                          (list (or this-command 'vm) 'startup))
              (if vm-raise-frame-at-startup
                  (vm-raise-frame))))

        ;; say this NOW, before the non-previewers read a message,
        ;; alter the new message count and confuse themselves.
        (if full-startup
            (progn
              ;; save blurb so we can repeat it later as necessary.
              (set-buffer folder-buffer)
              (setq totals-blurb (vm-emit-totals-blurb))
              (and buffer-file-name
                   (vm-store-folder-totals buffer-file-name (cdr vm-totals)))))

        (vm-thoughtfully-select-message)
        (vm-update-summary-and-mode-line)
        ;; need to do this after any frame creation because the
        ;; toolbar sets frame-specific height and width specifiers.
        (vm-toolbar-install-or-uninstall-toolbar)

        (and vm-use-menus (vm-menu-support-possible-p)
             (vm-menu-install-visited-folders-menu))

        (if full-startup
            (progn
              (if (and (vm-should-generate-summary)
                       ;; don't generate a summary if recover-file is
                       ;; likely to happen, since recover-file does
                       ;; not work in a summary buffer.
                       (not preserve-auto-save-file))
                  (vm-summarize t nil))
              ;; raise the summary frame if the user wants frames
              ;; raised and if there is a summary frame.
              (if (and vm-summary-buffer
                       vm-mutable-frames
                       vm-frame-per-summary
                       vm-raise-frame-at-startup)
                  (vm-raise-frame))
              ;; if vm-mutable-windows is nil, the startup
              ;; configuration can't be applied, so do
              ;; something to get a VM buffer on the screen
              (if vm-mutable-windows
                  (vm-display nil nil (list this-command)
                              (list (or this-command 'vm) 'startup))
                  (save-excursion
                    (switch-to-buffer (or vm-summary-buffer
                                          vm-presentation-buffer
                                          (current-buffer)))))))

        (if vm-message-list
            ;; don't decode MIME if recover-file is
            ;; likely to happen, since recover-file does
            ;; not work in a presentation buffer.
            (let ((vm-auto-decode-mime-messages
                   (and vm-auto-decode-mime-messages
                        (not preserve-auto-save-file))))
              (vm-preview-current-message)))

        (run-hooks 'vm-visit-folder-hook)

        ;; Warn user about auto save file, if appropriate.
        (if (and full-startup preserve-auto-save-file)
            (message 
             (substitute-command-keys
              "Auto save file is newer; consider \\[recover-file].  FOLDER IS READ ONLY.")))
        ;; if we're not doing a full startup or if doing more would
        ;; trash the auto save file that we need to preserve,
        ;; stop here.
        (if (or (not full-startup) preserve-auto-save-file)
            (throw 'done t))
        
        (if full-startup
            (message totals-blurb))

        (if (and vm-auto-get-new-mail
                 (not vm-block-new-mail)
                 (not vm-folder-read-only))
            (progn
              (message "Checking for new mail for %s..."
                       (or buffer-file-name (buffer-name)))
              (if (vm-get-spooled-mail t)
                  (progn
                    (setq totals-blurb (vm-emit-totals-blurb))
                    (if (vm-thoughtfully-select-message)
                        (vm-preview-current-message)
                        (vm-update-summary-and-mode-line))))
              (message totals-blurb)))

        ;; Display copyright and copying info.
        (if (and (interactive-p) (not vm-startup-message-displayed))
            (progn
              (vm-display-startup-message)
              (if (not (input-pending-p))
                  (message totals-blurb)))))))

  ) ;;when


;;;----------------------------------------------------------------------------
;;; GNUS

;; (defadvice gnus-summary-mark-as-expirable
;;     (after gnus-summary-mark-as-expirable+next-line activate)
;;   (next-line))
;; (ad-disable-advice 'gnus-summary-mark-as-expirable 'after 'gnus-summary-mark-as-expirable+next-line)


;; (local-set-key (kbd "e") 'gnus-summary-mark-as-expirable)

(setf *pjb-gnus-trash-mailbox* "nnimap+voyager.informatimago.com:INBOX.Trash")
(setf *pjb-gnus-junk-mailbox*  "nnimap+voyager.informatimago.com:INBOX.Junk")




(define-key gnus-summary-mode-map (kbd "v DEL") 'pjb-gnus-summary-move-article-to-trash)
(define-key gnus-article-mode-map (kbd "v DEL") 'pjb-gnus-summary-move-article-to-trash)

(define-key gnus-summary-mode-map (kbd "v j")   'pjb-gnus-summary-move-article-to-junk)
(define-key gnus-article-mode-map (kbd "v j")   'pjb-gnus-summary-move-article-to-junk)

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


(defun pjb/erc-meat ()
  (interactive)
  (reset-movement-keypad)
  (setf erc-insert-timestamp-function 'pjb/erc-insert-timestamp-left
        erc-fill-function 'pjb/erc-fill-static)
  (remove-hook 'erc-insert-modify-hook 'erc-unmorse))


(add-hook 'erc-insert-post-hook 'pjb/erc-meat)

;; (add-hook 'erc-join-hook 'pjb-erc-join-meat)  
;; (pjb-set-erc-nickserv-passwords)
;; (setf erc-timestamp-format "%Y-%m-%d %H:%M\n")
;; (erc-match-mode 1)
;; (global-set-key (kbd "C-y") 'erc-yank)


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
;;   (let ((d (map 'vector 'digit-char-p word))
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

(defcustom erc-ignore-per-channel-alist nil
  "*A-List of regexps matching user identifiers to ignore, for each channel.

Some users are obnoxious only in some channels (eg. rudybot on #emacs).

A user identifier has the form \"nick!login@host\".  If an
identifier matches, the message from the person will not be
processed."
  :group 'erc-ignore
  :type '(repeat (cons string regexp)))

(defcustom erc-ignore-per-channel-reply-alist nil
  "*A-List of regexps matching user identifiers to ignore completely, for each channel.

Some users are obnoxious only in some channels (eg. rudybot on #emacs).


This differs from `erc-ignore-list' in that it also ignores any
messages directed at the user.

A user identifier has the form \"nick!login@host\".

If an identifier matches, or a message is addressed to a nick
whose identifier matches, the message will not be processed.

CAVEAT: ERC doesn't know about the user and host of anyone who
was already in the channel when you joined, but never said
anything, so it won't be able to match the user and host of those
people.  You can update the ERC internal info using /WHO *."
  :group 'erc-ignore
  :type '(repeat (cons string regexp)))

;; ;; Note: it would be better to have  per-server-per-channel variables…
;; (make-variable-buffer-local 'erc-ignore-per-channel-list) ; in server buffers.
;; (make-variable-buffer-local 'erc-ignore-per-channel-reply-list) ; in server buffers.


(defun erc-ignored-user-in-channel-p (msg tgt spec)
  "Return non-nil if SPEC matches something in `erc-ignore-list'.

Takes a full SPEC of a user in the form \"nick!login@host\", and
matches against all the regexp's in `erc-ignore-list'.  If any
match, returns that regexp."
  (loop
     for (channel . regexp) in (erc-with-server-buffer erc-ignore-per-channel-alist)
     thereis (and (string= channel tgt)
                  (string-match regexp spec))))


(defun erc-message-target (msg)
  "Return the addressed target in MSG.

The addressed target is the string before the first colon in MSG."
  (if (string-match "^\\([^:, \n]*\\):" msg)
      (match-string 1 msg)
    nil))


(defun erc-ignored-reply-p (msg tgt proc)
  ;; FIXME: this docstring needs fixing -- Lawrence 2004-01-08
  "Return non-nil if MSG matches something in `erc-ignore-reply-list'.

Takes a message MSG to a channel and returns non-nil if the addressed
user matches any regexp in `erc-ignore-reply-list'."
  (let ((target-nick (erc-message-target msg)))
    (if (not target-nick)
        nil
        (erc-with-buffer (tgt proc)
          (let ((user (erc-get-server-user target-nick)))
            (when user
              (let ((spec (erc-user-spec user)))
                (or (erc-list-match erc-ignore-reply-list spec)
                    (loop
                       for (channel . regexp) in (erc-with-server-buffer erc-ignore-per-channel-reply-alist)
                       thereis (and (string= channel tgt)
                                    (string-match regexp spec)))))))))))

(when (require 'erc-backend nil t)
  (define-erc-response-handler (PRIVMSG NOTICE)
      "Handle private messages, including messages in channels." nil
      (let ((sender-spec (erc-response.sender parsed))
            (cmd (erc-response.command parsed))
            (tgt (car (erc-response.command-args parsed)))
            (msg (erc-response.contents parsed)))
        (if (or (erc-ignored-user-p                    sender-spec)
                (erc-ignored-user-in-channel-p msg tgt sender-spec)
                (erc-ignored-reply-p           msg tgt proc))
            (when erc-minibuffer-ignored
              (message "Ignored %s from %s to %s for %s %s %s" cmd sender-spec tgt
                       (erc-ignored-user-p                    sender-spec)
                       (erc-ignored-user-in-channel-p msg tgt sender-spec)
                       (erc-ignored-reply-p           msg tgt proc)))
            (let* ((sndr (erc-parse-user sender-spec))
                   (nick (nth 0 sndr))
                   (login (nth 1 sndr))
                   (host (nth 2 sndr))
                   (msgp (string= cmd "PRIVMSG"))
                   (noticep (string= cmd "NOTICE"))
                   ;; S.B. downcase *both* tgt and current nick
                   (privp (erc-current-nick-p tgt))
                   s buffer
                   fnick)
              (setf (erc-response.contents parsed) msg)
              (setq buffer (erc-get-buffer (if privp nick tgt) proc))
              (when buffer
                (with-current-buffer buffer
                  ;; update the chat partner info.  Add to the list if private
                  ;; message.  We will accumulate private identities indefinitely
                  ;; at this point.
                  (erc-update-channel-member (if privp nick tgt) nick nick
                                             privp nil nil host login nil nil t)
                  (let ((cdata (erc-get-channel-user nick)))
                    (setq fnick (funcall erc-format-nick-function
                                         (car cdata) (cdr cdata))))))
              (cond
                ((erc-is-message-ctcp-p msg)
                 (setq s (if msgp
                             (erc-process-ctcp-query proc parsed nick login host)
                             (erc-process-ctcp-reply proc parsed nick login host
                                                     (match-string 1 msg)))))
                (t
                 (setcar erc-server-last-peers nick)
                 (setq s (erc-format-privmessage
                          (or fnick nick) msg
                          ;; If buffer is a query buffer,
                          ;; format the nick as for a channel.
                          (and (not (and buffer
                                         (erc-query-buffer-p buffer)
                                         erc-format-query-as-channel-p))
                               privp)
                          msgp))))
              (when s
                (if (and noticep privp)
                    (progn
                      (run-hook-with-args 'erc-echo-notice-always-hook
                                          s parsed buffer nick)
                      (run-hook-with-args-until-success
                       'erc-echo-notice-hook s parsed buffer nick))
                    (erc-display-message parsed nil buffer s)))
              (when (string= cmd "PRIVMSG")
                (erc-auto-query proc parsed)))))))

;;;----------------------------------------------------------------------------

(defparameter *pjb-erc-answers*
  '((lisp-1         . "Please read: http://www.nhplace.com/kent/Papers/Technical-Issues.html")
    (equal          . "Please read: http://www.nhplace.com/kent/PS/EQUAL.html")
    (ambitious-eval . "Please read: http://www.nhplace.com/kent/PS/Ambitious.html")
    (choice         . "To get help choosing a CL implementation, connect to telnet://voyager.informatimago.com:8101 ; have a look at http://www.cliki.net/Common%20Lisp%20implementation")
    (clhs           . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
    (intersection   . "Have a look at (intersection common-lisp emacs-lisp scheme) http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/intersection-r5rs-common-lisp-emacs-lisp/")
    (scheme-or-cl   . "CL vs. Scheme http://irreal.org/blog/?p=813")
    (cliki          . "Have a look at http://cliki.net/ ; start with http://www.cliki.net/Getting%20Started")
    (getting-started  . "Start with http://www.cliki.net/Getting%20Started")
    (emacs-lisp-intro . "An Introduction to Programming in Emacs Lisp  http://www.gnu.org/software/emacs/emacs-lisp-intro/  or  M-: (info \"(eintr)Top\") RET (for non-programmers)")
    (emacs-lisp       . "Emacs Lisp Manual http://www.gnu.org/software/emacs/manual/elisp.html  or  M-: (info \"(elisp)Top\") RET")
    (emacs-manual     . "Emacs Manual http://www.gnu.org/software/emacs/manual/   or  M-: (info \"(emacs)Top\") RET")
    (taoup     . "The Art of Unix Programming http://www.faqs.org/docs/artu/")
    (htbah     . "http://www.catb.org/~esr/faqs/hacker-howto.html")
    (tcote     . "The Craft of Text Editing   http://www.finseth.com/craft/")
    (eopl      . "Essentials of Programming Languages, 3rd ed.   Daniel P. Friedman and Mitchell Wand   ISBN: 978-0-262-06279-4   http://MITPress.MIT.Edu/0262062798/  http://WWW.EoPL3.Com/")
    (pcl       . "Practical Common Lisp http://www.gigamonkeys.com/book/")
    (gentle    . "Common Lisp: A Gentle Introduction to Symbolic Computation  http://www.cs.cmu.edu/~dst/LispBook/  http://www-cgi.cs.cmu.edu/afs/cs.cmu.edu/user/dst/www/LispBook/index.html")
    (clpfai    . "Common Lisp Programming for Artificial Intelligence  Tony Hasemer & John Domingue - 1989  International Computer Science Series  Addison & Wesley  ISBN 0-201-17579-7")
    (claia     . "Common Lisp: An Interactive Approach  by Stuart C. Shapiro   http://www.cse.buffalo.edu/~shapiro/Commonlisp/")
    (paip      . "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp")
    (aima      . "Artificial Intelligence: A Modern Approach  http://aima.cs.berkeley.edu")
    (sicp      . "Structure and Interpretation of Computer Programs  http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html  http://swiss.csail.mit.edu/classes/6.001/abelson-sussman-lectures/")
    (sicp-mit  . "http://web.mit.edu/alexmv/6.S184/")
    (6.S184    . "http://web.mit.edu/alexmv/6.S184/")
    ;; http://www.codepoetics.com/wiki/index.php?title=Topics:SICP_in_other_languages
    ;; http://eli.thegreenplace.net/category/programming/lisp/sicp/
    ;; http://www.neilvandyke.org/sicp-plt/
    ;; http://www.youtube.com/watch?v=rdj6deraQ6k
    (r5rs      . "http://www.schemers.org/Documents/Standards/R5RS/HTML/")
    (htdp      . "How to Design Programs -- An Introduction to Computing and Programming  http://www.htdp.org/2003-09-26/Book/  ")
    (ca        . "Concrete Abstractions -- An Introduction to Computer Science Using Scheme  http://www.gustavus.edu/+max/concrete-abstractions.html")
    (lisp      . "Lisp in Small Pieces   http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html  http://pagesperso-systeme.lip6.fr/Christian.Queinnec/Books/LiSP-2ndEdition-2006Dec11.tgz")
    (onlisp    . "On Lisp  Paul Graham   http://www.paulgraham.com/onlisptext.html  http://www.bookshelf.jp/texi/onlisp/onlisp.html  http://www.bookshelf.jp/texi/onlisp/onlisp.tar.gz")
    (cptt      . "Compiler Principles Techniques and Tools, Aho et al. http://dragonbook.stanford.edu/")
    (taocp     . "The Art of Computer Programming  Donald E. Knuth  Addison & Wesley")
    (geb       . "Gödel, Escher, Bach: An Eternal Golden Braid  Douglas Hofstadter")
    (blt       . "Basic Lisp Techniques  Cooper - 2003 Franz, Inc. - 100 pages.  http://www.franz.com/resources/educational_resources/cooper.book.pdf")
    (casting   . "Casting Spels in Lisp  Conrad Barski, M.D.  http://www.lisperati.com/casting.html")
    (spell     . "Casting Spels in Lisp  Conrad Barski, M.D.  http://www.lisperati.com/casting.html")

    (gitorious-lisp  . "https://gitorious.org/com-informatimago/com-informatimago/trees/master")
    (gitorious-emacs . "https://gitorious.org/com-informatimago/emacs/trees/master")
    (rc        . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/rc")
    (bin       . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/bin")
    (idiots    . "There, there, we know there are idiots on the Internet.  Lisp will make it all better.")
    (implementation       . "what-implementation is at telnet://clis.informatimago.com:8101")
    (what-implementation  . "what-implementation is at telnet://clis.informatimago.com:8101")
    (float . "What Every Computer Scientist Should Know About Floating-Point Arithmetic http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html") 
    (ibcl . "Image Based Development http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/index.html")
    (see-defpackage . ";;;;    See defpackage documentation string.\n")
    (agpl3          . "
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

(defvar *pjb-speak-file-counter* 0)

(defun pjb-speak-file ()
  (format "%s/speak-%d.txt" *tempdir* (incf *pjb-speak-file-counter*)))


(defvar *pjb-speak-last-message* nil)

(defun speak (message)
  (interactive "sMessage: ")
  (let ((file (pjb-speak-file)))
    (with-current-buffer (get-buffer-create " *speak text*")
      (erase-buffer)
      (insert message)
      (setf *pjb-speak-last-message* message)
      (write-region (point-min) (point-max) file))
    (shell-command (format "speak -f %s" file))))

(defalias 'say 'speak)

(defun speak-repeat ()
  (interactive)
  (speak *pjb-speak-last-message*))



(defparameter *pjb-erc-spoken-nicks*
  '(("\\<e1f\\>"          . "elf")
    ("\\<tali[0-9]+"      . "tali")
    ("\\<fsbot\\>"        . "F. S. Bot")
    ("\\<qu1j0t3\\>"      . "quijote")
    ("\\<chromaticwt\\>"  . "chromatic W. T.")
    ("\\<jcowan\\>"       . "J. Cowan")
    ("\\<cky\\>"          . "C. K. Y.")
    ("\\<pjb\\>"          . "Pascal")
    ("\\<H4ns\\>"         . "Hans")
    ("\\<Corman[0-9]+\\>" . "Corman"))
  "An a-list mapping regexps of nicks to the corresponding text to be read aloud.")


(defun pjb-erc-spoken-nick (nick)
  "
RETURN:  The text to be read aloud for the `nick' in `*pjb-erc-spoken-nicks*'.
"
  (let ((entry (assoc* nick *pjb-erc-spoken-nicks*
                       :test (lambda (nick ref) (string-match ref nick)))))
    (if entry
        (cdr entry)
        nick)))


(defun erc-response.recipient (response)
  (first (erc-response.command-args response)))

(defun erc-response.sender-nick (response)
  (let ((sender (erc-response.sender response)))
   (subseq sender 0 (position ?! sender))))


(defparameter *pjb-erc-massage-substitutions*
  '(("\\<pjb\\>"                 "Pascal")
    ("\\<CL\\>"                  "See Ell") 
    ("\\<C-"                     "Control-")
    ("\\<M-"                     "Meta-")
    ("\\<A-"                     "Alt-")
    ("\\<S-"                     "Shift-")
    ("\\<s-"                     "super-")
    ("\\<H-"                     "Hyper-")
    ("\\(:-?)\\|(-?:\\)"         "AhAhAh!")
    (":-?("                      "BooBooBoo!")
    (":-/"                       "muek")
    (":-?[Pp]"                   "bruu")
    ("\\<\\(ty\\|thx\\)\\>"      "Thank you!")
    ("\\<LOL\\>"                 "AhAhAh! Laughting Out Loud!") 
    ("\\<ROFL\\>"                "AhAhAh! Rolling On the Floor!")
    ("\\<hrm\\>"                 "errrmmm") 
    ("\\<btw\\>"                 "by the way")
    ("\\<wtf\\>"                 "what the fuck")
    ("\\<imo\\>"                 "in my opinion")
    ("\\<imho\\>"                "in my humble opinion")
    ("\\<imnsho\\>"              "in my not so humble opinion")))


(defun pjb-erc-massage-message (message)
  (with-current-buffer (get-buffer-create "*pjb massage text*")
    (erase-buffer)
    (insert message)
    (let ((case-fold-search nil))
      (loop
         for (reg sub) in *pjb-erc-massage-substitutions*
         do (progn
              (goto-char (point-min))
              (loop
                 while (re-search-forward reg nil t)
                 do (progn
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert sub))))))
    (buffer-string)))



(defvar *pjb-erc-speak-reject-recipient* '()
  "can be:
nil   don't reject any channel.
:all  reject every channel.
or a list of nicknames or channel names \"nick\" \"\#chan\"
to reject (never speak them aloud).
See: `*pjb-erc-speak-reject-sender*', `*pjb-erc-speak-accept-sender*',
      and `pjb-erc-privmsg-meat'.

Messages are spoken if the recipient
")

(defvar *pjb-erc-speak-reject-sender* '()
  "can be:
nil   don't reject anybody.
:all  reject everybody.
or a list of nicknames or channel names \"nick\" \"\#chan\"
to reject (never speak them aloud).
See: `*pjb-erc-speak-reject-recipient*', `*pjb-erc-speak-accept-sender*',
      and `pjb-erc-privmsg-meat'.
")

(defvar *pjb-erc-speak-accept-sender* '()
  "can be:
nil   don't accept anything.
:all  accept everything.
or a list of nicknames or channel names \"nick\" \"\#chan\"
to accept (speak them aloud).
See: `*pjb-erc-speak-reject-recipient*', `*pjb-erc-speak-reject-sender*',
      and `pjb-erc-privmsg-meat'.
")

(setf *pjb-erc-speak-reject-recipient* '("#emacs")
      *pjb-erc-speak-reject-recipient* :all
      *pjb-erc-speak-reject-sender*    :all
      *pjb-erc-speak-accept-sender*    '("Posterdati" "pjb-"))


(defvar *pjb-erc-speak-last-speaker* nil)


(defun pjb-erc-privmsg-meat (process response)
  "The messages are spoken if the sender is in `*pjb-erc-speak-accept-sender*',
or the sender is not in `*pjb-erc-speak-reject-sender*',
or the recipient is not in `*pjb-erc-speak-reject-recipient*',
"
  (when (or
         (case *pjb-erc-speak-accept-sender*
           ((nil)    nil)
           ((:all t) t)
           (otherwise (member* (erc-response.sender-nick response)
                               *pjb-erc-speak-accept-sender* :test 'string=)))
         (case *pjb-erc-speak-reject-sender*
           ((nil)    t)
           ((:all t) nil)
           (otherwise (not (member* (erc-response.sender-nick response)
                                    *pjb-erc-speak-reject-sender* :test 'string=))))
         (case *pjb-erc-speak-reject-recipient*
           ((nil)    t)
           ((:all t) nil)
           (otherwise (not (member* (erc-response.recipient response)
                                    *pjb-erc-speak-reject-recipient* :test 'string=)))))
    (speak (let* ((nick (pjb-erc-spoken-nick (erc-response.sender-nick response)))
                  (chan (pjb-erc-spoken-nick (remove ?# (erc-response.recipient response))))
                  (mesg (pjb-erc-massage-message (erc-response.contents response))))
             (if (equal *pjb-erc-speak-last-speaker*
                        (cons nick chan))
                 (format "%s" mesg)
                 (progn
                   (setf *pjb-erc-speak-last-speaker* (cons nick chan))
                   (format "%s said to %s: ... %s" nick chan mesg))))))
  nil)


(defun pjb-erc-speak-on ()
  (interactive)
  (pushnew 'pjb-erc-privmsg-meat  erc-server-PRIVMSG-functions))

(defun pjb-erc-speak-off  ()
  (interactive)
  (setf erc-server-PRIVMSG-functions
        (remove 'pjb-erc-privmsg-meat  erc-server-PRIVMSG-functions)))


;;;----------------------------------------------------------------------------

(defvar *galatea-frame* nil)


(defun open-frame-on-galatea ()
  (interactive)
  (unless *galatea-frame*
    (setq *galatea-frame*
          (make-frame-on-display "galatea.informatimago.com:0.0")))
  (set-frame-size  *galatea-frame* 96 40)
  (let ((current-frame (selected-frame)))
    (select-frame *galatea-frame*)
    (set-background-color "#102040")
    (set-foreground-color "#80f0f0")
    ;;(set-face-foreground 'font-lock-comment-face "Green")
    ;;(set-face-foreground 'font-lock-function-name-face "Yellow")
    (select-frame current-frame))
  (setq common-lisp-hyperspec-frame *galatea-frame*))


(defun reopen-frame-on-galatea ()
  (interactive)
  (when *galatea-frame*
    (delete-frame *galatea-frame*)
    (setq *galatea-frame* nil))
  (open-frame-on-galatea))

;;;----------------------------------------------------------------------------
(.EMACS "server")

(setf server-socket-dir *tempdir*
      server-name       (format "server-%d" (emacs-pid)))


(defparameter *frame-server-job-ticket* "~/frame-emacs"
  "Path to the job-ticket file.")


(defun frame-server (&optional token-path)
  (setf token-path (or token-path *frame-server-job-ticket*))
  (when (file-exists-p token-path)
    (find-file token-path)
    (make-frame-on-display
     (delete ?\n (prog1 (buffer-string)
                   (kill-buffer (current-buffer))
                   (delete-file token-path)))
     (list (cons 'name (format "n%s" (frame-parameter nil 'name)))))))

(defun frame-server-start ()
  (interactive)
  (run-at-time nil 5 (function frame-server) nil))

(frame-server-start)



(cond
  (*pjb-pvs-is-running*)
  ((member "(gnus)"  command-line-args)
   (setf uptimes-auto-save-interval (* 7 60))
   (setf *activity-tag* "GNUS")
   (push '(name . "GNUS") default-frame-alist)
   (set-background-color "#ccccfefeebb7")
   ;; (when (fboundp 'set-default-frame-alist)
   ;;   (set-default-frame-alist *default-font*))
   (setf *frame-server-job-ticket* "~/frame-gnus"))
  ((member "(irc)"  command-line-args)
   (setf uptimes-auto-save-interval (* 11 60))
   (setf *activity-tag* "ERC")
   (push '(name . "ERC") default-frame-alist)
   (setf *frame-server-job-ticket* "~/frame-erc")
   ;; (when (fboundp 'set-default-frame-alist)
   ;;   (set-default-frame-alist *default-font*))
   )
  (t
   (setf *activity-tag* "EMACS")
   (setf uptimes-auto-save-interval (* 13 60))
   (push '(name . "PGM") default-frame-alist)

   (server-start)
   
   (setf (getenv "CVSEDITOR")  "emacsclient"
         (getenv "EDITOR")     "emacsclient"
         (getenv "VISUAL")     "emacsclient")
   (setf *frame-server-job-ticket* "~/frame-emacs")
   ;; (when (fboundp 'set-default-frame-alist)
   ;;   (set-default-frame-alist *default-font*))
   ))

;;;----------------------------------------------------------------------------


;; (defvar pjb-save-buffer-skip nil
;;   "*(buffer-local) Skip the update-def-names and pjb-update-eof
;; when saving this buffer.")
;; (make-variable-buffer-local 'pjb-save-buffer-skip)
;; (defun psb-xor (p q) "RETURN:  p xor q" (not (eq (not p) (not q))))
;; (defun pjb-save-buffer (&optional args)
;;   "This advice updates the eof comment before saving the buffer."
;;   (interactive "p")
;;   (when (and (not pjb-save-buffer-skip)
;;              (psb-xor current-prefix-arg
;;                       (string-match
;;                        "/pascal/\|/pjb/"
;;                        ;;(format  "^\\(%s\\|/local/users/pascal\\)/"
;;                        ;;      (regexp-quote (USER-HOMEDIR-PATHNAME)))
;;                        (or (buffer-file-name (current-buffer)) ""))))
;;     (unwind-protect
;;          (when (memq major-mode  '(lisp-mode emacs-lisp-mode))
;;            (.EMACS "Updating definition names...")
;;            (update-def-names))
;;       (.EMACS "Updating EOF tag...")
;;       (pjb-update-eof t)))
;;   (.EMACS "Saving buffer...")
;;   (save-buffer args)) ;;pjb-save-buffer

;; (when (and (fboundp 'update-def-names) (fboundp 'pjb-update-eof))
;;   (global-set-key "\C-x\C-s"    'pjb-save-buffer))
(defalias 'pjb-save-buffer 'save-buffer)


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

(dolist (hooks  '(lisp-mode-hook emacs-lisp-mode-hook common-lisp-mode-hook
                  c-mode-hook c++-mode-hook))
  (add-hook hooks 'sexp-movement))


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
  (unless (intersection
           '("-f" "-funcall" "--funcall" "-e" "-eval" "--eval" "-execute"
             "--execute" "-insert" "--insert") command-line-args
             :test (function string=))
    (afaire)))


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

(defun apple-search (search-string)
  "Search a string with Apple."
  (interactive "sApple Developer Documentation Search: ")
  (browse-url
   (format "https://developer.apple.com/library/mac/search/?q=%s"
	   (browse-url-url-encode-chars
	    (string-trim *whitespaces* search-string)
	    "[^A-Za-z0-9]"))))

(defun apple-search-region (start end)
  "Search the text in the region with Apple."
  (interactive "r")
  (%search-region start end 'symbol 'apple-search))

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
                  (format "http://developer.android.com/index.html#q=%s"
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

(global-set-key (kbd "C-h 1") 'apple-search-region)
(global-set-key (kbd "C-h 2") 'google-search-region)
(global-set-key (kbd "C-h 3") 'acronym-search-region)
(global-set-key (kbd "C-h 4") 'project-search-region)
(global-set-key (kbd "C-h 5") 'includes-search-region)
(global-set-key (kbd "C-h 6") 'hyperspec-search-region)
(global-set-key (kbd "C-h 7") 'here-search-region)

(add-hook 'objc-mode-hook (lambda ()
                            (interactive)
                            (local-set-key (kbd "C-h 1") 'apple-search-region)))

(add-hook 'java-mode-hook (lambda ()
                            (interactive)
                            (local-set-key (kbd "C-h 1") 'android-search-region)))

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
    (format "find ~/works/patchwork/patchwork/src/ ~/works/patchwork/src/mcl-unix -name \\*.lisp -print0 | xargs -0  grep -niH -e %S" what)))) 
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

(milliways-schedule (lambda () (sfn t)))

;; (setf (getenv "EMACS_USE") "erc")
;; (setf (getenv "EMACS_USE") "gnus")
;; (setf (getenv "EMACS_USE") "pgm")


(cond
  ((string= (getenv "EMACS_USE") "erc")
   (when (fboundp 'set-palette) (set-palette pal-dark-blue))
   (set-frame-name "ERC")
   (erc-select))
  ((string= (getenv "EMACS_USE") "gnus")
   (when (fboundp 'set-palette) (set-palette pal-dark-amber))
   (gnus))
  (t
   (when (fboundp 'set-palette) (set-palette pal-green))))


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

(defun set-random-colors ()
  (interactive)
  (set-background-color (get-random-color))
  (set-foreground-color (get-random-color)))

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

(defvar *echo-keys-last* nil "Last command processed by `echo-keys'.")

(defun echo-keys ()
  (interactive)
  (let ((deactivate-mark deactivate-mark))
   (when (this-command-keys)
     (with-current-buffer (get-buffer-create "*echo-key*")
       (goto-char (point-max))
       ;; self  self
       ;; self  other \n
       ;; other self  \n
       ;; other other \n
       (unless (and (eq 'self-insert-command *echo-keys-last*)
                    (eq 'self-insert-command this-command))
         (insert "\n"))
       (if (eql this-command 'self-insert-command)
           (let ((desc (key-description (this-command-keys))))
             (if (= 1 (length desc))
                 (insert desc)
                 (insert " " desc " ")))
           (insert (key-description (this-command-keys))))
       (setf *echo-keys-last* this-command)
       (dolist (window (window-list))
         (when (eq (window-buffer window) (current-buffer))
           ;; We need to use both to get the effect.
           (set-window-point window (point))
           (end-of-buffer)))))))


(defun toggle-echo-keys ()
  (interactive)
  (if (member 'echo-keys  pre-command-hook)
      (progn
        (remove-hook 'pre-command-hook 'echo-keys)
        (dolist (window (window-list))
          (when (eq (window-buffer window) (get-buffer "*echo-key*"))
            (delete-window window))))
      (progn
        (add-hook    'pre-command-hook 'echo-keys)
        (delete-other-windows)
        (split-window nil (- (window-width) 32) t)
        (other-window 1)
        (switch-to-buffer (get-buffer-create "*echo-key*"))
        (set-window-dedicated-p (selected-window) t)
        (other-window 1))))

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

;;;; THE END ;;;;
