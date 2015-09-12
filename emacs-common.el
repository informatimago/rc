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
;;; Start up.
;;;----------------------------------------------------------------------------
(setq-default lexical-binding t)
(setq byte-compile-warnings '(not obsolete))
(defvar *emacs-start-time*       (current-time) "For (emacs-uptime).")
(if (string= emacs-version "25.0.50.1")
    (setq source-directory
          ;; "/usr/local/src/emacs/src"
          "~/works/emacs/src")
    (setq source-directory (format "/usr/local/src/emacs-%s/src" emacs-version)))

;;;----------------------------------------------------------------------------
;;; Message Log
;;;----------------------------------------------------------------------------
(defvar *pjb-load-noerror*       t)
(defvar *pjb-load-silent*        nil)
(defvar *pjb-light-emacs*        nil "pjb-loader will load the minimum.")
(defvar *pjb-pvs-is-running*     (and (boundp 'x-resource-name)
                                      (string-equal x-resource-name "pvs")))


(defvar shell-file-name          "/bin/bash")
(defvar *tempdir*                (format "/tmp/emacs%d" (user-uid)))
(defvar *pjb-save-log-file-p*    nil "Whether .EMACS must save logs to /tmp/messages.txt")

(defun .EMACS (fctl &rest args)
  (if (file-exists-p "--version.lock")
	(error "version lock"))
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
;;; Essential configuration I want to have even when .emacs breaks.
;;;
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
        (transient-mark-mode +1)
        (goto-address-mode   +1)))

;; (progn (scroll-bar-mode -1) (menu-bar-mode -1) (tool-bar-mode -1) (transient-mark-mode +1))

(defun mac-adjust-full-screen ()
  (interactive)
  (tool-bar-mode +1)
  (tool-bar-mode -1)
  (ff -1))

(defun mac-vnc-keys ()
  (interactive)
  (setf mac-command-modifier    'alt ; emacsformacosx
        mac-option-modifier     'meta
        one-buffer-one-frame    nil)
  (setf mac-command-key-is-meta nil  ; which emacs?
        mac-reverse-ctrl-meta   nil))

(defun mac-vanilla-keys ()
  (interactive)
  (setf mac-command-modifier    'meta ; emacsformacosx
        mac-option-modifier     'alt
        one-buffer-one-frame    nil)
  (setf mac-command-key-is-meta t     ; which emacs?
        mac-reverse-ctrl-meta   nil))

;; MacOSX Modifiers:
;; C-
;; S-                     S-
;; C- A- M- SPC M- A- C-p C-


(when (or (boundp 'aquamacs-version) (eq window-system 'ns))
  (mac-vanilla-keys)
  ;; (if 'thru-vnc
  ;;     (mac-vnc-keys)
  ;;     (mac-vanilla-keys))
  (cua-mode 0))

(when (boundp 'x-toolkit-scroll-bars)
  (setf x-toolkit-scroll-bars nil))


;;;----------------------------------------------------------------------------

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

(defvar *hostname*
  (or (and (boundp 'system-name) system-name)
      (and (fboundp 'system-name) (system-name))
      (ignore-errors
        (shell-command-to-string
         "echo -n $( (hostname -f 2>/dev/null) || (hostname 2>/dev/null) )")
        "localhost")))


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

(set-default-file-modes #o755)


(setf open-paren-in-column-0-is-defun-start nil)
(setf minibuffer-max-depth nil)
(setf print-circle t)
(setf server-socket-dir *tempdir*
      server-name       (format "server-%d" (emacs-pid)))

(setf tetris-score-file "~/.tetris-scores")

(setf fancy-splash-text  '(
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


(setf visible-bell nil)
(when (eq window-system 'x)
  (setf ring-bell-function 
        (lambda ()
          (call-process-shell-command "xset led;sleep 0.1;xset -led;sleep 0.05;xset led;sleep 0.1;xset -led;sleep 0.05;xset led;sleep 0.2;xset -led" nil 0 nil))))






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



;;;----------------------------------------------------------------------------
;;; Setting up load-path & exec-path
;;;----------------------------------------------------------------------------
;;;
;;; When we start, emacs has already filled load-path with
;;; installation-local directories.
;;;
;;; So we only need to add the directories of specific packages (that
;;; could be used in the various emacs installations).  It also means
;;; that installing an emacs package must occur either in an emacs
;;; specific installation (notably if .elc are compiled for this
;;; specific version), or in  the package specific directory.
;;;
;;; If any of these directories contain one of the site or subdir el
;;; files, then it is loaded too.
;;;


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
         (mapcar (lambda (path)
                   (if (string-match "^\\(.*[^/]\\)/*$" path)
                       (match-string 1 path)
                       path))
                 load-path)
         :test (function string=))))


(defun load-pathname (file &optional nosuffix must-suffix)
  "Return the pathname of the file that would be loaded by (load file)."
  (let* ((file (substitute-in-file-name file))
         (size (length file)))
    (unless (zerop size)
      (when (and must-suffix
                 (or (and (< 3 size) (string= ".el"  (substring file (- size 3))))
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




(defun pjb-setup-load-path ()
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



(.EMACS "Loading my personal files -- My own stuff.")
(pjb-setup-load-path)
(unless (load "pjb-loader.el" t) (.EMACS "ERROR: Could not find and load 'My own stuff'!"))
(load "~/rc/emacs-directories")


(defun pjb-setup-exec-path ()
  (map-existing-files (lambda (dir) (pushnew dir exec-path))
                      (cons (expand-file-name "~/bin/")
                            '("/sw/sbin/"       "/sw/bin/"
                              "/usr/local/sbin" "/usr/local/bin"
                              "/opt/local/sbin" "/opt/local/bin")))
  (setf (getenv "PATH") (mapconcat (function identity) exec-path ":")))


(pjb-setup-exec-path)



;; (message "old load-path = %S" (with-output-to-string (dump-load-path)))
;; (message "new load-path = %S" (with-output-to-string (dump-load-path)))



;; (autoload 'd-mode "/usr/local/src/languages/clisp/clisp-cvs/clisp/emacs/d-mode"
;;   "Mode to edit clisp sources." t)

(deletef auto-mode-alist 'd-mode :key (function cdr))
;; (setq auto-mode-alist (append '(("\\.c\\'" . c-mode)) auto-mode-alist))
(appendf auto-mode-alist  '(("\\.pp\\'"                     . pascal-mode)
                            ("\\.\\(m[id]\\|mod\\|def\\)$"  . modula-2-mode)
                            ("-MIB$\\|-SMI$"                . snmp-mode)
                            ("\\.bison\\'"                  . c-mode)
                            ("\\.lex\\'"                    . c-mode)
                            ("\\.d\\'"                      . makefile-mode)))


(appendf auto-mode-alist '(("\\.jmf$"    . java-mode)
                           ("\\.j$"      . java-mode)))

(appendf auto-mode-alist '(("\\.pl1$"    . pl1-mode)))

(appendf auto-mode-alist '(("\\.html\\.in$"  . html-mode)))

;;;----------------------------------------------------------------------------
;;; PAREDIT: essential!
;;;----------------------------------------------------------------------------
(load "paredit")

(defvar pjb-paredit-space-for-delimiter-predicates '()
  "A list of predicates taking a buffer start end range indicating
whether no space should be inserted at the end before an opening
parenthesis.
The disjonction of all predicates is used.")

(defun pjb-dispatching-reader-macros-p (start end)
  "Whether there is a dispatching reader macro instance from `start' to `end'."
  (message "previous: %S" (buffer-substring-no-properties start end))
  (goto-char start)
  (and (looking-at "\\(#[0-9]*[^0-9]\\)")
       (= end (match-end 0))))

(defun pjb-comma-at-p (start end)
  "Whether there is ` ,@' just before `end'."
  (message "previous: %S" (buffer-substring-no-properties start end))
  (when (<= (point-min) (- end 3))
    (goto-char (- end 3))
    (looking-at " ,@")))

(defun pjb-at-p (start end)
  "Whether there is `@' just before `end'.
Useful for Objective-CL reader macros."
  (message "previous: %S" (buffer-substring-no-properties start end))
  (when (<= (point-min) (- end 3))
    (goto-char (- end 1))
    (looking-at "@")))

(defun pjb-paredit-space-for-delimiter-p/predicates (endp delimiter)
  (not (and (not endp)
            (save-excursion
             (let ((end (point))
                   (start (progn (backward-sexp) (point))))
               (some (lambda (predicate)
                       (funcall predicate start end))
                     pjb-paredit-space-for-delimiter-predicates)))
            t)))

(push 'pjb-dispatching-reader-macros-p          pjb-paredit-space-for-delimiter-predicates)
(push 'pjb-comma-at-p                           pjb-paredit-space-for-delimiter-predicates)
(push 'pjb-at-p                                 pjb-paredit-space-for-delimiter-predicates)
(push 'pjb-paredit-space-for-delimiter-p/predicates paredit-space-for-delimiter-predicates)
;; (setf  paredit-space-for-delimiter-predicates '(pjb-paredit-space-for-delimiter-p/predicates))

;; (defun bagger-lambda-p (start end)
;;   (goto-char start)
;;   (and (looking-at "λ")
;;        (= end (match-end 0))))
;; (push 'bagger-lambda-p pjb-paredit-space-for-delimiter-predicates)



;;;----------------------------------------------------------------------------
;;; Other packages.
;;;----------------------------------------------------------------------------


(require 'highlight-flet nil t)
(require 'rst nil t)
(require 'rst-mode nil t)
(mouse-avoidance-mode 'cat-and-mouse)



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


(when (< emacs-major-version 22)
  ;; feature simple is not provided on emacs < 22, so we use load-library:
  (load-library "simple"))
(require 'iso-transl)
(define-key ctl-x-map     "." nil)
(define-key iso-transl-ctl-x-8-map "E" [342604])


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
  (local-set-key  (kbd "<home>")        'beginning-of-buffer)
  (local-set-key  (kbd "<end>")         'end-of-buffer)
  (local-set-key  (kbd "<prior>")       'scroll-down)
  (local-set-key  (kbd "<next>")        'scroll-up)
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
  (global-set-key (kbd "C-<f10>") (lambda()(interactive)(set-input-method 'cyrillic-jis-russian))) ;'cyrillic-yawerty
  (global-set-key (kbd "C-<f10>") (lambda(&optional alternate)
                                    (interactive "P")
                                    (set-input-method (if alternate
                                                          'russian-typewriter
                                                          'russian-computer))))
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

  (global-set-key (kbd "H-<up>")    'backward-same-indent)
  (global-set-key (kbd "H-<down>")  'forward-same-indent)
  (global-set-key (kbd "H-`")       'next-error)

  nil)

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



;;------------------------------------------------------------------------
(pjb-terminal-key-bindings)
(pjb-global-key-bindings)
(pjb-function-keys)
;;------------------------------------------------------------------------

;; some more global key map are defined after loading my personal files below.




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

(when (and (boundp 'elscreen-display-tab) elscreen-display-tab)
  (elscreen-toggle-display-tab))

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
(when (require 'column-marker nil t)
  (.EMACS "columnmarker")
  (column-marker-1 80))

;;;----------------------------------------------------------------------------
(when (file-exists-p "/usr/local/share/emacs/bigloo/")
  (.EMACS "bee for bigloo")
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
(.EMACS "psgml mode")
(when (require 'psgml nil t)
  ;;(define-key sgml-mode-map "\M-\C-f"   'set-justification-full)
  (appendf auto-mode-alist '(("\\.html$"   . html-mode)
                             ("\\.htm$"    . html-mode))))

(defun pjb-make-hyperlink (start end)
  (interactive "r")
  (let ((url (buffer-substring start end)))
    (delete-region start end)
    (insert (format (if prefix-arg
                        "<a href=\"%s\">%s</a>"
                        "<a href=\\\"%s\\\">%s</a>") url url))))

(global-set-key (kbd "H-h a") 'pjb-make-hyperlink)


;;;----------------------------------------------------------------------------
(.EMACS "css mode")
(autoload 'css-mode "css-mode" "Major mode for editing CSS" t)
(appendf auto-mode-alist '(("\\.css\\'" . css-mode)))
(setf cssm-indent-function (function cssm-c-style-indenter))


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


;; ------------------------------------------------------------------------
(.EMACS "emms")
(defun lac-emms ()
  "Load and configure emms"
  (.EMACS "Load and configure emms")
  (when (require 'emms-setup nil t)
    (require 'emms-player-simple)
    (require 'emms-source-file)
    (require 'emms-source-playlist)
    (require 'emms-volume)
    ;; (require 'emms-history) ;; save playlist and load at emacs start
    (when (require 'emms-info-id3v2 nil t)
      (add-to-list 'emms-info-functions 'emms-info-id3v2))

    ;; (emms-standard)
    (emms-all)
    (emms-default-players)
    ;; (emms-history-load)
    (setq emms-playlist-default-major-mode 'emms-playlist-mode)
    ;; (setq emms-repeat-playlist 1)

    (global-set-key (kbd "C-c =") 'emms-volume-mode-plus)
    (global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
    (global-set-key (kbd "C-c -") 'emms-volume-mode-minus)
    (global-set-key (kbd "<f9>")  'emms-pause)
    (global-set-key (kbd "<f10>") 'emms-seek-backward)

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
    
    'lac-emms)
  (.EMACS "Load and configure emms complete."))

(add-lac 'lac-emms)
(lac-emms)


;;;----------------------------------------------------------------------------
;;; Distribution installs crypt++...
;;;
(deletef find-file-hook 'crypt-find-file-hook)

(defun find-file-read-args (prompt mustmatch)
  (list (read-file-name prompt nil default-directory mustmatch nil
                        (lambda (name) (not (string-match "~$" name))))
        t))


;; ;;;----------------------------------------------------------------------------
;; (.EMACS "mew")
;; 
;; (autoload 'mew      "mew" "Start Mew." t)
;; (autoload 'mew-send "mew" "Compose a new message." t)
;; ;;(defalias 'mail 'mew-send)
;; 
;; (setq mew-mailbox-type 'mbox)
;; (setq mew-mbox-command "incm")
;; (setq mew-mbox-command-arg "-d /var/spool/mail/pjb")


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
;;;
(.EMACS "gnus")
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



;;;---------------------------------------------------------------------
;;; erc
;;;---------------------------------------------------------------------
(.EMACS "erc")
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


(defcustom erc-foolish-content '("nigger")
  "Regular expressions to identify foolish content.
    Usually what happens is that you add the bots to
    `erc-ignore-list' and the bot commands to this list."
  :group 'erc
  :type '(repeat regexp))

(defun erc-foolish-content (msg)
  "Check whether MSG is foolish."
  (erc-list-match erc-foolish-content msg))

(add-hook 'erc-insert-pre-hook
	      (lambda (s)
            (when (erc-foolish-content s)
              (setq erc-insert-this nil))))

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

(require 'pjb-speak)
(require 'pjb-erc-speak)

;;;----------------------------------------------------------------------------

(defparameter *pjb-erc-answers*
  '((geb
     . "the most important book of the XX century: \"Gödel, Escher and Bach: An Eternal Golden Braid\" http://www.amazon.com/G%C3%B6del-Escher-Bach-Eternal-Golden/dp/0465026567")
    (lisp-applications
     . "“Please don't assume Lisp is only useful for Animation and Graphics, AI, Bioinformatics, B2B and Ecommerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list.”")
    (haskell-road . "\"The Haskell Road to Logic, Math and Programming\" http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.137.9312&rep=rep1&type=pdf")
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
    (the-little-lisper  . "http://kysmykseka.net/koti/wizardry/Programming/Lisp/Scheme/The%2520Little%2520Schemer%25204th%2520Ed.pdf")
    (the-schemer-lisper . "http://kysmykseka.net/koti/wizardry/Programming/Lisp/Scheme/The%2520Little%2520Schemer%25204th%2520Ed.pdf")
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
    (gitlab-lisp 
     . "https://gitlab.com/com-informatimago/com-informatimago/")
    (gitlab-emacs
     . "https://gitlab.com/com-informatimago/emacs/")
    (rc       
     . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/rc")
    (bin      
     . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/bin")
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

;; TODO reimplement pjb-erc-answer using define-global-abbrev ?
(defun pjb-erc-answer (key)
  (interactive (list 
                (intern (completing-read 
                         "What? " (mapcar (lambda (x) (cons x nil)) (pjb-erc-get-answers))
                         (lambda (answer) (setq *pjb-erc-last-answer* (car answer)))
                         t))))
  (insert (format "%s" (cdr (assoc key *pjb-erc-answers*)))))

;; (add-hook 'erc-join-hook (lambda () (local-set-key (kbd "H-a") 'pjb-erc-answer)))
(global-set-key (kbd "H-a") 'pjb-erc-answer)
(global-set-key (kbd "A-a") 'pjb-erc-answer)

;;;----------------------------------------------------------------------------
;;; C modes
;;;

(.EMACS "c modes")

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

(setf pgp-command 'pgp-gpg-command)
(setf pgp-signer  "0xEF5E9966") ;; "pjb@informatimago.com"
(setf *pjb-sources-initials* "PJB")

(require 'message)
(defalias 'rot13-region 'message-caesar-region)

(defalias 'scratch      'pjb-scratch)
(defalias 'eurotunnel   'pjb-eurotunnel)
(defalias 'address      'pjb-address)
(defalias 'attach-file  'pjb-mail-attach-file)
(defalias 'ff           'full-frame)
(defalias 'make         'compile)
(defalias 'uptime       'emacs-uptime)

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


(defun doing (what)
  (interactive "sWhat are you doing? ")
  (find-file "~/doing.txt")
  (goto-char (point-max))
  (insert (shell-command-to-string "date")  what "\n#\n")
  (save-buffer)
  (bury-buffer))


(defun emacs-cli-mode-p ()
  (intersection
   '("-f" "-funcall" "--funcall" "-e" "-eval" "--eval" "-execute"
     "--execute" "-insert" "--insert") command-line-args
     :test (function string=)))

(unless (emacs-cli-mode-p)
  (milliways-schedule 'afaire))

;;;----------------------------------------------------------------------------

(defun informatimago ()
  "Browse http://www.informatimago.com"
  (interactive)
  (browse-url "http://www.informatimago.com/toc.html"))


;;;----------------------------------------------------------------------------
(.EMACS "indent buffer on find-file")

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

(defun compile-and-run-file (src mode)
  (interactive "fC or C++ source file to compile and run: 
p")
  (flet ((name (path)
           (when (string-match "^.*/\\([^./]*\\)\\.[^/.]*$" path)
             (match-string 1 path)))
         (type (path)
           (when (string-match "^.*/[^./]*\\.\\([^/.]*\\)$" path)
             (match-string 1 path))))
    (let ((compiler (or (cdr (assoc* (type src)
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

(defun compile-and-run-region (start end)
  (interactive "r")
  (let ((path (make-temp-file "car" nil ".c")))
    (write-region start end path)
    (compile-and-run-file path nil)))

(defun compile-and-run (mode)
  (interactive "p")
  (compile-and-run-file (buffer-file-name (current-buffer)) mode))

;;;----------------------------------------------------------------------------
(.EMACS "psql mode")

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
    (sha1 message nil nil)))

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

;; put objc auto-mode
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
(.EMACS "Google Maps")
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
(.EMACS "semantic mode")
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


(defun merge-customization-variable (a b)
  (assert (eql (car a) (car b)))
  (assert (eql 'quote (car (second a))))
  (assert (eql 'quote (car (second b))))
  (let ((var (car a))
        (a (second (second a)))
        (b (second (second b))))
    `'(,var ',(remove-duplicates (append a b) :test (function equal)))))

;; (pushnew '("/midishare/libraries/.*\\.[hc]$" . iso-8859-1) auto-coding-alist :test (function equal))

(defun viper-mode () (interactive) (message "I want more life, fucker!"))


;;;----------------------------------------------------------------------------
(.EMACS "emacs-common complete.")
;;;; THE END ;;;;
