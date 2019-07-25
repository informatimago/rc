;;; emacs-common -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8; -*-
;;; Commentary:
;;; Code:

;;;;
;;;; Pascal J. Bourguignon's emacs startup file.
;;;;
;;;; We only run GNU emacs.
;;;;

;;; Code:

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

(when (= (user-uid) 0)
  ;; (load "/root/.emacs" pjb:*load-noerror* pjb:*load-silent*)
  ;; (error "~/.emacs: Cannot load ~/.emacs under root account.")
  (set-face-background 'fringe "red"))


;; tramp hops: /ssh:bird@bastion|ssh:you@remotehost:/path
;; tramp hops: /ssh:you@remotehost:/path


;;;----------------------------------------------------------------------------
;;; Start up.
;;;----------------------------------------------------------------------------
(setq-default lexical-binding t)
(setq byte-compile-warnings '(not obsolete))
(defvar *emacs-start-time*       (current-time) "For (emacs-uptime).")
(setq source-directory (format "/usr/local/src/emacs-%s/src" emacs-version))




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
  (when (file-exists-p "--version.lock")
    (message "Deleting version lock!")
    (delete-file  "--version.lock"))
  ;; (if (file-exists-p "--version.lock")
  ;;   (error "version lock"))
  (let ((text (apply (function format) (concat ".EMACS: " fctl) args)))
    (when *pjb-save-log-file-p*
      (with-current-buffer (get-buffer-create " .EMACS temporary buffer")
        (erase-buffer)
        (insert text "\n")
        (append-to-file (point-min) (point-max) (format "%s/messages.txt" *tempdir*))))
    (message text)))



(.EMACS "~/rc/emacs-common.el %s" "Pascal J. Bourguignon's emacs startup file.")
(load "~/rc/emacs-package.el")


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
(when (fboundp 'byte-compile-disable-warning)
  (byte-compile-disable-warning 'cl-functions))

(when (boundp 'byte-compile-warning-types)
  (setq byte-compile-warning-types (remove 'cl-functions byte-compile-warning-types)))

;; byte-compile-warning-types
;; (redefine callargs free-vars unresolved obsolete noruntime cl-functions interactive-only make-local mapcar constants suspicious lexical)
;; byte-compile-warnings
;; (not cl-functions)

(require 'tramp-sh nil t)
(defvar tramp-ssh-controlmaster-options "")
;; (setf tramp-ssh-controlmaster-options (concat "-o 'SendEnv TRAMP=yes' " tramp-ssh-controlmaster-options))
;; (setf tramp-ssh-controlmaster-options "")
;; (setf tramp-ssh-controlmaster-options (concat "-o 'SendEnv TRAMP=yes' -o 'SendEnv TERM=dumb' " tramp-ssh-controlmaster-options))

(setf shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")
(setf shell-prompt-pattern "^\\[.*\\][#$%>] *")
(setf tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
(setf tramp-shell-prompt-pattern shell-prompt-pattern)


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

(unless (fboundp 'digit-char-p)
  (defun digit-char-p (ch)
    "Return the value of the digit if ch is a digit character, or nil."
    (case ch
      ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9) (- ch ?0))
      (otherwise nil))))

(defun octal (n)
  "N is a decimal numbers whose digits are taken as octal digits
and converted as such."
  (loop
     for d across (format "%d" n)
     for r = (digit-char-p d) then (+ (* 8 r) (digit-char-p d))
     finally (return r)))

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


;;  (set-language-environment       'German)
;;  (setq default-file-name-coding-system   'utf-8)
;;  (setq file-name-coding-system       'utf-8)
;;  (setq default-buffer-file-coding-system 'iso-latin-9-unix))
;;  (set-default-coding-systems     'mac-roman-unix)
;;  ;(setq mac-keyboard-text-encoding    kTextEncodingISOLatin1)
;;  (set-keyboard-coding-system     'sjis-mac)
;;  (set-clipboard-coding-system        'sjis-mac)
;;  (prefer-coding-system           'mac-roman-unix)
;;  (modify-coding-system-alist  'file "\\.tex\\'" 'iso-latin-9-unix)
;;  (modify-coding-system-alist  'process
;; "\\*[Ss][Hh][Ee][Ll][Ll].*\\'"  'utf-8-unix)
;;  ;(set-buffer-process-coding-system  'utf-8 'utf8)



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
(standard-display-ascii #o220 [?\ ])
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
			    '(("/opt/lisp/emacs")
			      ("/opt/share/emacs/site-lisp")
			      ("/opt/local/share/emacs/site-lisp")
			      ("/usr/local/share/emacs/site-lisp")
			      ("/usr/share/emacs/site-lisp"))))
        (if (listp directories)
            (find-if (function add-if-good) directories)
            (add-if-good directories)))

      (setf load-path (append new-paths
                              (set-difference load-path base-load-path :test (function equal))
                              base-load-path)))))



(.EMACS "Loading my personal files -- My own stuff.")
(pjb-setup-load-path)
(unless (load "pjb-loader.el" ) (.EMACS "ERROR: Could not find and load 'My own stuff'!"))
(load "~/rc/emacs-directories")
(load "pjb-loader.el" )

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
;;; Other packages.
;;;----------------------------------------------------------------------------


(require 'epa-file)
(epa-file-enable)

(require 'highlight-flet nil t)
(require 'rst nil t)
(require 'rst-mode nil t)
(when window-system
  (mouse-avoidance-mode 'cat-and-mouse))



;;;----------------------------------------------------------------------------
;;; CEDET / EIEIO
;;;----------------------------------------------------------------------------
(when (and nil
           (require 'eieio nil t)
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
(defalias 'pjb-movement-keybindings 'reset-movement-keypad)

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
  ;; (keyboard-translate ?\§ ?\`)
  ;; (keyboard-translate ?\± ?\~)
  )
(defalias 'translate-macbook-keyboard  'translate-powerbook-keyboard)

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

  (define-key input-decode-map "\M-[A" [up])
  (define-key input-decode-map "\M-[B" [down])
  (define-key input-decode-map "\M-[C" [right])
  (define-key input-decode-map "\M-[D" [left])

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
         (number (first (cl:parse-integer name :start (- (length name) 2))))
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
     (setf *pjb-current-font-index* (or (position increment *pjb-font-list*
                                                  :test (function string=))))))
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
(when (and (boundp 'elscreen-display-tab) elscreen-display-tab)
  (elscreen-toggle-display-tab))


;;------------------------------
(.EMACS "Miscellaneous patches")

(when (< emacs-major-version 22)
  (unless (fboundp 'called-interactively-p)
    (defun called-interactively-p () (interactive-p))))


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
(require 'shell)
(add-hook 'ielm-mode-hook (lambda () (setf standard-output (current-buffer))))

;; https://github.com/szermatt/emacs-bash-completion
(unless (file-directory-p "~/emacs/emacs-bash-completion")
  (shell-command "mkdir -p ~/emacs ; cd ~/emacs/ ; git clone https://github.com/szermatt/emacs-bash-completion.git"))
(pushnew "~/emacs/emacs-bash-completion" load-path :test 'string=)
(when (require 'bash-completion nil t)
  (bash-completion-setup)
  (set-default 'shell-dirstack-query "pwd")
  (milliways-schedule (lambda ()
			(bash-completion-setup)
			(set-default 'shell-dirstack-query "pwd"))))

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
  (bash-completion-setup)
  (set-default 'shell-dirstack-query "pwd")
  ;; (cond
  ;;   ((let ((shell (getenv "ESHELL")))
  ;;       (or (null shell)
  ;;        (not (or (search "clash" shell)
  ;;              (search "scsh" shell)))))
  ;; Moved to ~/.emacs-bash:
  ;;    (process-send-string (get-buffer-process (current-buffer))
  ;;                      "alias less=cat ; alias more=cat ; ")))
  )
(add-hook 'shell-mode-hook 'pjb-shell-mode-meat)

(bash-completion-setup)
(set-default 'shell-dirstack-query "pwd")
;; (setf dirtrack-list '("^\\(~?/.*\\)\n\\[[_a-z0-9A-Z]+@[-_.a-z0-9A-Z]+ [^]]*\\]$ " 1))

(add-hook 'shell-mode-hook 'shell-dirtrack-mode)




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
(setf org-agenda-files '(;; ;; (file-expand-wildcards "~/firms/*/notes.txt")
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
(setf org-planning-line-re "^org-planning-line-re"
      org-clock-line-re    "^org-clock-line-re")
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


(appendf auto-mode-alist '(("\\.jmf$"    . java-mode)
                           ("\\.j$"      . java-mode)))

(appendf auto-mode-alist '(("\\.pl1$"    . pl1-mode)))



;;;----------------------------------------------------------------------------

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
      (or (find-if (lambda (frame) (equalp (pjb-frame-name frame) *browse-frame-name*))
                   (frame-list))
          (make-frame (list (cons 'name *browse-frame-name*))))))
    (w3m-goto-url url)))


(when (and (require 'w3m-load nil t)
	   (or (<= 23 emacs-major-version) (require 'mime-parse nil t))
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
  (cl-flet ((progn-body (body)
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
     (zerop (cl:parse-integer
             (shell-command-to-string
              (format "wget -O /dev/null %S >/dev/null 2>&1 ; echo -n $?"
                      url)))))))


;; (require 'clhs)
;; (require 'hyperspec)


(defvar *lw-clhs*)
(setf   *lw-clhs*          "www.lispworks.com/documentation/HyperSpec/")
(defvar *hyperspec-path*)
(setf   *hyperspec-path*   (first-existing-file
			    (list
			     (ignore-errors (get-directory :hyperspec))
			     (concat "/usr/local/html/local/lisp/" *lw-clhs*)
			     "/opt/local/share/doc/lisp/HyperSpec-7-0/"))
        common-lisp-hyperspec-root
        (dolist
            (url (list
                  (concat "file://" *hyperspec-path*)
                  "file:///usr/share/doc/hyperspec/HyperSpec/"
                  ;; (concat "http://thalassa.lan.informatimago.com/lisp/" *lw-clhs*)
                  (concat "http://" *lw-clhs*)))
          (when (probe-url url)
            (return url))))

(defvar common-lisp-hyperspec-browser (function ignore))
(defvar common-lisp-hyperspec-frame   (selected-frame))
(load "extra/hyperspec" *pjb-load-noerror* *pjb-load-silent*)

;; (setf common-lisp-hyperspec-browser 'w3m-browse-url 
;; (push '("."  .  w3m-browse-url) browse-url-browser-function)

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
    (browse-url (concat common-lisp-hyperspec-root "Body/" random-page))))




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
(when (require 'column-marker nil t)
  (.EMACS "columnmarker")
  (column-marker-1  80)
  (column-marker-2 120)
  (column-marker-3 132))

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

;; (add-lac 'lac-emms)
;; (lac-emms)


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
  (when (fboundp 'auto-complete-mode) (auto-complete-mode -1))
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

;; (require 'pjb-speak)
;; (require 'pjb-erc-speak)

;;;----------------------------------------------------------------------------

(defparameter *pjb-erc-answers*
  '((extensions
     . "https://www.talisman.org/~erlkonig/documents/commandname-extensions-considered-harmful/")
    (geb
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
     . "An Introduction to Programming in Emacs Lisp  https://www.gnu.org/software/emacs/manual/eintr.html  or  M-: (info \"(eintr)Top\") RET (for non-programmers)")
    (emacs-lisp
     . "Emacs Lisp Manual  http://www.gnu.org/software/emacs/manual/elisp.html  or  M-: (info \"(elisp)Top\") RET")
    (emacs-manual
     . "Emacs Manual http://www.gnu.org/software/emacs/manual/   or  M-: (info \"(emacs)Top\") RET")
    (the-art-of-unix-programming
     . "The Art of Unix Programming http://www.faqs.org/docs/artu/")
    (hacker-howto
     . "http://www.catb.org/~esr/faqs/hacker-howto.html")
    (the-craft-of-text-editing
     . "The Craft of Text Editing   http://www.finseth.com/craft/")
    (craft . the-craft-of-text-editing)
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
    (lisp-packages .
     "The Complete Idiot's Guide to Common Lisp Packages: http://www.flownet.com/ron/packages.pdf")
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
    (ring . "One Lisp to rule them all, One Lisp to find them, One Lisp to bring them all, And in the darkness bind them.")
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

")
    (erc-unix . "To prevent erc to send out unix commands: http://paste.lisp.org/+3F18")))



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
  (let ((walked '())
        (answer key))
    (loop while (and (symbolp answer)
                     (not (member answer walked)))
          do (push answer walked)
             (setf answer (cdr (assoc answer *pjb-erc-answers*))))
    (insert (format "%s" answer))))


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


(defun infer-indentation-style ()
  (let ((spc-count (how-many "^ "  (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (setf indent-tab-mode (cond ((< spc-count tab-count) t)
                                ((> spc-count tab-count) nil)
                                (t                       (when (boundp 'indent-tab-mode)
                                                           indent-tab-mode))))))


(require 'freerdp-c-style)

(defvar auto-c-style-alist
  '(("/.*FreeRDP.*/.*\\.[hc]" . "freerdp")
    ("." . "pjb")))

(defun c-mode-meat ()
  (interactive)
  (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (infer-indentation-style)
  (let ((path (buffer-file-name)))
    (when path
      (let ((c-style (cdr (find-if (lambda (entry) (string-match (car entry) path)) auto-c-style-alist))))
        (when c-style
          (message "Setting C style %s" c-style)
          (c-set-style c-style)
          (when (string= c-style "freerdp")
            (freerdp-style-set-local-bindings))))))
  (define-key c-mode-map (kbd "C-c p") 'pjb-ide-insert-tag-comment)
  (local-set-key  (kbd "C-c p") 'pjb-ide-insert-tag-comment)
  ;; (define-key c-mode-map "{" 'self-insert-command)
  (local-set-key (kbd "TAB") (quote c-indent-or-tab)))


;; (setf c-mode-hook nil c++-mode-hook nil objc-mode-hook nil )

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


(dolist (hook '(emacs-lisp-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

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


;; (add-hook 'write-file-hooks
;;           (lambda ()
;;             (unless indent-tabs-mode
;;               (untabify (point-min) (point-max)))))

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
       (cl-labels ((flatten
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

(defalias 'widden       'widen)
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
    (dolist (file '("rc/notes.gpg" "NOTES.txt" "notes.txt" "NOTES.*[a-z]" "notes.*[a-z]"
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

(defun pjb-grep-meat--remove-grep-option ()
  (interactive)
  ;; grep: warning: GREP_OPTIONS is deprecated; please use an alias or script
  (setenv "GREP_OPTIONS" nil))

(add-hook 'grep-setup-hook 'pjb-grep-meat--remove-grep-option)


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

(setf calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))
(setf mode-line-format '("%e" mode-line-front-space
                         mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                         mode-line-frame-identification mode-line-buffer-identification
                         "   " mode-line-position (vc-mode vc-mode) "  "
                         mode-line-modes mode-line-misc-info mode-line-end-spaces))

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
           (when (re-search-forward "https?://www.google.[a-z]+/url\\?\\(url=\\|.*&url=\\)")
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
                         (mapcar (function color-clamp) rgb)))
                (let* ((fh (apply (function color-rgb-to-hsl) f))
                       (bh (apply (function color-rgb-to-hsl) b)))
                  (flet ((spread (l d)
                           (list (color-lighten-hsl (first l) (second l) (third l) 80)
                                 (color-darken-hsl  (first d) (second d) (third d) -50))))
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
(defun remove-meat-from-all-hooks (meat)
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
;;;----------------------------------------------------------------------------
;; (.EMACS "semantic mode")
;; (require 'semantic)
;; (semantic-mode 1)
;; (push '(objc-mode . semantic-default-c-setup) semantic-new-buffer-setup-functions)
(remove-meat-from-all-hooks 'semantic-default-elisp-setup)
(remove-meat-from-all-hooks 'semantic-default-c-setup)
(remove-meat-from-all-hooks 'semantic-make)
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


(defun pjb-disable-erc-fill-mode-meat ()
  (when erc-fill-mode
    (erc-fill-mode -1)))

(defvar erc-fill-mode-hook '())
(push 'pjb-disable-erc-fill-mode-meat erc-fill-mode-hook)


(.EMACS "emacs-common complete.")
(provide 'pjb-emacs-common)

;; Local Variables:
;; coding: utf-8
;; End Variables:
;;;; THE END ;;;;
