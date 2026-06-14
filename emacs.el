;;; emacs.el -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(message "~/rc/emacs.el %s" "Pascal J. Bourguignon's emacs startup file.")

;; (setf debug-on-quit t
;;       debug-on-error t)
(setq force-load-messages t
      message-log-max 20000)

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


;; (require 'package)
;; 
;; (advice-add 'package-desc-from-define
;;             :around
;;             (lambda(old &rest args)
;;               (message "(package-desc-from-define %S" args)
;;               (let ((result (apply old args)))
;;                 (message "--> %S)" result)
;;                 result)))
;;
;; (package-initialize)


(when (< emacs-major-version 24)
  (setq safe-local-variable-values (cons '(lexical-binding . t) safe-local-variable-values)))

(defvar *root-pathname*         (expand-file-name "/"))
(defvar *user-homedir-pathname* (expand-file-name "~/"))
(defvar *rc-pathname*           (expand-file-name "~/rc/"))

(defun pjb-collapse-slashes (path)
  "Replace every run of two or more slashes in PATH by a single slash."
  (replace-regexp-in-string "/\\{2,\\}" "/" path))

(defun pjb-join-pathname (directory path)
  "Join DIRECTORY and PATH robustly, then `expand-file-name' the result.
Either argument may independently have — or lack — a leading or trailing
slash (informally, a `…-directory-pathname' ends in / like in Common Lisp,
a `…-directory' does not, like in Unix and Emacs; but this is not
enforced).  Exactly one slash is kept at the junction and everywhere else,
so no spurious // appears, and ~ and .. in the result are resolved."
  (expand-file-name (pjb-collapse-slashes (concat directory "/" path))))

(defun root (path) (pjb-join-pathname *root-pathname*         path))
(defun home (path) (pjb-join-pathname *user-homedir-pathname* path))
(defun rc   (path) (pjb-join-pathname *rc-pathname*           path))
(defun pjb-emacs-source (path)
  (pjb-join-pathname (concat *rc-pathname* "../src/public/emacs/") path))

(cl-case system-type
  ((darwin linux)
   (setq *user-homedir-pathname* (file-name-directory
                                  (cond (user-init-file  user-init-file)
                                        ((getenv "HOME") (concat (getenv "HOME") "/"))
                                        (t                (car (file-expand-wildcards "~/.emacs")))))
         *rc-pathname*           (expand-file-name (concat *user-homedir-pathname* "rc/"))
         default-directory *user-homedir-pathname*))
  ((cygwin) ; msys2 etc
   ;; We have different HOME directories
   ;; but we store a single RC directory and a single PJB-EMACS-SOURCES directory.
   (cl-case (and (boundp 'FROM) FROM)
     ((WOME)
      (setq *root-pathname* "C:/msys64/"
            *user-homedir-pathname* WOME
            *rc-pathname* RC))
     ((ROME)
      (setq *root-pathname* "C:/msys64/"
            *user-homedir-pathname* ROME
            *rc-pathname* RC))
     ((MOME)
      (setq *root-pathname* "/"
            *user-homedir-pathname* MOME
            *rc-pathname* RC))
     (otherwise
      (message "!!!! unexpected FROM value %S" (and (boundp 'FROM) FROM)))))
  (otherwise
   (message "!!!! system-type = %S not processed yet" system-type)))


;; Depending on the host computer, we may load one or another of the
;; actual emacs configuration files we have in ~/rc/.

(defun hostname ()
  "Return the hostname of the system, or nil if it cannot be retrieved."
  (or (and (boundp  'system-name) system-name)
      (and (fboundp 'system-name) (system-name))
      (ignore-errors
        (shell-command-to-string
         "echo -n $( (hostname -f 2>/dev/null) || (hostname 2>/dev/null) )"))
      "localhost"))

(let ((configuration
       (if (boundp 'aquamacs-version)
           (cons 'aquamacs  (rc "emacs-aquamacs.el"))
         (or (let* ((hostname-configuration-map
                     (list (cons "^PF5S26BT"  (rc "emacs-sncf-reseau.el"))))
                    (hostname (hostname))
                    (conf  hostname-configuration-map))
               (while (and conf (not (let ((case-fold-search t))
                                       (string-match (caar conf) hostname))))
                 (setq conf (cdr conf)))
               (car conf))
             (cons "informatimago.com" (rc "emacs-pjb.el"))))))
  (if configuration
      (let ((file (file-truename (cdr configuration))))
	    (if (<= 27 emacs-major-version)
	        (with-suppressed-warnings ((obsolete assert block case
						                         check-type decf
						                         defstruct
						                         destructuring-bind
						                         do do* do-symbols
						                         ecase etypecase flet
						                         incf labels loop
						                         macrolet
						                         multiple-value-bind
						                         psetf pushnew return
						                         return-from the
						                         typecase)
				                       ;; (callargs zot)
				                       )
			  (load file))
	      (load file))
        ;; setting custom-file needs to be done after we've customized our stuff
        ;; otherwise it may be overridden with an empty customization.
        (setq custom-file (or file custom-file)))
    (message "Found no configuration to load for %s" (hostname))))

(when (file-exists-p (rc "emacs-patches.el"))
  (load (rc "emacs-patches.el")))
(load (rc "emacs-epilog.el"))

;; Local Variables:
;; coding: utf-8
;; End Variables:
