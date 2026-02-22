;;; emacs gg-- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(message "~/rc/emacs.el %s" "Pascal J. Bourguignon's emacs startup file.")

;; (setf debug-on-quit t
;;       debug-on-error t)
(setq force-load-messages t
      message-log-max 20000)

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

(setf default-directory
      (file-name-directory (cond (user-init-file  user-init-file)
                                 ((getenv "HOME") (concat (getenv "HOME") "/"))
                                 (t                (car (file-expand-wildcards "~/.emacs"))))))

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
            '(aquamacs . "~/rc/emacs-aquamacs.el")
            (or (let* ((hostname-configuration-map
                         '(("^fr\\(dark\\|prld\\|prwn\\)"  . "~/rc/emacs-harman.el")
                           ("^qorvo"                       . "~/rc/emacs-qorvo.el")
                           ("^\\(.*span\\|[WwLl][0-9]\\{7\\}$\\|vm-u[0-9]*$\\)"
                            . "~/rc/emacs-span.el")
                           ("macbook[-.]trustonic.local"   . "~/rc/emacs-trustonic.el")
                           ("^vmlinux"                     . "~/rc/emacs-trustonic.el")
                           ("^mercure"                     . "~/rc/emacs-ubudu.el")
                           ("^uiserver"                    . "~/rc/emacs-ubudu.el")
                           ("^ubudair"                     . "~/rc/emacs-ubudu.el")
                           ("^ubudu-vm-mac-[0-9]"          . "~/rc/emacs-ubudu.el")))
                       (hostname (hostname))
                       (conf  hostname-configuration-map))
                  (while (and conf (not (let ((case-fold-search t))
                                          (string-match (caar conf) hostname))))
                         (setq conf (cdr conf)))
                  (car conf))
                '("informatimago.com" . "~/rc/emacs-pjb.el")))))
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

;; Local Variables:
;; coding: utf-8
;; End Variables:
