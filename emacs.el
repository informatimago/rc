;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file.
;;;;
;;;; We only run GNU emacs.
;;;;

(message "Pascal J. Bourguignon's emacs startup file.")
(setq message-log-max 5000)
(when (< emacs-major-version 24) 
  (setq safe-local-variable-values (cons '(lexical-binding . t) safe-local-variable-values)))

;; Depending on the host computer, we may load one or another of the
;; actual emacs configuration files we have in ~/rc/.

(defun hostname ()
  (or (and (boundp  'system-name) system-name)
      (and (fboundp 'system-name) (system-name))
      (ignore-errors
        (shell-command-to-string
         "echo -n $( (hostname -f 2>/dev/null) || (hostname 2>/dev/null) )"))
      "localhost"))

(let ((configuration (if (boundp 'aquamacs-version)
                         '(aquamacs . "~/rc/emacs-aquamacs.el")
                         (or (let* ((hostname-configuration-map
                                     '(("mercure"                . "~/rc/emacs-employer-c.el")
                                       ("mercure.lan.employer-c.com"  . "~/rc/emacs-employer-c.el")
                                       ("uiserver.local"         . "~/rc/emacs-employer-c.el")
                                       ("imac-core-i5.local"     . "~/rc/emacs-employer-d.el")
                                       ("employer-d-pbo.local"          . "~/rc/emacs-employer-d.el")
                                       ("pbo-employer-d.local"          . "~/rc/emacs-employer-d.el")))
                                    (hostname  (hostname))
                                    (conf  hostname-configuration-map))
                               (while (and conf (not (string-equal (upcase hostname) (upcase (caar conf)))))
                                 (setq conf (cdr conf)))
                               (car conf))
                             '("informatimago.com" . "~/rc/emacs-pjb.el")))))
  (if configuration
      (let ((file (file-truename (cdr configuration))))
        (load file)
        (setq custom-file (or file custom-file)))
      (message "Found no configuration to load for %s" (hostname))))

;;;; THE END ;;;;

