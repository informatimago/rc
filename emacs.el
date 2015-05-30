;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file.
;;;;
;;;; We only run GNU emacs.
;;;;

(message "~/rc/emacs.el %s" "Pascal J. Bourguignon's emacs startup file.")
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
                                     '(("^mercure"                . "~/rc/emacs-ubudu.el")
                                       ("^uiserver"               . "~/rc/emacs-ubudu.el")
                                       ("^ubudair"                . "~/rc/emacs-ubudu.el")
                                       ("^ubudu-vm-mac-[0-9]"     . "~/rc/emacs-ubudu.el")
                                       ("^imac-core-i5.local"     . "~/rc/emacs-dxo.el")
                                       ("^dxo-pbo.local"          . "~/rc/emacs-dxo.el")
                                       ("^pbo-dxo.local"          . "~/rc/emacs-dxo.el")))
                                    (hostname  (hostname))
                                    (conf  hostname-configuration-map))
                               (while (and conf (not (let ((case-fold-search t))
                                                       (string-match (caar conf) hostname))))
                                 (setq conf (cdr conf)))
                               (car conf))
                             '("informatimago.com" . "~/rc/emacs-pjb.el")))))
  (if configuration
      (let ((file (file-truename (cdr configuration))))
        (load file)
        ;; setting custom-file needs to be done after we've customized our stuff
        ;; otherwise it may be overridden with an empty customization.
        (setq custom-file (or file custom-file)))
      (message "Found no configuration to load for %s" (hostname))))

;;;; THE END ;;;;
