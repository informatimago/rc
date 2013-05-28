;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file.
;;;;
;;;; We only run GNU emacs.
;;;;

(message "Pascal J. Bourguignon's emacs startup file.")
(setq message-log-max 5000)

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
                         (let* ((hostname-configuration-map
                                 '(("mercure"            . "~/rc/emacs-employer-c.el")
                                   ("imac-core-i5.local" . "~/rc/emacs-employer-d.el")
                                   ("employer-d-pbo.local"      . "~/rc/emacs-employer-d.el")
                                   ( "pbo-employer-d.local"     . "~/rc/emacs-employer-d.el")
                                   (t                    . "~/rc/emacs-pjb.el"))))
                           (or (assoc* (hostname) hostname-configuration-map :test (function equalp))
                               (assoc  t          hostname-configuration-map))))))
  (if configuration
      (let ((file (file-truename (cdr configuration))))
        (load file)
        (setq custom-file (or file custom-file)))
      (message "Found no configuration to load for %s" (hostname))))

;;;; THE END ;;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((lexical-binding . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
