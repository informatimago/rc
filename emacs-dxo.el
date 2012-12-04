;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file at DxO Consumers SAS.

(load "~/rc/emacs-common.el")

;;;----------------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------------

(.EMACS "custom faces")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "darkgreen"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-preprocessor-face ((t (:foreground "#550000"))))
 '(font-lock-string-face ((t (:foreground "#aa2211"))))
 '(font-lock-type-face ((t (:foreground "#6620b0")))))



(.EMACS "custom variables")
(custom-set-variables
 '(indent-tabs-mode                  nil)
 '(c-backslash-column                set-from-style)
 '(c-backslash-max-column            set-from-style)
 '(c-basic-offset                    set-from-style)
 '(c-block-comment-prefix            set-from-style)
 '(c-cleanup-list                    set-from-style)
 '(c-comment-only-line-offset        set-from-style)
 '(c-comment-prefix-regexp           set-from-style)
 '(c-doc-comment-style               set-from-style)
 '(c-echo-syntactic-information-p    t)
 '(c-hanging-braces-alist            set-from-style)
 '(c-hanging-colons-alist            set-from-style)
 '(c-hanging-semi&comma-criteria     set-from-style)
 '(c-indent-comment-alist            set-from-style)
 '(c-indent-comments-syntactically-p set-from-style)
 '(c-label-minimum-indentation       set-from-style)
 '(c-offsets-alist                   set-from-style)
 '(c-special-indent-hook             set-from-style)
 '(mail-host-address                 nil)
 '(starttls-use-gnutls               nil)
 '(user-mail-address                 "pbourguignon@dxo.com")
 '(warning-suppress-types (quote ((undo discard-info))))
 '(message-log-max 5000))


;;;----------------------------------------------------------------------------
;;; DxO specific stuff
;;;----------------------------------------------------------------------------

(deletef auto-mode-alist "\\.m$"  :test (function equal) :key (function car))
(deletef auto-mode-alist "\\.mm$" :test (function equal) :key (function car))
(deletef auto-mode-alist "\\.md$" :test (function equal) :key (function car))
(appendf auto-mode-alist '(("\\.m$"  . objc-mode)
                           ("\\.mm$" . objc-mode)
                           ("\\.md$" . text-mode)))



(require 'tls)
(setf tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --insecure -p %p %h"
                    "gnutls-cli --insecure -p %p %h --protocols ssl3"))

(require 'org-jira)
(setf jiralib-url "https://jira:8443/")

(when (require 'semantic nil t) 
  (semantic-mode 1))

(require 'pjb-c-style)
(require 'pjb-objc-edit)
(require 'dxo)


(ignore-errors (visit-tags-table "~/src/Cocoa.ETAGS"))
(ignore-errors (visit-tags-table "~/src/ETAGS"))




(defparameter *opticspro-branch* "OpticsProMac-filmstripRefactor")
(defparameter *opticspro-branch* "OpticsProMac-trunk")
(set-sources (file-truename (format "~/src/%s" *opticspro-branch*)))





;;----------------------------------------------------------------------------
;; Gherkin/Cucumber stuff.

(setf feature-default-language "en")
(setf feature-default-i18n-file "~/emacs/cucumber/i18n.yml")
(when (require 'feature-mode nil t)
 (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

;;;----------------------------------------------------------------------------
 (load "~/rc/emacs-epilog.el")
;;;; THE END ;;;;

