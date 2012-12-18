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
 '(c-backslash-column (quote set-from-style))
 '(c-backslash-max-column (quote set-from-style))
 '(c-basic-offset (quote set-from-style))
 '(c-block-comment-prefix (quote set-from-style))
 '(c-cleanup-list (quote set-from-style))
 '(c-comment-only-line-offset (quote set-from-style))
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-doc-comment-style (quote set-from-style))
 '(c-echo-syntactic-information-p t)
 '(c-hanging-braces-alist (quote set-from-style))
 '(c-hanging-colons-alist (quote set-from-style))
 '(c-hanging-semi&comma-criteria (quote set-from-style))
 '(c-indent-comment-alist (quote set-from-style))
 '(c-indent-comments-syntactically-p (quote set-from-style))
 '(c-label-minimum-indentation (quote set-from-style))
 '(c-offsets-alist (quote nil))
 '(c-special-indent-hook (quote nil))
 '(eval-expression-print-length nil)
 '(indent-tabs-mode nil)
 '(mail-host-address nil)
 '(message-log-max 5000)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "voyager.informatimago.com")
 '(smtpmail-smtp-service 25)
 '(starttls-use-gnutls nil)
 '(user-mail-address "pbourguignon@dxo.com")
 '(warning-suppress-types (quote ((undo discard-info)))))


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

(let ((tags-add-tables t))
  (setf tags-table-list '()) 
  (ignore-errors (visit-tags-table "~/src/Cocoa.etags"))
  (ignore-errors (visit-tags-table "~/src/Ruby.etags"))
  (ignore-errors (visit-tags-table "~/src/OpticsPro.etags")))



(defparameter *opticspro-branch* "OpticsProMac-filmstripRefactor")
(defparameter *opticspro-branch* "OpticsProMac-trunk")
(set-sources (file-truename (format "~/src/%s" *opticspro-branch*)))


;;----------------------------------------------------------------------------
;; Gherkin/Cucumber stuff.

(setf feature-default-language "en")
(setf feature-default-i18n-file "~/emacs/cucumber/i18n.yml")
(when (require 'feature-mode nil t)
 (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

;;----------------------------------------------------------------------------


(defface gherkin-error
    '((default (:foreground "red")))
  "Error in Gherkin logs.")
(defface gherkin-simple
    '((default (:foreground "yellow")))
  "Simple lines in Gherkin logs.")
(defface gherkin-command
    '((default (:foreground "blue")))
  "Command lines in Gherkin logs.")
(defface gherkin-build
    '((default (:foreground "grey")))
  "Build lines in Gherkin logs.")


(defun gherkin-log-meat ()
  (interactive)
  (toggle-truncate-lines 1)
  (font-lock-mode 1)
  (setf font-lock-maximum-size 10000000)
  (setf font-lock-keywords '())
  (font-lock-add-keywords
   nil
   '(("\\(^error .*\\)"                1 gherkin-error)
     ("\\(^build .*error on line.*\\)" 1 gherkin-error)
     ("\\(^.*\\.m:[0-9]+: error:.*\\)" 1 gherkin-error)
     ("\\(^simple .*\\)"               1 gherkin-simple)
     ("\\(^command .*\\)"              1 gherkin-command)
     ("\\(^build .*\\)"                1 gherkin-build)))
  (font-lock-fontify-buffer))


(put 'define-derived-mode 'lisp-indent-function 3)


(define-derived-mode gherkin-log-mode view-mode "Gherkin"
  "Mode for Gherkin logs"
  (gherkin-log-meat))


(deletef auto-mode-alist "/DOP-[^/]*\\.log$" :test (function equal) :key (function car))
(deletef auto-mode-alist "\\.log$"           :test (function equal) :key (function car))
(appendf auto-mode-alist '(("/DOP-[^/]*\\.log$" . gherkin-log-mode)
                           ("\\.log$"          . view-mode)))



;; (setf eval-expression-print-length nil)
;;----------------------------------------------------------------------------


(desktop-read)

;;;----------------------------------------------------------------------------
 (load "~/rc/emacs-epilog.el")
;;;; THE END ;;;;

