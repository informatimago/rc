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
 '(font-lock-type-face ((t (:foreground "#6620b0"))))
 '(smerge-refined-change ((t (:background "#997722"))))
 '(rst-level-1-face ((t (:background "grey20" :height 1.9))) t)
 '(rst-level-2-face ((t (:background "grey20" :height 1.7))) t)
 '(rst-level-3-face ((t (:background "grey20" :height 1.4))) t)
 '(rst-level-4-face ((t (:background "grey20" :height 1.2))) t)
 '(rst-level-5-face ((t (:background "grey20" :height 1.1 :weight bold))) t)
 '(rst-level-6-face ((t (:background "grey20" :height 1.0 :weight bold))) t)
 )



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
 '(erc-auto-query (quote window))
 '(erc-autojoin-channels-alist (quote (("freenode.net"  "#lisp") ("irc.oftc.net" "#uml"))))
 '(erc-away-timestamp-format "<%H:%M:%S>")
 '(erc-echo-notices-in-current-buffer t)
 '(erc-echo-timestamps nil)
 '(erc-email-userid t)
 '(erc-encoding-coding-alist (quote (("#emacsfr" . iso-8859-15) ("#scheme-es" . iso-8859-15))))
 '(erc-fill-column 90)
 '(erc-fill-function (quote erc-fill-variable))
 '(erc-fill-prefix "\"\"")
 '(erc-fill-static-center 0)
 '(erc-fill-variable-maximum-indentation 0)
 '(erc-hide-list (quote nil))
 '(erc-ignore-list (quote ("ad37e918" "173.55.233.24")))
 '(erc-ignore-per-channel-alist (quote (("#scheme" . "rudybot") ("#emacs" . "rudybot"))))
 '(erc-ignore-per-channel-reply-alist (quote (("#scheme" . "rudybot") ("#emacs" . "rudybot"))))
 '(erc-ignore-reply-list (quote nil))
 '(erc-insert-away-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-interpret-mirc-color t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-max-buffer-size 300000)
 '(erc-minibuffer-ignored t)
 '(erc-minibuffer-notice t)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols log match netsplit readonly replace ring scrolltobottom services stamp track truncate)))
 '(erc-nick (quote ("pjb-d")))
 '(erc-notice-prefix "   *** ")
 '(erc-pals (quote ()))
 '(erc-port 6667)
 '(erc-prompt (lambda nil (buffer-name (current-buffer))))
 '(erc-prompt-for-password t)
 '(erc-quit-reason-various-alist nil)
 '(erc-server "irc.freenode.org")
 '(erc-server-coding-system (quote (utf-8 . undecided)))
 '(erc-server-reconnect-attempts 100)
 '(erc-server-reconnect-timeout 60)
 '(erc-timestamp-format nil)
 '(erc-timestamp-intangible nil)
 '(erc-user-full-name "Pascal J. Bourguignon")
 '(eval-expression-print-length nil)
 '(indent-tabs-mode nil)
 '(mail-host-address nil)
 '(message-log-max 5000)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "voyager.informatimago.com")
 '(smtpmail-smtp-service 25)
 '(starttls-use-gnutls nil)
 '(user-mail-address "pbourguignon@dxo.com")
 '(visible-bell nil)
 '(warning-suppress-types (quote ((undo discard-info)))))


;;;----------------------------------------------------------------------------
;;; DxO specific stuff
;;;----------------------------------------------------------------------------
(push (lambda ()
        (when (string-match "^/usr/local/Cellar/" (buffer-file-name (current-buffer)))
          (view-mode 1)))
      find-file-hook)

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
   '(("^\\(error .*\\)"                (1 gherkin-error))
     ("^\\(build .*error on line.*\\)" (1 gherkin-error))
     ("^\\(.*\\.m:[0-9]+: error:.*\\)" (1 gherkin-error))
     ("^\\(simple .*\\)"               (1 gherkin-simple))
     ("^\\(command .*\\)"              (1 gherkin-command))
     ("^\\(build .*\\)"                (1 gherkin-build))))
  (font-lock-fontify-buffer))

(put 'define-derived-mode 'lisp-indent-function 3)


(define-derived-mode gherkin-log-mode view-mode "Gherkin"
  "Mode for Gherkin logs"
  (gherkin-log-meat))


(deletef auto-mode-alist "/DOP-[^/]*\\.log$" :test (function equal) :key (function car))
(deletef auto-mode-alist "\\.log$"           :test (function equal) :key (function car))
(appendf auto-mode-alist '(("/DOP-[^/]*\\.log$" . gherkin-log-mode)
                           ("\\.log$"          . view-mode)))




(define-derived-mode valgrind-mode compilation-mode "Valgrind"
 "Mode to read valgrind log files."
 (setf font-lock-keywords nil)
 (font-lock-add-keywords
  nil
  '(("^{[^}]*}"
     (0 font-lock-preprocessor-face))
    ("^\\(==[0-9]*==\\) \\([^ \n].*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-warning-face))
    ("^\\(==[0-9]*==\\)   \\([^ \n].*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-comment-face))
    ("^\\(==[0-9]*==\\)    [a-z]+ \\(0x[0-9A-F]*\\): \\([^( \n]*\\)(\\(.*\\)) (\\([^:]+:[0-9]+\\))$"
     (1 font-lock-variable-name-face)
     (2 font-lock-constant-face)
     (3 font-lock-function-name-face)
     (4 font-lock-type-face)
     (5 font-lock-comment-face)))))

(add-to-list 'auto-mode-alist '("\\.\\(val\\|hel\\)grind$" . valgrind-mode))



(defun dxo-bell ()
  (message "bell"))

(setf ring-bell-function 'dxo-bell)


(dolist (key (list (kbd "<C-wheel-down>")
                   (kbd "<S-wheel-down>")
                   (kbd "<wheel-down>")
                   (kbd "<C-wheel-up>")
                   (kbd "<S-wheel-up>")
                   (kbd "<wheel-up>")))
  (global-unset-key key))

;; (setf eval-expression-print-length nil)
;;----------------------------------------------------------------------------

(require 'confluence)
(setq confluence-url "https://confluence:8453/rpc/xmlrpc")

;;----------------------------------------------------------------------------

(require 'erc-notify)

(desktop-read)

;;;----------------------------------------------------------------------------
 (load "~/rc/emacs-epilog.el")
;;;; THE END ;;;;

