;;;; -*- Mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file for ruby development.

(.EMACS "~/rc/emacs-ruby.el %s" "Pascal J. Bourguignon's emacs ruby stuff.")

(require 'enh-ruby-mode nil t)
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))


(require 'smartparens-config)
(require 'smartparens-ruby)
;; (smartparens-global-mode)
;; (show-smartparens-global-mode t)
;; (sp-with-modes '(rhtml-mode)
;;                (sp-local-pair "<" ">")
;;                (sp-local-pair ""))


 (require 'highlight-indentation)
 (add-hook 'enh-ruby-mode-hook
           (lambda () (highlight-indentation-current-column-mode)))

 (add-hook 'coffee-mode-hook
           (lambda () (highlight-indentation-current-column-mode)))


(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

(require 'dash-at-point nil t)
(require 'textmate)

;;;; THE END ;;;;
