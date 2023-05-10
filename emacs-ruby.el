;;; emacs-ruby -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "~/rc/emacs-ruby.el %s" "Pascal J. Bourguignon's emacs ruby stuff.")

(require 'enh-ruby-mode nil t)
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))


(require 'smartparens-config nil t)
(require 'smartparens-ruby nil t)
;; (smartparens-global-mode)
;; (show-smartparens-global-mode t)
;; (sp-with-modes '(rhtml-mode)
;;                (sp-local-pair "<" ">")
;;                (sp-local-pair ""))


(when (require 'highlight-indentation nil t)
  (add-hook 'enh-ruby-mode-hook
            (lambda () (highlight-indentation-current-column-mode)))

  (add-hook 'coffee-mode-hook
            (lambda () (highlight-indentation-current-column-mode))))


(when (require 'robe nil t)
 (add-hook 'ruby-mode-hook 'robe-mode))

;; (require 'dash-at-point)
(require 'textmate nil t)


(setf inf-ruby-first-prompt-pattern "irb --> ")
(setf inf-ruby-prompt-pattern (format inf-ruby-prompt-format "[?>]" "[\]>*\"'/`]"))


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:

