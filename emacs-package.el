;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
(.EMACS "~/rc/emacs-package.el %s" "Loading elpa.")

(when (and (zerop (length (shell-command-to-string "git config --global http.proxy")))
	   (require 'package nil t))
  ;; Anyn add to list for package-archives (to add marmalade or melpa) goes here
  (setq package-archives '(("gnu"           . "https://elpa.gnu.org/packages/")
                           ;; ("marmalade"     . "https://marmalade-repo.org/packages/")
                           ;; ("melpa-stable"  . "https://stable.melpa.org/packages/")
                           ;; ("melpa"         . "https://stable.melpa.org/packages/")
                           ;; ("org"           . "https://orgmode.org/elpa/")
			   ))

  (package-initialize)
  (package-refresh-contents)

  ;; (defun package-update-load-path ()
  ;;   "Update the load path for newly installed packages."
  ;;   (interactive)
  ;;   (let ((package-dir (expand-file-name package-user-dir)))
  ;;     (mapc (lambda (pkg)
  ;;             (let ((stem (symbol-name (car pkg)))
  ;;                   (version "")
  ;;                   (first t)
  ;;                   path)
  ;;               (mapc (lambda (num)
  ;;                       (if first
  ;;                         (setq first nil)
  ;;                         (setq version (format "%s." version)))
  ;;                       (setq version (format "%s%s" version num)))
  ;;                     (aref (cdr pkg) 0))
  ;;               (setq path (format "%s/%s-%s" package-dir stem version))
  ;;               (add-to-list 'load-path path)))
  ;;           package-alist)))

  (when (fboundp 'run-lac-functions)
    (run-lac-functions))


  (defvar *packages* '())
  (setq *packages* '(

                     paredit

                     emms json popup

                     company auto-complete inf-ruby enh-ruby-mode
                     smartparens highlight-indentation robe textmate
                     flycheck

                     ))

  (defun pjb-install-packages ()
    (interactive)
    (mapc (function package-install) *packages*))

  (pjb-install-packages))

;;;; The End ;;;;
