;;; emacs-package -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

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
                     smartparens highlight-indentation textmate
                     flycheck

		     ; robe not on emacs-24.4 ?

                     ))

  (defun pjb-install-packages ()
    (interactive)
    (mapc (lambda (package)
	    (ignore-errors (package-install package)))
	  *packages*))

  (pjb-install-packages))

;; Local Variables:
;; coding: utf-8
;; End Variables:
