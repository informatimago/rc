;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
(.EMACS "~/rc/emacs-package.el %s" "Loading elpa.")
(when (require 'package nil t)
  ;; Anyn add to list for package-archives (to add marmalade or melpa) goes here
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "http://stable.melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")))

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
    (run-lac-functions)))


(defun pjb-install-packages ()
  (interactive)
  ;;
  (package-install 'company)
  (package-install 'paredit)
  (package-install 'auto-complete)
  ;; (package-install 'column-marker) ;; not available?
  ;;
  (package-install 'inf-ruby)
  (package-install 'enh-ruby-mode)
  (package-install 'smartparens)
  (package-install 'highlight-indentation)
  (package-install 'robe)
  (package-install 'dash)
<<<<<<< HEAD
  ;; (package-install 'dash-at-point) ;; not available?
=======
  ;; (package-install 'dash-at-point)
>>>>>>> a010c211fb19d6834a6dba551e02641be791a52e
  (package-install 'textmate)
  (package-install 'flycheck))

;;;; The End ;;;;
