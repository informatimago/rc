;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
(.EMACS "~/rc/emacs-package.el %s" "Loading elpa.")
(when (require 'package nil t)
  ;; Anyn add to list for package-archives (to add marmalade or melpa) goes here
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")))

  (package-initialize)

  (defun package-update-load-path ()
    "Update the load path for newly installed packages."
    (interactive)
    (let ((package-dir (expand-file-name package-user-dir)))
      (mapc (lambda (pkg)
              (let ((stem (symbol-name (car pkg)))
                    (version "")
                    (first t)
                    path)
                (mapc (lambda (num)
                        (if first
                          (setq first nil)
                          (setq version (format "%s." version)))
                        (setq version (format "%s%s" version num)))
                      (aref (cdr pkg) 0))
                (setq path (format "%s/%s-%s" package-dir stem version))
                (add-to-list 'load-path path)))
            package-alist)))


  (when (fboundp 'run-lac-functions)
    (run-lac-functions)))


;;;; The End ;;;;

