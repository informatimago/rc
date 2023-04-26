;;; emacs-package -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "~/rc/emacs-package.el %s" "Loading elpa.")

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")

(.EMACS "Initializing emacs packages.")
(setq package-archives '(("gnu"           . "http://elpa.gnu.org/packages/")
                         ("melpa-stable"  . "https://stable.melpa.org/packages/")
                         ("melpa"         . "https://melpa.org/packages/")
                         ("org"           . "http://orgmode.org/elpa/")))

;; (package-initialize) in emacs.el
(when (not package-archive-contents)
  (package-refresh-contents))


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

                   ;; intero
                   stack
                   ;; ormolu
                   ;; retrie
                   ;; shm
                   ;; ghc
                   ;; ghc-imported-from
                   ;; ghci-completion
                   ;; ac-haskell-process
                   ;; dante
                   ;; flycheck-ghcmod
                   ;; flycheck-haskell
                   ;; flycheck-hdevtools
                   ;; flycheck-liquidhs
                   ;; haskell-emacs
                   ;; haskell-emacs-base
                   ;; haskell-emacs-text
                   ;; haskell-mode
                   ;; haskell-snippets
                   ;; haskell-tab-indent
                   ;; hi2
                   ;; hindent
                   ;; hyai
                                        ; robe not on emacs-24.4 ?
                   cobol-mode

                   lsp-mode
                   lsp-haskell
                   lsp-ui
                   ))

(defun pjb-install-packages ()
  (interactive)
  (mapc (lambda (package)
          (ignore-errors (package-install package)))
        *packages*))

(pjb-install-packages)

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
