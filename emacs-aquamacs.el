;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;; PJB's Aquamacs rc file.
(.EMACS "~/rc/emacs-aquamacs.el PJB's Aquamacs rc file.")

(defun mac-vanilla-keys ()
  (interactive)
  (setf mac-command-modifier    'meta ; emacsformacosx
        mac-option-modifier     'alt
        one-buffer-one-frame    nil)
  (setf mac-command-key-is-meta t     ; which emacs?
        mac-reverse-ctrl-meta   nil))


(when (or (boundp 'aquamacs-version) (eq window-system 'ns))
  (mac-vanilla-keys)
  ;; (if 'thru-vnc
  ;;     (mac-vnc-keys)
  ;;     (mac-vanilla-keys))
  (cua-mode 0))




(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.dop$" . lua-mode))

(defun lua-meat ()
  (setq comment-start "--"
	comment-end "")
  (hs-minor-mode 1))

(add-hook 'lua-mode-hook 'lua-meat)


(find-file "~/rc/emacs-aquamacs.el")

