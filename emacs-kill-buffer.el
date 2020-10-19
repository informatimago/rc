;;; emacs-kill-buffer -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(defun mode-line--kill-buffer (event)
  (interactive "e")
  (kill-buffer))


(defvar mode-line-kill-buffer
  '("["
    (:propertize ("Kill")
     mouse-face mode-line-highlight
     help-echo "mouse-1: Kill the current buffer"
     local-map (keymap
                (mode-line keymap
                           (down-mouse-1 . mode-line--kill-buffer))))

    "]"
    " "))


(set-default 'mode-line-format (append mode-line-format mode-line-kill-buffer))

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
