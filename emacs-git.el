;;; emacs-git -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

;; 9d90cf9 (HEAD, origin/rework_mapping_smartcard_2, rework_mapping_smartcard_2)

(defun gbra-font-lock ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(( "\\([0-9a-f]+\\) \\(\\(([^)]*) \\)?\\)\\(<[^>]*>\\)"
      (1 font-lock-comment-face)
      (2 font-lock-string-face)
      (4 font-lock-function-name-face)))))

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
