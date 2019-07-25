;;; emacs-search -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(defvar *rfc-base-directory* "/data/doc/rfc/")
(defun search-rfc (word)
  (interactive "sRFC Search Word: ")
  (let ((quoted-word (shell-quote-argument (concat "\\<" word "\\>"))))
    (find-grep (format  "find %s \\( -name \\*.txt -o -name \\*.txt.gz \\) -print -exec zgrep -nHi %s {} \\;"
                        (shell-quote-argument *rfc-base-directory*) quoted-word quoted-word))))

;; Local Variables:
;; coding: utf-8
;; End Variables:
