;;; emacs-patches -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(load "~/rc/emacs-patches-compilation.el")

(require 'faces)

(defun defined-colors (&optional frame)
  "Return a list of colors supported for a particular frame.
The argument FRAME specifies which frame to try.
The value may be different for frames on different display types.
If FRAME doesn't support colors, the value is nil.
If FRAME is nil, that stands for the selected frame."
  (remove* nil
           (if (memq (framep (or frame (selected-frame))) '(x w32 ns))
               (xw-defined-colors frame)
               (mapcar 'car (tty-color-alist frame)))
           :key (function color-values)))

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
