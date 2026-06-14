;;; emacs-patches -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(load (rc "emacs-patches-compilation.el"))
;; (load (rc "emacs-patches-ox.el"))

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

;; auto-complete pre-30 obarray compatibility shim — convert obarrays
;; to their symbol list before AC tries to sequence-iterate them.
(with-eval-after-load 'auto-complete
  (defun my/ac-symbols-source (&rest _)
    (let ((acc '()))
      (mapatoms (lambda (s) (push (symbol-name s) acc)) obarray)
      acc))
  (when (boundp 'ac-source-symbols)
    (setq ac-source-symbols
          '((candidates . my/ac-symbols-source)
            (cache)))))



;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:

