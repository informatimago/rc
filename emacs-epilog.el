;;; emacs-epilog -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "epilogue")

(unless (file-exists-p (concat source-directory "/emacs.c"))
  (warn "~/rc/emacs-epilog.el: %S is missing. Trying to fetch the sources and set source-directory."
        (concat source-directory "/emacs.c"))
  (let ((source-root (expand-file-name "~/emacs/src")))
    (shell-command (format "( mkdir -p %S ; cd %S && ncftpget ftp://ftp.gnu.org/pub/gnu/emacs/emacs-%s.tar.xz && tar Jxf emacs-%s.tar.xz ) &"
                           source-root source-root emacs-version emacs-version))
    (setf source-directory (format "%s/emacs-%s" source-root emacs-version))))


(when (fboundp 'milliways-activate)
  (milliways-activate)
  (.EMACS "milliways activated!"))
(.EMACS "DONE")

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
