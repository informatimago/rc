;;; emacs-epilog -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "epilogue")

(unless (file-exists-p (concat source-directory "/emacs.c"))
  (warn "~/rc/emacs-epilog.el: Please set the right source-directory."))


(when (fboundp 'milliways-activate)
  (milliways-activate)
  (.EMACS "milliways activated!"))
(.EMACS "DONE")

;; Local Variables:
;; coding: utf-8
;; End Variables:
