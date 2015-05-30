;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs common epilog.

(.EMACS "epilogue")

(unless (file-exists-p (concat source-directory "/emacs.c"))
  (warn "~/rc/emacs-epilog.el: Please set the right source-directory."))


(when (fboundp 'milliways-activate) 
  (milliways-activate)
  (.EMACS "milliways activated!"))
(.EMACS "DONE")
