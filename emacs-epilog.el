;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs common epilog.

(.EMACS "epilogue")
(when (fboundp 'milliways-activate) 
  (milliways-activate)
  (.EMACS "milliways activated!"))
(.EMACS "DONE")
