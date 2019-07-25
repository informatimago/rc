;;; emacs-backups -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(setq backup-by-copying   t             ; don't clobber symlinks
      backup-directory-alist '(("/" . "~/.backups"))
                                        ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions   5
      kept-old-versions   3
      version-control     t)


;; Local Variables:
;; coding: utf-8
;; End Variables:
