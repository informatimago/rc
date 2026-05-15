;;; emacs-epilog -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "epilog")

(defun pjb-epilog-shell-bool (command)
  (car (read-from-string
        (shell-command-to-string
         (format "( %s ; ) 2>/dev/null 1>&2 && echo t || echo nil"
                 command)))))

(if (file-exists-p (concat source-directory "/emacs.c"))
    (message "emacs sources are installed in %s"source-directory)
    (progn
      (warn "~/rc/emacs-epilog.el: %S is missing. Trying to fetch the sources and set source-directory."
            (concat source-directory "/emacs.c"))
      (let ((source-root (expand-file-name "~/emacs/src"))
            (fetcher
              (cond
                ((pjb-epilog-shell-bool "which ncftpget") "ncftpget %S")
                ((pjb-epilog-shell-bool "which wget")     "wget --quiet %S")
                ((pjb-epilog-shell-bool "which curl")     "curl -O -s %S")
                (t (warn "I have no ftp fetching program available. Not fetching emacs sources.")
                   nil))))
        (when fetcher
          (shell-command (format "( mkdir -p %S ; cd %S && %s && tar Jxf emacs-%s.tar.xz ) &"
                                 source-root source-root
                                 (format fetcher
                                         (format "ftp://ftp.gnu.org/pub/gnu/emacs/emacs-%s.tar.xz"
                                                 emacs-version))
                                 emacs-version))
          (setf source-directory (format "%s/emacs-%s" source-root emacs-version))
          (message "emacs sources installed in %s" source-directory)))))


(when (fboundp 'milliways-activate)
  (milliways-activate)
  (.EMACS "milliways activated!"))
(.EMACS "DONE")

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
