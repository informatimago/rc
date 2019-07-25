;;; emacs-android -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:


(.EMACS "~/rc/emacs-android.el %s" "Android Development Pascal Bourguignon's emacs startup file.")
(require 'cc-mode)



;; (setf auto-mode-alist
;;       (sort* (list* '("\\.md$" . text-mode)
;;                     '("/ubudu-sdk/documentation/ios/.*\\.h$". objc-mode)
;;                     '("/src/bwin_tournament_ios/.*\\.h$". objc-mode)
;;                     '("/src/\\(IOS-SDK\\|audio\\|uscdtest\\)/.*\\.[hm]$" . objc-mode)
;;                     (remove* 'modula-2-mode auto-mode-alist :key (function cdr)))
;;              (function string<)
;;              :key (function car)))


;; (when (require 'yasnippet nil t)
;;   (yas-global-mode 1))

(require 'auto-complete nil t)

(defun java-meat ()
  (interactive)
  (when (fboundp 'auto-complete-mode)
    (auto-complete-mode 1))
  (setf tab-stop 2
        tab-width 2
        c-indent-level 2
        c-basic-offset 2
        c-tab-always-indent t))

(add-hook 'java-mode-hook 'java-meat)

;;;----------------------------------------------------------------------------
;; Get the Groovy support for Emacs from http://svn.codehaus.org/groovy/trunk/groovy/ide/emacs
;; Symlink the downloaded Groovy support into your .emacs.d folder as "groovy"


;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; For some reason, these recommendations don't seem to work with Aquamacs
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
;; This does work with Aquamacs
(add-to-list 'auto-mode-alist (cons "\\.gradle\\'" 'groovy-mode))
(add-to-list 'auto-mode-alist (cons "\\.groovy\\'" 'groovy-mode))
;; This _might_ not work with Aquamacs (not sure what value it offers)
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("gradle" . groovy-mode))
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          (lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))
;;;----------------------------------------------------------------------------

;; (when (and (file-exists-p "/data/sound/beeps/Macintosh_Question.wav")
;;            (file-exists-p "/usr/bin/mplayer"))
;;   (setf visible-bell nil
;;         ring-bell-function (lambda ()
;;                              (shell-command-to-string
;;                               "mplayer /data/sound/beeps/Macintosh_Question.wav"))))

(global-set-key (kbd "C-h 1") 'android-search-region)
(add-hook 'java-mode 'pjb-java-edit-meat)


(defparameter *android-tools-directory*
  (if (string= (hostname) "kuiper")
      "~/firms/ubudu/tools"
      "~/opt"))
(setf *android-tools-directory* "~/opt")


;; (require 'ubudu) ; c-style

(push (expand-file-name (concat *android-tools-directory* "/adt/sdk/tools/lib/")) load-path)

(require 'android-mode nil t)
(when (require 'android nil t)
  (setf android-mode-sdk-dir (expand-file-name (concat *android-tools-directory* "/adt/sdk")))
  (require 'android-classes)

  (defun gud-meat ()
    (interactive)
    (add-to-list 'gud-jdb-classpath
                 (expand-file-name (concat *android-tools-directory*
                                           "/adt/sdk/platforms/android-17/android.jar"))))
  (add-hook 'gud-mode-hook 'gud-meat)

  ;; (require 'cedet)
  ;; (let ((path (expand-file-name "~/emacs/jdee/lisp")))
  ;;   (when (file-exists-p path)
  ;;     (pushnew path load-path)
  ;;     (require 'jde nil t)))


  ;; eclipse has a hard time dealing with backup files.
  (let ((temporary-file-directory "~/.backups"))
    (setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))



  (setf android-filter-function nil)
  (setf android-filter-function (android-filter-match-tag     "Listeria"))

  (setf android-filter-function (android-filter-or
                                 (android-filter-match-tag     "ubudu\\|bwin\\|AndroidRuntime")
                                 (android-filter-match-message "ubudu\\|bwin\\|AndroidRuntime")))

  (setf android-filter-function (android-filter-or
                                 (android-filter-match-tag     "ubudu\\|AndroidRuntime\\|Volley")
                                 (android-filter-match-message "ubudu\\|AndroidRuntime\\|Volley\\|Exception")))

  )

;; Local Variables:
;; coding: utf-8
;; End Variables:
;;;; THE END ;;;;
