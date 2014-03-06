;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs android startup file.
;;;;----------------------------------------------------------------------------

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


(when (require 'yasnippet nil t)
  (yas-global-mode 1))


(require 'auto-complete)

(defun java-meat ()
  (interactive)
  (auto-complete-mode 1)
  (setf tab-stop 2
        tab-width 2
        c-indent-level 2
        c-basic-offset 2
        c-tab-always-indent t))

(add-hook 'java-mode-hook 'java-meat)

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
(when (require 'android nil t)
 (setf android-mode-sdk-dir (expand-file-name (concat *android-tools-directory* "/adt/sdk")))
 (require 'android-mode)

 (defun gud-meat ()
   (interactive)
   (add-to-list 'gud-jdb-classpath
                (expand-file-name (concat *android-tools-directory*
                                          "/adt/sdk/platforms/android-17/android.jar"))))
 (add-hook 'gud-mode-hook 'gud-meat))

(require 'android-classes)

(require 'cedet)
(pushnew (expand-file-name "~/emacs/jdee/lisp") load-path)
(require 'jde)


;; eclipse has a hard time dealing with backup files.
(let ((temporary-file-directory "~/.backups"))
  (setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))



(setf android-filter-function nil)

(setf android-filter-function (android-filter-or
                                (android-filter-match-tag     "ubudu\\|bwin")
                                (android-filter-match-message "ubudu\\|bwin")))

(setf android-filter-function (android-filter-or
                                (android-filter-match-tag     "ubudu")
                                (android-filter-match-message "ubudu")))


;;;; THE END ;;;;

