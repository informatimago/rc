;;; emacs-windows-paths.el --- Convert paths
;;; -*- mode:emacs-lisp; coding:utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Code:

(require 'cl-lib)

;; ;; windows home
;; 
;; export WOME='c:/users/user'
;; export ROME='c:/users/user/AppData/Roaming/'
;; export MOME='c:/msys64/home/user'
;; export FROM=WOME
;; export RC="$MOME/rc"
;; source "$RC/bashrc-windows-minimal"
;; 
;; 
;; ;; windows roaming home
;; 
;; export WOME='c:/users/user'
;; export ROME='c:/users/user/AppData/Roaming/'
;; export MOME='c:/msys64/home/user'
;; export FROM=ROME
;; export RC="$MOME/rc"
;; source "$RC/bashrc-windows-minimal"
;; 
;; 
;; ;; msys2 home
;; 
;; export WOME='/c/users/user'
;; export ROME='/c/users/user/AppData/Roaming/'
;; export MOME='/home/user'
;; export FROM=MOME
;; export RC="$MOME/rc"
;; source "$RC/bashrc-windows-minimal"
;; 
;; 
;; 
;; export USERPROFILE='C:/Users/user'
;; export LOCALAPPDATA="$USERPROFILE/AppData/Local"
;; export APPDATA="$USERPROFILE/AppData/Roaming"
;; export TEMP='C:/msys64/tmp'
;; export TMP="$TEMP"
;; export PATH="/msys64/bin:/usr/bin:$HOME/bin:$PATH"
;; 
;; 
;; | Windows path  | Posix path: | file system         |
;; |---------------+-------------+---------------------|
;; | C:/foo        | /c/foo      | windows file system |
;; | c:/msys64/foo | /foo        | msys2 file system   |

(defun posix-device-letter-prefix-p (path)
  (and (= ?/ (aref path 0))
        (= ?/ (aref path 2))
        (let ((ch (aref path 1)))
          (or (<= ?A ch ?Z)
              (<= ?a ch ?z)))))

(defvar *root-windows-prefix* "c:/msys64")

(defun posix-to-windows-path (path)
  (if (posix-device-letter-prefix-p path)
      (format "%c:%s" (aref path 1) (substring path 2))
      (format "%s%s" *root-windows-prefix* path)))

(assert (equalp (posix-to-windows-path "/c/users/user/.bashrc") "c:/users/user/.bashrc"))
(assert (equalp (posix-to-windows-path "/home/user/.bashrc")    "c:/msys64/home/user/.bashrc"))

(defun windows-device-letter-prefix-p (path)
  (if (and (let ((ch (aref path 0)))
          (or (<= ?A ch ?Z)
              (<= ?a ch ?z)))
        (= ?: (aref path 1)))
       (string (aref path 0))
       nil))

(defun windows-absolute-path-p (path)
  (and (windows-device-letter-prefix-p path)
       (= ?/ (aref path 2))))

(defun windows-to-posix-path (path)
  (if (string-prefix-p *root-windows-prefix* path 'ignore-case)
      (substring path (length *root-windows-prefix*))
      (let ((device (windows-device-letter-prefix-p path)))
        (if device
            (if (windows-absolute-path-p path)
                (format "/%s%s" device (substring path 2))
                (error "relative windows paths not supported: %S" path))))))

(assert (equalp (windows-device-letter-prefix-p "c:/users/user/.bashrc") "c"))
(assert (equalp (windows-device-letter-prefix-p "c:users/user/.bashrc")  "c"))
(assert (windows-absolute-path-p        "c:/users/user/.bashrc"))
(assert (not (windows-absolute-path-p   "c:users/user/.bashrc")))
(assert (equalp (windows-to-posix-path "c:/users/user/.bashrc")       "/c/users/user/.bashrc"))
(assert (equalp (windows-to-posix-path "c:/msys64/home/user/.bashrc") "/home/user/.bashrc"))


(provide  'emacs-windows-paths)
;;;; THE END ;;;;
