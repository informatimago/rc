;;; emacs-esp -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(require 'flycheck)
(require 'pjb-cl)
;; (require 'pjb-emacs-common)

(defparameter *pjb-esp-includes*
  (mapcar (function expand-file-name)
   (cons "~/esp/xtensa-esp32-elf/xtensa-esp32-elf/sysroot/usr/include"
         (let ((includes '()))
           (mapdirectories (lambda (path)
                             (if (string= (file-name-nondirectory path) "include")
                                 (push path includes)))
                           "~/esp/esp-idf/components")
           includes))))

(defun pjb-esp-project-file-p (path)
  "Indicate whether PATH is in a ESP project directory."
   (do-directories-up (dir (directory-namestring (expand-file-name path)) nil)
     (let ((cmakelist (concat dir "CMakeLists.txt")))
       (when (file-exists-p cmakelist)
         (with-temp-buffer nil
           (insert-file-contents-literally cmakelist)
           (goto-char (point-min))
           (when (re-search-forward "^include(\\$ENV{IDF_PATH}/tools/cmake/project\\.cmake)" nil t)
             (return (directory-namestring cmakelist))))))))

(defun pjb-esp-project-subdirectories (path)
  "Return a list of all subdirectories in PATH."
  (let ((subdirs '()))
    (mapdirectories (lambda (path)
                      (unless (string-match "/build\\(/\\|$\\)" path)
                        (push path subdirs)))
                    path :prefix)
    subdirs))

(defun pjb-esp-include-meat ()
  "Meat for c-mode-common-hook.  Select esp includes for esp projects."
  (when (buffer-file-name)
    (let ((project-root (pjb-esp-project-file-p (buffer-file-name))))
      (when project-root
        (let ((includes (concatenate 'list
                                     (list "." (concat project-root "build/include"))
                                     (pjb-esp-project-subdirectories project-root)
                                     *pjb-esp-includes*)))
          (make-variable-buffer-local 'flycheck-clang-include-path)
          (make-variable-buffer-local 'flycheck-gcc-include-path)
          (setq-local flycheck-clang-include-path includes)
          (setq-local flycheck-gcc-include-path   includes)
          (setq-local flycheck-checker 'c/c++-gcc)
          (setq-local flycheck-c/c++-gcc-executable (expand-file-name "~/esp/xtensa-esp32-elf/bin/xtensa-esp32-elf-gcc")))))))

(add-hook 'c-mode-common-hook 'pjb-esp-include-meat)

(provide 'emacs-esp)

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
