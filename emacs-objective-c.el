;;; emacs-objective-c -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(defvar *xcode-developer-path* "/Applications/Xcode.app/Contents/Developer/")
(defvar *osx-base-path* (concat *xcode-developer-path* "Platforms/"))

(case system-type
  (darwin
   ;; Forgot what this was for..think some os x issues.
   (setenv "LC_CTYPE" "UTF-8")
   (setq flycheck-make-executable "/usr/local/bin/make"
         pjb-clang-executable (concat *xcode-developer-path*
                                          "Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++")
         pjb-clang-arguments `("-std=c++11"
                                   "-isysroot"
                                   ;; If coding for iOS
                                   ,(concat *osx-base-path*
                                            "iPhoneOS.platform/Developer/SDKs/iPhoneOS9.2.sdk")
                                   ;; If coding for OS X
                                   ;; ,(concat *osx-base-path*
                                   ;;         "MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
                                   "-I" "/usr/include/c++/4.2.1"
                                   ;; "-I" "/usr/local/lib/ocaml/"
                                   )
         flycheck-c/c++-clang-executable (concat *xcode-developer-path*
                                                 "Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++"))
   (setq pjb-clang-executable "armv7-apple-darwin11-clang"
         pjb-clang-arguments (list "-std=c++11"
                                       "-isysroot"
                                       (home ".nix-profile/iPhoneOS9.2.sdk")
                                       ;; "-I/usr/local/lib/ocaml/"
                                       )))
  (linux

   ))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
