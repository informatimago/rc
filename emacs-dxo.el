;;; emacs-dxo -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

;;;;
;;;; Pascal J. Bourguignon's emacs startup file at DxO Consumers SAS.

(load "~/rc/emacs-common.el")
(.EMACS "~/rc/emacs-dxo.el %s" "Pascal J. Bourguignon's emacs startup file at DxO Consumers SAS.")

;;;----------------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------------

(.EMACS "custom faces")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "darkgreen"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-preprocessor-face ((t (:foreground "#550000"))))
 '(font-lock-string-face ((t (:foreground "#aa2211"))))
 '(font-lock-type-face ((t (:foreground "#6620b0"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(rst-level-1-face ((t (:background "grey20" :height 1.9))) t)
 '(rst-level-2-face ((t (:background "grey20" :height 1.7))) t)
 '(rst-level-3-face ((t (:background "grey20" :height 1.4))) t)
 '(rst-level-4-face ((t (:background "grey20" :height 1.2))) t)
 '(rst-level-5-face ((t (:background "grey20" :height 1.1 :weight bold))) t)
 '(rst-level-6-face ((t (:background "grey20" :height 1.0 :weight bold))) t)
 '(smerge-refined-change ((t (:background "#997722")))))





(.EMACS "custom variables")
(custom-set-variables
 '(c-backslash-column (quote set-from-style))
 '(c-backslash-max-column (quote set-from-style))
 '(c-basic-offset (quote set-from-style))
 '(c-block-comment-prefix (quote set-from-style))
 '(c-cleanup-list (quote set-from-style))
 '(c-comment-only-line-offset (quote set-from-style))
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-doc-comment-style (quote set-from-style))
 '(c-echo-syntactic-information-p t)
 '(c-hanging-braces-alist (quote set-from-style))
 '(c-hanging-colons-alist (quote set-from-style))
 '(c-hanging-semi&comma-criteria (quote set-from-style))
 '(c-indent-comment-alist (quote set-from-style))
 '(c-indent-comments-syntactically-p (quote set-from-style))
 '(c-label-minimum-indentation (quote set-from-style))
 '(c-offsets-alist (quote nil))
 '(c-special-indent-hook (quote nil))
 '(erc-auto-query (quote window))
 '(erc-autojoin-channels-alist (quote (("freenode.net") ("irc.oftc.net" "#uml"))))
 '(erc-away-timestamp-format "<%H:%M:%S>")
 '(erc-echo-notices-in-current-buffer t)
 '(erc-echo-timestamps nil)
 '(erc-email-userid t)
 '(erc-encoding-coding-alist (quote (("#emacsfr" . iso-8859-15) ("#scheme-es" . iso-8859-15))))
 '(erc-fill-column 90)
 '(erc-fill-function (quote erc-fill-variable))
 '(erc-fill-prefix "\"\"")
 '(erc-fill-static-center 0)
 '(erc-fill-variable-maximum-indentation 0)
 '(erc-hide-list (quote nil))
 '(erc-ignore-list (quote ("ad37e918" "173.55.233.24")))
 '(erc-ignore-per-channel-alist (quote (("#scheme" . "rudybot") ("#emacs" . "rudybot"))))
 '(erc-ignore-per-channel-reply-alist (quote (("#scheme" . "rudybot") ("#emacs" . "rudybot"))))
 '(erc-ignore-reply-list (quote nil))
 '(erc-insert-away-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-interpret-mirc-color t)
 '(erc-log-write-after-insert nil)
 '(erc-log-write-after-send nil)
 '(erc-max-buffer-size 300000)
 '(erc-minibuffer-ignored t)
 '(erc-minibuffer-notice t)
 '(erc-modules (quote (autoaway autojoin button completion irccontrols log match netsplit readonly replace ring scrolltobottom services stamp track truncate)))
 '(erc-nick (quote ("pjb-d")))
 '(erc-notice-prefix "   *** ")
 '(erc-pals (quote nil))
 '(erc-port 6667)
 '(erc-prompt (lambda nil (buffer-name (current-buffer))))
 '(erc-prompt-for-password t)
 '(erc-quit-reason-various-alist nil)
 '(erc-save-buffer-on-part nil)
 '(erc-save-queries-on-quit nil)
 '(erc-server "irc.freenode.org")
 '(erc-server-coding-system (quote (utf-8 . undecided)))
 '(erc-server-reconnect-attempts 100)
 '(erc-server-reconnect-timeout 60)
 '(erc-timestamp-format nil)
 '(erc-timestamp-intangible nil)
 '(erc-user-full-name "Pascal J. Bourguignon")
 '(eval-expression-print-length nil)
 '(gnus-select-method (quote (nntp "news.individual.com")))
 '(indent-tabs-mode nil)
 '(mail-host-address nil)
 '(message-log-max 5000)
 '(org-fontify-done-headline t)
 '(org-todo-keywords (quote ((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE(d)") (sequence "|" "CANCELED(c)"))))
 '(safe-local-variable-values (quote ((encoding . utf-8) (Readtable . PY-AST-READTABLE) (Package . CLPYTHON\.PARSER) (Readtable . PY-AST-USER-READTABLE) (Package . CLPYTHON) (Package . "CCL") (syntax . COMMON-LISP) (Package . CLPYTHON\.UTIL) (Package . CCL) (Package . CLPYTHON\.MODULE\.OPERATOR) (Syntax . COMMON-LISP))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "hubble.informatimago.com")
 '(smtpmail-smtp-service 25)
 '(starttls-use-gnutls nil)
 '(user-mail-address "pbourguignon@dxo.com")
 '(visible-bell t)
 '(warning-suppress-types (quote ((undo discard-info)))))

(put 'erase-buffer 'disabled nil)


;;;----------------------------------------------------------------------------
;;; DxO specific stuff
;;;----------------------------------------------------------------------------
(push (lambda ()
        (when (string-match "^/usr/local/Cellar/" (buffer-file-name (current-buffer)))
          (view-mode 1)))
      find-file-hook)

(deletef auto-mode-alist "\\.m$"  :test (function equal) :key (function car))
(deletef auto-mode-alist "\\.mm$" :test (function equal) :key (function car))
(deletef auto-mode-alist "\\.md$" :test (function equal) :key (function car))
(appendf auto-mode-alist '(("\\.m$"  . objc-mode)
                           ("\\.mm$" . objc-mode)
                           ("\\.md$" . text-mode)))
(push '("/src/OpticsPro-Mac.*\\.[hm]" . objc-mode)  auto-mode-alist)


(require 'vc-svn)

(require 'tls)
(setf tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --insecure -p %p %h"
                    "gnutls-cli --insecure -p %p %h --protocols ssl3"))

(require 'org-jira)
(setf jiralib-url "https://jira:8443/")

(when (require 'semantic nil t)
  (semantic-mode 1))

(require 'pjb-c-style)
(require 'pjb-objc-edit)
(require 'pjb-objc-ide)
(require 'dxo)


(defun dxo-objc-ide--dxo-class-type-p (type)
  ;; TODO: It would be better to collect the exact list of OPM classes…
  (and (listp type)
       (eq (second type) '*)
       (null (cddr type))
       (symbolp (first type))
       (member* (substring (symbol-name (first type)) 0 3) '("DOP" "DXF")
                :test (function string=))))


(defun dxo-objc-ide-settings ()
  (interactive)
  (setf *pjb-objc-ide--nslog-function* "DXFLogDebug"
        *pjb-objc-ide--entry-log-tag*  "PJB-DEBUG")
  (pushnew '((ZoomMode) . "%d")  *pjb-objc-ide--type-formatter-map*
           :test (function equal))
  (pushnew '(dxo-objc-ide--dxo-class-type-p . "%@") *pjb-objc-ide--type-formatter-map*
           :test (function equal)))

(dxo-objc-ide-settings)


(let ((tags-add-tables t))
  (setf tags-table-list '())
  (ignore-errors (visit-tags-table "~/src/Cocoa.etags"))
  ;; ;(ignore-errors (visit-tags-table "~/src/Ruby.etags"))
  (ignore-errors (visit-tags-table "~/src/OpticsPro.etags")))



;;----------------------------------------------------------------------------
;; Gherkin/Cucumber stuff.

(setf feature-default-language "en")
(setf feature-default-i18n-file "~/emacs/cucumber/i18n.yml")
(when (require 'feature-mode nil t)
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

;;----------------------------------------------------------------------------


(defface gherkin-error
    '((default (:foreground "red")))
  "Error in Gherkin logs.")
(defface gherkin-simple
    '((default (:foreground "yellow")))
  "Simple lines in Gherkin logs.")
(defface gherkin-command
    '((default (:foreground "blue")))
  "Command lines in Gherkin logs.")
(defface gherkin-build
    '((default (:foreground "grey")))
  "Build lines in Gherkin logs.")


(defun gherkin-log-meat ()
  (interactive)
  (toggle-truncate-lines 1)
  (font-lock-mode 1)
  (setf font-lock-maximum-size 10000000)
  (setf font-lock-keywords '())
  (font-lock-add-keywords
   nil
   '(("^\\(error .*\\)"                (1 gherkin-error))
     ("^\\(build .*error on line.*\\)" (1 gherkin-error))
     ("^\\(.*\\.m:[0-9]+: error:.*\\)" (1 gherkin-error))
     ("^\\(simple .*\\)"               (1 gherkin-simple))
     ("^\\(command .*\\)"              (1 gherkin-command))
     ("^\\(build .*\\)"                (1 gherkin-build))))
  (font-lock-fontify-buffer))

(put 'define-derived-mode 'lisp-indent-function 3)


(define-derived-mode gherkin-log-mode view-mode "Gherkin"
  "Mode for Gherkin logs"
  (gherkin-log-meat))


(deletef auto-mode-alist "/DOP-[^/]*\\.log$" :test (function equal) :key (function car))
(deletef auto-mode-alist "\\.log$"           :test (function equal) :key (function car))
(appendf auto-mode-alist '(("/DOP-[^/]*\\.log$" . gherkin-log-mode)
                           ("\\.log$"          . view-mode)))




(define-derived-mode valgrind-mode compilation-mode "Valgrind"
  "Mode to read valgrind log files."
  (setf font-lock-keywords nil)
  (font-lock-add-keywords
   nil
   '(("^{[^}]*}"
      (0 font-lock-preprocessor-face))
     ("^\\(==[0-9]*==\\) \\([^ \n].*\\)$"
      (1 font-lock-variable-name-face)
      (2 font-lock-warning-face))
     ("^\\(==[0-9]*==\\)   \\([^ \n].*\\)$"
      (1 font-lock-variable-name-face)
      (2 font-lock-comment-face))
     ("^\\(==[0-9]*==\\)    [a-z]+ \\(0x[0-9A-F]*\\): \\([^( \n]*\\)(\\(.*\\)) (\\([^:]+:[0-9]+\\))$"
      (1 font-lock-variable-name-face)
      (2 font-lock-constant-face)
      (3 font-lock-function-name-face)
      (4 font-lock-type-face)
      (5 font-lock-comment-face)))))

(add-to-list 'auto-mode-alist '("\\.\\(val\\|hel\\)grind$" . valgrind-mode))



(defun dxo-bell ()
  (message "bell"))

(setf ring-bell-function 'dxo-bell)


(dolist (key (list (kbd "<C-wheel-down>")
                   (kbd "<S-wheel-down>")
                   (kbd "<wheel-down>")
                   (kbd "<C-wheel-up>")
                   (kbd "<S-wheel-up>")
                   (kbd "<wheel-up>")))
  (global-unset-key key))
;; (setf eval-expression-print-length nil)
;;----------------------------------------------------------------------------

(when (require 'auto-complete nil t)
 (dolist (mode '(python-mode-hook ruby-mode-hook lisp-mode-hook emacs-lisp-mode-hook))
   (add-hook mode 'auto-complete-mode)))

;; (setf (getenv "PYTHONPATH") "/usr/local/lib/python2.7/site-packages:/usr/local/Cellar/mercurial/2.4.2/libexec")
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setf jedi:setup-keys t
      jedi:complete-on-dot t)


(require 'confluence)
(setq confluence-url "https://confluence:8453/rpc/xmlrpc")

;;----------------------------------------------------------------------------

(require 'erc-notify)
(desktop-read)

;;----------------------------------------------------------------------------

(defparameter *opticspro-branch* "OpticsPro-Mac-branches-filmstripRefactor")
(defparameter *opticspro-branch* "OpticsPro-Mac-branches-filmstripRefactor-BCCollectionView")
(defparameter *opticspro-branch* "OpticsPro-Mac-trunk")
(set-sources (file-truename (format "~/src/%s" *opticspro-branch*)))


(require 'pjb-thi)
(set-shadow-map '(("~/src/OpticsPro-Mac-trunk/"                                               . "~/src/OpticsPro-Mac-shadow/")
                  ("/Volumes/Data/pbourguignon/src/OpticsPro-Mac-trunk/"                      . "/Volumes/Data/pbourguignon/src/OpticsPro-Mac-shadow/")
                  ("~/src/OpticsPro-Mac-branches-filmstripRefactor/"                          . "~/src/OpticsPro-Mac-shadow/")
                  ("/Volumes/Data/pbourguignon/src/OpticsPro-Mac-branches-filmstripRefactor/" . "/Volumes/Data/pbourguignon/src/OpticsPro-Mac-shadow/")))

(setf c-macro-preprocessor
      (join
       '("cat > /tmp/cpp.c"
         ";"
         "SRC=$(pwd) ; while [ ! -z \"${SRC}\" -a ! -d \"${SRC}/DXOOpticsPro.xcodeproj\" ] ; do SRC=\"$(dirname \"${SRC}\")\" ; done"
         ";"
         "/Applications/Xcode.app/Contents/Developer/usr/bin/llvm-gcc-4.2"

         "-O0"
         "-Wall"
         "-Werror-implicit-function-declaration"
         "-Wmissing-braces"
         "-Wnewline-eof"
         "-Wno-deprecated-declarations"
         "-Wno-trigraphs"
         "-Wparentheses"
         "-Wreturn-type"
         "-Wshadow"
         "-Wshorten-64-to-32"
         "-Wsign-compare"
         "-Wswitch"
         "-Wunknown-pragmas"
         "-Wunused-function"
         "-Wunused-label"
         "-Wunused-value"
         "-Wunused-variable"

         "-arch x86_64"

         "-fasm-blocks"
         "-fmessage-length=0"
         "-fpascal-strings"
         "-fvisibility=hidden"

         "-gdwarf-2"
         "-mmacosx-version-min=10.6"

         "-pipe"
         "-std=c99"
         "-x objective-c"

         "-DDEBUG"
         "-DDLLAPPKIT"
         "-DDXF_LOG_CATEGORY=DXFAppKitUI"
         "-DDXO_FOUNDATION_DYNAMIC"
         "-DDXO_FWK_LOG_CATEGORY=DXFAppKitUI"
         "-D_DEBUG"

         "-F/Applications/Xcode.app/Contents/Developer/Library/Frameworks"

         "$(find ${SRC} \\( -name \\*.app -o -name \\*.octest -o -name \\*.ibplugin \\) -prune -o \\( -name \\*.framework -print \\) |xargs -n 1 dirname |sort -u|sed -e 's/^/-F/')"

         "-F${HOME}/src/OpticsPro-Mac-trunk/AppKit/bin/debug"

         "-F${SRC}/AppKit/bin/debug"
         "-F${SRC}/AppKit/externals/Common/bin/debug"
         "-F${SRC}/bin/debug"
         "-F${SRC}/externals/OCHamcrest"
         "-F${SRC}/externals/OCMock"

         "-framework Cocoa"
         "-framework OCHamcrest"
         "-framework OCMock"
         "-framework SenTestingKit"
         "-framework DXFAppKitUI"
         "-framework DXFAppKitUI"

         "$(find ${SRC} -name 'src' -o -name 'Classes' -o -name 'Tests' -o -name 'Interfaces' | sed -e 's/^/-I/')"

         "-I${SRC}/AppKit/bin/debug/include"
         "-I${SRC}/AppKit/build/AppKit.build/debug/DXFAppKitUI.build/DXFAppKitUI-all-target-headers.hmap"
         "-I${SRC}/AppKit/build/AppKit.build/debug/DXFAppKitUI.build/DXFAppKitUI-own-target-headers.hmap"
         "-I${SRC}/AppKit/build/AppKit.build/debug/DXFAppKitUI.build/DerivedSources"
         "-I${SRC}/AppKit/build/AppKit.build/debug/DXFAppKitUI.build/DerivedSources/x86_64"
         "-I${SRC}/AppKit/externals/Common/include/DxOFramework"
         "-I${SRC}/AppKit/externals/Common/include/Foundation"

         "-iquote ${SRC}/AppKit/build/AppKit.build/debug/DXFAppKitUI.build/DXFAppKitUI-generated-files.hmap"
         "-iquote ${SRC}/AppKit/build/AppKit.build/debug/DXFAppKitUI.build/DXFAppKitUI-project-headers.hmap"
         "-isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk"


         "-E"
         "-c /tmp/cpp.c")
       " "))


(setf c-macro-cppflags "-I. -framework /Applications/Xcode.app/Contents/Developer/Library/Frameworks/SenTestingKit.framework/Versions/A/Headers/")

(require 'pjb-xcode)
(ignore-errors (load-faces-from-xcode-dvtcolortheme "~/Library/Developer/Xcode/UserData/FontAndColorThemes/PJB Midnight.dvtcolortheme"))



(defun re-delete-lines-between (start end start-re end-re)
  (interactive "rsStart regexp: \nsEnd regexp: ")
  (save-excursion
    (narrow-to-region start end)
    (while (re-search-forward start-re nil t)
      (let ((end (match-end 0)))
        (goto-char (match-beginning 0))
        (beginning-of-line)
        (let ((start (point)))
          (if (re-search-forward end-re nil t)
              (progn
                (goto-char (match-end 0))
                (end-of-line)
                (forward-char)
                (delete-region start (point)))
              (goto-char end)))))
    (widen)))

(defun dxo-log-clean ()
  (interactive)
  (delete-matching-lines "not doing caf matching on unsupported image" (point-min) (point-max))
  (re-delete-lines-between (point-min) (point-max)
                           "\\(UpdateParameters\\|DOPQuickPreview\\|DOPThumbnailsPreloadingController\\.ToThumbnailCache\\).*{"
                           "^\t}\\()\\| error: \\)"))

;;;----------------------------------------------------------------------------

(defun my-nxml-forward-element ()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))

(setf hs-special-modes-alist
      (append '((lua-mode
                 "\\([[{(]\\|\\<\\(do\\|\\(?:functio\\|the\\)n\\)\\>\\)"
                 "\\([]})]\\|\\<\\(end\\)\\>\\)"
                 nil
                 lua-forward-sexp
                 nil)
                (nxml-mode
                 "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                 ""
                 "<!--" ;; won't work on its own; uses syntax table
                 (lambda (arg) (my-nxml-forward-element))
                 nil))
              (remove-if (lambda (key) (member key '(lua-mode nxml-mode)))
                         hs-special-modes-alist :key 'car)))

(global-set-key (kbd "<f9>") 'hs-toggle-hiding)


(defun outline-easy-bindings-meat ()
  (interactive)
  (require 'outline-mode-easy-bindings))

(add-hook 'outline-mode-hook       'outline-easy-bindings-meat)
(add-hook 'outline-minor-mode-hook 'outline-easy-bindings-meat)

(defun lua-mode-meat ()
  (interactive)
  (auto-complete-mode 1)
  ;; (setf outline-regexp ".*\\([[{(]\\|\\<\\(do\\|\\(?:functio\\|the\\)n\\)\\>\\)")
  ;; (outline-minor-mode 1)
  (hs-minor-mode 1))

(add-hook 'lua-mode-hook 'lua-mode-meat)

(find-library "outline-mode-easy-bindings")

(load "~/rc/emacs-epilog.el")

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
