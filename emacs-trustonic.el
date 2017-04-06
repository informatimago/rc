;;;; -*- Mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file at DxO Consumers SAS.

(load "~/rc/emacs-common.el")
(.EMACS "~/rc/emacs-trustonic.el %s" "Pascal J. Bourguignon's emacs startup file at Trustonic SA.")

(translate-powerbook-keyboard)

;;;----------------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------------

(.EMACS "custom faces")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error ((t (:inherit error :foreground "red"))))
 '(enh-ruby-string-delimiter-face ((t (:foreground "darkgreen"))))
 '(erc-input-face ((t (:foreground "light blue"))))
 '(erc-my-nick-face ((t (:foreground "dark blue" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "dark violet"))))
 '(font-lock-comment-face ((t (:foreground "dark violet"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-preprocessor-face ((t (:foreground "#cc5500"))))
 '(font-lock-string-face ((t (:foreground "yellow4"))))
 '(font-lock-type-face ((t (:foreground "#6620b0"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(region ((t (:background "#6080b0"))))
 '(rst-level-1-face ((t (:background "grey20" :height 1.9))) t)
 '(rst-level-2-face ((t (:background "grey20" :height 1.7))) t)
 '(rst-level-3-face ((t (:background "grey20" :height 1.4))) t)
 '(rst-level-4-face ((t (:background "grey20" :height 1.2))) t)
 '(rst-level-5-face ((t (:background "grey20" :height 1.1 :weight bold))) t)
 '(rst-level-6-face ((t (:background "grey20" :height 1.0 :weight bold))) t)
 '(smerge-refined-change ((t (:background "#997722"))) t))





(.EMACS "custom variables")
(custom-set-variables
 '(backup-by-copying-when-linked t)
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
 '(confluence-save-credentials t)
 '(display-time-24hr-format t)
 '(erc-auto-query (quote window))
 '(erc-autojoin-channels-alist (quote (("irc.oftc.net" "#openjdk") ("irc.freenode.org" "#maven" "#lisp" "#clnoobs" "#smack") ("irc.trustonic.internal" "#meudon" "#jenkins" "#tbase" "#newSDK" "#kinibi"))))
 '(erc-autojoin-delay 10)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing (quote connect))
 '(erc-away-timestamp-format "<%H:%M:%S>")
 '(erc-echo-notices-in-current-buffer t)
 '(erc-echo-timestamps nil)
 '(erc-email-userid t)
 '(erc-encoding-coding-alist (quote (("#emacsfr" . iso-8859-15) ("#scheme-es" . iso-8859-15))))
 '(erc-fill-column 90)
 '(erc-fill-function (quote erc-fill-variable))
 '(erc-fill-mode -1)
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
 '(erc-modules (quote (autoaway button completion irccontrols log match netsplit readonly replace ring scrolltobottom services stamp track truncate)))
 '(erc-nick (quote ("Pascal")))
 '(erc-notice-prefix "   *** ")
 '(erc-pals (quote nil))
 '(erc-port 6667)
 '(erc-prompt (lambda nil (buffer-name (current-buffer))))
 '(erc-prompt-for-password nil)
 '(erc-quit-reason-various-alist nil)
 '(erc-save-buffer-on-part nil)
 '(erc-save-queries-on-quit nil)
 '(erc-server "irc.trustonic.internal")
 '(erc-server-coding-system (quote (utf-8 . undecided)))
 '(erc-server-reconnect-attempts 100)
 '(erc-server-reconnect-timeout 60)
 '(erc-timestamp-format nil)
 '(erc-timestamp-intangible nil)
 '(erc-user-full-name "Pascal J. Bourguignon")
 '(eval-expression-print-length nil)
 '(gnus-select-method (quote (nntp "news.individual.com")))
 '(indent-tabs-mode nil)
 '(jde-java-font-lock-javadoc-face ((t (:inherit font-lock-doc-face :foreground "pink"))) t)
 '(jde-java-font-lock-link-face ((t (:foreground "cyan" :underline t))) t)
 '(mail-host-address nil)
 '(message-log-max 5000)
 '(org-agenda-files (quote ("~/src/trustonic/notes.txt")))
 '(org-fontify-done-headline t)
 '(org-todo-keywords (quote ((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE(d)") (sequence "|" "CANCELED(c)"))))
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp) (Syntax . ANSI-Common-Lisp) (Base . 10) (eval font-lock-add-keywords nil (\` (((\, (concat "(" (regexp-opt (quote ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")) t) "\\_>")) 1 (quote font-lock-variable-name-face))))) (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "|" "DONE(d@)" "CANCELED(c@)")) (org-fontify-done-headline . t) (tab-always-indent . t) (electric-indent-mode) (encoding . utf-8) (Readtable . PY-AST-READTABLE) (Package . CLPYTHON\.PARSER) (Readtable . PY-AST-USER-READTABLE) (Package . CLPYTHON) (Package . "CCL") (syntax . COMMON-LISP) (Package . CLPYTHON\.UTIL) (Package . CCL) (Package . CLPYTHON\.MODULE\.OPERATOR) (Syntax . COMMON-LISP))))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-trailing-whitespace t)
 '(smtpmail-smtp-server "hubble.informatimago.com")
 '(smtpmail-smtp-service 25)
 '(starttls-use-gnutls nil)
 '(user-mail-address "pascal.bourguignon@trustonic.com")
 '(visible-bell nil)
 '(warning-suppress-types (quote ((undo discard-info)))))

(put 'erase-buffer 'disabled nil)



;;;----------------------------------------------------------------------------
(load "~/rc/emacs-font.el")
(load "~/rc/emacs-palette.el")
(load "~/rc/emacs-slime-simple.el")
(load "~/rc/emacs-redshank.el")
(load "~/rc/emacs-hyperspec.el")
(load "~/rc/emacs-android.el")
(load "~/rc/emacs-ruby.el")
(load "~/rc/emacs-objective-c.el")
;;;----------------------------------------------------------------------------
(display-time-mode 1)
(defun dummy-bell () (message "bell"))
(setf ring-bell-function 'dummy-bell)

(setq backup-by-copying       t                      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.backups")) ; don't litter my fs tree
      delete-old-versions     t
      kept-new-versions       5
      kept-old-versions       3
      version-control         t)


;;;----------------------------------------------------------------------------
;;; Trustonic specific stuff
;;;----------------------------------------------------------------------------

(progn
  (dolist (extension '("h" "hh" "m" "mm" "md" "d"))
    (deletef auto-mode-alist (format "\\.%s$" extension) :test (function equal) :key (function car)))

  (appendf auto-mode-alist '(("\\.h$"  . c-mode)
                             ("\\.m$"  . objc-mode)
                             ("\\.hh$" . c++-mode)
                             ("\\.mm$" . objc-mode)
                             ("\\.md$" . text-mode)
                             ("\\.d$"  . makefile-mode)))
  (add-to-list 'auto-mode-alist '(".*/Apps/iOS/.*\\.\\(h\\|m\\|hh\\|mm\\)$" . objc-mode)))
(deletef auto-mode-alist "\\.rb$")
(appendf auto-mode-alist '(("\\.rb$" . ruby-mode)))


(require 'vc-svn)

(require 'tls)
(setf tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --insecure -p %p %h"
                    "gnutls-cli --insecure -p %p %h --protocols ssl3"))



(push "~/emacs/org-jira" load-path)
(require 'org-jira nil)
(setf jiralib-url "http://jira.trustonic.internal")

;; (require 'confluence)
;; (setq confluence-url "http://wiki.trustonic.internal/rpc/xmlrpc")

;; (when (require 'semantic nil t)
;;   (semantic-mode 1))

(require 'pjb-c-style)
(require 'pjb-objc-edit)
(require 'pjb-objc-ide)


(let ((tags-add-tables t))
  (setf tags-table-list '())
  (ignore-errors (visit-tags-table "~/src/Cocoa.etags"))
  ;; ;(ignore-errors (visit-tags-table "~/src/Ruby.etags"))
  ;; (ignore-errors (visit-tags-table "~/src/tbase.etags"))
  )


;;----------------------------------------------------------------------------
;; java

(require 'auto-complete nil t)
(defun java-meat ()
  (interactive)
  (when (fboundp 'auto-complete-mode)
    (auto-complete-mode 1))
  (setf tab-stop 4
        tab-width 4
        c-indent-level 4
        c-basic-offset 4
        c-tab-always-indent t))

(add-hook 'java-mode-hook 'java-meat)

(global-set-key (kbd "C-h 1") 'android-search-region)
(add-hook 'java-mode 'pjb-java-edit-meat)


(defparameter *android-tools-directory*
  (if (string= (hostname) "macbook-trustonic.local")
      "~/Library/Android"
      "~/opt"))
(push (expand-file-name (concat *android-tools-directory* "/sdk/tools/lib/")) load-path)
(when (require 'android nil t)
 (setf android-mode-sdk-dir (expand-file-name (concat *android-tools-directory* "sdk")))
 (require 'android-mode nil t)

 (defun gud-meat ()
   (interactive)
   (add-to-list 'gud-jdb-classpath (expand-file-name (concat *android-tools-directory* "sdk/platforms/android-23/android.jar"))))
 (add-hook 'gud-mode-hook 'gud-meat))

;; (require 'cedet)
;; (pushnew (expand-file-name "~/emacs/jdee/lisp") load-path)
;; (require 'jde)


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


;;----------------------------------------------------------------------------

(require 'erc-notify)
;; (desktop-read)

;;----------------------------------------------------------------------------

(require 'pjb-thi)
;; (set-shadow-map '(("~/src/OpticsPro-Mac-trunk/"                                               . "~/src/OpticsPro-Mac-shadow/")
;;                   ("/Volumes/Data/pbourguignon/src/OpticsPro-Mac-trunk/"                      . "/Volumes/Data/pbourguignon/src/OpticsPro-Mac-shadow/")
;;                   ("~/src/OpticsPro-Mac-branches-filmstripRefactor/"                          . "~/src/OpticsPro-Mac-shadow/")
;;                   ("/Volumes/Data/pbourguignon/src/OpticsPro-Mac-branches-filmstripRefactor/" . "/Volumes/Data/pbourguignon/src/OpticsPro-Mac-shadow/")))

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

;; (defun dxo-log-clean ()
;;   (interactive)
;;   (delete-matching-lines "not doing caf matching on unsupported image" (point-min) (point-max))
;;   (re-delete-lines-between (point-min) (point-max)
;;                            "\\(UpdateParameters\\|DOPQuickPreview\\|DOPThumbnailsPreloadingController\\.ToThumbnailCache\\).*{"
;;                            "^\t}\\()\\| error: \\)"))

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


(defun lua-mode-meat ()
  (interactive)
  (auto-complete-mode 1)
  ;; (setf outline-regexp ".*\\([[{(]\\|\\<\\(do\\|\\(?:functio\\|the\\)n\\)\\>\\)")
  ;; (outline-minor-mode 1)
  (hs-minor-mode 1))

(add-hook 'lua-mode-hook 'lua-mode-meat)

;; (find-library "outline-mode-easy-bindings")


(defvar *c-comment-regexp*)

(defun looking-at-c-comment ()
  (looking-at "[ \n\t]*/[*/]"))

(defun c-comment-block-to-line (comment)
  (if (and (string= "/*" (subseq comment 0 2))
           (string= "*/" (subseq comment (- (length comment) 2))))
      (concat "// " (mapconcat (function identity)
                               (split-string (subseq comment 2 (- (length comment) 2)) "\n")
                               "\n// "))
      comment))

(defvar *default-method-with-log-tag* "")

(defun objc-signatures-to-methods-with-log (start end &optional tag)
  (interactive "r")
  (goto-char start)
  (let ((tag (or tag *default-method-with-log-tag*)))
    (loop
      (let (comment signature)
        (if (looking-at-c-comment)
            (progn
              (forward-comment 1)
              (let ((endc (point)))
                (forward-comment -1)
                (let* ((startc (point)))
                  (goto-char endc)
                  (unless (setf signature (ignore-errors (pjb-objc-parser--parse-method-signature)))
                    (return))
                  (setf comment (c-comment-block-to-line
                                 (prog1 (buffer-substring startc endc)
                                   (delete-region startc endc)))))))
            (unless (setf signature (ignore-errors (pjb-objc-parser--parse-method-signature)))
              (return)))
        (when (looking-at " *;")
          (delete-region (match-beginning 0) (match-end 0)))
        (insert "{\n")
        (when comment (insert comment "\n"))
        (insert "#ifdef DEBUG\n")
        (insert (format "NSLog(@\"%s" tag))
        (loop with parameters = (pjb-objc-method-signature-parameters signature)
              for component in (pjb-objc-selector-components
                                (pjb-objc-method-signature-selector signature))
              for parameter = (pop parameters)
              do (insert (prin1-to-string component))
                 (when parameter (insert " %@ "))
              when parameter
                collect (pjb-objc-parameter-name parameter) into arguments
              finally (insert "\"")
                      (when arguments
                        (insert "," (mapconcat (function prin1-to-string)
                                               arguments
                                               ","))))
        (insert ");\n")
        (insert "#endif\n")
        (insert "}\n")
        (let ((end (point)))
          (backward-sexp)
          (indent-region (point) end)
          (forward-sexp))))))

(global-flycheck-mode)



;;;
;;; Trustonic TODO comments:
;;;
;;; Binding suggestion: (kbd "A-;") (kbd "H-;")  or (kbd "C-c t ;")
;;; (no predefined emacs key binding use Alt- or Hyper- (and no known usual emacs package does),
;;; and C-c <letter> is the standard prefix for user defined bindings, t = trustonic, ; = comment)
;;;
;;; (global-set-key (kbd "C-c t ;") 'trustonic-insert-todo-comment)
;;; (global-set-key (kbd "H-;")     'trustonic-insert-todo-comment)
;;; (setf *trustonic-user-name* "PreNom01")


(defvar *trustonic-user-name* nil
  "Set it to your Trustonic user name if it is different from the variable `user-login-name'.")

(defun trustonic-insert-todo-comment (start end)
  "Insert a Trustonic TODO comment.
If the region is set, then use it as comment text.
START is the start of the region.
END is the end of the region."
  (interactive "*r")
  (let* ((user (or *trustonic-user-name* user-login-name))
         (date (calendar-current-date))
         (head (format "TODO-[%04d-%02d-%02d]-[%s] "
                       (third date) (first date) (second date)
                       user))
         (endm   (make-marker)))
    (unwind-protect
         (progn
           (if (and mark-active start end)
               (progn
                 (goto-char start)
                 (set-marker endm end)
                 (insert head))
               (progn
                 (setf start (point))
                 (insert head)
                 (set-marker endm (point))))
           (set-mark start)
           (goto-char endm)
           (comment-region start endm)
           (goto-char endm))
      (set-marker endm nil))))


(global-set-key (kbd "C-c t ;") 'trustonic-insert-todo-comment)
(global-set-key (kbd "H-;")     'trustonic-insert-todo-comment)
(setf *trustonic-user-name* "pasbou01")


;;;
;;; Trailing Whitespaces
;;;
;;; Let's use a find-file-hook to check and alert for trailing whitespaces upon opening a file,
;;; and a before-save-hook to delete the trailing whitespaces upons saving.
;;;
;;; When the alert is given, the file should be touched and saved, the
;;; whitespace removal should be commited, and only then the file can
;;; be edited for the current task.

(defun trustonic-check-trailing-whitespace-meat ()
  "Check for trailing whitespaces in source files."
  (interactive)
  (when (vc-backend (buffer-file-name)) ; We don't care about files that are not in SVN.
    (save-excursion
     (goto-char (point-min))
     (when (re-search-forward " $"nil t)
       (message-box "*** WARNING: There are Trailing White Spaces in %s ***"
                    (buffer-name))))))

(defun pjb-alert-trailing-whitespace ()
  (set-background-color "#600c0c")
  (read-char "ALERT! Trailing whitespaces!     (Type the Any key)")
  (set-palette *current-palette*))

(defun pjb-check-trailing-whitespace-meat ()
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (when (re-search-forward " $"nil t)
     (pjb-alert-trailing-whitespace))))

(add-hook 'find-file-hook   'pjb-check-trailing-whitespace-meat)
(add-hook 'before-save-hook 'delete-trailing-whitespace)






(require 'confluence)
(setf confluence-url "http://wiki.trustonic.internal/confluence2/rpc/xmlrpc")


(load "~/rc/emacs-epilog.el")
(provide 'emacs-trustonic)
;;;; THE END ;;;;
