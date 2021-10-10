;;; emacs-pjb -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:


(load "~/rc/emacs-common.el") ; defines .EMACS

(.EMACS "~/rc/emacs-pjb.el %s" "Pascal J. Bourguignon's emacs startup file.")
(require 'cc-mode)
(require 'vc)
(require 'vc-hooks)

;;;----------------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------------

(.EMACS "custom faces")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(3dbutton ((t (:background "grey33" :foreground "grey11" :box (:line-width 2 :color "white" :style released-button)))))
 '(3dbutton-highlighted ((t (:inherit 3dbutton :background "grey66" :foreground "grey11" :box (:line-width 2 :color "grey75" :style pressed-button)))))
 '(android-mode-debug-face ((((class color) (min-colors 89)) (:foreground "#859900"))))
 '(android-mode-info-face ((((class color) (min-colors 89)) (:foreground "#839496"))))
 '(android-mode-verbose-face ((((class color) (min-colors 89)) (:foreground "#586e75"))))
 '(android-mode-warning-face ((((class color) (min-colors 89)) (:foreground "#b58900"))))
 '(column-marker-1 ((t (:background "gray22"))))
 '(column-marker-1-face ((t (:background "AntiqueWhite"))))
 '(custom-comment ((((class grayscale color) (background dark)) (:background "light green"))))
 '(custom-group-tag ((((class color) (min-colors 89)) (:inherit variable-pitch :foreground "#268bd2" :height 1.2))))
 '(custom-variable-tag ((((class color) (min-colors 89)) (:inherit variable-pitch :foreground "#2aa198" :height 1.2))))
 '(diff-nonexistent ((t (:background "grey11" :foreground "light green"))))
 '(ediff-even-diff-A ((t (:background "#073642"))))
 '(ediff-odd-diff-A ((t (:background "#073642"))))
 '(ediff-odd-diff-B ((t (:background "#073642"))))
 '(erc-action-face ((t (:foreground "DodgerBlue3" :weight bold))))
 '(erc-default-face ((t (:foreground "turquoise3"))))
 '(erc-fool-face ((((class color) (min-colors 89)) (:inherit erc-default-face))))
 '(erc-input-face ((t (:foreground "spring green"))))
 '(erc-notice-face ((t (:foreground "salmon4"))))
 '(erc-pal-face ((t (:foreground "purple2" :weight bold))))
 '(erc-timestamp-face ((((class color) (min-colors 89)) (:foreground "#859900"))))
 '(fg:erc-color-face12 ((t (:foreground "cyan" :weight bold))))
 '(fg:erc-color-face2 ((t (:foreground "LightBlue1"))))
 '(fill-column-indicator ((((class color) (min-colors 89)) :foreground "#073642" :weight semilight)))
 '(font-lock-cl-function-face ((t (:foreground "DodgerBlue" :weight bold))))
 '(font-lock-cl-standard-generic-function-face ((t (:foreground "turquoise" :weight bold))))
 '(font-lock-comment-delimiter-face ((((class color) (min-colors 89)) (:foreground "#586e75" :slant normal))))
 '(font-lock-comment-face ((t (:foreground "DarkOliveGreen1"))))
 '(font-lock-doc-face ((((class color) (min-colors 89)) (:foreground "#2aa198" :slant normal))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#2aa198"))))
 '(font-lock-warning-face ((t (:inherit error :foreground "red" :weight bold))))
 '(frdg-yellow-face ((t (:foreground "yellow"))) t)
 '(fringe ((t (:background "black" :foreground "#93a1a1"))))
 '(gnus-cite-1 ((((class color) (min-colors 89)) (:foreground "#268bd2"))))
 '(gnus-cite-10 ((((class color) (min-colors 89)) (:foreground "#b58900"))))
 '(gnus-cite-11 ((((class color) (min-colors 89)) (:foreground "#b58900"))))
 '(gnus-cite-2 ((((class color) (min-colors 89)) (:foreground "#268bd2"))))
 '(gnus-cite-3 ((((class color) (min-colors 89)) (:foreground "#268bd2"))))
 '(gnus-cite-4 ((((class color) (min-colors 89)) (:foreground "#859900"))))
 '(gnus-cite-5 ((((class color) (min-colors 89)) (:foreground "#859900"))))
 '(gnus-cite-6 ((((class color) (min-colors 89)) (:foreground "#859900"))))
 '(gnus-cite-7 ((((class color) (min-colors 89)) (:foreground "#dc322f"))))
 '(gnus-cite-8 ((((class color) (min-colors 89)) (:foreground "#dc322f"))))
 '(gnus-cite-9 ((((class color) (min-colors 89)) (:foreground "#dc322f"))))
 '(gnus-group-mail-3 ((((class color) (min-colors 89)) (:weight bold :inherit gnus-group-mail-3-empty))))
 '(gnus-summary-normal-read ((((class color) (min-colors 89)) (:foreground "#859900"))))
 '(gnus-summary-selected ((((class color) (min-colors 89)) (:foreground "#b58900" :weight bold))))
 '(jde-java-font-lock-javadoc-face ((t (:inherit font-lock-doc-face :foreground "pink"))))
 '(jde-java-font-lock-link-face ((t (:foreground "cyan" :underline t))))
 '(match ((((class color) (min-colors 89)) (:background "#073642" :foreground "#93a1a1" :weight bold))))
 '(message-cited-text-1 ((((class color) (min-colors 89)) (:foreground "#586e75"))))
 '(message-header-xheader ((((class color) (min-colors 89)) (:foreground "#2aa198"))))
 '(message-separator ((((class color) (min-colors 89)) (:foreground "#586e75" :slant italic))))
 '(mmm-default-submode-face ((t (:foreground "cyan"))))
 '(mode-line ((((class color) (min-colors 89)) (:inverse-video unspecified :overline #1="#073642" :underline "#284b54" :foreground "#839496" :background #1# :box (:line-width 1 :color #1# :style unspecified)))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:inverse-video unspecified :overline "#073642" :underline "#284b54" :foreground "#586e75" :background #1="#002b36" :box (:line-width 1 :color #1# :style unspecified)))))
 '(org-agenda-dimmed-todo-face ((((class color) (min-colors 89)) (:foreground "#586e75"))))
 '(org-done ((((class color) (min-colors 89)) (:weight bold :foreground "#859900"))))
 '(org-headline-done ((((class color) (min-colors 89)) (:foreground "#859900"))))
 '(read-only-face ((t (:background "gray30"))) t)
 '(rst-level-1-face ((t (:background "grey20" :height 1.9))) t)
 '(rst-level-2-face ((t (:background "grey20" :height 1.7))) t)
 '(rst-level-3-face ((t (:background "grey20" :height 1.4))) t)
 '(rst-level-4-face ((t (:background "grey20" :height 1.2))) t)
 '(rst-level-5-face ((t (:background "grey20" :height 1.1 :weight bold))) t)
 '(rst-level-6-face ((t (:background "grey20" :height 1.0 :weight bold))) t)
 '(semantic-unmatched-syntax-face ((((class color) (background dark)) nil)))
 '(slime-repl-output-face ((t (:foreground "yellow green")))))


(.EMACS "custom variables")
(custom-set-variables
 '(ad-redefinition-action 'accept)
 '(ansi-color-names-vector ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auth-source-save-behavior t)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-image-file-mode t)
 '(auto-save-interval 2500)
 '(auto-save-timeout 60)
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-by-copying-when-mismatch t)
 '(backup-by-copying-when-privileged-mismatch 1000)
 '(backup-directory-alist nil)
 '(backup-file-version-format "%03d")
 '(bash-completion-initial-timeout 0.7)
 '(bash-completion-process-timeout 0.3)
 '(bcc-user-mail-address "pjb@informatimago.com" t)
 '(blink-matching-paren t)
 '(boxquote-bottom-corner "+")
 '(boxquote-top-and-tail "----------------------------------------------------------------------------")
 '(boxquote-top-corner "+")
 '(browse-url-browser-function 'w3m-browse-url)
 '(browse-url-new-window-flag nil)
 '(browse-url-secondary-browser-function 'browse-url-default-macosx-browser)
 '(c-argdecl-indent 4 t)
 '(c-auto-newline nil t)
 '(c-backslash-column 'set-from-style)
 '(c-backslash-max-column 'set-from-style)
 '(c-basic-offset 'set-from-style)
 '(c-block-comment-prefix 'set-from-style)
 '(c-brace-imaginary-offset 0 t)
 '(c-brace-offset 0 t)
 '(c-cleanup-list 'set-from-style)
 '(c-comment-continuation-stars "" t)
 '(c-comment-only-line-offset 'set-from-style)
 '(c-comment-prefix-regexp 'set-from-style)
 '(c-continued-brace-offset 0 t)
 '(c-continued-statement-offset 4 t)
 '(c-default-style "pjb")
 '(c-doc-comment-style 'set-from-style)
 '(c-echo-syntactic-information-p t)
 '(c-hanging-braces-alist 'set-from-style)
 '(c-hanging-colons-alist 'set-from-style)
 '(c-hanging-comment-ender-p nil t)
 '(c-hanging-comment-starter-p nil t)
 '(c-hanging-semi&comma-criteria 'set-from-style)
 '(c-indent-comment-alist 'set-from-style)
 '(c-indent-comments-syntactically-p 'set-from-style)
 '(c-indent-level 4 t)
 '(c-label-minimum-indentation 2)
 '(c-label-offset -4 t)
 '(c-macro-shrink-window-flag t)
 '(c-offsets-alist 'nil)
 '(c-special-indent-hook 'nil)
 '(c-tab-always-indent t)
 '(calendar-christian-all-holidays-flag t)
 '(calendar-date-display-form '((if dayname (format "%4s-%2s-%2s  %-9s %2s %-9s" year month day monthname day dayname) (format "%4s-%2s-%2s  %-9s %2s %-9s" year month day monthname day ""))))
 '(calendar-date-style 'iso)
 '(calendar-hebrew-all-holidays-flag nil)
 '(calendar-mark-holidays-flag t)
 '(calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")) t)
 '(calendar-view-holidays-initially-flag t)
 '(calendar-week-start-day 1)
 '(canlock-password "87f2de14edf31a9ebb2c5b5619c818a49937a47b")
 '(case-fold-search t)
 '(chess-default-engine '(chess-gnuchess chess-crafty chess-phalanx) t)
 '(chess-images-directory "/usr/share/pixmaps/chess/xboard" t)
 '(chess-sound-directory "/usr/share/sounds/chess" t)
 '(comint-dynamic-complete-functions nil t)
 '(comint-process-echoes nil)
 '(comment-empty-lines t)
 '(comment-force-also-empty-lines t)
 '(compilation-error-regexp-alist '(absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek gradle-kotlin iar ibm irix java jikes-file maven jikes-line clang-include gcc-include ruby-Test::Unit gmake gnu lcc makepp mips-1 mips-2 omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line ghc))
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(current-language-environment "UTF-8")
 '(default-input-method nil)
 '(default-major-mode 'text-mode t)
 '(delete-old-versions t)
 '(delete-selection-mode nil)
 '(dired-kept-versions 4)
 '(display-fill-column-indicator t)
 '(display-fill-column-indicator-character 32)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ecb-auto-activate nil)
 '(ecb-cedet-url "http://sourceforge.net/project/showfiles.php?group_id=17484")
 '(ecb-options-version "2.32")
 '(emms-info-functions '(emms-info-id3v2 emms-info-ogginfo emms-info-mp3info))
 '(emms-info-mp3info-coding-system 'iso-8859-1)
 '(emms-lyrics-display-on-minibuffer t)
 '(emms-player-started-hook '(emms-show))
 '(emms-show-format "NP %s")
 '(emms-source-file-default-directory "/movies/sound/music/")
 '(emms-source-playlist-formats '(native pls m3u))
 '(enable-recursive-minibuffers t)
 '(erc-auto-query 'window)
 '(erc-autojoin-channels-alist '(("irc.libera.chat" "#lisp" "##lisp" "#commonlisp" "##lispm" "#clschool" "#lisp-es" "#abcl" "#ccl" "#ecl" "#sicl" "#clim" "#clcs" "#slime" "#lispcafe" "#lispweb" "#lispgames" "#common-lisp.net" "#emacs" "#hn" "#space" "##space") ("freenode.org" "##Freelancers" "##binutils" "##cinema" "##coders" "##coding" "##france" "#lisp" "##lisp" "##lispm" "##sci-fi" "##smalltalk" "##tesla" "##teslamotors" "##teslamotors-staging" "##trains" "##ufo" "##workingset" "#abcl" "#bourguinux" "#ccl" "#cl" "#cl-bodge" "#clcs" "#clim" "#clschool" "#clus" "#cobol" "#cpp" "#cuda" "#ecl" "#emacs" "#emacsconf" "#emacsfr-off" "#haskell" "#haskell-fr" "#hn" "#iphonedev" "#iphonedev-chat" "#lisp-es" "#lispcafe" "#lispgames" "#lispweb" "#lispx" "#macintosh" "#macos9lives" "#qlisp" "#sicl" "#sicp" "#slime" "#space" "#swift-lang" "#synthesizers" "#tesla") ("disabled.freenode.org" "#macports") ("esper.net" "#SpaceX") ("irc.sbde.fr" "#laboite" "#sbde") ("irc.oftc.net" "#uml")))
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing 'ident)
 '(erc-away-timestamp-format "<%H:%M:%S>")
 '(erc-beep-match-types '(current-nick keyword pal))
 '(erc-echo-notices-in-current-buffer t)
 '(erc-echo-timestamps nil)
 '(erc-email-userid "pjb")
 '(erc-encoding-coding-alist '(("#scheme-es" . iso-8859-15)))
 '(erc-fill-column 90)
 '(erc-fill-function 'erc-fill-variable)
 '(erc-fill-mode -1)
 '(erc-fill-prefix "\"\"")
 '(erc-fill-static-center 0)
 '(erc-fill-variable-maximum-indentation 0)
 '(erc-hide-list 'nil)
 '(erc-identd-mode t)
 '(erc-ignore-list '("ad37e918" "173.55.233.24" "Abby26" "^snv1?!~Adium@" "no-defun-allowed" "theemacsshibe" "paule32" "frickfacejr" "fe\\[nl]ix"))
 '(erc-ignore-per-channel-alist '(("#lisp" . "^\\(?:A\\(?:aliyah\\|b\\(?:b\\(?:ey\\|i\\(?:e\\|gail\\)\\|y\\)\\|igail\\)\\|d\\(?:dison\\|ri\\(?:an\\(?:n?a\\)?\\|enne\\)\\)\\|gath[ae]\\|i\\(?:leen\\|mee\\|sha\\)\\|ja\\|l\\(?:a\\(?:[iny]?na\\)\\|e\\(?:ah\\|cia\\|jandra\\|na\\|s\\(?:\\(?:h\\|sandr\\)a\\)\\|x\\(?:a\\(?:ndr\\(?:[ei]?a\\)\\)?\\|i[as]\\|us\\)?\\)\\|i\\(?:c\\(?:e\\|ia\\)\\|na\\|s\\(?:a\\|ha\\|on\\|sa\\)\\|yah\\)?\\|l\\(?:i\\(?:e\\|son\\)\\|ys\\(?:on\\|sa\\)\\)\\|ma\\|ondra\\|y\\(?:cia\\|s\\(?:a\\|ha\\|ia\\|on\\|sa\\)\\)\\)\\|m\\(?:an\\(?:da\\|i\\)\\|ber\\|elia\\|y\\)\\|n\\(?:a\\(?:bel\\|is\\|stasia\\)\\|drea\\|g\\(?:el\\(?:a\\|i\\(?:ca\\|na\\|que\\)\\)?\\|ie\\)\\|i\\(?:\\(?:ss\\|[kt]\\)a\\)\\|jelica\\|n\\(?:amarie\\|ette\\|i\\(?:e\\|ka\\)\\|marie\\|[ae]\\)\\|to\\(?:inette\\|nia\\)\\|[an]\\)\\|pril\\|r\\(?:aceli\\|i\\(?:an\\(?:n?a\\)\\|el\\(?:le\\)?\\)\\|lene\\)\\|s\\(?:h\\(?:a\\(?:nti\\)?\\|ely\\|l\\(?:e\\(?:igh\\|[ey]\\)\\|ie\\|ynn?\\|[iy]\\)\\|ton\\)\\|ia\\|pen\\|trid\\)\\|thena\\|u\\(?:bre[ey]\\|dr\\(?:a\\|ey\\)\\|rora\\|tumn\\)\\|v\\(?:a\\|ery\\)\\|y\\(?:\\(?:ann?\\|l\\)a\\)\\)\\|B\\(?:a\\(?:by\\|ile[ey]\\|rbara\\|ylee\\)\\|e\\(?:atri\\(?:ce\\|z\\)\\|linda\\|r\\(?:\\(?:enic\\|nadett\\)e\\)\\|t\\(?:h\\(?:any\\)?\\|[st]y\\)\\|verly\\)\\|i\\(?:anca\\|llie\\)\\|la\\(?:ir\\|nca\\)\\|o\\(?:bbie?\\|nnie\\)\\|r\\(?:and\\(?:ie\\|[iy]\\)\\|e\\(?:a\\(?:n\\(?:n[ae]\\|[an]\\)\\)?\\|\\(?:n[dn]\\|onn\\)a\\)\\|i\\(?:a\\(?:n\\(?:a\\|n[ae]\\)\\)?\\|dget\\(?:te\\)?\\|elle\\|t\\(?:any\\|n\\(?:ey\\|i\\)\\|t\\(?:an\\(?:ie\\|[iy]\\)\\|n\\(?:ey\\|i\\)\\)\\)\\)\\|ook\\(?:e\\|lyn\\)?\\|yan\\(?:n?a\\)\\)\\)\\|C\\(?:a\\(?:itl\\(?:[iy]n\\)\\|llie\\|m\\(?:eron\\|ille\\)\\|nd\\(?:[ai]ce\\)\\|r\\(?:a\\|i\\(?:\\(?:n\\|ss\\)a\\)\\|l\\(?:e[ey]\\|ie\\|[aiy]\\)\\|men\\|ol\\(?:in[ae]\\|yn\\)?\\|rie\\|son\\)\\|s\\(?:andra\\|ey\\|s\\(?:andra\\|i\\(?:dy\\|e\\)\\)\\)\\|t\\(?:alina\\|herine\\)\\|yla\\)\\|e\\(?:c\\(?:[ei]lia\\)\\|l\\(?:este\\|i\\(?:a\\|n[ae]\\)\\)\\)\\|h\\(?:a\\(?:n\\(?:dler\\|\\(?:e\\|t[ae]\\)l\\)\\|r\\(?:ity\\|l\\(?:\\(?:en\\|ott\\)e\\)\\)\\|sity\\|ya\\)\\|e\\(?:ls\\(?:e[ay]\\|ie?\\)\\|ryl\\|y\\(?:[ae]nne\\)\\)\\|ina\\|loe\\|rist\\(?:en\\|i\\(?:ana?\\|n[ae]\\|[en]\\)\\|[ay]\\)\\)\\|i\\(?:ara\\|er\\(?:r?a\\)\\|ndy\\)\\|la\\(?:ire\\|r\\(?:issa\\|[ae]\\)\\|udia\\)\\|o\\(?:lleen\\|n\\(?:\\(?:ni\\|stanc\\)e\\)\\|r\\(?:ey\\|in\\(?:a\\|ne\\)\\|tney\\|[ai]\\)\\|urtney\\)\\|r\\(?:ist\\(?:al\\|ina\\)\\|ystal\\)\\|ynthia\\)\\|D\\(?:a\\(?:isy\\|kota\\|l\\(?:ia\\|las\\)\\|maris\\|n\\(?:a\\|i\\(?:ca\\|el\\(?:a\\|l[ae]\\)\\)\\)\\|phne\\|r\\(?:a\\|by\\|cy\\|ian\\|lene\\)\\|wn\\|yna\\)\\|e\\(?:an\\(?:n?a\\)\\|b\\(?:bie\\|orah\\|ra\\)\\|ja\\|laney\\|mi\\|nise\\|s\\(?:ir\\(?:[ae]e\\)\\|tin\\(?:e[ey]\\|[iy]\\)\\)\\|v\\(?:[aioy]n\\)\\)\\|i\\(?:a\\(?:mond\\|n\\(?:na\\|[ae]\\)\\)\\|na\\)\\|o\\(?:m\\(?:\\(?:ini?\\|oni\\)que\\)\\|nna\\|r\\(?:is\\|othy\\)\\)\\|rew\\|ulce\\)\\|E\\(?:bon[iy]\\|d\\(?:en\\|ith\\)\\|ileen\\|l\\(?:ain[ae]\\|e\\(?:anor\\|na\\)\\|i\\(?:ana\\|s\\(?:abeth\\|[hs]a\\|[ae]\\)\\|za\\(?:beth\\)?\\)\\|l\\(?:a\\|en\\|ie\\)\\|sa\\|ys\\(?:e\\|sa\\)\\)\\|m\\(?:erald\\|il\\(?:ee\\|i[ae]\\|y\\)\\|ma\\)\\|ri\\(?:c\\(?:k?a\\)\\|ka\\|n\\)\\|s\\(?:meralda\\|sence\\|t\\(?:efania\\|her\\)\\)\\|unice\\|v\\(?:a\\|elyn\\)\\)\\|F\\(?:a\\(?:biola\\|ith\\|tima\\)\\|elicia\\|iona\\|ranc\\(?:es\\(?:ca\\)?\\|hes\\(?:[ck]a\\)\\)\\)\\|G\\(?:abriel\\(?:a\\|l[ae]\\)?\\|e\\(?:ne\\(?:sis\\|vieve\\)\\|orgi\\(?:n?a\\)\\)\\|i\\(?:anna\\|llian\\|na\\|ovanna\\|selle\\)\\|l\\(?:adys\\|oria\\)\\|r\\(?:ac\\(?:e\\|iela\\)\\|etchen\\|iselda\\)\\|uadalupe\\|wendolyn\\)\\|H\\(?:a\\(?:ile[ey]\\|l\\(?:e\\(?:igh\\|y\\)\\|ie?\\|l\\(?:i?e\\)\\)\\|nnah?\\|rley\\|yl\\(?:e[ey]\\|ie\\)\\|zel\\)\\|e\\(?:a\\(?:ther\\|ven\\)\\|idi\\|lena?\\)\\|il\\(?:l?ary\\)\\|o\\(?:ll\\(?:ie\\|y\\)\\|pe\\)\\|unter\\)\\|I\\(?:esha\\|liana\\|mani\\|n\\(?:dia\\|fant\\|grid\\)\\|r\\(?:ene\\|is\\|ma\\)\\|sa\\(?:bel\\(?:l[ae]\\)?\\|mar\\)\\|tzel\\|v\\(?:ette\\|y\\)\\)\\|J\\(?:a\\(?:c\\(?:k\\(?:ie\\|lyn\\)\\|lyn\\|quel\\(?:ine\\|yn\\)\\)\\|d[ae]\\|im\\(?:i?e\\)\\|lisa\\|mi\\(?:e\\|la\\)?\\|n\\(?:a[ey]\\|e\\(?:lle\\|ssa\\|t\\(?:te\\)?\\)\\|i\\(?:[cn]?e\\)\\|[ae]\\)\\|queline\\|smine?\\|y\\(?:la\\|me\\)\\|zm\\(?:ine?\\|yn\\)\\)\\|e\\(?:an\\(?:ette\\)?\\|n\\(?:a\\|ifer\\|n\\(?:i\\(?:e\\|fer\\)\\|[ay]\\)\\)\\|rrica\\|ss\\(?:enia\\|i\\(?:ca\\|e\\|ka\\)\\|[ei]\\)\\)\\|ill\\(?:ian\\)?\\|o\\(?:an\\(?:n[ae]\\|[an]\\)?\\|celyn\\|die?\\|elle\\|hanna\\|lene\\|rd\\(?:[ay]n\\)\\|s\\(?:e\\(?:lyn\\|phine\\)\\|ie\\)\\|y\\(?:ce\\)?\\)\\|u\\(?:an\\(?:\\(?:it\\)?a\\)\\|d\\(?:ith\\|y\\)\\|li\\(?:an\\(?:a\\|n[ae]\\)\\|et\\|ssa\\|[ae]\\)\\|sti\\(?:ce\\|n[ae]\\)\\)\\)\\|K\\(?:a\\(?:c\\(?:ey\\|ie?\\)\\|ela\\|i\\(?:l\\(?:a\\|e[ey]\\|yn\\)\\|tl\\(?:\\(?:yn\\|[iy]\\)n\\)\\)\\|l\\(?:e\\(?:igh\\|y\\)\\|ie\\|lie\\|yn\\|[ai]\\)\\|r\\(?:en\\|i\\(?:\\(?:n\\|ss\\)a\\)\\|l\\(?:e[ey]\\|ie\\|[aiy]\\)\\|[ai]\\)\\|s\\(?:andra\\|ey\\|s\\(?:andra\\|i\\(?:dy\\|e\\)\\)\\)\\|t\\(?:arina\\|e\\(?:l\\(?:\\(?:yn\\|[iy]\\)n\\)\\|rina\\)\\|h\\(?:arine\\|er\\(?:ine\\|yn\\)\\|leen\\|r\\(?:ine\\|yn\\)\\|y\\)\\|ie\\|l\\(?:\\(?:yn\\|[iy]\\)n\\)\\|rina\\|[ey]\\)\\|y\\(?:cee\\|l\\(?:e\\(?:igh\\|[ey]\\)\\|i[en]\\|ynn?\\|[ai]\\)\\)\\)\\|e\\(?:ely\\|i\\(?:\\(?:l\\|shl?\\)a\\)\\|l\\(?:cie\\|l\\(?:ey\\|ie\\|[iy]\\)\\|s\\(?:e[ay]\\|ie?\\)\\)\\|n\\(?:d\\(?:all?\\|ra\\)\\|ia\\|nedy\\|ya\\)\\|r\\(?:i\\|r[iy]\\)\\)\\|hadijah\\|i\\(?:a\\(?:\\(?:nn\\|[nr]\\)a\\)?\\|er\\(?:a\\|ra\\|sten\\)\\|ley\\|mberl\\(?:e[ey]\\|y\\)\\|r\\(?:a\\|st\\(?:en\\|i[en]\\)\\)\\)\\|o\\(?:r\\(?:i\\|tney\\)\\|urtney\\)\\|r\\(?:ist\\(?:al\\|en\\|i\\(?:an\\|n[ae]\\|[en]\\)\\|yn\\|[aiy]\\)\\|ystal?\\)\\|y\\(?:l\\(?:a\\|[ei]e\\)\\|ra\\)\\)\\|L\\(?:a\\(?:c\\(?:ey\\|ie\\|y\\)\\|keisha\\|na\\|r\\(?:\\(?:iss\\)?a\\)\\|t\\(?:\\(?:ash\\|ish\\|oy\\)a\\)\\|ur\\(?:a\\|e[ln]\\|ie\\|yn\\)\\|yla\\)\\|e\\(?:a\\(?:h\\|n\\(?:dra\\|n[ae]?\\)\\)?\\|eann\\|i\\(?:gh\\|la\\)\\|na\\|sl\\(?:ey\\|ie\\|y\\)\\|ticia\\|x\\(?:ie?\\|us\\)\\)\\|i\\(?:ana\\|dia\\|l\\(?:iana\\|lian\\|y\\)\\|nd\\(?:a\\|s\\(?:[ae]y\\)\\)\\|sa\\|z\\(?:a\\|beth\\|et\\(?:h\\|te\\)\\)\\)\\|o\\(?:gan\\|r\\(?:ena?\\|i\\|raine\\)\\|urdes\\)\\|u\\(?:c\\(?:ero\\|ia\\|y\\)\\|z\\)\\|y\\(?:dia\\|n\\(?:dsey\\|ette\\|n\\)\\)\\)\\|M\\(?:a\\(?:c\\(?:ey\\|ie\\|kenzie\\|y\\)\\|d\\(?:alyn\\|dison\\|el\\(?:eine\\|ine\\|yn\\)\\|ison\\)\\|egan\\|g\\(?:dalena\\|gie\\)\\|ira\\|k\\(?:ayla\\|en\\(?:na\\|zie\\)\\)\\|l\\(?:ia\\|lory\\)\\|ndy\\|r\\(?:anda\\|cella\\|gar\\(?:et\\|ita\\)\\|i\\(?:a\\(?:na\\|[hm]\\)\\|bel\\|cela\\|ela?\\|lyn\\|na\\|s\\(?:a\\|ela\\|ol\\|sa\\)\\|tza\\|[ae]\\)\\|le\\(?:e\\|n[ae]\\)\\|t\\(?:\\(?:h\\|in\\)a\\)\\|[ay]\\)\\|ur\\(?:a\\|een\\)\\|y\\(?:r?a\\)\\)\\|ck\\(?:ayla\\|en\\(?:na\\|zie\\)\\)\\|e\\(?:ag\\(?:h?an\\)\\|g\\(?:h?an\\)\\|l\\(?:anie\\|i\\(?:\\(?:nd\\|ss\\|[ns]\\)a\\)\\|ody\\)\\|r\\(?:anda\\|cedes\\|edith\\)\\)\\|i\\(?:a\\|c\\(?:a\\(?:ela\\|h\\)\\|h\\(?:aela\\|el\\(?:l?e\\)\\)\\)\\|ka\\(?:[ey]?la\\)\\|ndy\\|r\\(?:a\\(?:cle\\|nda\\)\\|eya\\|iam\\)\\|sty\\)\\|o\\(?:ll\\(?:ie\\|y\\)\\|n\\(?:i\\(?:ca\\|ka\\|que\\)\\|tana\\)\\|r\\(?:gan\\|iah\\)\\)\\|y\\(?:\\(?:r\\(?:and\\)?\\)?a\\)\\)\\|N\\(?:a\\(?:di\\(?:a\\|ne\\)\\|ncy\\|omi\\|t\\(?:a\\(?:l\\(?:i[ae]\\|y\\)\\|sha\\)\\|halie\\)\\|yeli\\)\\|i\\(?:a\\|c\\(?:\\(?:hol\\|ol\\(?:ett\\)?\\)e\\)\\|k\\(?:ita\\|ki\\|ole\\)\\|na\\)\\|o\\(?:e\\(?:l\\(?:le\\)?\\|mi\\)\\|r\\(?:m?a\\)\\)\\)\\|O\\(?:\\(?:ctavi\\|l\\(?:g\\|ivi\\)\\)a\\)\\|P\\(?:a\\(?:ige\\|loma\\|mela\\|ola\\|ris\\|tric\\(?:e\\|ia\\)\\|ul\\(?:a\\|in[ae]\\)\\|yton\\)\\|e\\(?:rla\\|yton\\)\\|hoebe\\|r\\(?:ecious\\|i\\(?:ncess\\|scilla\\)\\)\\)\\|R\\(?:a\\(?:ch\\(?:ael\\|e\\(?:al\\|l\\(?:le\\)?\\)\\)\\|ndi\\|quel\\|ven\\)\\|e\\(?:agan\\|be\\(?:c\\(?:c?a\\)\\|kah\\)\\|g\\(?:an\\|ina\\)\\|ina\\|nee\\|yna\\)\\|h\\(?:iannon\\|onda\\)\\|i\\(?:kki\\|ley\\|ta\\)\\|o\\(?:b\\(?:[iy]n\\)\\|c\\(?:helle\\|io\\)\\|s\\(?:emary\\|[ae]\\)\\|xan\\(?:a\\|ne\\)\\)\\|u\\(?:by\\|th\\)\\|y\\(?:an\\|lee\\)\\)\\|S\\(?:a\\(?:brina\\|d\\(?:i?e\\)\\|ge\\|l\\(?:ina\\|ly\\)\\|mantha\\|nd\\(?:ra\\|y\\)\\|r\\(?:a[hi]?\\|ina\\)\\|sha\\|van\\(?:ah\\|nah?\\)\\)\\|carlett\\|e\\(?:\\(?:l[ei]\\|re\\)na\\)\\|h\\(?:a\\(?:ina\\|kira\\|n\\(?:a\\|i\\(?:a\\|ce\\|qua\\)\\|n\\(?:a\\|on\\)\\|tel\\)\\|ron\\|\\(?:un\\|wn\\|y[ln]\\)a\\)\\|e\\(?:a\\|ena\\|ila\\|l\\(?:b\\(?:ie\\|[iy]\\)\\|ly\\)\\|rry\\)\\|irley\\|yanne\\)\\|i\\(?:dney\\|erra\\|lvia\\|mone\\)\\|ky\\(?:e\\|l\\(?:[ae]r\\)\\)\\|o\\(?:fia\\|n\\(?:[iy]a\\)\\|phi[ae]\\)\\|t\\(?:ac\\(?:ey\\|ie\\|[iy]\\)\\|e\\(?:fanie\\|phan\\(?:ie\\|y\\)\\|vie\\)\\|ormy\\)\\|u\\(?:mmer\\|san\\(?:n?a\\)?\\|zanne\\)\\|y\\(?:dn\\(?:e[ey]\\|ie\\)\\|lvia\\)\\)\\|T\\(?:a\\(?:b\\(?:[ai]tha\\)\\|lia\\|m\\(?:ara\\|my\\)\\|n\\(?:\\(?:ish\\|[iy]\\)a\\)\\|r\\(?:a\\|yn\\)\\|sha\\|t\\(?:iana\\|um\\|yana\\)\\|yl\\(?:[eo]r\\)\\)\\|e\\(?:r\\(?:esa\\|r[ai]\\)\\|ssa?\\)\\|h\\(?:\\(?:ali\\|eres\\)a\\)\\|i\\(?:a\\(?:\\(?:nn\\|[nr]\\)a\\)?\\|er\\(?:r?a\\)\\|ffan\\(?:ie\\|[iy]\\)\\|na\\)\\|o\\(?:n\\(?:i\\|ya\\)\\|ri\\)\\|r\\(?:ac\\(?:ey\\|[iy]\\)\\|i\\(?:cia\\|nity\\|s\\(?:[ht]a\\)\\)\\)\\|y\\(?:esha\\|ler\\|ra\\)\\)\\|V\\(?:a\\(?:leri[ae]\\|nes\\(?:s?a\\)\\)\\|eronica\\|i\\(?:ctoria\\|rginia\\|viana?\\)\\)\\|W\\(?:\\(?:end\\|hit\\(?:[ln]e\\)\\)y\\)\\|Xiomara\\|Y\\(?:a\\(?:dira\\|jaira\\|ritza\\|sm\\(?:een\\|ine?\\)\\|zmin\\)\\|es\\(?:s?enia\\)\\|olanda\\|v\\(?:\\(?:ett\\|onn\\)e\\)\\)\\|Zoey?\\)[0-9]+$") ("#scheme" . "rudybot") ("#emacs" . "rudybot")))
 '(erc-ignore-per-channel-reply-alist '(("#scheme" . "rudybot") ("#emacs" . "rudybot")))
 '(erc-ignore-reply-list '("snv1?"))
 '(erc-insert-away-timestamp-function 'erc-insert-timestamp-left)
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-interpret-mirc-color t)
 '(erc-join-buffer 'bury)
 '(erc-keywords '("^\\(<.*>\\|\\* \\)"))
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-max-buffer-size 300000)
 '(erc-minibuffer-ignored t)
 '(erc-minibuffer-notice t)
 '(erc-modules '(autoaway button completion irccontrols log match netsplit networks readonly replace ring services stamp track truncate))
 '(erc-nick '("pjb"))
 '(erc-nickserv-passwords '((freenode (("ogamita" . "ogre-a-mite") ("pjb" . "let's-chat")))))
 '(erc-notice-prefix "   *** ")
 '(erc-pals '("bolet.*" "Posterdati" "martinl"))
 '(erc-port 6697)
 '(erc-prompt (lambda nil (buffer-name (current-buffer))))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password t)
 '(erc-quit-reason-various-alist nil)
 '(erc-scrolltobottom-mode nil)
 '(erc-server "irc.libera.chat")
 '(erc-server-coding-system '(utf-8 . undecided))
 '(erc-server-reconnect-attempts 100)
 '(erc-server-reconnect-timeout 60)
 '(erc-system-name "informatimago.com")
 '(erc-text-matched-hook '(erc-log-matches erc-beep-on-match))
 '(erc-timestamp-format nil)
 '(erc-timestamp-intangible nil)
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353"))
 '(erc-user-full-name "Pascal J. Bourguignon")
 '(eval-expression-debug-on-error t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(fci-rule-color "#073642")
 '(file-precious-flag t)
 '(flycheck-c/c++-clang-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++-disabled")
 '(flycheck-c/c++-gcc-executable "/opt/local/bin/g++")
 '(flycheck-cppcheck-include-path '("/Library/Developer/CommandLineTools/SDKs/MacOSX11.3.sdk/usr/include/"))
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-gcc-include-path '("/Library/Developer/CommandLineTools/SDKs/MacOSX11.3.sdk/usr/include/"))
 '(flycheck-gcc-language-standard "c11")
 '(flycheck-gcc-pedantic t)
 '(focus-follows-mouse nil)
 '(font-lock-extra-types '("FILE" "\\sw+_t" "[A-Z][A-Za-z]+[A-Z][A-Za-z0-9]+" "bool" "INT8" "INT16" "INT32" "INT64" "INTPTR" "CARD8" "CARD16" "CARD32" "CARD64" "CARDPTR" "SignT" "CHAR" "UNICODE" "DECIMAL" "ADDRESS" "CSTRING255" "CSTRING63" "CSTRING31" "BOOLEAN") t)
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-article-loose-mime t)
 '(gnus-article-sort-functions '(gnus-article-sort-by-score))
 '(gnus-cacheable-groups "*")
 '(gnus-carpal nil t)
 '(gnus-default-charset 'iso-8859-15)
 '(gnus-default-posting-charset 'utf-8 t)
 '(gnus-group-posting-charset-alist '(("^\\(no\\|fr\\)\\.[^,]*\\(,[
]*\\(no\\|fr\\)\\.[^,]*\\)*$" iso-8859-15 (iso-8859-15)) ("^\\(fido7\\|relcom\\)\\.[^,]*\\(,[
]*\\(fido7\\|relcom\\)\\.[^,]*\\)*$" koi8-r (koi8-r)) (message-this-is-mail nil nil) (message-this-is-news iso-8859-15 (iso-8859-15))))
 '(gnus-ignored-headers '("^Path:" "^Expires:" "^Date-Received:" "^References:" "^Xref:" "^Lines:" "^Relay-Version:" "^Approved:" "^Sender:" "^Received:" "^X-UIDL:" "^MIME-Version:" "^Return-Path:" "^In-Reply-To:" "^Content-Type:" "^Content-Transfer-Encoding:" "^X-WebTV-Signature:" "^X-MimeOLE:" "^X-MSMail-Priority:" "^X-Priority:" "^X-Loop:" "^X-Authentication-Warning:" "^X-MIME-Autoconverted:" "^X-Face:" "^X-Attribution:" "^X-Originating-IP:" "^Delivered-To:" "^NNTP-[-A-Za-z]+:" "^Distribution:" "^X-no-archive:" "^X-Trace:" "^X-Complaints-To:" "^X-NNTP-Posting-Host:" "^X-Orig.*:" "^Abuse-Reports-To:" "^Cache-Post-Path:" "^X-Article-Creation-Date:" "^X-Poster:" "^X-Mail2News-Path:" "^X-Server-Date:" "^X-Cache:" "^Originator:" "^X-Problems-To:" "^X-Auth-User:" "^X-Post-Time:" "^X-Admin:" "^X-UID:" "^Resent-[-A-Za-z]+:" "^X-Mailing-List:" "^Precedence:" "^Original-[-A-Za-z]+:" "^X-filename:" "^X-Orcpt:" "^Old-Received:" "^X-Pgp:" "^X-Auth:" "^X-From-Line:" "^X-Gnus-Article-Number:" "^X-Majordomo:" "^X-Url:" "^X-Sender:" "^MBOX-Line:" "^Priority:" "^X400-[-A-Za-z]+:" "^Status:" "^X-Gnus-Mail-Source:" "^Cancel-Lock:" "^X-FTN:" "^X-EXP32-SerialNo:" "^Encoding:" "^Importance:" "^Autoforwarded:" "^Original-Encoded-Information-Types:" "^X-Ya-Pop3:" "^X-Face-Version:" "^X-Vms-To:" "^X-ML-NAME:" "^X-ML-COUNT:" "^Mailing-List:" "^X-finfo:" "^X-md5sum:" "^X-md5sum-Origin:" "^X-Sun-Charset:" "^X-Accept-Language:" "^X-Envelope-Sender:" "^List-[A-Za-z]+:" "^X-Listprocessor-Version:" "^X-Received:" "^X-Distribute:" "^X-Sequence:" "^X-Juno-Line-Breaks:" "^X-Notes-Item:" "^X-MS-TNEF-Correlator:" "^x-uunet-gateway:" "^X-Received:" "^Content-length:" "^X-precedence:" "^X-Authenticated-User:" "^X-Comment:" "^X-Report:" "^X-Abuse-Info:" "^X-HTTP-Proxy:" "^X-Mydeja-Info:" "^X-Copyright:" "^X-No-Markup:" "^X-Abuse-Info:" "^X-From_:" "^X-Accept-Language:" "^Errors-To:" "^X-BeenThere:" "^X-Mailman-Version:" "^List-Help:" "^List-Post:" "^List-Subscribe:" "^List-Id:" "^List-Unsubscribe:" "^List-Archive:" "^X-Content-length:" "^X-Posting-Agent:" "^Original-Received:" "^X-Request-PGP:" "^X-Fingerprint:" "^X-WRIEnvto:" "^X-WRIEnvfrom:" "^X-Virus-Scanned:" "^X-Delivery-Agent:" "^Posted-Date:" "^X-Gateway:" "^X-Local-Origin:" "^X-Local-Destination:" "^X-UserInfo1:" "^X-Received-Date:" "^X-Hashcash:" "^Face:" "^X-DMCA-Notifications:" "^X-Abuse-and-DMCA-Info:" "^X-Postfilter:" "^X-Gpg-.*:" "^X-Disclaimer:"))
 '(gnus-message-setup-hook '(pjb-gnus-message-setup-meat))
 '(gnus-nntp-server nil)
 '(gnus-play-startup-jingle nil)
 '(gnus-secondary-select-methods 'nil)
 '(gnus-select-method '(nntp "news.individual.net"))
 '(gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)
 '(gnus-summary-line-format "%U%R%z%o %B%(%[%4L: %-23,23f%]%) %s
")
 '(gnus-treat-display-x-face 'head)
 '(gnus-use-full-window nil)
 '(gnus-use-nocem nil)
 '(gnus-uu-post-encode-method 'gnus-uu-post-encode-mime)
 '(gnus-visible-headers '("^From:" "^Newsgroups:" "^Subject:" "^Date:" "^Followup-To:" "^Reply-To:" "^Organization:" "^Summary:" "^Keywords:" "^To:" "^[BGF]?Cc:" "^Posted-To:" "^Mail-Copies-To:" "^Mail-Followup-To:" "^Apparently-To:" "^Gnus-Warning:" "^Resent-From:" "^Message-ID:" "^X-Sent:"))
 '(gnutls-verify-error t)
 '(grep-command "grep -niH -e ")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors '("#3b6b40f432d7" "#07b9463d4d37" "#47a3341f358a" "#1d873c4056d5" "#2d87441c3362" "#43b7362e3199" "#061e418059d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors '((#1="#073642" . 0) ("#5b7300" . 20) ("#007d76" . 30) ("#0061a8" . 50) ("#866300" . 60) ("#992700" . 70) ("#a00559" . 85) (#1# . 100)))
 '(hl-bg-colors '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors '(#1="#002b36" #1# #1# #1# #1# #1# #1# #1#))
 '(hl-paren-colors '("red" "orange" "yellow" "green" "blue" "violet" "gray" "gray" "gray" "gray" "gray"))
 '(holiday-other-holidays '((holiday-fixed 10 28 "Frédérique Saubot") (holiday-fixed 10 11 "Henri Bourguignon") (holiday-fixed 6 10 "Désirée Mayer") (holiday-fixed 3 23 "Françoise Keller") (holiday-fixed 11 25 "Joëlle Bourguignon") (holiday-fixed 12 16 "Agathe De Robert") (holiday-fixed 5 12 "Guillaume De Robert") (holiday-fixed 1 4 "Isabelle Saubot") (holiday-fixed 10 23 "Marc Moini") (holiday-fixed 2 10 "Anne-Marie Castel") (holiday-fixed 6 28 "Jean-François Gaillon") (holiday-fixed 6 28 "Sylvie Gaillon") (holiday-fixed 8 27 "Jean-Philippe Capy") (holiday-fixed 1 25 "Raoul Fruhauf") (holiday-fixed 3 15 "Pascal Bourguignon") (holiday-fixed 4 12 "Jalal Adamsah") (holiday-fixed 5 3 "Samy Karsenty") (holiday-fixed 8 17 "Alain Pierre") (holiday-fixed 1 14 "Bernard Bourguignon") (holiday-fixed 3 3 "Emmanuelle Chaize") (holiday-fixed 12 12 "Nicoleta Reinald") (holiday-fixed 1 3 "Florence Petit") (holiday-fixed 11 16 "Wei Van Chi") (holiday-fixed 12 6 "Marie Lecomte") (holiday-fixed 7 3 "Alain Bourguignon") (holiday-fixed 4 15 "André Reinald") (holiday-fixed 12 13 "Michelle Keller") (holiday-fixed 5 27 "Grégoire Saubot") (holiday-fixed 3 27 "Olivia De Robert") (holiday-fixed 11 18 "Vincent De Robert") (holiday-fixed 7 23 "Gabriel De Robert") (holiday-fixed 3 18 "Claire De Robert") (holiday-fixed 10 26 "Maxime De Robert") (holiday-fixed 3 26 "Edward-Amadeus Reinald") (holiday-fixed 3 4 "Louise Akiko Poullain") (holiday-fixed 8 26 "Iris-Alea Reinald") (holiday-fixed 9 4 "Baptiste Rouit") (holiday-fixed 2 22 "Camille Saubot") (holiday-fixed 8 2 "Clémence Saubot-Fiant") (holiday-fixed 5 29 "François Saubot") (holiday-fixed 1 2 "Henry Saubot") (holiday-fixed 2 8 "Jean-Pierre Baccache") (holiday-fixed 10 28 "Lucia (fille de Camille)") (holiday-fixed 11 26 "Marine Rouit") (holiday-fixed 3 13 "Mathias Fiant") (holiday-fixed 4 8 "Mathilde Rouit") (holiday-fixed 2 2 "Olivier Scmidt Chevalier") (holiday-fixed 2 23 "PtiDoigt Deamon") (holiday-fixed 8 10 "Kiteri (fille de Camille)") (holiday-fixed 9 10 "Remy Rouit") (holiday-fixed 8 7 "Valerie Saubot-Rouit") (holiday-fixed 1 6 "Los Reyes") (holiday-fixed 6 9 "Santa Murcia") (holiday-fixed 7 25 "Fiesta?") (holiday-fixed 10 12 "Los Reyes") (holiday-fixed 12 6 "Fiesta de la Consitución") (holiday-fixed 7 14 "Fête Nationale France")) t)
 '(ido-enable-flex-matching nil)
 '(indent-tabs-mode nil)
 '(inferior-lisp-filter-regexp "\\`\\s*\\'")
 '(inihibit-default-init t)
 '(initial-major-mode 'emacs-lisp-mode)
 '(ispell-choices-win-default-height 4)
 '(ispell-highlight-p t)
 '(ispell-local-dictionary "francais")
 '(ispell-local-dictionary-alist '(("francais" "[A-Za-zÀ-ÖØ-öø-ÿ]" "[^A-Za-zÀ-ÖØ-öø-ÿ]" "[-']" t nil "~latin9" iso-8859-15)))
 '(ispell-message-dictionary-alist '(("\"^Newsgroups:[ \\t]*fr\\\\.\"" . "\"francais\"") ("\"^To:[^\\n,]+\\\\.fr[ \\t\\n,>]\"" . "\"francais\"") ("\"^Newsgroups:[ \\t]*(es|mx|ar)\\\\.\"" . "\"castillano\"") ("\"^To:[^\\n,]+\\\\.(es|mx|ar)[ \\t\\n,>]\"" . "\"castillano\"") ("\"^Newsgroups:[ \\t]*uk\\\\.\"" . "\"english\"") ("\"^To:[^\\n,]+\\\\.uk[ \\t\\n,>]\"" . "\"english\"") ("\".*\"" . "\"american\"")))
 '(ispell-query-replace-choices nil)
 '(kept-new-versions 9)
 '(kept-old-versions 0)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(lpr-page-header-switches '("-F" "-t"))
 '(lsp-enable-snippet nil)
 '(lsp-ui-doc-border "#93a1a1")
 '(mail-archive-file-name nil)
 '(mail-bury-selects-summary t)
 '(mail-default-headers "Organization: Informatimago
X-Accept-Language:         fr, es, en
MIME-Version: 1.0
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: 8bit
Bcc: pjb@informatimago.com
")
 '(mail-default-reply-to "pjb@informatimago.com")
 '(mail-dont-reply-to-names "info-\\|\\(pjb\\|pascal\\)@triton.afaa.asso.fr\\|\\(pjb\\|pascal\\)@thalassa.afaa.asso.fr\\|669155386@correo.movistar.net\\|pjb@imaginet.fr\\|\\(pjb\\|pascal\\).bourguignon@afaa.asso.fr\\|\\(pjb\\|pascal\\)@afaa.asso.fr\\|pjb@afaa.asso.fr\\|pbourguignon@jazzfree.com\\|pbourguignon@jazzcyber.com\\|pajabou@worldonline.fr\\|pbo21957@worldonline.fr\\|\\(pjb\\|pascal\\)@informatimago.com\\|pjb@informatimago.com\\|informatimago@yahoo.es\\|informatimago@terra.es\\|informatimago@free.fr\\|pjb@larural.es\\|tradymago@etrademail.com\\|informatimago@users.sourceforge.net\\|pbourgui@afaa.asso.fr\\|grozilla@offcampus.es\\|latymer@jazzcyber.com\\|latymer_designs@yahoo.com\\|latymer@afaa.asso.fr\\|latymer.designs@afaa.asso.fr\\|latymer.designs@worldonline.fr\\|dla68836@worldonline.fr\\|latymer@worldonline.fr\\|idrv8338@worldonline.fr\\|\\(pjb\\|pascal\\|pascal.bourguignon\\)@informatimago.com")
 '(mail-from-style 'angle)
 '(mail-host-address "informatimago.com")
 '(mail-interactive t)
 '(mail-mode-hook '(mail-abbrevs-setup (lambda nil (set-buffer-file-coding-system 'utf-8 t t) (set-input-method default-input-method) (local-set-key "	" 'expand-mail-aliases))))
 '(mail-self-blind t)
 '(mail-setup-hook '(pjb-mail-mode-meat))
 '(mail-signature t)
 '(mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^reply-to:\\|^return-path:\\|^mailing-list:\\|^precedence:\\|^x-\\|^content-\\|^cc:\\|^list-\\|^resent\\|^organization:\\|^sender:\\|^user-agent:\\|^mime-version:\\|^delivered-to:\\|^references:")
 '(mail-yank-prefix "> ")
 '(mark-even-if-inactive t)
 '(matlab-comment-line-s "// " t)
 '(matlab-comment-on-line-s "// " t)
 '(matlab-comment-region-s "// " t)
 '(max-specpdl-size 2048)
 '(menu-bar-mode nil)
 '(message-default-charset 'iso-8859-15)
 '(message-default-headers "Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwAQMAAABtzGvEAAAABlBMVEUAAAD///+l2Z/dAAAA
      oElEQVR4nK3OsRHCMAwF0O8YQufUNIQRGIAja9CxSA55AxZgFO4coMgYrEDDQZWPIlNAjwq9
      033pbOBPtbXuB6PKNBn5gZkhGa86Z4x2wE67O+06WxGD/HCOGR0deY3f9Ijwwt7rNGNf6Oac
      l/GuZTF1wFGKiYYHKSFAkjIo1b6sCYS1sVmFhhhahKQssRjRT90ITWUk6vvK3RsPGs+M1RuR
      mV+hO/VvFAAAAABJRU5ErkJggg==
X-Accept-Language:         fr, es, en
")
 '(message-directory "~/mail/")
 '(message-from-style 'angles)
 '(message-log-max 5000)
 '(message-required-news-headers '(From Newsgroups Subject Date Message-ID (optional . Organization) (optional . User-Agent) (X-Face lambda nil (gnus-x-face-from-file "~/my-face.xbm"))))
 '(message-user-organization "Informatimago")
 '(mew-conf-path "~/.new")
 '(mew-mail-path "~/mail")
 '(mew-pop-auth 'pass)
 '(mew-pop-header-only nil)
 '(mew-pop-server "pop.informatimago.com")
 '(mew-pop-size 0)
 '(mew-smtp-server "smtp.informatimago.com")
 '(mew-use-biff t)
 '(mew-use-biff-bell t)
 '(mew-use-full-window t)
 '(mew-use-other-frame-for-draft nil)
 '(mew-use-text/html t)
 '(minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(mm-coding-system-priorities '(ascii iso-latin-1 iso-latin-9 utf-8))
 '(mspools-update t)
 '(next-screen-context-lines 0)
 '(nntp-authinfo-file "~/.authinfo")
 '(nrepl-message-colors '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-adapt-indentation nil)
 '(org-agenda-files '("~/works/qorvo/notes.org" "~/works/sbde/todo.org" "~/works/sbde/smt/sources/README.org" "~/works/sbde/laboite/specifications/components.org" "~/works/sbde/laboite/analysis/controller.org" "~/works/manif/TODO.org" "~/works/synth/schmidt/todo.txt" "~/works/enolaba/macosx/src/TODO" "~/src/pjb/nasium-lse/ISSUES.txt" "~/works/patchwork/src/mclgui/TODO.org" "~/works/patchwork/src/patchwork/notes.txt" "~/works/abnotation/abnotation/todo.txt" "~/works/ops/TODO.org"))
 '(org-confirm-babel-evaluate nil)
 '(org-fontify-done-headline t)
 '(org-latex-remove-logfiles nil)
 '(org-todo-keywords '((sequence "TODO(t@)" "IN-PROGRESS(p@)" "SUSPENDED(s@)" "|" "DONE(d@)" "CANCELED(c@)")))
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/")))
 '(package-selected-packages '(ox-gfm smalltalk-mode twittering-mode company-coq coq-commenter flycheck-swift flycheck-swift3 flycheck-swiftlint flycheck-swiftx ob-swift swift-helpful swift-playground-mode swift3-mode web-server pg polymode lsp-mode dash lsp-ui stack lsp-haskell stack-mode hyai hindent hi2 haskell-tab-indent haskell-snippets haskell-emacs-text haskell-emacs-base haskell-emacs flycheck-liquidhs flycheck-hdevtools flycheck-haskell flycheck-ghcmod dante ac-haskell-process ghci-completion ghc-imported-from ghc shm retrie ormolu intero htmlize cobol-mode swift-mode haskell-mode helm markdown-mode inf-ruby w3m popup json emms paredit textmate smartparens robe jdee highlight-indentation flycheck enh-ruby-mode dash-at-point company column-marker auto-complete))
 '(ph-server "localhost" t)
 '(pjb-test-var 2 t)
 '(pop-up-frames nil)
 '(pop-up-windows t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(pr-faces-p t)
 '(print-gensym t t)
 '(printer-name "Samsung_M2020_Series__SEC30CDA7116AD9_" t)
 '(prolog-program-name "/usr/bin/swipl")
 '(ps-font-size '(7 . 11))
 '(ps-header-lines 0)
 '(ps-left-header nil)
 '(ps-paper-type 'a4 t)
 '(ps-print-header nil)
 '(ps-print-header-frame nil)
 '(ps-printer-name "normal_gray")
 '(ps-right-header nil)
 '(ps-show-n-of-n nil)
 '(read-mail-command 'vm)
 '(read-quoted-char-radix 10)
 '(redshank-accessor-name-function 'identity)
 '(redshank-canonical-package-designator-function 'redshank-package-designator/string)
 '(redshank-licence-names '("BSD-style" "GPL" "LGPL" "LLGPL" "MIT" "MIT-style" "GPL2" "GPL2+" "GPL3" "AGPL3"))
 '(require-final-newline 'visit-save)
 '(rmail-confirm-expunge nil)
 '(rmail-display-summary t)
 '(rmail-enable-mime nil)
 '(rmail-enable-multibyte t t)
 '(rmail-ignored-headers "^user-agent:\\|^\\(importa\\|precede\\)nce:\\|^priority:\\|^list-\\|^mailing-list\\|^via:\\|^mail-\\(from:\\|follow\\)\\|^\\(in-\\)?reply-to:\\|^sender:\\|^origin:\\|^references:\\|^status:\\|^received:\\|^summary-line:\\|^resent-\\|^\\(resent-\\)?message-id:\\|^nntp-posting-host:\\|^path:\\|^delivered-to:\\|^lines:\\|^mime-version:\\|^content-\\|^return-path:\\|^errors-to:\\|^return-receipt-to:\\|^x400-\\|^x-\\|^x-attribution:\\|^x-char.*:\\|^x-coding-system:\\|^x-face:\\|^x-mailer:\\|^x-disclaimer:\\|phone:")
 '(rmail-output-file-alist nil t)
 '(rmail-preserve-inbox nil)
 '(rmail-redisplay-summary t)
 '(rmail-remote-password nil)
 '(rmail-remote-password-required nil)
 '(rmail-secondary-file-directory "~/mail")
 '(rmail-summary-line-decoder 'identity)
 '(rmail-summary-window-size 12)
 '(safe-local-variable-values '((whitespace-mode . 1) (c-tab-always-indent . t) (smie-indent-basic . 8) (Package . USOCKET-TEST) (eval put 'report-error 'fi:common-lisp-indent-hook 1) (package . net\.aserve\.test) (package . net\.aserve\.client) (package . net\.aserve) (Package . User) (Package . GRAPH) (Package . DWIM) (pretty-greek) (package . common\.surveille-host) (view-mode . t) (Package X86 :use CL) (Package . TRIVIAL-GRAY-STREAMS) (Log . clx\.log) (Package . Xlib) (Package . SYSTEM) (Package . CLIM-POSTSCRIPT) (Package . MCCLIM-TRUETYPE) (Package nstep) (Package discrete-walk) (Package bandits :use (common-lisp ccl ut)) (Package cl-user) (Package rss-utilities :use (common-lisp ccl) :nicknames (:ut)) (mode:scheme:mode . paredit) (Package . USOCKET) (Package . BORDEAUX-THREADS) (Syntax . ANSI-Common-lisp) (flycheck . -1) (flycheck-mode . -1) (package . user) (eval cl-indent 'for-all-cell 1) (Package JPEG :use (common-lisp)) (org-plantuml-jar-path . "../tools/plantuml.jar") (org-plantuml-jar-path expand-file-name "../tools/plantuml.jar") (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "SUSPENDED(s@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . com\.informatimago\.languages\.linc\.c) (eval cl-indent 'define-function 3) (eval cl-indent 'define-type 1) (eval cl-indent 'define-module 1) (eval cl-indent 'in-parens 1) (c-file-offsets (innamespace . 0)) (eval add-hook 'prog-mode-hook (lambda nil (whitespace-mode 1)) (not :APPEND) :BUFFER-LOCAL) (swift-basic-offset . 2) (whitespace-style face lines indentation:space) (eval let* ((x (dir-locals-find-file default-directory)) (this-directory (if (listp x) (car x) (file-name-directory x)))) (unless (or (featurep 'swift-project-settings) (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p this-directory))) (add-to-list 'load-path (concat this-directory "utils") :append) (let ((swift-project-directory this-directory)) (require 'swift-project-settings))) (set (make-local-variable 'swift-project-directory) this-directory)) (Package . CHUNGA) (Package ARCH :use CL) (Package . CL-UNICODE) (eval set-input-method 'latin-1-prefix) (org-confirm-babel-evaluate lambda (lang body) (not (string= lang "dot"))) (Base . 10) (Log . hemlock\.log) (Lowercase . T) (Lowercase . Yes) (Package . CLIM-INTERNALS) (Package . LET-OVER-LAMBDA) (Package . CL-FAD) (Package . FUTURE-COMMON-LISP-USER) (Package . WORDNET-INTERFACE) (Package . WORDNET) (Package . Lisp-Binary) (Package . hemlock-internals) (Package . INSPECTOR) (Package . SURF) (Package . bind) (Package ARM :use CL) (Package ANSI-LOOP "COMMON-LISP") (Package . BORDEAUX-FFT) (Package . XLIB) (Package . gambol) (Package . C) (Package . Hemlock) (Package . GUI) (Package . cl-user) (Package . Portable-Threads-System) (Package . LEXER) (Package . F2CL) (Package . DRAKMA) (Package . CL-WHO) (Package . LISP-UNIT) (Package . Hemlock-Internals) (Package . FLEXI-STREAMS) (Package . COMMON-LISP-USER) (Package . PS) (Package . SCHEME-TRANSLATOR) (Package DATABASE :USE LISP) (Package . HUNCHENTOOT) (Package . CLEVER-LOAD) (Package . REVISED^4-SCHEME) (Package . CL-USER) (Package X8664 :use CL) (Package . SERROR) (Package . CL-PPCRE) (Package . CCL) (Syntax . common-lisp) (Syntax . Common-Lisp) (Syntax . Common-lisp) (Syntax . COMMON-LISP) (Syntax . ANSI-Common-Lisp) (bug-reference-url-format . clisp-bug-reference-url-format) (c-file-style . ruby) (c-indent-level . 4) (compile-cmd . "gcc -DMODULE -Wall -Wstrict-prototypes -O6 -c natsemi.c") (default-input-method . latin-1-prefix) (electric-indent-mode) (eval activate-input-method 'latin-1-prefix) (eval add-hook 'before-save-hook 'time-stamp) (eval let ((inhibit-read-only t) (compilation-filter-start (point-min))) (save-excursion (goto-char (point-max)) (grep-filter) (set-buffer-modified-p nil))) (eval cl-indent 'defmeth 3) (eval buttonize-buffer) (eval cl-indent 'cvm-do-symbols 1) (eval cl-indent 'cvm-dolist 1) (eval cl-indent 'cvm-define-structure 2) (eval cl-indent 'raw-memory:WITH-SIGSEG-HANDLER 0) (eval cl-indent 'when-debug 1) (eval cl-indent 'with-generation 1) (eval cl-indent 'with-gc-lock 0) (eval cl-indent 'define-common-structure 1) (eval cl-indent 'defenum 1) (eval put 'let-errno 'common-lisp-indent-function 1) (eval cl-indent 'defbf 2) (eval cl-indent 'ffi:with-c-place 1) (eval cl-indent 'xlib:event-case '((&whole 6 1 1 1 1 1 1) &rest (&whole 2 1 1 1 1 1 1 1 1 1 1 1))) (eval when (fboundp 'asm7090) (asm7090)) (eval cl-indent 'defcommand 3) (eval progn (local-set-key (kbd "<kp-divide>") #'lisp-indent-line) (local-set-key (kbd "<kp-multiply>") (lambda nil (interactive) (insert (kbd "SPC")))) (local-set-key (kbd "<XF86_Ungrab>") #'backward-delete-char-untabify)) (eval cl-indent 'dolist/separator 1) (eval put 'define-structure 'common-lisp-indent-function 1) (flycheck-mode) (lexical-binding . t) (more . org) (org-fontify-done-headline . t) (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . net\.mail) (package . net\.post-office) (package . rune-dom) (package . WILBUR) (sentence-end-double-space . t) (tab-always-indent . t) (tab-always-indent) (view-mode t) (Package . "CCL") (syntax . COMMON-LISP) (Package . April) (Package . USOCKET-TEST) (eval put 'report-error 'fi:common-lisp-indent-hook 1) (package . net\.aserve\.test) (package . net\.aserve\.client) (package . net\.aserve) (Package . User) (Package . GRAPH) (Package . DWIM) (pretty-greek) (package . common\.surveille-host) (view-mode . t) (Package X86 :use CL) (Package . TRIVIAL-GRAY-STREAMS) (Log . clx\.log) (Package . Xlib) (Package . SYSTEM) (Package . CLIM-POSTSCRIPT) (Package . MCCLIM-TRUETYPE) (Package nstep) (Package discrete-walk) (Package bandits :use (common-lisp ccl ut)) (Package cl-user) (Package rss-utilities :use (common-lisp ccl) :nicknames (:ut)) (mode:scheme:mode . paredit) (Package . USOCKET) (Package . BORDEAUX-THREADS) (Syntax . ANSI-Common-lisp) (flycheck . -1) (flycheck-mode . -1) (package . user) (eval cl-indent 'for-all-cell 1) (Package JPEG :use (common-lisp)) (org-plantuml-jar-path . "../tools/plantuml.jar") (org-plantuml-jar-path expand-file-name "../tools/plantuml.jar") (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "SUSPENDED(s@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . com\.informatimago\.languages\.linc\.c) (eval cl-indent 'define-function 3) (eval cl-indent 'define-type 1) (eval cl-indent 'define-module 1) (eval cl-indent 'in-parens 1) (c-file-offsets (innamespace . 0)) (eval add-hook 'prog-mode-hook (lambda nil (whitespace-mode 1)) (not :APPEND) :BUFFER-LOCAL) (swift-basic-offset . 2) (whitespace-style face lines indentation:space) (eval let* ((x (dir-locals-find-file default-directory)) (this-directory (if (listp x) (car x) (file-name-directory x)))) (unless (or (featurep 'swift-project-settings) (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p this-directory))) (add-to-list 'load-path (concat this-directory "utils") :append) (let ((swift-project-directory this-directory)) (require 'swift-project-settings))) (set (make-local-variable 'swift-project-directory) this-directory)) (Package . CHUNGA) (Package ARCH :use CL) (Package . CL-UNICODE) (eval set-input-method 'latin-1-prefix) (org-confirm-babel-evaluate lambda (lang body) (not (string= lang "dot"))) (Base . 10) (Log . hemlock\.log) (Lowercase . T) (Lowercase . Yes) (Package . CLIM-INTERNALS) (Package . LET-OVER-LAMBDA) (Package . CL-FAD) (Package . FUTURE-COMMON-LISP-USER) (Package . WORDNET-INTERFACE) (Package . WORDNET) (Package . Lisp-Binary) (Package . hemlock-internals) (Package . INSPECTOR) (Package . SURF) (Package . bind) (Package ARM :use CL) (Package ANSI-LOOP "COMMON-LISP") (Package . BORDEAUX-FFT) (Package . XLIB) (Package . gambol) (Package . C) (Package . Hemlock) (Package . GUI) (Package . cl-user) (Package . Portable-Threads-System) (Package . LEXER) (Package . F2CL) (Package . DRAKMA) (Package . CL-WHO) (Package . LISP-UNIT) (Package . Hemlock-Internals) (Package . FLEXI-STREAMS) (Package . COMMON-LISP-USER) (Package . PS) (Package . SCHEME-TRANSLATOR) (Package DATABASE :USE LISP) (Package . HUNCHENTOOT) (Package . CLEVER-LOAD) (Package . REVISED^4-SCHEME) (Package . CL-USER) (Package X8664 :use CL) (Package . SERROR) (Package . CL-PPCRE) (Package . CCL) (Syntax . common-lisp) (Syntax . Common-Lisp) (Syntax . Common-lisp) (Syntax . COMMON-LISP) (Syntax . ANSI-Common-Lisp) (bug-reference-url-format . clisp-bug-reference-url-format) (c-file-style . ruby) (c-indent-level . 4) (compile-cmd . "gcc -DMODULE -Wall -Wstrict-prototypes -O6 -c natsemi.c") (default-input-method . latin-1-prefix) (electric-indent-mode) (eval activate-input-method 'latin-1-prefix) (eval add-hook 'before-save-hook 'time-stamp) (eval let ((inhibit-read-only t) (compilation-filter-start (point-min))) (save-excursion (goto-char (point-max)) (grep-filter) (set-buffer-modified-p nil))) (eval cl-indent 'defmeth 3) (eval buttonize-buffer) (eval cl-indent 'cvm-do-symbols 1) (eval cl-indent 'cvm-dolist 1) (eval cl-indent 'cvm-define-structure 2) (eval cl-indent 'raw-memory:WITH-SIGSEG-HANDLER 0) (eval cl-indent 'when-debug 1) (eval cl-indent 'with-generation 1) (eval cl-indent 'with-gc-lock 0) (eval cl-indent 'define-common-structure 1) (eval cl-indent 'defenum 1) (eval put 'let-errno 'common-lisp-indent-function 1) (eval cl-indent 'defbf 2) (eval cl-indent 'ffi:with-c-place 1) (eval cl-indent 'xlib:event-case '((&whole 6 1 1 1 1 1 1) &rest (&whole 2 1 1 1 1 1 1 1 1 1 1 1))) (eval when (fboundp 'asm7090) (asm7090)) (eval cl-indent 'defcommand 3) (eval progn (local-set-key (kbd "<kp-divide>") #'lisp-indent-line) (local-set-key (kbd "<kp-multiply>") (lambda nil (interactive) (insert (kbd "SPC")))) (local-set-key (kbd "<XF86_Ungrab>") #'backward-delete-char-untabify)) (eval cl-indent 'dolist/separator 1) (eval put 'define-structure 'common-lisp-indent-function 1) (flycheck-mode) (lexical-binding . t) (more . org) (org-fontify-done-headline . t) (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . net\.mail) (package . net\.post-office) (package . rune-dom) (package . WILBUR) (sentence-end-double-space . t) (tab-always-indent . t) (tab-always-indent) (view-mode t)))
 '(select-enable-clipboard t)
 '(select-enable-primary t)
 '(send-mail-function 'sendmail-send-it)
 '(server-use-tcp nil)
 '(sh-indent-after-case 0)
 '(sh-indent-after-switch 0)
 '(sh-indent-for-case-alt '+)
 '(sh-indent-for-case-label 0)
 '(shell-dynamic-complete-functions '(bash-completion-dynamic-complete comint-c-a-p-replace-by-expanded-history shell-environment-variable-completion shell-command-completion shell-c-a-p-replace-by-expanded-directory pcomplete-completions-at-point shell-filename-completion comint-filename-completion))
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch nil)
 '(slime-autodoc-use-multiline-p t)
 '(slime-compilation-finished-hook '(slime-maybe-show-xrefs-for-notes))
 '(slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
 '(slime-space-information-p nil)
 '(slime-startup-animation nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(smtpmail-default-smtp-server "hubble.informatimago.com")
 '(smtpmail-local-domain "lan.informatimago.com")
 '(smtpmail-sendto-domain "informatimago.com")
 '(smtpmail-smtp-server "hubble.informatimago.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-smtp-user "pjb@informatimago.com")
 '(smtpmail-stream-type 'starttls)
 '(smtpmail-warn-about-unknown-extensions t)
 '(spam-autodetect-recheck-messages t)
 '(speedbar-show-unknown-files t)
 '(stack-trace-on-error nil)
 '(tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
 '(tab-width 4)
 '(tags-table-list 'nil)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tls-checktrust t)
 '(tls-program '("gnutls-cli --x509cafile %t -p %p %h" "gnutls-cli --x509cafile %t -p %p %h --protocols ssl3"))
 '(tnt-use-timestamps t)
 '(tnt-username-alist '(("matimago") ("ogamita")))
 '(tool-bar-mode nil)
 '(tooltip-frame-parameters '((nil . "tooltip") (right-fringe . 6) (left-fringe . 6) (nil . "lightyellow") (nil . 0) (nil . 1)))
 '(tramp-default-method "ssh" nil (tramp))
 '(truncate-partial-width-windows nil)
 '(twittering-password "rixhox-Ruvfyd-qemqi6")
 '(twittering-username "PaulGE4")
 '(url-be-asynchronous t)
 '(url-honor-refresh-requests nil)
 '(user-full-name "Pascal J. Bourguignon")
 '(user-mail-address "pjb@informatimago.com")
 '(vc-annotate-background "snow1")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map '((4.943848 . "#0000f0") (10.217285 . "#0000e0") (15.490723 . "#0000d0") (20.76416 . "#0000c0") (26.037598 . "#0000b0") (31.311035 . "#0000a0") (36.584473 . "#000090") (41.85791 . "#000080") (47.131348 . "#000070") (52.404785 . "#000060") (57.678223 . "#000050") (62.95166 . "#000040") (68.225098 . "#000030") (73.498535 . "#000020") (78.771973 . "#000010")))
 '(vc-annotate-very-old-color "#000000")
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(vm-auto-displayed-mime-content-types '("text/enriched" "text/plain" "message" "message/rfc822" "message/disposition-notification" "multipart"))
 '(vm-auto-folder-alist '(("^\\(From:\\|To:\\|Cc:\\)" ("svn-.*anevia.com" . "~/mail/anevia-svn.mbox") ("staff@anevia.com" . "~/mail/anevia-staff.mbox") ("cpptest@anevia.com" . "~/mail/cpptest.mbox") ("\\(bese.*common-lisp\\)" . "~/mail/ucw.mbox") ("\\(lispme\\|clisp\\|sbcl\\|cmucl\\|openmcl\\|ilisp\\|clocc\\|clump\\|cclan\\|ecls\\|nocrew.org\\|biolisp\\|lispweb\\|climacs\\|gardeners\\|acl2\\|Planet Lisp\\|lisa-users\\|opencyc\\|small-cl-src\\|cl-faq\\|cl-pdf\\|cl-typesetting\\|movitz\\|quiz@common-lisp\\|slime\\)" . "~/mail/lisp.mbox") ("cert-advisory@cert.org" . "~/mail/cert.mbox") ("gentoo" . "~/mail/gentoo.mbox")) ("^Subject:" ("\\[libanevia\\|manager2\\|aipc\\]" . "~/mail/anevia-manager2.mbox") ("cs daily" . "~/mail/cs-papers.mbox") ("CRYPTO.*GRAM" . "~/mail/crypto-gram.mbox") ("ipnsig" . "~/mail/ipnsig.mbox") ("\\(lispme\\|clisp\\|sbcl\\|cmucl\\|openmcl\\|ilisp\\|clocc\\|clump\\|cclan\\|ecls\\|nocrew.org\\|biolisp\\|lispweb\\|climacs\\|gardeners\\|acl2\\|Planet Lisp\\|lisa-users\\|opencyc\\|small-cl-src\\|cl-faq\\|cl-pdf\\|cl-typesetting\\|movitz\\|quiz@common-lisp\\|slime\\)" . "~/mail/lisp.mbox"))))
 '(vm-auto-folder-case-fold-search t)
 '(vm-display-xfaces t)
 '(vm-folder-directory "~/mail/")
 '(vm-highlighted-header-face 'font-lock-comment-face)
 '(vm-honor-mime-content-disposition nil)
 '(vm-included-text-prefix "> ")
 '(vm-infer-mime-types t)
 '(vm-mail-mode-hook nil)
 '(vm-mime-8bit-composition-charset "utf-8")
 '(vm-mime-8bit-text-transfer-encoding '8bit)
 '(vm-mime-alternative-select-method '(favorite-internal "text/enriched" "text/plain"))
 '(vm-mime-attachment-auto-suffix-alist '(("image/tiff" . ".tif") ("image/jpeg" . ".jpg") ("image/gif" . ".gif") ("image/png" . ".png") ("text/html" . ".html") ("audio/basic" . ".au") ("video/mpeg" . ".mpg") ("video/quicktime" . ".mov") ("application/postscript" . ".ps") ("application/pdf" . ".pdf") ("application/vnd.ms-excel" . ".xls") ("application/mac-binhex40" . ".hqx") ("application/pdf" . ".pdf") ("application/zip" . ".zip")))
 '(vm-mime-default-face-charsets '("us-ascii" "iso-8859-1" "iso-8859-15" "win-1250" "ANSI_X3.4-1968"))
 '(vm-mime-external-content-types-alist '(("application/pdf" "acroread") ("image/gif" "xview") ("image/jpg" "xview") ("image/tiff" "xview") ("image/jpeg" "xview")))
 '(vm-mime-use-w3-for-text/html t)
 '(vm-mutable-frames nil)
 '(vm-preview-lines nil)
 '(vm-reply-subject-prefix "Re: ")
 '(vm-spool-files '(("~/INBOX" "/var/spool/mail/pjb" "~/INBOX-local.crash")))
 '(vm-summary-highlight-face 'font-lock-comment-face)
 '(vm-url-browser 'pjb-browse-url)
 '(vm-use-lucid-highlighting t)
 '(w3-default-homepage "http://www.google.com")
 '(w3-delay-image-loads t)
 '(w3-display-frames nil)
 '(w3-do-incremental-display t)
 '(w3-honor-stylesheets nil)
 '(w3-horizontal-rule-char 45)
 '(w3-source-file-hook nil)
 '(w3-use-terminal-characters nil)
 '(w3-use-terminal-characters-on-tty nil)
 '(w3-user-colors-take-precedence t)
 '(w3-user-fonts-take-precedence t)
 '(w3m-arrived-file "~/.w3m/arrived")
 '(w3m-bookmark-file "~/.w3m/bookmark.html")
 '(w3m-bookmark-file-coding-system 'utf-8)
 '(w3m-coding-system 'utf-8)
 '(w3m-cookie-file "~/.w3m/cookie")
 '(w3m-default-display-inline-images t)
 '(w3m-fb-mode nil)
 '(w3m-file-coding-system 'utf-8)
 '(w3m-file-name-coding-system 'iso-8859-1)
 '(w3m-form-textarea-directory "~/.w3m/textarea")
 '(w3m-home-page "http://localhost/")
 '(w3m-pop-up-frames nil)
 '(w3m-pop-up-windows nil)
 '(w3m-session-file "~/.w3m/sessions")
 '(w3m-terminal-coding-system 'utf-8)
 '(w3m-use-cookies t)
 '(w3m-use-tab nil)
 '(w3m-use-tab-menubar nil)
 '(w3m-use-title-buffer-name t)
 '(warning-suppress-types '((undo discard-info)))
 '(weechat-color-list '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

;; To reset w3m-cookies: (setf w3m-cookies nil)

;; '(gnus-secondary-select-methods (quote ((nntp "news.gmane.org") (nnimap "hubble.informatimago.com"))))
;;;----------------------------------------------------------------------------

(defvar ecb-source-path)
(setf ecb-source-path (expand-file-name "~/src/"))

;;;----------------------------------------------------------------------------
(when (or (< emacs-major-version 26)
          (and (= emacs-major-version 26)
               (< emacs-minor-version 3)))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defvar gnutls-trustfiles '())
(unless (fboundp 'string-trim)
  (defun string-trim (string)
    (cl:string-trim " \n\t" string)))

;; (let ((trustfile (string-trim (shell-command-to-string "python -m certifi")q)))
;;   (setf tls-program
;;         (list
;;          (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
;;                  (if (eq window-system 'w32) ".exe" "")
;;                  trustfile)))
;;   (push trustfile gnutls-trustfiles))


(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                                     -CAfile /home/ootput/.private/certs/CAs.pem
                                     -cert /home/ootput/.private/certs/nick.pem"
                    "gnutls-cli --priority secure256
                               --x509cafile /home/ootput/.private/certs/CAs.pem
                               --x509certfile /home/ootput/.private/certs/nick.pem -p %p %h"
                    "gnutls-cli --priority secure256 -p %p %h"))

(setf tls-program '("gnutls-cli --x509cafile %t -p %p %h"
                    "gnutls-cli --x509cafile %t -p %p %h --protocols ssl3"))




(defun test-tls-configuration ()
  (let ((bad-hosts
          (loop for bad
                  in `("https://wrong.host.badssl.com/"
                       "https://self-signed.badssl.com/")
                if (condition-case e
                                   (url-retrieve
                                    bad (lambda (retrieved) t))
                                   (error nil))
                  collect bad)))
    (if bad-hosts
        (error (format "tls misconfigured; retrieved %s ok"
                       bad-hosts))
        (url-retrieve "https://badssl.com"
                      (lambda (retrieved) t)))))

;;;----------------------------------------------------------------------------
(when (file-exists-p "~/rc/emacs-patches.el")
  (load "~/rc/emacs-patches.el"))
(load "~/rc/emacs-package.el")
(load "~/rc/emacs-font.el")
(load "~/rc/emacs-paredit.el")
(when (not *pjb-pvs-is-running*)
  ;; (load "~/rc/emacs-theme.el")
  (load "~/rc/emacs-palette.el"))

(add-to-load-path "~/emacs/slime/")
(load "~/rc/emacs-slime.el")

(load "~/rc/emacs-hyperspec.el")
(load "~/rc/emacs-redshank.el")
(load "~/rc/emacs-objective-c.el")
(load "~/rc/emacs-android.el")
(load "~/rc/emacs-ruby.el")
(load "~/rc/emacs-cobol.el")
(load "~/rc/emacs-erc.el")
(cond
  ((string-match "^larissa.*" (hostname))
   )
  ((string-match "^despina.*" (hostname))
   (load "~/rc/emacs-haskell.el")))

(load "~/rc/emacs-linux.el")
;;;----------------------------------------------------------------------------
(display-time-mode 1)
(setf visible-bell nil
      ring-bell-function nil)
;;;----------------------------------------------------------------------------

(or (ignore-errors (set-frame-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*"))
    (ignore-errors (set-frame-font "terminus-18")))

(add-to-list 'auto-mode-alist '("\\.yml$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ergo$"  . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$"   . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.swift$" . swift-mode))
(add-to-list 'auto-mode-alist '( ".*/\\(Readme\\|readme\\|README\\)\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/works/abalone/.*\\.\\(h\\|m\\mm\\)$")   . objc-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/src/ios/.*\\.\\(h\\|m\\mm\\)$")         . objc-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/private/etudes/stanford/.*\\.\\(m\\)$") . octave-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/.*/coursera-robotics/.*\\.m$")          . matlab-mode))
(setf auto-mode-alist  (sort* auto-mode-alist
                              (function string<)
                              :key (function car)))

(ignore-errors (set-sources (expand-file-name "~/works/patchwork/src/patchwork/")))

(when (require 'flycheck nil t)
  (global-flycheck-mode))

(autoload 'cflow-mode "cflow-mode")
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.cflow$" . cflow-mode))))

;;;----------------------------------------------------------------------------

;; (when (and (file-exists-p "/data/sound/beeps/Macintosh_Question.wav")
;;            (file-exists-p "/usr/bin/mplayer"))
;;   (setf visible-bell nil
;;         ring-bell-function (lambda ()
;;                              (shell-command-to-string
;;                               "mplayer /data/sound/beeps/Macintosh_Question.wav"))))


(add-to-load-path "~/emacs/emacs-w3m/share/emacs/site-lisp/w3m/")

(dolist (hooks  '(lisp-mode-hook emacs-lisp-mode-hook common-lisp-mode-hook
                  c-mode-hook c++-mode-hook))
  (add-hook hooks 'sexp-movement))

(defun pjb-w3m-mode-meat ()
  (interactive)
  (local-set-key (kbd "<up>") 'previous-line)
  (local-set-key (kbd "<down>") 'next-line)
  (local-set-key (kbd "<left>") 'backward-char)
  (local-set-key (kbd "<right>") 'forward-char)
  (local-set-key (kbd "C-<left>") 'w3m-view-previous-page)
  (local-set-key (kbd "C-<right>") 'w3m-view-this-url))

(defvar w3m-mode-hook '())
(pushnew 'pjb-w3m-mode-meat w3m-mode-hook)
;; (setf w3m-mode-hook (delete 'pjb-w3m-mode-meat w3m-mode-hook))


;; (setf (getenv "EMACS_USE") "erc")
;; (setf (getenv "EMACS_USE") "gnus")
;; (setf (getenv "EMACS_USE") "pgm")


(cond
  (*pjb-pvs-is-running*)
  ((string= (getenv "EMACS_USE") "erc")
   (when (fboundp 'set-palette) (set-palette pal-dark-blue))
   (set-frame-name "ERC")
   (erc-select))
  ((string= (getenv "EMACS_USE") "gnus")
   (when (fboundp 'set-palette) (set-palette pal-dark-amber))
   (gnus))
  (t
   (when (fboundp 'set-palette)
     (if (string= (hostname) "larissa.local")
         (set-palette pal-stripe1)
         (set-palette pal-green)))))


(cond
  (*pjb-pvs-is-running*)
  ((member "(gnus)"  command-line-args)
   (setf uptimes-auto-save-interval (* 7 60))
   (setf *activity-tag* "GNUS")
   (push '(name . "GNUS") default-frame-alist)
   (set-background-color "#ccccfefeebb7"))
  ((member "(irc)"  command-line-args)
   (setf uptimes-auto-save-interval (* 11 60))
   (setf *activity-tag* "ERC")
   (push '(name . "ERC") default-frame-alist))
  (t
   (setf *activity-tag* "EMACS")
   (setf uptimes-auto-save-interval (* 13 60))
   (push '(name . "PGM") default-frame-alist)
   (server-start)
   (setf (getenv "CVSEDITOR")  "emacsclient"
         (getenv "EDITOR")     "emacsclient"
         (getenv "VISUAL")     "emacsclient")))

(global-set-key (kbd "<f25>")
                (lambda ()
                  (interactive)
                  (let ((file  (ffap-file-at-point)))
                    (unless (endp (rest (window-list)))
                      (other-window 1))
                    (find-file file))))

(global-set-key (kbd "C-c M-o") 'erase-buffer)
(global-set-key (kbd "C-c m")   'set-mark-command)
(global-set-key (kbd "C-c C-m") 'set-mark-command)
(global-set-key (kbd "C-x 8 i") 'insert-char)

(when nil
 (dolist (multi '(("`" (("a" "à") ("e" "è") ("i" "ì") ("o" "ò") ("u" "ù")
                        ("A" "À") ("E" "È") ("I" "Ì") ("O" "Ò") ("U" "Ù")))
                  ("'" (("a" "á") ("e" "é") ("i" "í") ("o" "ó") ("u" "ú") ("y" "ý")
                        ("A" "Á") ("E" "É") ("I" "Í") ("O" "Ó") ("U" "Ú") ("Y" "Ý")))
                  ("^" (("a" "â") ("e" "ê") ("i" "î") ("o" "ô") ("u" "û")
                        ("A" "Â") ("E" "Ê") ("I" "Î") ("O" "Ô") ("U" "Û")))
                  ("~" (("A" "Ã") ("N" "Ñ") ("O" "Õ")
                        ("a" "ã") ("n" "ñ") ("o" "õ")))
                  ("\"" (("a" "ä") ("e" "ë") ("i" "ï") ("o" "ö") ("u" "ü") ("y" "ÿ")
                         ("A" "Ä") ("E" "Ë") ("I" "Ï") ("O" "Ö") ("U" "Ü")))
                  ("s" (("s" "ß")))
                  ("t" (("h" "þ") ("H" "þ")))
                  ("T" (("h" "Þ") ("H" "Þ")))
                  ("d" (("h" "ð") ("H" "ð")))
                  ("D" (("h" "Ð") ("H" "Ð")))
                  ("A" (("E" "Æ") ("e" "Æ") ("o" "Å")("O" "Å")))
                  ("a" (("E" "æ") ("e" "æ") ("o" "å")("O" "å")))
                  ("/" ((":" "÷") ("o" "ø") ("O" "Ø")))
                  ("," (("C" "Ç") ("c" "ç")))))
   (let* ((first (first multi))
          (name  (intern (format "hyper-%s-map"
                                 (cond
                                   ((string= "`" first) "grave")
                                   ((string= "'" first) "acute")
                                   ((string= "^" first) "circumflex")
                                   ((string= "~" first) "tilde")
                                   ((string= "\"" first) "umlaut")
                                   ((string= "/" first) "slash")
                                   ((string= "," first) "comma")
                                   (t first)))))
          (table (define-prefix-command name)))
     (message "%S" `(global-set-key (kbd ,(format "H-%s" first)) ,name))
     (global-set-key (kbd (format "H-%s" first)) name)
     (dolist (entry (second multi))
       (let ((second (first entry))
             (result (second entry)))
         (message "%S" `(define-key ,name (kbd ,(format "%s" first second))
                          (lambda (n)
                            (interactive "p")
                            (dotimes (i n)
                              (insert ,result)))))
         (define-key name (kbd (format "%s" first second))
           (lambda (n)
             (interactive "p")
             (dotimes (i n)
               (insert result)))))))))

(when (and (< 23 emacs-major-version) (fboundp 'vc-workfile-version))
  (defun vc-workfile-revision (file-name) (vc-workfile-version file-name)))

(defparameter *pjb-trailing-whitespace-inclusions*
  '("/pjb/src/bash/"
    "/pjb/src/bash-tidbits/"
    "/pjb/src/c-tidbits/"
    "/pjb/src/cobol/"
    "/pjb/src/coq-tidbits/"
    "/pjb/src/emacs/"
    "/pjb/src/emacs-patches/"
    "/pjb/src/fortran-tidbits/"
    "/pjb/src/haskell-tidbits/"
    "/pjb/src/hw/"
    "/pjb/src/hw-src/"
    "/pjb/src/lisp/"
    "/pjb/src/lisp-tidbits/"
    "/pjb/src/lisp-vs-c/"
    "/pjb/src/listeria/"
    "/pjb/src/naiad/"
    "/pjb/src/objc/"
    "/pjb/src/pascal-tidbits/"
    "/pjb/src/pjb/"
    "/pjb/src/public/"
    "/pjb/src/python/"
    "/pjb/src/sql-tidbits/"
    "/pjb/works/")
  "A list of path regexps to include warning for trailing whitespaces.")

(defparameter *pjb-trailing-whitespace-exclusions*
  '("/pjb/works/mts/Harag/"
    "/pjb/works/mts/cl-naive-store/"
    "/pjb/works/qorvo/")
  "A list of path regexps to exclude warning for trailing whitespaces.")


(defun trailing-whitespace-candidate-p (file-name)
  (let ((home (cond (user-init-file  (dirname user-init-file))
                    ((getenv "HOME") (concat (getenv "HOME") "/"))
                    (t               (dirname (first (file-expand-wildcards "~/.emacs")))))))
    (and file-name
         (string-match (format "^%s" home) file-name)
         (vc-workfile-revision file-name)
         (notany (lambda (regexp)
                   (string-match regexp file-name))
                 *pjb-trailing-whitespace-exclusions*)
         (if *pjb-trailing-whitespace-inclusions*
             (some (lambda (regexp)
                     (string-match regexp file-name))
                   *pjb-trailing-whitespace-inclusions*)
             t))))

(defparameter *pjb-binary-modes*
  '(image-mode hexl-mode)
  "A list of major-mode used to visit binary files.")

(defun pjb-find-file-meat/warn-trailing-whitespace ()
  "Meat for find-file-hook: warn about trailing whitespace."
  (let ((home (cond (user-init-file  (dirname user-init-file))
                    ((getenv "HOME") (concat (getenv "HOME") "/"))
                    (t               (dirname (first (file-expand-wildcards "~/.emacs"))))))
        (file-name (buffer-file-name)))
    (when (and (not (member major-mode *pjb-binary-modes*))
               (trailing-whitespace-candidate-p file-name))
      (goto-char (point-min))
      (when (re-search-forward "[ \t]$" nil t)
        (case (ignore-errors
               (if (fboundp 'x-popup-dialog)
                   (x-popup-dialog t (list (format "There are trailing whitespaces in %S."
                                                   file-name)
                                           '("Remove them, save file and vc-next-action" . 1)
                                           '("Remove them, and go on editing"            . 2)
                                           '("Go on editing"                             . 3)
                                           '("Abort"                                     . 4)))
                   (read-minibuffer "Trailing whitespaces alert! 1->remove,save,vc-next; 2->remove,edit; 3->edit; 4->abort?")))
          ((1)
           (let ((delete-trailing-lines t))
             (delete-trailing-whitespace))
           (save-buffer)
           (vc-next-action nil))
          ((2) (let ((delete-trailing-lines t))
                 (delete-trailing-whitespace)))
          ((3))
          ((4) (kill-buffer)))))))

(defun pjb-before-save-meat/delete-trailing-whitespace ()
  "Meat for before-save-hook: delete trailing whitespace."
  (when (and (not (member major-mode *pjb-binary-modes*))
             (trailing-whitespace-candidate-p (buffer-file-name)))
    (let ((delete-trailing-lines t))
      (delete-trailing-whitespace (point-min) (point-max)))))

(add-hook 'find-file-hook   'pjb-find-file-meat/warn-trailing-whitespace)
(add-hook 'before-save-hook 'pjb-before-save-meat/delete-trailing-whitespace)

(defun pjb-find-file-meat/log ()
  "Log the path of the visited file."
  (message "find-file %S" (buffer-file-name)))

(defun pjb-save-buffer-meat/log ()
  "Log the path of the killed file."
  (when (buffer-file-name)
    (message "save-buffer %S" (buffer-file-name))))

(defun pjb-kill-buffer-meat/log ()
  "Log the path of the killed file."
  (when (buffer-file-name)
    (message "kill-buffer %S" (buffer-file-name))))

(add-hook 'find-file-hook   'pjb-find-file-meat/log)
(add-hook 'after-save-hook  'pjb-save-buffer-meat/log)
(add-hook 'kill-buffer-hook 'pjb-kill-buffer-meat/log)

(add-hook 'after-save-hook  'executable-make-buffer-file-executable-if-script-p)

;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook 'endless/colorize-compilation)


;; (setf org-finish-function 'org-store-log-note)
;; org-finish-function
;; org-store-log-note
;; <YoungFrog> igam: In org-add-log-note there is (org-set-local 'org-finish-function
;; 'org-store-log-note). I'd try adding a (message "Curbuf %S" (current-buffer)) in there to
;; see in which buffer that goes... probably not the Org Note buffer as it should be.

;;  irc://artigue.org.es
(cd (user-homedir-pathname))
;; (slime)

(ignore-errors (progn (setf *pjb-current-font-index* 4) (set-current-font)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (latex . t)
   (dot . t)
   (plantuml . t)
   (shell . t)))

(setq org-plantuml-jar-path
      (expand-file-name "/opt/local/share/java/plantuml.jar"))


(require 'cmake-mode nil t)
(setq auto-mode-alist (append auto-mode-alist
                              '(("CMakeLists.txt$" . cmake-mode))))


(when (require 'projectile nil t)
  (setf projectile-keymap-prefix (kbd "C-c p"))
  (define-key projectile-mode-map  (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode))


(defun gnus (&rest ignored)
  (interactive)
  (error "We don't have a NNTP server anymore."))

(require 'dired)
(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-omit-mode)


(defun show-tab-stuff ()
  (interactive)
  (dolist (var '(tab-width
                 tab-stop-list
                 c-basic-offset
                 indent-tabs-mode
                 tab-always-indent
                 c-tab-always-indent
                 show-trailing-whitespace
                 whitespace-mode))
    (message "%S = %S" var (symbol-value var)))
  (switch-to-buffer "*Messages*"))


(add-to-load-path "~/.emacs.d/site-lisp/with-editor")
(add-to-load-path "~/.emacs.d/site-lisp/magit/lisp")

(require 'with-editor)
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(require 'magit)

(when (file-exists-p "~/rc/emacs-patches.el")
  (load "~/rc/emacs-patches.el"))
(load "~/rc/emacs-epilog.el")

;; (customize-save-variable 'erc-autojoin-channels-alist erc-autojoin-channels-alist)

(setq org-element-use-cache nil)

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:




