(unless (featurep 'emacs-pjb)   (load "~/rc/emacs-pjb.el"))
(unless (featurep 'emacs-linux) (load "~/rc/emacs-linux.el"))

(require 'cl)
(require 'sh-script)
(require 'shell)
(require 'cc-cmds)
(require 'simple)
(require 'inf-ruby)

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
 '(browse-url-browser-function 'browse-url-default-browser)
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
 '(copilot-idle-delay 3)
 '(copilot-node-executable "/home/pbourguignon/opt/ubuntu-22.04/bin/node")
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
 '(erc-autojoin-channels-alist '(("libera.chat" "#lisp" "#commonlisp" "##lispm" "#interlisp" "#clschool" "#lisp-es" "#abcl" "#ccl" "#ecl" "#sicl" "#clim" "#clcs" "#slime" "#lisppub" "#lispweb" "#lispgames" "#common-lisp.net" "#nyxt" "#cl-naive" "#clergo" "#emacs" "#org-mode" "#erc" "#gnus" "#hn" "#space" "#emacsfr-off") ("freenode.org" "##Freelancers" "##binutils" "##cinema" "##coders" "##coding" "##france" "#lisp" "##lisp" "##lispm" "##sci-fi" "##smalltalk" "##tesla" "##teslamotors" "##teslamotors-staging" "##trains" "##ufo" "##workingset" "#abcl" "#bourguinux" "#ccl" "#cl" "#cl-bodge" "#clcs" "#clim" "#clschool" "#clus" "#cobol" "#cpp" "#cuda" "#ecl" "#emacs" "#emacsconf" "#emacsfr-off" "#haskell" "#haskell-fr" "#hn" "#iphonedev" "#iphonedev-chat" "#lisp-es" "#lispcafe" "#lispgames" "#lispweb" "#lispx" "#macintosh" "#macos9lives" "#qlisp" "#sicl" "#sicp" "#slime" "#space" "#swift-lang" "#synthesizers" "#tesla") ("disabled.freenode.org" "#macports") ("esper.net" "#SpaceX") ("sbde.fr" "#laboite" "#sbde") ("oftc.net" "#uml")))
 '(erc-autojoin-delay 60)
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
 '(erc-hide-list nil)
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
 '(erc-networks-alist '((libera "libera.chat") (4-irc "4-irc.com") (A5KNet "a5knet.com") (AbleNet "ablenet.org") (Accessirc "accessirc.net") (Acestar "acestar.org") (Action-IRC "action-irc.net") (AfterNET "afternet.org") (Alternativenet "altnet.org") (AmigaNet "amiganet.org") (AngelEyez "angeleyez.net") (Anothernet "another.net") (ArabChat "arabchat.org") (Ars "arstechnica.com") (AsiaTalk "asiatalk.org") (AstroLink "astrolink.org") (Asylumnet "asylumnet.org") (Austnet "austnet.org") (AwesomeChat "awesomechat.net") (Awesomechristians "awesomechristians.com") (Axenet "axenet.org") (Beyondirc "beyondirc.net") (BGIRC "bulgaria.org") (Blabbernet "blabber.net") (Blitzed "blitzed.org") (BrasIRC "brasirc.net") (BRASnet "brasnet.org") (BubbleNet "bubblenet.org") (CCnet "christian-chat.net") (Chat-Net "chat-net.org") (Chat-Solutions "chat-solutions.org") (Chatcafe "chatcafe.net") (Chatchannel "chatchannel.org") (ChatCircuit "chatcircuit.com") (Chatlink "chatlink.org") (Chatnet "chatnet.org") (ChatNut "chatnut.net") (Chatpinoy "chatpinoy.com") (ChatPR "chatpr.org") (Chatroom "chatroom.org") (Chatster "chatster.org") (ChatX "chatx.net") (China263 "263.net") (Cineplex1 "cineplex1.com") (CNN "cnn.com") (CobraNet "cobra.net") (Coolchat "coolchat.net") (Criten "criten.net") (Cyberchat "cyberchat.org") (CyGanet "cyga.net") (DALnet "dal.net") (Dark-Tou-Net "d-t-net.de") (Darkfire "darkfire.net") (DarkMyst "darkmyst.org") (Darkserv "darkserv.net") (Darksystem "darksystem.com") (Darktree "darktree.net") (DayNet "daynet.org") (Deepspace "deepspace.org") (Different "different.net") (Digarix "digarix.net") (Digatech "digatech.net") (Digital-Base "digital-base.net") (Digitalirc "digitalirc.net") (Discussioni "discussioni.org") (DorukNet "doruk.net.tr") (DWChat "dwchat.net") (Dynastynet "dynastynet.net") (EFnet nil) (EgyptianIRC "egyptianirc.net") (Eircnet "eircnet.org") (Eleethal "eleethal.com") (EntertheGame "enterthegame.com") (EpiKnet "epiknet.org") (EsperNet "esper.net") (Esprit "esprit.net") (euIRC "euirc.net") (Evilzinc "evilzinc.net") (ExodusIRC "exodusirc.net") (FDFnet "fdfnet.net") (FEFnet "fef.net") (Financialchat "financialchat.com") (Forestnet "forestnet.org") (ForeverChat "foreverchat.net") (Fraggers "fraggers.co.uk") (FreedomChat "freedomchat.net") (FreedomIRC "freedomirc.net") (freenode "freenode.net") (FunNet "funnet.org") (GalaxyNet "galaxynet.org") (Gamesnet "gamesnet.net") (GammaForce "gammaforce.org") (GIKInet "giki.edu.pk") (GizNet "giznet.org") (Globalchat "globalchat.org") (GlobIRC "globirc.net") (Goldchat "goldchat.nl") (Goodchatting "goodchatting.com") (GravityLords "gravitylords.net") (GRnet "irc.gr") (GulfChat "gulfchat.net") (HabberNet "habber.net") (HanIRC "hanirc.org") (Hellenicnet "mirc.gr") (IceNet "icenet.org.za") (ICQnet "icq.com") (iip "anon.iip") (Infatech "infatech.net") (Infinity "infinity-irc.org") (Infomatrix "infomatrix.net") (Inside3D "inside3d.net") (InterlinkChat "interlinkchat.net") (IRC-Chile "irc.cl") (IRC-Hispano "irc-hispano.org") (IRCchat "ircchat.tk") (IRCGate "ircgate.net") (IRCGeeks "ircgeeks.org") (IRChat "irchat.net") (IrcLordz "irclordz.com") (IrcMalta "ircmalta.org") (IRCnet nil) (IRCSoulZ "ircsoulz.net") (IRCSul "wnet.com.br") (IrcTalk "irctalk.net") (Irctoo "irctoo.net") (IRCtown "irc.irctown.net") (IRCworld "ircworld.org") (ircXtreme "ircXtreme.net") (Israelnet "israel.net") (K0wNet "k0w.net") (KDFSnet "kdfs.net") (Kemik "kemik.net") (Kewl\.Org "kewl.org") (Kickchat "kickchat.com") (KidsWorld "kidsworld.org") (Knightnet "knightnet.net") (Konfido\.Net "konfido.net") (Kreynet "krey.net") (Krono "krono.net") (Krushnet "krushnet.org") (LagNet "lagnet.org.za") (Librenet "librenet.net") (LinkNet "link-net.org") (LinuxChix "cats\\.meow\\.at\\|linuxchix\\.org") (Liquidized "liquidized.net") (M-IRC "m-sys.org") (MagicStar "magicstar.net") (Mavra "mavra.net") (MediaDriven "mediadriven.com") (mIRC-X "mircx.com") (Morat "morat.net") (MusicCity "musiccity.com") (Mysteria "mysteria.net") (Mysterychat "mysterychat.net") (Mystical "mystical.net") (Narancs "narancs.com") (Net-France "net-france.com") (Nevernet "nevernet.net") (Newnet "newnet.net") (Nexusirc "nexusirc.org") (NightStar "nightstar.net") (NitrousNet "nitrousnet.net") (Novernet "novernet.com") (Nullrouted "nullrouted.org") (NullusNet "nullus.net") (OFTC "oftc.net") (OpChat "opchat.org") (Openprojects "openprojects.net") (Othernet "othernet.org") (OtherSide "othersideirc.net") (Outsiderz "outsiderz.com") (OzOrg "oz.org") (Peacefulhaven "peacefulhaven.net") (PhazedIRC "phazedirc.net") (Philchat "philchat.net") (phrozN "phrozn.net") (PiNet "praetorians.org") (Pinoycentral "abs-cbn.com") (Planetarion "planetarion.com") (POLNet "ircnet.pl") (Psionics "psionics.net") (PTirc "ptirc.com.pt") (PTlink "ptlink.net") (PTnet "ptnet.org") (QChat "qchat.net") (QuakeNet "quakenet.org") (Realirc "realirc.org") (RealmNET "realmnet.com") (Rebelchat "rebelchat.org") (Red-Latina "red-latina.org") (RedLatona "redlatona.net") (Relicnet "relic.net") (Rezosup "rezosup.org") (Risanet "risanet.com") (Rubiks "rubiks.net") (Rusnet "nil") (Sandnet "sandnet.net") (Scunc "scunc.net") (SerbianCafe "serbiancafe.ws") (SexNet "sexnet.org") (ShadowFire "shadowfire.org") (ShadowWorld "shadowworld.net") (SkyNet "bronowski.pl") (SlashNET "slashnet.org") (SolarStone "solarstone.net") (Sorcery "sorcery.net") (SourceIRC "sourceirc.net") (SpaceTronix "spacetronix.net") (Spirit-Harmony "spirit-harmony.com") (StarChat "starchat.net") (StarEquinox "starequinox.net") (Starlink "starlink.net") (starlink-irc "starlink-irc.org") (StarWars-IRC "starwars-irc.net") (Stormdancing "stormdancing.net") (Superchat "superchat.org") (Sysopnet "sysopnet.org") (Telstra "telstra.com") (TR-net "dominet.com.tr") (Tri-net "tri-net.org") (TriLink "ft4u.net") (TurkishChat "turkishchat.org") (UberNinja "uberninja.net") (UICN "uicn.net") (UltraIRC "ultrairc.net") (UnderChat "underchat.it") (Undernet "undernet.org") (UnderZ "underz.org") (UniChat "irc.uni-chat.net") (UnionLatina "unionlatina.org") (Univers "univers.org") (UnixR "unixr.net") (Vidgamechat "vidgamechat.com") (VirtuaNet "virtuanet.org") (Vitamina "vitamina.ca") (Voila "voila.fr") (Wahou "wf-net.org") (Warpednet "warped.net") (Weaklinks "weaklinks.net") (Webnet "webchat.org") (WinChat "winchat.net") (WinIRC "winirc.org") (WorldIRC "worldirc.org") (WyldRyde "wyldryde.net") (XentoniX "xentonix.net") (Xevion "xevion.net") (XNet "xnet.org") (XWorld "xworld.org") (ZAnetNet "zanet.net") (ZAnetOrg "zanet.org.za") (ZiRC "zirc.org") (ZUHnet "zuh.net") (Zurna "zurna.net")))
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
 '(erc-server-send-ping-interval 60)
 '(erc-server-send-ping-timeout 300)
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
 '(org-agenda-files '("~/rc/notes.txt" "~/works/sbde/todo.org" "~/works/sbde/smt/sources/README.org" "~/works/sbde/laboite/specifications/components.org" "~/works/sbde/laboite/analysis/controller.org" "~/works/manif/TODO.org" "~/works/synth/schmidt/todo.txt" "~/works/enolaba/macosx/src/TODO" "~/src/pjb/nasium-lse/ISSUES.txt" "~/works/patchwork/src/mclgui/TODO.org" "~/works/patchwork/src/patchwork/notes.txt" "~/works/abnotation/abnotation/todo.txt" "~/works/ops/TODO.org"))
 '(org-confirm-babel-evaluate nil)
 '(org-fontify-done-headline t)
 '(org-latex-remove-logfiles nil)
 '(org-startup-with-inline-images t)
 '(org-todo-keywords '((sequence "TODO(t@)" "IN-PROGRESS(p@)" "SUSPENDED(s@)" "|" "DONE(d@)" "CANCELED(c@)")))
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/")))
 '(package-selected-packages '(dts-mode use-package editorconfig compat with-editor transient dired-git-info docbook ox-gfm smalltalk-mode twittering-mode company-coq coq-commenter flycheck-swift flycheck-swift3 flycheck-swiftlint flycheck-swiftx ob-swift swift-helpful swift-playground-mode swift3-mode web-server pg polymode lsp-mode dash lsp-ui stack lsp-haskell stack-mode hyai hindent hi2 haskell-tab-indent haskell-snippets haskell-emacs-text haskell-emacs-base haskell-emacs flycheck-liquidhs flycheck-hdevtools flycheck-haskell flycheck-ghcmod dante ac-haskell-process ghci-completion ghc-imported-from ghc shm retrie ormolu intero htmlize cobol-mode swift-mode haskell-mode helm markdown-mode inf-ruby w3m popup json emms paredit textmate smartparens robe jdee highlight-indentation flycheck enh-ruby-mode dash-at-point company column-marker auto-complete))
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
 '(safe-local-variable-values '((whitespace-line-column . 80) (lsp-file-watch-ignored-directories "[/\\\\]\\.git$" "[/\\\\]\\..*cache$" "[/\\\\]\\.deps$" "[/\\\\]autom4te\\.cache$" "[/\\\\]build-aux$" "[/\\\\]doc[/\\\\]manual$" "[/\\\\]tests[/\\\\]work$" "[/\\\\]make-[0-9]") (ccls-initialization-options :index (:threads 6 :initialBlacklist ["/make-[0-9]" "tests/work/" "/\\.deps" "/\\..*cache" "/\\.git"])) (Package PPC32 :use CL) (Package PPC64 :use CL) (auto-fill-mode . 1) (eval setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist)) (org-latex-inputenc-alist ("utf8" . "utf8x")) (Package . FIVEAM) (Syntax . Ansi-Common-Lisp) (eval cl-flet ((enhance-imenu-lisp (&rest keywords) (dolist (keyword keywords) (let ((prefix (when (listp keyword) (cl-second keyword))) (keyword (if (listp keyword) (cl-first keyword) keyword))) (add-to-list 'lisp-imenu-generic-expression (list (purecopy (concat (capitalize keyword) (if (string= (substring-no-properties keyword -1) "s") "es" "s"))) (purecopy (concat "^\\s-*(" (regexp-opt (list (if prefix (concat prefix "-" keyword) keyword) (concat prefix "-" keyword)) t) "\\s-+\\(" lisp-mode-symbol-regexp "\\)")) 2)))))) (enhance-imenu-lisp '("bookmarklet-command" "define") '("class" "define") '("command" "define") '("ffi-method" "define") '("ffi-generic" "define") '("function" "define") '("internal-page-command" "define") '("internal-page-command-global" "define") '("mode" "define") '("parenscript" "define") "defpsmacro")) (whitespace-mode . 1) (c-tab-always-indent . t) (smie-indent-basic . 8) (Package . USOCKET-TEST) (eval put 'report-error 'fi:common-lisp-indent-hook 1) (package . net\.aserve\.test) (package . net\.aserve\.client) (package . net\.aserve) (Package . User) (Package . GRAPH) (Package . DWIM) (pretty-greek) (package . common\.surveille-host) (view-mode . t) (Package X86 :use CL) (Package . TRIVIAL-GRAY-STREAMS) (Log . clx\.log) (Package . Xlib) (Package . SYSTEM) (Package . CLIM-POSTSCRIPT) (Package . MCCLIM-TRUETYPE) (Package nstep) (Package discrete-walk) (Package bandits :use (common-lisp ccl ut)) (Package cl-user) (Package rss-utilities :use (common-lisp ccl) :nicknames (:ut)) (mode:scheme:mode . paredit) (Package . USOCKET) (Package . BORDEAUX-THREADS) (Syntax . ANSI-Common-lisp) (flycheck . -1) (flycheck-mode . -1) (package . user) (eval cl-indent 'for-all-cell 1) (Package JPEG :use (common-lisp)) (org-plantuml-jar-path . "../tools/plantuml.jar") (org-plantuml-jar-path expand-file-name "../tools/plantuml.jar") (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "SUSPENDED(s@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . com\.informatimago\.languages\.linc\.c) (eval cl-indent 'define-function 3) (eval cl-indent 'define-type 1) (eval cl-indent 'define-module 1) (eval cl-indent 'in-parens 1) (c-file-offsets (innamespace . 0)) (eval add-hook 'prog-mode-hook (lambda nil (whitespace-mode 1)) (not :APPEND) :BUFFER-LOCAL) (swift-basic-offset . 2) (whitespace-style face lines indentation:space) (eval let* ((x (dir-locals-find-file default-directory)) (this-directory (if (listp x) (car x) (file-name-directory x)))) (unless (or (featurep 'swift-project-settings) (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p this-directory))) (add-to-list 'load-path (concat this-directory "utils") :append) (let ((swift-project-directory this-directory)) (require 'swift-project-settings))) (set (make-local-variable 'swift-project-directory) this-directory)) (Package . CHUNGA) (Package ARCH :use CL) (Package . CL-UNICODE) (eval set-input-method 'latin-1-prefix) (org-confirm-babel-evaluate lambda (lang body) (not (string= lang "dot"))) (Base . 10) (Log . hemlock\.log) (Lowercase . T) (Lowercase . Yes) (Package . CLIM-INTERNALS) (Package . LET-OVER-LAMBDA) (Package . CL-FAD) (Package . FUTURE-COMMON-LISP-USER) (Package . WORDNET-INTERFACE) (Package . WORDNET) (Package . Lisp-Binary) (Package . hemlock-internals) (Package . INSPECTOR) (Package . SURF) (Package . bind) (Package ARM :use CL) (Package ANSI-LOOP "COMMON-LISP") (Package . BORDEAUX-FFT) (Package . XLIB) (Package . gambol) (Package . C) (Package . Hemlock) (Package . GUI) (Package . cl-user) (Package . Portable-Threads-System) (Package . LEXER) (Package . F2CL) (Package . DRAKMA) (Package . CL-WHO) (Package . LISP-UNIT) (Package . Hemlock-Internals) (Package . FLEXI-STREAMS) (Package . COMMON-LISP-USER) (Package . PS) (Package . SCHEME-TRANSLATOR) (Package DATABASE :USE LISP) (Package . HUNCHENTOOT) (Package . CLEVER-LOAD) (Package . REVISED^4-SCHEME) (Package . CL-USER) (Package X8664 :use CL) (Package . SERROR) (Package . CL-PPCRE) (Package . CCL) (Syntax . common-lisp) (Syntax . Common-Lisp) (Syntax . Common-lisp) (Syntax . COMMON-LISP) (Syntax . ANSI-Common-Lisp) (bug-reference-url-format . clisp-bug-reference-url-format) (c-file-style . ruby) (c-indent-level . 4) (compile-cmd . "gcc -DMODULE -Wall -Wstrict-prototypes -O6 -c natsemi.c") (default-input-method . latin-1-prefix) (electric-indent-mode) (eval activate-input-method 'latin-1-prefix) (eval add-hook 'before-save-hook 'time-stamp) (eval let ((inhibit-read-only t) (compilation-filter-start (point-min))) (save-excursion (goto-char (point-max)) (grep-filter) (set-buffer-modified-p nil))) (eval cl-indent 'defmeth 3) (eval buttonize-buffer) (eval cl-indent 'cvm-do-symbols 1) (eval cl-indent 'cvm-dolist 1) (eval cl-indent 'cvm-define-structure 2) (eval cl-indent 'raw-memory:WITH-SIGSEG-HANDLER 0) (eval cl-indent 'when-debug 1) (eval cl-indent 'with-generation 1) (eval cl-indent 'with-gc-lock 0) (eval cl-indent 'define-common-structure 1) (eval cl-indent 'defenum 1) (eval put 'let-errno 'common-lisp-indent-function 1) (eval cl-indent 'defbf 2) (eval cl-indent 'ffi:with-c-place 1) (eval cl-indent 'xlib:event-case '((&whole 6 1 1 1 1 1 1) &rest (&whole 2 1 1 1 1 1 1 1 1 1 1 1))) (eval when (fboundp 'asm7090) (asm7090)) (eval cl-indent 'defcommand 3) (eval progn (local-set-key (kbd "<kp-divide>") #'lisp-indent-line) (local-set-key (kbd "<kp-multiply>") (lambda nil (interactive) (insert (kbd "SPC")))) (local-set-key (kbd "<XF86_Ungrab>") #'backward-delete-char-untabify)) (eval cl-indent 'dolist/separator 1) (eval put 'define-structure 'common-lisp-indent-function 1) (flycheck-mode) (lexical-binding . t) (more . org) (org-fontify-done-headline . t) (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . net\.mail) (package . net\.post-office) (package . rune-dom) (package . WILBUR) (sentence-end-double-space . t) (tab-always-indent . t) (tab-always-indent) (view-mode t) (Package . "CCL") (syntax . COMMON-LISP) (Package . April) (Package . USOCKET-TEST) (eval put 'report-error 'fi:common-lisp-indent-hook 1) (package . net\.aserve\.test) (package . net\.aserve\.client) (package . net\.aserve) (Package . User) (Package . GRAPH) (Package . DWIM) (pretty-greek) (package . common\.surveille-host) (view-mode . t) (Package X86 :use CL) (Package . TRIVIAL-GRAY-STREAMS) (Log . clx\.log) (Package . Xlib) (Package . SYSTEM) (Package . CLIM-POSTSCRIPT) (Package . MCCLIM-TRUETYPE) (Package nstep) (Package discrete-walk) (Package bandits :use (common-lisp ccl ut)) (Package cl-user) (Package rss-utilities :use (common-lisp ccl) :nicknames (:ut)) (mode:scheme:mode . paredit) (Package . USOCKET) (Package . BORDEAUX-THREADS) (Syntax . ANSI-Common-lisp) (flycheck . -1) (flycheck-mode . -1) (package . user) (eval cl-indent 'for-all-cell 1) (Package JPEG :use (common-lisp)) (org-plantuml-jar-path . "../tools/plantuml.jar") (org-plantuml-jar-path expand-file-name "../tools/plantuml.jar") (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "SUSPENDED(s@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . com\.informatimago\.languages\.linc\.c) (eval cl-indent 'define-function 3) (eval cl-indent 'define-type 1) (eval cl-indent 'define-module 1) (eval cl-indent 'in-parens 1) (c-file-offsets (innamespace . 0)) (eval add-hook 'prog-mode-hook (lambda nil (whitespace-mode 1)) (not :APPEND) :BUFFER-LOCAL) (swift-basic-offset . 2) (whitespace-style face lines indentation:space) (eval let* ((x (dir-locals-find-file default-directory)) (this-directory (if (listp x) (car x) (file-name-directory x)))) (unless (or (featurep 'swift-project-settings) (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p this-directory))) (add-to-list 'load-path (concat this-directory "utils") :append) (let ((swift-project-directory this-directory)) (require 'swift-project-settings))) (set (make-local-variable 'swift-project-directory) this-directory)) (Package . CHUNGA) (Package ARCH :use CL) (Package . CL-UNICODE) (eval set-input-method 'latin-1-prefix) (org-confirm-babel-evaluate lambda (lang body) (not (string= lang "dot"))) (Base . 10) (Log . hemlock\.log) (Lowercase . T) (Lowercase . Yes) (Package . CLIM-INTERNALS) (Package . LET-OVER-LAMBDA) (Package . CL-FAD) (Package . FUTURE-COMMON-LISP-USER) (Package . WORDNET-INTERFACE) (Package . WORDNET) (Package . Lisp-Binary) (Package . hemlock-internals) (Package . INSPECTOR) (Package . SURF) (Package . bind) (Package ARM :use CL) (Package ANSI-LOOP "COMMON-LISP") (Package . BORDEAUX-FFT) (Package . XLIB) (Package . gambol) (Package . C) (Package . Hemlock) (Package . GUI) (Package . cl-user) (Package . Portable-Threads-System) (Package . LEXER) (Package . F2CL) (Package . DRAKMA) (Package . CL-WHO) (Package . LISP-UNIT) (Package . Hemlock-Internals) (Package . FLEXI-STREAMS) (Package . COMMON-LISP-USER) (Package . PS) (Package . SCHEME-TRANSLATOR) (Package DATABASE :USE LISP) (Package . HUNCHENTOOT) (Package . CLEVER-LOAD) (Package . REVISED^4-SCHEME) (Package . CL-USER) (Package X8664 :use CL) (Package . SERROR) (Package . CL-PPCRE) (Package . CCL) (Syntax . common-lisp) (Syntax . Common-Lisp) (Syntax . Common-lisp) (Syntax . COMMON-LISP) (Syntax . ANSI-Common-Lisp) (bug-reference-url-format . clisp-bug-reference-url-format) (c-file-style . ruby) (c-indent-level . 4) (compile-cmd . "gcc -DMODULE -Wall -Wstrict-prototypes -O6 -c natsemi.c") (default-input-method . latin-1-prefix) (electric-indent-mode) (eval activate-input-method 'latin-1-prefix) (eval add-hook 'before-save-hook 'time-stamp) (eval let ((inhibit-read-only t) (compilation-filter-start (point-min))) (save-excursion (goto-char (point-max)) (grep-filter) (set-buffer-modified-p nil))) (eval cl-indent 'defmeth 3) (eval buttonize-buffer) (eval cl-indent 'cvm-do-symbols 1) (eval cl-indent 'cvm-dolist 1) (eval cl-indent 'cvm-define-structure 2) (eval cl-indent 'raw-memory:WITH-SIGSEG-HANDLER 0) (eval cl-indent 'when-debug 1) (eval cl-indent 'with-generation 1) (eval cl-indent 'with-gc-lock 0) (eval cl-indent 'define-common-structure 1) (eval cl-indent 'defenum 1) (eval put 'let-errno 'common-lisp-indent-function 1) (eval cl-indent 'defbf 2) (eval cl-indent 'ffi:with-c-place 1) (eval cl-indent 'xlib:event-case '((&whole 6 1 1 1 1 1 1) &rest (&whole 2 1 1 1 1 1 1 1 1 1 1 1))) (eval when (fboundp 'asm7090) (asm7090)) (eval cl-indent 'defcommand 3) (eval progn (local-set-key (kbd "<kp-divide>") #'lisp-indent-line) (local-set-key (kbd "<kp-multiply>") (lambda nil (interactive) (insert (kbd "SPC")))) (local-set-key (kbd "<XF86_Ungrab>") #'backward-delete-char-untabify)) (eval cl-indent 'dolist/separator 1) (eval put 'define-structure 'common-lisp-indent-function 1) (flycheck-mode) (lexical-binding . t) (more . org) (org-fontify-done-headline . t) (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "|" "DONE(d@)" "CANCELED(c@)")) (package . net\.mail) (package . net\.post-office) (package . rune-dom) (package . WILBUR) (sentence-end-double-space . t) (tab-always-indent . t) (tab-always-indent) (view-mode t)))
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
 '(warning-suppress-log-types '((emacs) (emacs) (emacs) (#1=(flycheck syntax-checker)) (#1#) (undo discard-info)))
 '(warning-suppress-types '((emacs) (emacs) (emacs) (#1=(flycheck syntax-checker)) (#1#) (undo discard-info)))
 '(weechat-color-list '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
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
 '(erc-default-face ((t (:foreground "MediumPurple1"))))
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
 '(slime-repl-output-face ((t (:foreground "yellow green"))))
 '(window-divider ((t (:background "yellow" :foreground "yellow" :underline nil)))))



(defvar *pjb-force-linux-tabulation* nil)
(pushnew '(include "^/build/") *pjb-force-linux-tabulation* :test (function equal))



(defun redbend--comint-output-filter--remove-esc-b (string)
  "Remove ESC(B sequences between `comint-last-output-start’ and `process-mark’."
  (save-excursion
   (goto-char comint-last-output-start)
   (let ((end (process-mark (get-buffer-process (current-buffer)))))
     (while (search-forward "\e(B" end t)
       (delete-region (match-beginning 0) (match-end 0))))))

(defun redbend--shell-mode-meat ()
  (set-variable 'tab-width 8)
  (setf comint-process-echoes nil)
  (add-to-list 'comint-output-filter-functions #'redbend--comint-output-filter--remove-esc-b)
  ;; (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (when (fboundp 'ansi-color-for-comint-mode-on) (ansi-color-for-comint-mode-on))
  (when (fboundp 'bash-completion-setup) (bash-completion-setup))
  (set-default 'shell-dirstack-query "pwd"))

(add-hook 'shell-mode-hook 'redbend--shell-mode-meat)

(setf org-agenda-files '("~/pjb/notes.org"))


(require 'subr-x)

(defun compare-ruby-objects (start end)
  (interactive "r")
  (let ((stuff
          (concat
           "printf \"\\n\\n\" ; print_differences("
           (mapconcat 'identity
                      (mapcar (lambda (line)
                                (if (or (prefixp "-" line) (prefixp "+" line))
                                    (subseq line 1)
                                    line))
                              (split-string (buffer-substring start end) "\n" :omit-nulls))
                      "," )
           ")")))
    (with-current-buffer (get-buffer-create " *ruby-scratch*")
      (erase-buffer)
      (progn
        (insert stuff)
        (goto-char (point-min))
        (while (re-search-forward "=>" nil t)
          (delete-region (match-beginning 0) (match-end 0))
          (insert " => ")
          (forward-char 4))
        (ruby-send-region-and-go (point-min) (point-max))
        (ruby-print-result)))))


(global-set-key (kbd"M-TAB") 'suspend-frame)

;; set binding in sh-mode-map:
;; (define-key sh-mode-map (kbd "TAB") 'sh-indent-line)
(define-key sh-mode-map (kbd "TAB") 'indent-for-tab-command)




(c-add-style
 "harman.hypervisor"
 '((c-backslash-column             .  72)

   (c-backslash-max-column         . 128)

   (c-basic-offset                 . 4)  ; Amount of basic offset used by `+' and
                                        ; `-' symbols in `c-offsets-alist'.

   (c-block-comment-prefix         .  "")

   (c-cleanup-list                 . (brace-else-brace
                                      brace-elseif-brace
                                      brace-catch-brace
                                      list-close-comma
                                      scope-operator))

   (c-comment-only-line-offset     . (0 . 0))

   ;; (c-comment-prefix-regexp)

   (c-doc-comment-style            . javadoc)

   (c-hanging-braces-alist
    . (
       ;; (statement-cont)
       ;; (brace-list-intro)
       (inexpr-class-open    . (after))
       (inexpr-class-close   . (before))

       (defun-open           . (after))         ; Brace that opens a function definition.
       (defun-close          . (before))        ; Brace that closes a function definition.
       (class-open           . (before after))  ; Brace that opens a class definition.
       (class-close          . ())              ; Brace that closes a class definition.
       (inline-open          . (before after))  ; Brace that opens an in-class inline method.
       (inline-close         . (before))        ; Brace that closes an in-class inline method.
       (block-open           . ())              ; Statement block open brace.
       (block-close          . c-snug-do-while) ; Statement block close brace.
       (brace-list-open      . (after))         ; Open brace of an enum or static array list.
       (brace-list-close     . (before after))  ; Close brace of an enum or static array list.
       (brace-entry-open     . (before after))  ; Subsequent lines in an enum or static array
       (statement-case-open  . (after))         ; The first line in a case block starting with brace.
       (substatement-open    . (after))         ; The brace that opens a substatement block.
       (extern-lang-open     . (after))         ; Brace that opens an "extern" block.
       (extern-lang-close    . (before after))  ; Brace that closes an "extern" block.
       (namespace-open       . (after))
       (namespace-close      . ())
       (module-open          . (after))
       (module-close         . ())
       (composition-open     . (after))
       (composition-close)   . ()))

   (c-hanging-colons-alist         . ((case-label           . (after))
                                      (label                . (after))
                                      (access-label         . (after))
                                      (member-init-intro    . ())
                                      (inher-intro          . ())))

   (c-hanging-semi&comma-criteria  . ())

   ;; (c-indent-comment-alist)

   (c-indent-comments-syntactically-p . nil)

   (c-label-minimum-indentation       . 2)

   (c-offsets-alist
    . (

       (string                . 0)
       ;; Inside multi-line string.

       (c                     . 0)
       ;; Inside a multi-line C style block comment.

       (defun-open            . -5)
       ;; Brace that opens a function definition.

       (defun-close           . -5)
       ;; Brace that closes a function definition.

       (defun-block-intro     . +)
       ;; The first line in a top-level defun.

       (class-open            . 0)
       ;; Brace that opens a class definition.

       (class-close           . 0)
       ;; Brace that closes a class definition.

       (inline-open           . 0)
       ;; Brace that opens an in-class inline method.

       (inline-close          . 0)
       ;; Brace that closes an in-class inline method.

       (func-decl-cont        . +)
       ;; The region between a function definition's
       ;; argument list and the function opening brace
       ;; (excluding K&R argument declarations).  In C, you
       ;; cannot put anything but whitespace and comments
       ;; between them; in C++ and Java, throws declarations
       ;; and other things can appear in this context.

       (knr-argdecl-intro     . +)
       ;; First line of a K&R C argument declaration.

       (knr-argdecl           . +)
       ;; Subsequent lines in a K&R C argument declaration.

       (topmost-intro         . 5)
       ;; The first line in a topmost construct definition.

       (topmost-intro-cont    . (c-lineup-string-cont
                                 -5))
       ;; Topmost definition continuation lines.

       (member-init-intro     . +)
       ;; First line in a member initialization list.

       (member-init-cont      . ++)
       ;; Subsequent member initialization list lines.

       (inher-intro           . +)
       ;; First line of a multiple inheritance list.

       (inher-cont            . ++)
       ;; Subsequent multiple inheritance lines.

       (block-open            . 0)
       ;; Statement block open brace.

       (block-close           . 0)
       ;; Statement block close brace.

       (brace-list-open       . 0)
       ;; Open brace of an enum or static array list.

       (brace-list-close      . 0)
       ;; Close brace of an enum or static array list.

       (brace-list-intro      . +)
       ;; First line in an enum or static array list.

       (brace-list-entry      . 0)
       ;; Subsequent lines in an enum or static array list.

       (brace-entry-open      . 0)
       ;; Subsequent lines in an enum or static array
       ;; list that start with an open brace.

       (statement             . (c-lineup-runin-statements
                                 0))
       ;; A C (or like) statement.

       (statement-cont        . (c-lineup-string-cont
                                 ++))
       ;; A continuation of a C (or like) statement.

       (statement-block-intro . +)
       ;; The first line in a new statement block.

       (statement-case-intro  . +)
       ;; The first line in a case "block".

       (statement-case-open   . +)
       ;; The first line in a case block starting with brace.

       (substatement          . +)
       ;; The first line after an if/while/for/do/else.

       (substatement-open     . 0)
       ;; The brace that opens a substatement block.

       (substatement-label    . /)
       ;; Labelled line after an if/while/for/do/else.

       (case-label            . *)
       ;; A "case" or "default" label.

       (access-label          . -)
       ;; C++ private/protected/public access label.

       (label                 . /)
       ;; Any ordinary label.

       (do-while-closure      . 0)
       ;; The "while" that ends a do/while construct.

       (else-clause           . 0)
       ;; The "else" of an if/else construct.

       (catch-clause          . 0)
       ;; The "catch" or "finally" of a try/catch construct.

       (comment-intro         . 0)
       ;; A line containing only a comment introduction.

       (arglist-intro         . (c-lineup-arglist-intro-after-paren
                                 +))
       ;; The first line in an argument list.

       (arglist-cont          . (c-lineup-string-cont
                                 c-lineup-arglist-intro-after-paren
                                 c-lineup-argcont
                                 +))
       ;; Subsequent argument list lines when no
       ;; arguments follow on the same line as the
       ;; arglist opening paren.

       (arglist-cont-nonempty . (c-lineup-string-cont
                                 c-lineup-arglist-intro-after-paren
                                 c-lineup-argcont
                                 +))
       ;; Subsequent argument list lines when at
       ;; least one argument follows on the same
       ;; line as the arglist opening paren.

       (arglist-close         . (c-lineup-argcont
                                 c-lineup-arglist-intro-after-paren
                                 -))
       ;; The solo close paren of an argument list.

       (stream-op             . (c-lineup-streamop +))
       ;; Lines continuing a stream operator construct.

       (inclass               . +)
       ;; The construct is nested inside a class definition.
       ;; Used together with e.g. `topmost-intro'.

       (cpp-macro             . [0])
       ;; The start of a C preprocessor macro definition.

       (cpp-macro-cont        . [8])
       ;; Inside a multi-line C preprocessor macro definition.

       (friend                . 0)
       ;; A C++ friend declaration.

       (objc-method-intro     . 0)
       ;; The first line of an Objective-C method definition.

       (objc-method-args-cont . (c-lineup-ObjC-method-args-2
                                 +))
       ;; Lines continuing an Objective-C method definition.

       (objc-method-call-cont . (c-lineup-ObjC-method-call-colons
                                 c-lineup-ObjC-method-call
                                 +))
       ;; Lines continuing an Objective-C method call.

       (extern-lang-open      . 0)
       ;; Brace that opens an "extern" block.

       (extern-lang-close     . 0)
       ;; Brace that closes an "extern" block.

       (inextern-lang         . +)
       ;; Analogous to the `inclass' syntactic symbol,
       ;; but used inside "extern" blocks.

       (namespace-open        . 0)

       (namespace-close       . 0)

       (innamespace           . +)
       ;; Similar to the three `extern-lang' symbols, but for
       ;; C++ "namespace" blocks.

       (module-open           . 0)

       (module-close          . 0)

       (inmodule              . +)
       ;; Similar to the three `extern-lang' symbols, but for
       ;; CORBA IDL "module" blocks.

       (composition-open      . 0)

       (composition-close     . 0)

       (incomposition         . +)
       ;; Similar to the three `extern-lang' symbols, but for
       ;; CORBA CIDL "composition" blocks.

       (template-args-cont    . (c-lineup-template-args +))
       ;; C++ template argument list continuations.

       (inlambda              . +)
       ;; In the header or body of a lambda function.

       (lambda-intro-cont     . ++)
       ;; Continuation of the header of a lambda function.

       (inexpr-statement      . +)
       ;; The statement is inside an expression.

       (inexpr-class          . +)
       ;; The class is inside an expression.  Used e.g. for
       ;; Java anonymous classes.
       ))

   ;; Only called when c-syntactic-indentation is non nil.
   ;; (c-special-indent-hook . user-fun)
   (c-label-minimum-indentation    . 2)

   ;; other emacs variables:
   ;; (c-comment-continuation-stars "" t)
   ;; (c-echo-syntactic-information-p t)
   ;; (c-hanging-comment-ender-p nil t)
   ;; (c-hanging-comment-starter-p nil t)
   ;; (c-macro-shrink-window-flag          . t)


   (tab-width                      . 8)  ; the true one!

   (c-indent-level                 . 4)  ; Indentation of C statements with
                                        ; respect to containing block.

   (c-brace-imaginary-offset       . 0)  ; Imagined indentation of a C open brace
                                        ; that actually follows a statement.

   (c-brace-offset                 . 0)  ; Extra indentation for braces, compared
                                        ; with other text in same context.

   (c-argdecl-indent               . 4)  ; Indentation level of declarations of
                                        ; C function arguments.

   (c-label-offset                 . -)  ; Offset of C label lines and case
                                        ; statements relative to usual
                                        ; indentation.

   (c-continued-statement-offset   . 4)  ; Extra indent for lines not starting
                                        ; new statements.

   (c-continued-brace-offset       . 0)  ; Extra indent for substatements that
                                        ; start with open-braces.


   (c-auto-newline                . nil) ; Non-nil means automatically newline
                                        ; before and after braces, and after
                                        ; colons and semicolons, inserted in C
                                        ; code. If you do not want a leading
                                        ; newline before braces then use:
                                        ; (define-key c-mode-map \"{\"
                                        ;          'electric-c-semi)"

   (c-tab-always-indent           . t)  ; Non-nil means TAB in C mode should
                                        ; always reindent the current line,
                                        ; regardless of where in the line point
                                        ; is when the TAB command is used.
   ))


(defun harman-c-mode-meat ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (message "harman-c-mode-meat %S --> %S" filename
             (not (not (string-match "/hypervisor/" filename))))
    ;; Enable hypervisor mode for the appropriate files:
    (when (string-match "/hypervisor/" filename)
      (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
      (c-set-style "harman.hypervisor")
      (setq indent-tabs-mode t)
      (setq show-trailing-whitespace t)
      (local-set-key (kbd "TAB") (quote c-indent-or-tab)))))

(add-hook 'c-mode-hook 'harman-c-mode-meat)

(defvar *harman-wrap-signature*)

(defun harman-compilation-file-line-col ()
  (beginning-of-line)
  (when (re-search-forward "^\\([^:]*\\):\\([^:]*\\):\\([^:]*\\):" nil t)
    (list (match-string 1)
          (car (read-from-string (match-string 2)))
          (car (read-from-string (match-string 3))))))

(defun harman-wrap ()
  (interactive)
  (switch-to-buffer "*compilation*")
  (beginning-of-line)
  (message "point = %S" (point))
  (when (re-search-forward "'\\([^']*\\)\\(([^']*)\\)'" nil t)
    (let ((retype    (string-trim (match-string 1)))
          (signature (string-trim (match-string 2))))
      (setf *harman-wrap-signature* (cons retype signature))
      (compile-goto-error)
      (if (string= "void" retype)
          (insert "PWRAP(")
          (insert (format "FWRAP(%s, " retype)))
      (forward-sexp)
      (insert (format ", %s)" signature))
      (delete-other-windows (selected-window) t)
      (compilation-next-error)
      (message "%S" *harman-wrap-signature*))))

(defun harman-update-wrap ()
  (interactive)
  (message "harman-update-wrap start")
  (when (re-search-forward "\\(\\(FWRAP *( *\\([^,]*[^, ]\\) *,\\)\\|\\(PWRAP *(\\)\\) *\\([_a-zA-Z0-9]*\\) *, *\\(([][*&_a-zA-Z0-9, ]*)\\) *, *\\(([][*&_a-zA-Z0-9, ]*)\\)) *" nil t)
    (let ((sig-start  (match-beginning 6))
          (sig-end    (match-end 6))
          (par-end    (match-end 7)))
      (delete-region sig-end par-end)
      (let* ((signature   (buffer-substring-no-properties sig-start sig-end))
             (x (progn       (message "signature   = %S" signature)
                             (message "split  = %S" (split-string signature "," nil " "))
                             ))
             (replacement (concat  (mapconcat (lambda (parameter)
                                                (let ((name-start (position 32 parameter :from-end t)))
                                                  (if name-start
                                                      (string-trim (substring parameter 0 name-start))
                                                    parameter)))
                                              (split-string signature "," nil " ")
                                              ", ")
                                   ")")))
        (message "signature   = %S" signature)
        (message "replacement = %S" replacement)
        (delete-region sig-start sig-end)
        (goto-char sig-start)
        (insert replacement))))
    (message "harman-update-wrap end"))

(defun harman-flet ()
  (interactive)
  (backward-sexp 2)
  (let ((namepos (make-marker)))
    (set-marker namepos (point))
    (beginning-of-line)
    (indent-for-tab-command)
    (insert "FLET(")
    (goto-char namepos)
    (while (= 32 (char-before (point))) (backward-char))
    (insert ", ")
    (forward-sexp)
    (insert ", ")
    (forward-sexp)
    (insert ")")
    (let ((end (point)))
      (backward-sexp)
      (indent-region (point) end)
      (forward-sexp))
    (forward-sexp)
    (insert ";")))


(cl-defun harman-split-parameters (paramlist)
  "
The function takes a string with the following syntax:

    paramlist = '(' parameter { ',' parameter } ')' .
    parameter = { notcommaparen | paramlist } .
    notcommaparen = /[^,()]*/ .

and parses it, returning a list of strings and sublist.

Each notcommaparen is collected into a string, and  each paramlist
into a list, in the result.
"
  (labels ((eat-whitespace (pos)
             (loop
                   while (and (< pos (length paramlist))
                              (= 32 (aref paramlist pos)))
                   do (incf pos))
             pos)

           (parse-notcommaparen (pos)
             (loop
                   with start = pos
                   while (and (< pos (length paramlist))
                              (not (or (= ?\, (aref paramlist pos))
                                       (= ?\( (aref paramlist pos))
                                       (= ?\) (aref paramlist pos)))))
                   do (incf pos)
                   finally (return pos)))
           
           (parse-parameter (pos)
             (loop
                   with start = pos
                   while (not (or (= ?\, (aref paramlist pos))
                                  (= ?\) (aref paramlist pos))))
                   do (setf pos (if (= ?\( (aref paramlist pos))
                                    (second (parse-paramlist pos))
                                  (parse-notcommaparen pos)))
                   finally (return pos)))
           
           (parse-paramlist (pos)
             (let ((result '()))
               (setf pos (eat-whitespace pos))
               (unless (< pos (length paramlist))
                 (error "End of parameter list text reached while parsing a parameter list."))
               (unless (= ?\( (aref paramlist pos))
                 (error "Expected a '(' at position %d in %S" pos paramlist))
               (loop
                     do (let ((start (+ 1 pos)))
                          (setf pos (eat-whitespace start)) ; skip ( or ,
                          (unless (< pos (length paramlist))
                            (error "End of parameter list text reached while parsing a parameter list."))
                          (destructuring-bind end (parse-parameter pos)
                            (push (subseq paramlist start end) result)
                            (setf pos (eat-whitespace end))))
                     while (= ?\, (aref paramlist pos)))
               (unless (= ?\) (aref paramlist pos))
                 (error "Expected a ')' at position %d in %S" pos paramlist))
               (incf pos)
               (list (nreverse result)  pos))))

    (destructuring-bind (list pos) (parse-paramlist 0)
      (setf pos (eat-whitespace pos))
      (when (< pos (length paramlist))
        (error "Unexpected text at position %d in %S" pos paramlist))
      list)))

(assert (equal (list
                (harman-split-parameters "()")
                (harman-split-parameters "(int a)")
                (harman-split-parameters "(int a, char* b)")
                (harman-split-parameters "(int a, char* b, int (*foo)(int, int))")
                (harman-split-parameters "(int, foo, (int a, char* b, int (*foo)(int, int)))"))
               '(("") 
                 ("int a")
                 ("int a" " char* b")
                 ("int a" " char* b" " int (*foo)(int, int)")
                 ("int" " foo" " (int a, char* b, int (*foo)(int, int))"))))


(defun harman-marker (point)
  (let ((marker (make-marker)))
    (set-marker marker point)
    marker))

(defun harman-search-flet ()
  (when (re-search-forward "\\<FLET\\>" nil t)
    (let ((flet-start (match-beginning 0)))
      (forward-sexp)
      (let ((signature-end (point))
            (signature-start (prog1 (progn (backward-sexp) (point)) (forward-sexp))))
        (forward-sexp)
        (let ((body-end (point))
              (body-start (prog1 (progn (backward-sexp) (point)) (forward-sexp))))
          (when (looking-at "[ \t\n]*;")
            (delete-region (match-beginning 0) (match-end 0)))
          (message "flet-start: %d, signature-start: %d, signature-end: %d, body-start: %d, body-end: %d"
                   flet-start signature-start signature-end body-start body-end)
          (list flet-start
                (point)
                (buffer-substring-no-properties signature-start signature-end)
                (harman-marker body-start)
                (harman-marker body-end)))))))


(defun harman-collect-vars (start endm &optional new-prefix)
  (interactive "r")
  (goto-char start)
  (let ((new-prefix (or new-prefix "g_"))
        (vars '()))
    (while (re-search-forward "\\_<\\(VAR  *\\)\\(\\([^;=]*\\)\\_<\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t\n]*\\(=[^;]*\\)?;\\)" endm t)
      ;; get the position of VAR, the start of the declaration, and the end of the declaration
      ;; and the variable name.
      (let ((var-start (match-beginning 1))
            (decl-start (match-beginning 2))
            (decl-end (match-end 2))
            (var-name (match-string 4)))
        (push (list (concat new-prefix var-name)
                    var-name
                    (harman-marker var-start)
                    (harman-marker decl-start)
                    (harman-marker decl-end))
              vars)))
    (nreverse vars)))

(defun harman-rename-identifier (new-name old-name start endm)
  (let ((re       (concat "\\_<" old-name "\\_>"))
        (sizediff (- (length new-name) (length old-name))))
    (goto-char start)
    (while (re-search-forward re endm t)
      (let ((next (+ (match-end 0) sizediff)))
        (delete-region (match-beginning 0) (match-end 0))
        (goto-char (match-beginning 0))
        (insert new-name)
        (goto-char next)))))

(defun harman-move-before-doxygen-docstring ()
  (interactive)
  (let ((start (point)))
    (previous-line)
    (if (not (nth 4 (syntax-ppss)))
        (goto-char start)
      ;; we're in a comment
      (let ((doxygen-comment-start
             (when (progn (goto-char start)
                          (re-search-backward "/\\*\\*"))
               (match-beginning 0)))
            (c-comment-start                
             (when (progn (goto-char start)
                          (re-search-backward "/\\*"))
               (match-beginning 0))))
        (if (and c-comment-start
                 doxygen-comment-start
                 (<= c-comment-start doxygen-comment-start))
            ;; doxygen comment
            (goto-char doxygen-comment-start)
          (goto-char start))))))

(defun harman-make-flet-global ()
  (interactive)
  (let ((fendm  (make-marker))
        (startm (make-marker))
        (endm   (make-marker)))
    (unwind-protect
         (let ((flet (harman-search-flet)))
           (when flet
             (destructuring-bind (start end flet-parameters bstart bend) flet
               (unwind-protect
                    (destructuring-bind (retype lname signature) (harman-split-parameters flet-parameters)
                      (set-marker startm start)
                      (set-marker endm end)
                      (set-marker fendm (progn (c-end-of-defun) (point)))
                      (let* ((lname   (string-trim lname))
                             (fstart  (harman-marker (progn (c-beginning-of-defun) (point))))
                             (fname   (c-defun-name))
                             (gname   (concat fname "_" lname))
                             (newvars (harman-collect-vars fstart startm (concat fname "_"))))
                        (unwind-protect
                             (progn
                               (loop for (new-name old-name var-start decl-start decl-end) in newvars
                                     do (message "%s -> %s : %s" old-name new-name (buffer-substring-no-properties decl-start decl-end)))
                               
                               (harman-rename-identifier gname lname fstart fendm)

                               (loop for (new-name old-name var-start decl-start decl-end) in newvars
                                     do (harman-rename-identifier new-name old-name fstart fendm))
                               
                               (setf newvars (mapcar (lambda (var)
                                                       (destructuring-bind (new-name old-name var-start decl-start decl-end) var
                                                         (prog1 (buffer-substring-no-properties decl-start decl-end)
                                                           (delete-region var-start decl-end)
                                                           (set-marker var-start nil)
                                                           (set-marker decl-start nil)
                                                           (set-marker decl-end nil))))
                                                     newvars))

                               (message "markers=%S" (list fstart startm endm bstart bend fendm))
                               (let ((body (buffer-substring-no-properties bstart bend)))
                                 (message "body=%S" body)

                                 ;; delete the local function (FLET), AFTER.
                                 (delete-region startm endm)

                                 ;; move before docstring doc.
                                 (goto-char fstart)

                                 (harman-move-before-doxygen-docstring)
                                 (open-line 2)
                                 (let ((start (point)))
                                   ;; insert vars
                                   (loop for var in newvars
                                         do (insert "static " var "\n"))
                                   ;; insert the new global function
                                   (insert (format "     static %s\n%s%s\n%s\n"
                                                   retype gname signature body))
                                   (indent-region start (point))
                                   (goto-char fstart))))
                          ;; cleanup
                          (set-marker fstart nil)))))
               ;; cleanup
               (set-marker bstart nil)
               (set-marker bend nil)))))
    ;; cleanup
    (set-marker fendm nil)
    (set-marker startm nil)
    (set-marker endm nil)))



(defun harman-delete-diff-chunk ()
  "Delete the @@.*@@.*\n… block containing the point from the current diff."
  (interactive)
  (when (re-search-backward "^@@.*@@.*\n" nil t)
    (let ((start (match-beginning 0)))
      (let ((end-at (if (progn (goto-char start)
                               (forward-line 1)
                               (re-search-forward "^@@.*@@.*\n" nil t))
                        (match-beginning 0)
                      (point-max)))
            (end-diff (if (progn (goto-char start)
                                 (forward-line 1)
                                 (re-search-forward "^diff --git" nil t))
                          (match-beginning 0)
                        (point-max))))
        (delete-region start (min end-at end-diff))
        (goto-char start)
        (forward-line 1)))))

(progn
  (set-sources "/build/pbourguignon/clang/src.devel/hypervisor/" 'c)
  (add-sources "/build/pbourguignon/clang/work/build.devel/utest-nkernel/" 'c))


'("/mnt/BlobArea/external/ceedling/pyenv/bin"
  "/mnt/BlobArea/external/ceedling/lcov/lcov-1.14/bin"
  "/mnt/BlobArea/external/ceedling/rbenv/shims"
  )
(setf exec-path '("/home/pbourguignon/works/harman/Ceedling/bin"
                  "/home/pbourguignon/bin"
                  "/home/pbourguignon/.rbenv/shims"
                  "/home/pbourguignon/.local/bin"
                  "/usr/local/sbin"
                  "/usr/local/bin"
                  "/usr/sbin"
                  "/usr/bin"
                  "/usr/lib/rbenv/libexec"
                  "/sbin"
                  "/bin" ))


(setf inf-ruby-first-prompt-pattern "irb[^>]*> ")

(progn
  (cond
    ((string= (hostname) "frdark")
     (set-face-background 'fringe "yellow"))
    (t
     (set-face-background 'fringe "lightpink4")))
  ;; (set-frame-font "DejaVu Sans Mono-10")
  (set-frame-font "DejaVu Sans Mono-14")
  ;; (set-frame-font "DejaVu Sans Mono-18")
  ;; (set-foreground-color "turquoise1")
  (set-foreground-color "pale turquoise"))

;; --> x
;; (ff 1213)

(provide 'emacs-harman)
