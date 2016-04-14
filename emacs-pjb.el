;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file.

(load "~/rc/emacs-common.el")
(.EMACS "~/rc/emacs-pjb.el %s" "Pascal J. Bourguignon's emacs startup file.")
(require 'cc-mode)



;;;----------------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------------


(.EMACS "custom faces")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(3dbutton ((t (:background "grey33" :foreground "black" :box (:line-width 2 :color "white" :style released-button)))))
 '(3dbutton-highlighted ((t (:inherit 3dbutton :background "grey66" :foreground "black" :box (:line-width 2 :color "grey75" :style pressed-button)))))
 '(android-mode-debug-face ((t (:foreground "cyan"))))
 '(android-mode-info-face ((t (:foreground "chartreuse"))))
 '(android-mode-verbose-face ((t (:foreground "medium spring green"))))
 '(android-mode-warning-face ((t (:foreground "pink"))))
 '(column-marker-1-face ((t (:background "AntiqueWhite"))))
 '(compilation-error ((t (:inherit error :foreground "red"))))
 '(custom-comment ((((class grayscale color) (background dark)) (:background "light green"))))
 '(custom-group-tag ((t (:foreground "blue" :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:inherit variable-pitch :foreground "cadet blue" :weight bold :height 1.2))))
 '(enh-ruby-string-delimiter-face ((t (:foreground "#ddff77"))))
 '(erc-fool-face ((t (:foreground "#ffffee"))))
 '(erc-input-face ((t (:foreground "cyan2"))))
 '(erc-my-nick-face ((t (:foreground "dark blue" :weight bold))))
 '(erc-notice-face ((t (:foreground "gray70"))))
 '(erc-pal-face ((t (:foreground "cadetblue4" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "dark green" :weight bold))))
 '(fg:erc-color-face12 ((t (:foreground "cyan" :weight bold))))
 '(fg:erc-color-face2 ((t (:foreground "LightBlue1"))))
 '(font-lock-cl-function-face ((t (:foreground "DodgerBlue" :weight bold))))
 '(font-lock-cl-standard-generic-function-face ((t (:foreground "turquoise" :weight bold))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face :foreground "red")) (((class color) (min-colors 16)) nil)))
 '(font-lock-comment-face ((t (:foreground "red" :slant italic))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "darkviolet"))))
 '(font-lock-preprocessor-face ((t (:foreground "#550000"))))
 '(font-lock-string-face ((t (:foreground "Orchid"))))
 '(font-lock-type-face ((t (:foreground "#6620b0"))))
 '(gnus-cite-1 ((((class color) (background light)) (:foreground "blue"))))
 '(gnus-cite-10 ((((class color) (background light)) (:foreground "brown"))))
 '(gnus-cite-11 ((((class color) (background light)) (:foreground "red"))))
 '(gnus-cite-2 ((((class color) (background light)) (:foreground "yellow1"))))
 '(gnus-cite-3 ((((class color) (background light)) (:foreground "lightblue2"))))
 '(gnus-cite-4 ((((class color) (background light)) (:foreground "yellow2"))))
 '(gnus-cite-5 ((((class color) (background light)) (:foreground "lightblue3"))))
 '(gnus-cite-6 ((((class color) (background light)) (:foreground "yellow3"))))
 '(gnus-cite-7 ((((class color) (background light)) (:foreground "lightblue4"))))
 '(gnus-cite-8 ((((class color) (background light)) (:foreground "yellow4"))))
 '(gnus-cite-9 ((((class color) (background light)) (:foreground "steelblue3"))))
 '(gnus-group-mail-3 ((t (:foreground "cyan" :weight bold))))
 '(gnus-summary-normal-read ((((class color) (background light)) (:foreground "green"))))
 '(gnus-summary-selected ((t (:foreground "green2" :underline t))))
 '(jde-java-font-lock-javadoc-face ((t (:inherit font-lock-doc-face :foreground "pink"))))
 '(jde-java-font-lock-link-face ((t (:foreground "cyan" :underline t))))
 '(match ((t (:background "#3a3a3e"))))
 '(message-cited-text ((((class color) (background light)) (:foreground "blue"))))
 '(message-header-xheader ((((class color) (background dark)) (:foreground "DodgerBlue"))))
 '(message-separator ((((class color) (background dark)) (:foreground "DodgerBlue" :weight bold))))
 '(mmm-default-submode-face ((t (:foreground "cyan"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "black" :foreground "cyan" :box (:line-width -1 :color "cyan" :style released-button)))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background dark)) (:background "black" :foreground "gray30" :box (:line-width -1 :color "cyan") :weight light))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(read-only-face ((t (:background "gray30"))) t)
 '(rst-level-1-face ((t (:background "grey20" :height 1.9))) t)
 '(rst-level-2-face ((t (:background "grey20" :height 1.7))) t)
 '(rst-level-3-face ((t (:background "grey20" :height 1.4))) t)
 '(rst-level-4-face ((t (:background "grey20" :height 1.2))) t)
 '(rst-level-5-face ((t (:background "grey20" :height 1.1 :weight bold))) t)
 '(rst-level-6-face ((t (:background "grey20" :height 1.0 :weight bold))) t)
 '(semantic-unmatched-syntax-face ((((class color) (background dark)) nil)))
 '(slime-repl-output-face ((t (:inherit font-lock-string-face :foreground "dark green"))))
 '(smerge-refined-change ((t (:background "#997722")))))




(.EMACS "custom variables")
(custom-set-variables
 '(ad-redefinition-action (quote accept))
 '(auto-compression-mode t nil (jka-compr))
 '(auto-image-file-mode t)
 '(auto-save-interval 2500)
 '(auto-save-timeout 60)
 '(backup-by-copying-when-linked t)
 '(backup-by-copying-when-mismatch t)
 '(backup-by-copying-when-privileged-mismatch 1000)
 '(backup-directory-alist nil)
 '(backup-file-version-format "%03d")
 '(bcc-user-mail-address "pjb@informatimago.com" t)
 '(blink-matching-paren t)
 '(boxquote-bottom-corner "+")
 '(boxquote-top-and-tail "----------------------------------------------------------------------------")
 '(boxquote-top-corner "+")
 '(browse-url-browser-function (quote w3m-browse-url))
 '(browse-url-new-window-flag nil)
 '(c-argdecl-indent 4 t)
 '(c-auto-newline nil t)
 '(c-backslash-column (quote set-from-style))
 '(c-backslash-max-column (quote set-from-style))
 '(c-basic-offset (quote set-from-style))
 '(c-block-comment-prefix (quote set-from-style))
 '(c-brace-imaginary-offset 0 t)
 '(c-brace-offset 0 t)
 '(c-cleanup-list (quote set-from-style))
 '(c-comment-continuation-stars "" t)
 '(c-comment-only-line-offset (quote set-from-style))
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-continued-brace-offset 0 t)
 '(c-continued-statement-offset 4 t)
 '(c-default-style "pjb")
 '(c-doc-comment-style (quote set-from-style))
 '(c-echo-syntactic-information-p t)
 '(c-hanging-braces-alist (quote set-from-style))
 '(c-hanging-colons-alist (quote set-from-style))
 '(c-hanging-comment-ender-p nil t)
 '(c-hanging-comment-starter-p nil t)
 '(c-hanging-semi&comma-criteria (quote set-from-style))
 '(c-indent-comment-alist (quote set-from-style))
 '(c-indent-comments-syntactically-p (quote set-from-style))
 '(c-indent-level 4 t)
 '(c-label-minimum-indentation (quote set-from-style))
 '(c-label-offset -4 t)
 '(c-macro-shrink-window-flag t)
 '(c-offsets-alist (quote nil))
 '(c-special-indent-hook (quote nil))
 '(c-tab-always-indent t)
 '(calendar-christian-all-holidays-flag t)
 '(calendar-date-display-form (quote ((if dayname (format "%4s-%2s-%2s  %-9s %2s %-9s" year month day monthname day dayname) (format "%4s-%2s-%2s  %-9s %2s %-9s" year month day monthname day "")))))
 '(calendar-date-style (quote iso))
 '(calendar-hebrew-all-holidays-flag nil)
 '(calendar-mark-holidays-flag t)
 '(calendar-time-display-form (quote (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))) t)
 '(calendar-view-holidays-initially-flag t)
 '(calendar-week-start-day 1)
 '(canlock-password "87f2de14edf31a9ebb2c5b5619c818a49937a47b")
 '(case-fold-search t)
 '(chess-default-engine (quote (chess-gnuchess chess-crafty chess-phalanx)) t)
 '(chess-images-directory "/usr/share/pixmaps/chess/xboard" t)
 '(chess-sound-directory "/usr/share/sounds/chess" t)
 '(comint-dynamic-complete-functions nil t)
 '(comint-process-echoes nil)
 '(comment-empty-lines t)
 '(comment-force-also-empty-lines t)
 '(current-language-environment "UTF-8")
 '(default-input-method nil)
 '(default-major-mode (quote text-mode) t)
 '(delete-old-versions t)
 '(delete-selection-mode nil)
 '(dired-kept-versions 4)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ecb-auto-activate nil)
 '(ecb-cedet-url "http://sourceforge.net/project/showfiles.php?group_id=17484")
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("/home/pjb/src/")))
 '(emms-info-functions (quote (emms-info-id3v2 emms-info-ogginfo emms-info-mp3info)))
 '(emms-info-mp3info-coding-system (quote iso-8859-1))
 '(emms-lyrics-display-on-minibuffer t)
 '(emms-player-started-hook (quote (emms-show)))
 '(emms-show-format "NP %s")
 '(emms-source-file-default-directory "/movies/sound/music/")
 '(emms-source-playlist-formats (quote (native pls m3u)))
 '(enable-recursive-minibuffers t)
 '(erc-auto-query (quote window))
 '(erc-autojoin-channels-alist (quote (("irc.oftc.net" "#openjdk") ("irc.freenode.org" "#maven" "#lisp" "#clnoobs" "#smack") ("irc.trustonic.internal" "#meudon" "#jenkins" "#tbase" "#newSDK" "#kinibi"))))
 '(erc-autojoin-delay 10)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing (quote connect))
 '(erc-away-timestamp-format "<%H:%M:%S>")
 '(erc-beep-match-types (quote (current-nick keyword pal)))
 '(erc-echo-notices-in-current-buffer t)
 '(erc-echo-timestamps nil)
 '(erc-email-userid t)
 '(erc-encoding-coding-alist (quote (("#emacsfr" . iso-8859-15) ("#scheme-es" . iso-8859-15))))
 '(erc-fill-column 90)
 '(erc-fill-function (quote erc-fill-variable))
 '(erc-fill-mode nil)
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
 '(erc-keywords (quote ("^\\(<.*>\\|\\* \\)")))
 '(erc-log-write-after-insert nil)
 '(erc-log-write-after-send nil)
 '(erc-max-buffer-size 300000)
 '(erc-minibuffer-ignored t)
 '(erc-minibuffer-notice t)
 '(erc-modules (quote (autoaway button completion fill irccontrols log match netsplit readonly replace ring scrolltobottom services stamp track truncate)))
 '(erc-nick (quote ("Pascal")))
 '(erc-nickserv-passwords (quote ((freenode (("ogamita" . "ogre-a-mite"))))))
 '(erc-notice-prefix "   *** ")
 '(erc-pals (quote nil))
 '(erc-port 6667)
 '(erc-prompt (lambda nil (buffer-name (current-buffer))))
 '(erc-prompt-for-password nil)
 '(erc-quit-reason-various-alist nil)
 '(erc-save-buffer-on-part nil)
 '(erc-save-queries-on-quit nil)
 '(erc-scrolltobottom-mode nil)
 '(erc-server "irc.trustonic.internal")
 '(erc-server-coding-system (quote (utf-8 . undecided)))
 '(erc-server-reconnect-attempts 100)
 '(erc-server-reconnect-timeout 60)
 '(erc-text-matched-hook (quote (erc-log-matches erc-beep-on-match)))
 '(erc-timestamp-format nil)
 '(erc-timestamp-intangible nil)
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude-types (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(erc-user-full-name "Pascal J. Bourguignon")
 '(eval-expression-debug-on-error t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(file-precious-flag t)
 '(focus-follows-mouse nil)
 '(font-lock-extra-types (quote ("FILE" "\\sw+_t" "[A-Z][A-Za-z]+[A-Z][A-Za-z0-9]+" "bool" "INT8" "INT16" "INT32" "INT64" "INTPTR" "CARD8" "CARD16" "CARD32" "CARD64" "CARDPTR" "SignT" "CHAR" "UNICODE" "DECIMAL" "ADDRESS" "CSTRING255" "CSTRING63" "CSTRING31" "BOOLEAN")) t)
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-article-loose-mime t)
 '(gnus-article-sort-functions (quote (gnus-article-sort-by-score)))
 '(gnus-cacheable-groups "*")
 '(gnus-carpal nil t)
 '(gnus-default-charset (quote iso-8859-15))
 '(gnus-default-posting-charset (quote utf-8) t)
 '(gnus-group-posting-charset-alist (quote (("^\\(no\\|fr\\)\\.[^,]*\\(,[ 	
]*\\(no\\|fr\\)\\.[^,]*\\)*$" iso-8859-15 (iso-8859-15)) ("^\\(fido7\\|relcom\\)\\.[^,]*\\(,[ 	
]*\\(fido7\\|relcom\\)\\.[^,]*\\)*$" koi8-r (koi8-r)) (message-this-is-mail nil nil) (message-this-is-news iso-8859-15 (iso-8859-15)))))
 '(gnus-ignored-headers (quote ("^Path:" "^Expires:" "^Date-Received:" "^References:" "^Xref:" "^Lines:" "^Relay-Version:" "^Approved:" "^Sender:" "^Received:" "^X-UIDL:" "^MIME-Version:" "^Return-Path:" "^In-Reply-To:" "^Content-Type:" "^Content-Transfer-Encoding:" "^X-WebTV-Signature:" "^X-MimeOLE:" "^X-MSMail-Priority:" "^X-Priority:" "^X-Loop:" "^X-Authentication-Warning:" "^X-MIME-Autoconverted:" "^X-Face:" "^X-Attribution:" "^X-Originating-IP:" "^Delivered-To:" "^NNTP-[-A-Za-z]+:" "^Distribution:" "^X-no-archive:" "^X-Trace:" "^X-Complaints-To:" "^X-NNTP-Posting-Host:" "^X-Orig.*:" "^Abuse-Reports-To:" "^Cache-Post-Path:" "^X-Article-Creation-Date:" "^X-Poster:" "^X-Mail2News-Path:" "^X-Server-Date:" "^X-Cache:" "^Originator:" "^X-Problems-To:" "^X-Auth-User:" "^X-Post-Time:" "^X-Admin:" "^X-UID:" "^Resent-[-A-Za-z]+:" "^X-Mailing-List:" "^Precedence:" "^Original-[-A-Za-z]+:" "^X-filename:" "^X-Orcpt:" "^Old-Received:" "^X-Pgp:" "^X-Auth:" "^X-From-Line:" "^X-Gnus-Article-Number:" "^X-Majordomo:" "^X-Url:" "^X-Sender:" "^MBOX-Line:" "^Priority:" "^X400-[-A-Za-z]+:" "^Status:" "^X-Gnus-Mail-Source:" "^Cancel-Lock:" "^X-FTN:" "^X-EXP32-SerialNo:" "^Encoding:" "^Importance:" "^Autoforwarded:" "^Original-Encoded-Information-Types:" "^X-Ya-Pop3:" "^X-Face-Version:" "^X-Vms-To:" "^X-ML-NAME:" "^X-ML-COUNT:" "^Mailing-List:" "^X-finfo:" "^X-md5sum:" "^X-md5sum-Origin:" "^X-Sun-Charset:" "^X-Accept-Language:" "^X-Envelope-Sender:" "^List-[A-Za-z]+:" "^X-Listprocessor-Version:" "^X-Received:" "^X-Distribute:" "^X-Sequence:" "^X-Juno-Line-Breaks:" "^X-Notes-Item:" "^X-MS-TNEF-Correlator:" "^x-uunet-gateway:" "^X-Received:" "^Content-length:" "^X-precedence:" "^X-Authenticated-User:" "^X-Comment:" "^X-Report:" "^X-Abuse-Info:" "^X-HTTP-Proxy:" "^X-Mydeja-Info:" "^X-Copyright:" "^X-No-Markup:" "^X-Abuse-Info:" "^X-From_:" "^X-Accept-Language:" "^Errors-To:" "^X-BeenThere:" "^X-Mailman-Version:" "^List-Help:" "^List-Post:" "^List-Subscribe:" "^List-Id:" "^List-Unsubscribe:" "^List-Archive:" "^X-Content-length:" "^X-Posting-Agent:" "^Original-Received:" "^X-Request-PGP:" "^X-Fingerprint:" "^X-WRIEnvto:" "^X-WRIEnvfrom:" "^X-Virus-Scanned:" "^X-Delivery-Agent:" "^Posted-Date:" "^X-Gateway:" "^X-Local-Origin:" "^X-Local-Destination:" "^X-UserInfo1:" "^X-Received-Date:" "^X-Hashcash:" "^Face:" "^X-DMCA-Notifications:" "^X-Abuse-and-DMCA-Info:" "^X-Postfilter:" "^X-Gpg-.*:" "^X-Disclaimer:")))
 '(gnus-message-setup-hook (quote (pjb-gnus-message-setup-meat)))
 '(gnus-nntp-server nil)
 '(gnus-play-startup-jingle nil)
 '(gnus-secondary-select-methods (quote ((nntp "news.gmane.org") (nnimap "hubble.informatimago.com"))))
 '(gnus-select-method (quote (nntp "news.individual.com")))
 '(gnus-subscribe-newsgroup-method (quote gnus-subscribe-zombies))
 '(gnus-summary-line-format "%U%R%z%o %B%(%[%4L: %-23,23f%]%) %s
")
 '(gnus-treat-display-x-face (quote head))
 '(gnus-use-full-window nil)
 '(gnus-use-nocem nil)
 '(gnus-uu-post-encode-method (quote gnus-uu-post-encode-mime))
 '(gnus-visible-headers (quote ("^From:" "^Newsgroups:" "^Subject:" "^Date:" "^Followup-To:" "^Reply-To:" "^Organization:" "^Summary:" "^Keywords:" "^To:" "^[BGF]?Cc:" "^Posted-To:" "^Mail-Copies-To:" "^Mail-Followup-To:" "^Apparently-To:" "^Gnus-Warning:" "^Resent-From:" "^Message-ID:" "^X-Sent:")))
 '(grep-command "grep -niH -e ")
 '(hl-paren-colors (quote ("red" "orange" "yellow" "green" "blue" "violet" "gray" "gray" "gray" "gray" "gray")))
 '(holiday-other-holidays (quote ((holiday-fixed 10 28 "Frédérique Saubot") (holiday-fixed 10 11 "Henri Bourguignon") (holiday-fixed 6 10 "Désirée Mayer") (holiday-fixed 3 23 "Françoise Keller") (holiday-fixed 11 25 "Joëlle Bourguignon") (holiday-fixed 12 16 "Agathe De Robert") (holiday-fixed 5 12 "Guillaume De Robert") (holiday-fixed 1 4 "Isabelle Saubot") (holiday-fixed 10 23 "Marc Moini") (holiday-fixed 2 10 "Anne-Marie Castel") (holiday-fixed 6 28 "Jean-François Gaillon") (holiday-fixed 6 28 "Sylvie Gaillon") (holiday-fixed 8 27 "Jean-Philippe Capy") (holiday-fixed 1 25 "Raoul Fruhauf") (holiday-fixed 3 15 "Pascal Bourguignon") (holiday-fixed 4 12 "Jalal Adamsah") (holiday-fixed 5 3 "Samy Karsenty") (holiday-fixed 8 17 "Alain Pierre") (holiday-fixed 1 14 "Bernard Bourguignon") (holiday-fixed 3 3 "Emmanuelle Chaize") (holiday-fixed 12 12 "Nicoleta Reinald") (holiday-fixed 1 3 "Florence Petit") (holiday-fixed 11 16 "Wei Van Chi") (holiday-fixed 12 6 "Marie Lecomte") (holiday-fixed 7 3 "Alain Bourguignon") (holiday-fixed 4 15 "André Reinald") (holiday-fixed 12 13 "Michelle Keller") (holiday-fixed 5 27 "Grégoire Saubot") (holiday-fixed 3 27 "Olivia De Robert") (holiday-fixed 11 18 "Vincent De Robert") (holiday-fixed 7 23 "Gabriel De Robert") (holiday-fixed 3 18 "Claire De Robert") (holiday-fixed 10 26 "Maxime De Robert") (holiday-fixed 3 26 "Edward-Amadeus Reinald") (holiday-fixed 3 4 "Louise Akiko Poullain") (holiday-fixed 8 26 "Iris-Alea Reinald") (holiday-fixed 9 4 "Baptiste Rouit") (holiday-fixed 2 22 "Camille Saubot") (holiday-fixed 8 2 "Clémence Saubot-Fiant") (holiday-fixed 5 29 "François Saubot") (holiday-fixed 1 2 "Henry Saubot") (holiday-fixed 2 8 "Jean-Pierre Baccache") (holiday-fixed 10 28 "Lucia (fille de Camille)") (holiday-fixed 11 26 "Marine Rouit") (holiday-fixed 3 13 "Mathias Fiant") (holiday-fixed 4 8 "Mathilde Rouit") (holiday-fixed 2 2 "Olivier Scmidt Chevalier") (holiday-fixed 2 23 "PtiDoigt Deamon") (holiday-fixed 8 10 "Kiteri (fille de Camille)") (holiday-fixed 9 10 "Remy Rouit") (holiday-fixed 8 7 "Valerie Saubot-Rouit") (holiday-fixed 1 6 "Los Reyes") (holiday-fixed 6 9 "Santa Murcia") (holiday-fixed 7 25 "Fiesta?") (holiday-fixed 10 12 "Los Reyes") (holiday-fixed 12 6 "Fiesta de la Consitución") (holiday-fixed 7 14 "Fête Nationale France"))) t)
 '(ido-enable-flex-matching nil)
 '(indent-tabs-mode nil)
 '(inferior-lisp-filter-regexp "\\`\\s*\\'")
 '(inihibit-default-init t)
 '(initial-major-mode (quote emacs-lisp-mode))
 '(ispell-choices-win-default-height 4)
 '(ispell-highlight-p t)
 '(ispell-local-dictionary "francais")
 '(ispell-local-dictionary-alist (quote (("francais" "[A-Za-zÀ-ÖØ-öø-ÿ]" "[^A-Za-zÀ-ÖØ-öø-ÿ]" "[-']" t nil "~latin9" iso-8859-15))))
 '(ispell-message-dictionary-alist (quote (("\"^Newsgroups:[ \\t]*fr\\\\.\"" . "\"francais\"") ("\"^To:[^\\n,]+\\\\.fr[ \\t\\n,>]\"" . "\"francais\"") ("\"^Newsgroups:[ \\t]*(es|mx|ar)\\\\.\"" . "\"castillano\"") ("\"^To:[^\\n,]+\\\\.(es|mx|ar)[ \\t\\n,>]\"" . "\"castillano\"") ("\"^Newsgroups:[ \\t]*uk\\\\.\"" . "\"english\"") ("\"^To:[^\\n,]+\\\\.uk[ \\t\\n,>]\"" . "\"english\"") ("\".*\"" . "\"american\""))))
 '(ispell-query-replace-choices nil)
 '(jde-java-font-lock-javadoc-face ((t (:inherit font-lock-doc-face :foreground "pink"))) t)
 '(jde-java-font-lock-link-face ((t (:foreground "cyan" :underline t))) t)
 '(kept-new-versions 9)
 '(kept-old-versions 0)
 '(kill-whole-line t)
 '(line-number-mode t)
 '(lpr-page-header-switches (quote ("-F" "-t")))
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
 '(mail-from-style (quote angle))
 '(mail-host-address nil)
 '(mail-interactive t)
 '(mail-mode-hook (quote (mail-abbrevs-setup (lambda nil (set-buffer-file-coding-system (quote utf-8) t t) (set-input-method default-input-method) (local-set-key "	" (quote expand-mail-aliases))))))
 '(mail-self-blind t)
 '(mail-setup-hook (quote (pjb-mail-mode-meat)))
 '(mail-signature t)
 '(mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^reply-to:\\|^return-path:\\|^mailing-list:\\|^precedence:\\|^x-\\|^content-\\|^cc:\\|^list-\\|^resent\\|^organization:\\|^sender:\\|^user-agent:\\|^mime-version:\\|^delivered-to:\\|^references:")
 '(mail-yank-prefix "> ")
 '(mark-even-if-inactive t)
 '(matlab-comment-line-s "// " t)
 '(matlab-comment-on-line-s "// " t)
 '(matlab-comment-region-s "// " t)
 '(max-specpdl-size 2048)
 '(message-default-charset (quote iso-8859-15))
 '(message-default-headers "Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwAQMAAABtzGvEAAAABlBMVEUAAAD///+l2Z/dAAAA
      oElEQVR4nK3OsRHCMAwF0O8YQufUNIQRGIAja9CxSA55AxZgFO4coMgYrEDDQZWPIlNAjwq9
      033pbOBPtbXuB6PKNBn5gZkhGa86Z4x2wE67O+06WxGD/HCOGR0deY3f9Ijwwt7rNGNf6Oac
      l/GuZTF1wFGKiYYHKSFAkjIo1b6sCYS1sVmFhhhahKQssRjRT90ITWUk6vvK3RsPGs+M1RuR
      mV+hO/VvFAAAAABJRU5ErkJggg==
X-Accept-Language:         fr, es, en
")
 '(message-directory "~/mail/")
 '(message-from-style (quote angles))
 '(message-log-max 5000)
 '(message-required-news-headers (quote (From Newsgroups Subject Date Message-ID (optional . Organization) (optional . User-Agent) (X-Face lambda nil (gnus-x-face-from-file "~/my-face.xbm")))))
 '(message-user-organization "Informatimago")
 '(mew-conf-path "~/.new")
 '(mew-mail-path "~/mail")
 '(mew-pop-auth (quote pass))
 '(mew-pop-header-only nil)
 '(mew-pop-server "pop.informatimago.com")
 '(mew-pop-size 0)
 '(mew-smtp-server "smtp.informatimago.com")
 '(mew-use-biff t)
 '(mew-use-biff-bell t)
 '(mew-use-full-window t)
 '(mew-use-other-frame-for-draft nil)
 '(mew-use-text/html t)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mm-coding-system-priorities (quote (ascii iso-latin-1 iso-latin-9 utf-8)))
 '(mspools-update t)
 '(next-screen-context-lines 0)
 '(nntp-authinfo-file "~/.authinfo")
 '(org-agenda-files (quote ("~/src/trustonic/notes.txt")))
 '(org-fontify-done-headline t)
 '(org-todo-keywords (quote ((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE(d)") (sequence "|" "CANCELED(c)"))))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(ph-server "localhost" t)
 '(pjb-test-var 2 t)
 '(pop-up-frames nil)
 '(pop-up-windows t)
 '(pr-faces-p t)
 '(print-gensym t t)
 '(printer-name "Samsung_M2020_Series__SEC30CDA7116AD9_" t)
 '(prolog-program-name "/usr/bin/swipl")
 '(ps-font-size (quote (7 . 11)))
 '(ps-header-lines 0)
 '(ps-left-header nil)
 '(ps-paper-type (quote a4) t)
 '(ps-print-header nil)
 '(ps-print-header-frame nil)
 '(ps-printer-name "normal_gray")
 '(ps-right-header nil)
 '(ps-show-n-of-n nil)
 '(read-mail-command (quote vm))
 '(read-quoted-char-radix 10)
 '(redshank-accessor-name-function (quote identity))
 '(redshank-canonical-package-designator-function (quote redshank-package-designator/string))
 '(redshank-licence-names (quote ("BSD-style" "GPL" "LGPL" "LLGPL" "MIT" "MIT-style" "GPL2" "GPL2+" "GPL3" "AGPL3")))
 '(require-final-newline (quote visit-save))
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
 '(rmail-summary-line-decoder (quote identity))
 '(rmail-summary-window-size 12)
 '(safe-local-variable-values (quote ((eval font-lock-add-keywords nil (\` (((\, (concat "(" (regexp-opt (quote ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")) t) "\\_>")) 1 (quote font-lock-variable-name-face))))) (org-todo-keywords (sequence "TODO(t@)" "IN-PROGRESS(p@)" "|" "DONE(d@)" "CANCELED(c@)")) (org-fontify-done-headline . t) (tab-always-indent . t) (electric-indent-mode) (encoding . utf-8) (Readtable . PY-AST-READTABLE) (Package . CLPYTHON\.PARSER) (Readtable . PY-AST-USER-READTABLE) (Package . CLPYTHON) (Package . "CCL") (syntax . COMMON-LISP) (Package . CLPYTHON\.UTIL) (Package . CCL) (Package . CLPYTHON\.MODULE\.OPERATOR) (Syntax . COMMON-LISP))))
 '(send-mail-function (quote smtpmail-send-it))
 '(sh-indent-after-case 0)
 '(sh-indent-after-switch 0)
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch nil)
 '(slime-autodoc-use-multiline-p t)
 '(slime-compilation-finished-hook (quote (slime-maybe-show-xrefs-for-notes)))
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
 '(slime-space-information-p nil)
 '(slime-startup-animation nil)
 '(smtpmail-default-smtp-server "hubble.informatimago.com")
 '(smtpmail-local-domain "lan.informatimago.com")
 '(smtpmail-sendto-domain "informatimago.com")
 '(smtpmail-smtp-server "hubble.informatimago.com")
 '(smtpmail-smtp-service 25)
 '(smtpmail-smtp-user "pjb@informatimago.com")
 '(smtpmail-stream-type (quote ssl))
 '(smtpmail-warn-about-unknown-extensions t)
 '(spam-autodetect-recheck-messages t)
 '(speedbar-show-unknown-files t)
 '(stack-trace-on-error nil)
 '(starttls-use-gnutls nil)
 '(tab-stop 4 t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))
 '(tab-width 4)
 '(tags-table-list (quote nil))
 '(tnt-use-timestamps t)
 '(tnt-username-alist (quote (("matimago") ("ogamita"))))
 '(tooltip-frame-parameters (quote ((nil . "tooltip") (right-fringe . 6) (left-fringe . 6) (nil . "lightyellow") (nil . 0) (nil . 1))))
 '(truncate-partial-width-windows nil)
 '(url-be-asynchronous t)
 '(url-honor-refresh-requests nil)
 '(user-full-name "Pascal J. Bourguignon")
 '(user-mail-address "pascal.bourguignon@trustonic.com")
 '(vc-annotate-background "snow1")
 '(vc-annotate-color-map (quote ((4.943848 . "#0000f0") (10.217285 . "#0000e0") (15.490723 . "#0000d0") (20.76416 . "#0000c0") (26.037598 . "#0000b0") (31.311035 . "#0000a0") (36.584473 . "#000090") (41.85791 . "#000080") (47.131348 . "#000070") (52.404785 . "#000060") (57.678223 . "#000050") (62.95166 . "#000040") (68.225098 . "#000030") (73.498535 . "#000020") (78.771973 . "#000010"))))
 '(vc-annotate-very-old-color "#000000")
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(vm-auto-displayed-mime-content-types (quote ("text/enriched" "text/plain" "message" "message/rfc822" "message/disposition-notification" "multipart")))
 '(vm-auto-folder-alist (quote (("^\\(From:\\|To:\\|Cc:\\)" ("svn-.*anevia.com" . "~/mail/anevia-svn.mbox") ("staff@anevia.com" . "~/mail/anevia-staff.mbox") ("cpptest@anevia.com" . "~/mail/cpptest.mbox") ("\\(bese.*common-lisp\\)" . "~/mail/ucw.mbox") ("\\(lispme\\|clisp\\|sbcl\\|cmucl\\|openmcl\\|ilisp\\|clocc\\|clump\\|cclan\\|ecls\\|nocrew.org\\|biolisp\\|lispweb\\|climacs\\|gardeners\\|acl2\\|Planet Lisp\\|lisa-users\\|opencyc\\|small-cl-src\\|cl-faq\\|cl-pdf\\|cl-typesetting\\|movitz\\|quiz@common-lisp\\|slime\\)" . "~/mail/lisp.mbox") ("cert-advisory@cert.org" . "~/mail/cert.mbox") ("gentoo" . "~/mail/gentoo.mbox")) ("^Subject:" ("\\[libanevia\\|manager2\\|aipc\\]" . "~/mail/anevia-manager2.mbox") ("cs daily" . "~/mail/cs-papers.mbox") ("CRYPTO.*GRAM" . "~/mail/crypto-gram.mbox") ("ipnsig" . "~/mail/ipnsig.mbox") ("\\(lispme\\|clisp\\|sbcl\\|cmucl\\|openmcl\\|ilisp\\|clocc\\|clump\\|cclan\\|ecls\\|nocrew.org\\|biolisp\\|lispweb\\|climacs\\|gardeners\\|acl2\\|Planet Lisp\\|lisa-users\\|opencyc\\|small-cl-src\\|cl-faq\\|cl-pdf\\|cl-typesetting\\|movitz\\|quiz@common-lisp\\|slime\\)" . "~/mail/lisp.mbox")))))
 '(vm-auto-folder-case-fold-search t)
 '(vm-display-xfaces t)
 '(vm-folder-directory "~/mail/")
 '(vm-highlighted-header-face (quote font-lock-comment-face))
 '(vm-honor-mime-content-disposition nil)
 '(vm-included-text-prefix "> ")
 '(vm-infer-mime-types t)
 '(vm-mail-mode-hook nil)
 '(vm-mime-8bit-composition-charset "utf-8")
 '(vm-mime-8bit-text-transfer-encoding (quote 8bit))
 '(vm-mime-alternative-select-method (quote (favorite-internal "text/enriched" "text/plain")))
 '(vm-mime-attachment-auto-suffix-alist (quote (("image/tiff" . ".tif") ("image/jpeg" . ".jpg") ("image/gif" . ".gif") ("image/png" . ".png") ("text/html" . ".html") ("audio/basic" . ".au") ("video/mpeg" . ".mpg") ("video/quicktime" . ".mov") ("application/postscript" . ".ps") ("application/pdf" . ".pdf") ("application/vnd.ms-excel" . ".xls") ("application/mac-binhex40" . ".hqx") ("application/pdf" . ".pdf") ("application/zip" . ".zip"))))
 '(vm-mime-default-face-charsets (quote ("us-ascii" "iso-8859-1" "iso-8859-15" "win-1250" "ANSI_X3.4-1968")))
 '(vm-mime-external-content-types-alist (quote (("application/pdf" "acroread") ("image/gif" "xview") ("image/jpg" "xview") ("image/tiff" "xview") ("image/jpeg" "xview"))))
 '(vm-mime-use-w3-for-text/html t)
 '(vm-mutable-frames nil)
 '(vm-preview-lines nil)
 '(vm-reply-subject-prefix "Re: ")
 '(vm-spool-files (quote (("~/INBOX" "/var/spool/mail/pjb" "~/INBOX-local.crash"))))
 '(vm-summary-highlight-face (quote font-lock-comment-face))
 '(vm-url-browser (quote pjb-browse-url))
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
 '(w3m-bookmark-file-coding-system (quote utf-8))
 '(w3m-coding-system (quote utf-8))
 '(w3m-cookie-file "~/.w3m/cookie")
 '(w3m-default-display-inline-images t)
 '(w3m-fb-mode nil)
 '(w3m-file-coding-system (quote utf-8))
 '(w3m-file-name-coding-system (quote iso-8859-1))
 '(w3m-form-textarea-directory "~/.w3m/textarea")
 '(w3m-home-page "http://localhost/")
 '(w3m-pop-up-frames nil)
 '(w3m-pop-up-windows nil)
 '(w3m-session-file "~/.w3m/sessions")
 '(w3m-terminal-coding-system (quote utf-8))
 '(w3m-use-cookies t)
 '(w3m-use-tab nil)
 '(w3m-use-tab-menubar nil)
 '(w3m-use-title-buffer-name t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t))


;;;----------------------------------------------------------------------------
(load "~/rc/emacs-font.el")
(load "~/rc/emacs-package.el")
(when (not *pjb-pvs-is-running*)
  (load "~/rc/emacs-palette.el"))
;; (load "~/rc/emacs-slime.el")
;; (load "~/rc/emacs-cl-indent.el")
(load "~/rc/emacs-slime-simple.el")
(load "~/rc/emacs-hyperspec.el")
(load "~/rc/emacs-redshank.el")
(load "~/rc/emacs-objective-c.el")
(load "~/rc/emacs-android.el")
(load "~/rc/emacs-ruby.el")

;;;----------------------------------------------------------------------------
(display-time-mode 1)
(setf visible-bell nil
      ring-bell-function nil)
;;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("/Users/pjb/works/abalone/.*\\.\\(h\\|m\\mm\\)$" . objc-mode))
(add-to-list 'auto-mode-alist '("/Users/pjb/src/ios/.*\\.\\(h\\|m\\mm\\)$" . objc-mode))

(add-to-list 'auto-mode-alist '("/home/pjb/private/etudes/stanford/.*\\.\\(m\\)$" . octave-mode))

(setf auto-mode-alist  (sort* auto-mode-alist
                              (function string<)
                              :key (function car)))

(ignore-errors (set-sources "/home/pjb/works/patchwork/src/patchwork/"))

;;;----------------------------------------------------------------------------

;; (when (and (file-exists-p "/data/sound/beeps/Macintosh_Question.wav")
;;            (file-exists-p "/usr/bin/mplayer"))
;;   (setf visible-bell nil
;;         ring-bell-function (lambda ()
;;                              (shell-command-to-string
;;                               "mplayer /data/sound/beeps/Macintosh_Question.wav"))))

(push "~/emacs/emacs-w3m/share/emacs/site-lisp/w3m/" load-path)

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

(setf *pjb-intervention-firm* '((trustonic thp)))


;; (setf org-finish-function 'org-store-log-note)
;; org-finish-function
;; org-store-log-note
;; <YoungFrog> igam: In org-add-log-note there is (org-set-local 'org-finish-function
;; 'org-store-log-note). I'd try adding a (message "Curbuf %S" (current-buffer)) in there to
;; see in which buffer that goes... probably not the Org Note buffer as it should be.

;;  irc://artigue.org.es
(cd (user-homedir-pathname))
(slime)
(load "~/rc/emacs-epilog.el")
;;;; THE END ;;;;

