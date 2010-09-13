;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs startup file.
;;;;
;;;; We only run GNU emacs.
;;;;

;; Emacs Makes All Computing Simple.
;; Eine Is Not Emacs.
;; Zwei Was Eine Initially.
;; Drei Ressembled Emacs Intelligently.
;; Vier Integrates Emacs Regexps.
;; Fünf
;; Sechs 
;; Sieben Is Even Better Emacs Now
;; Acht
;; Neun
;; Zehn
;; Hemlock 
;; Climacs Common Lisp Interface Manager Application Creating Sources 


;;(when (= (user-uid) 0)
;;  (load "/root/.emacs" pjb:*load-noerror* pjb:*load-silent*)
;;  (error "~/.emacs: Cannot load ~/.emacs under root account."))





;;;----------------------------------------------------------------------------
;;; Message Log
;;;----------------------------------------------------------------------------

(defvar *emacs-start-time*       (current-time) "For (emacs-uptime).")

(defvar *pjb-load-noerror*       t)
(defvar *pjb-load-silent*        nil)
(defvar *pjb-light-emacs*        nil "pjb-loader will load the minimum.")
(defvar *pjb-pvs-is-running*     (and (boundp 'x-resource-name)
                                      (string-equal x-resource-name "pvs")))
(defvar *pjb-save-log-file-p*    nil "Whether .EMACS must save logs to /tmp/messages.txt")

(defvar *hostname*
  (or (and (boundp 'system-name) system-name)
      (and (fboundp 'system-name) (system-name))
      (ignore-errors
        (shell-command-to-string
         "echo -n $( (hostname -f 2>/dev/null) || (hostname 2>/dev/null) )")
        "localhost")))



(defun .EMACS (fctl &rest args)
  (let ((text (apply (function format) (concat ".EMACS: " fctl) args)))
    (when *pjb-save-log-file-p*
      (with-current-buffer (get-buffer-create " .EMACS temporary buffer")
        (erase-buffer)
        (insert text "\n")
        (append-to-file (point-min) (point-max) "/tmp/messages.txt")))
    (message text)))


;;;----------------------------------------------------------------------------
;;; Life saver
;;;----------------------------------------------------------------------------
(.EMACS "REQUIRE CL...")
(require 'cl)
(require 'parse-time)
(require 'tramp nil t)


(.EMACS "STARTING...")
(mapc (lambda (f) (when (fboundp (car f)) (apply (function funcall) f)))
      '((scroll-bar-mode     -1)
        (menu-bar-mode       -1)
        (tool-bar-mode       -1)
        (transient-mark-mode +1)))


(when (or (boundp 'aquamacs-version) (eq window-system 'ns))
   (setf mac-command-modifier 'meta
         mac-option-modifier  'alt
         one-buffer-one-frame nil)
   (cua-mode 0))

(when (boundp 'x-toolkit-scroll-bars)
  (setf x-toolkit-scroll-bars nil))


;;;----------------------------------------------------------------------------
(when t
  (.EMACS "emacs-major-version  = %S" emacs-major-version)
  (.EMACS "emacs-minor-version  = %S" emacs-minor-version)
  (.EMACS "emacs-version        = %S" emacs-version)
  (.EMACS "system-type          = %S" system-type)
  (.EMACS "system-name          = %S" system-name)
  (.EMACS "system-configuration = %S" system-configuration)
  (.EMACS "window-system        = %S" window-system)
  (when (boundp 'aquamacs-version)
    (.EMACS "aquamacs-version     = %S" aquamacs-version)))

;; system-type          darwin   gnu/linux  cygwin
;; system-name          "naiad.informatimago.com" "hermes.afaa.asso.fr"
;; system-configuration "i686-pc-linux-gnu" "i686-pc-cygwin" "i386-apple-darwin9.8.0"
;; window-system        nil x mac ns w32
;; emacs-major-version  18 19 20 21 23
;; emacs-minor-version  0 1 2 3
;; emacs-version        "20.7.2" "21.2.1" 
;; NO emacs-type! We won't run on anything else than GNU emacs.

;; window-system ==> (display-multi-frame-p)
;; xterm-mouse-mode ;; To use the mouse inside xterm!


(require 'rst)

;;;----------------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------------

(.EMACS "custom faces")
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-marker-1-face ((t (:background "AntiqueWhite"))))
 '(custom-comment ((((class grayscale color) (background dark)) (:background "light green"))))
 '(custom-group-tag ((t (:foreground "blue" :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:inherit variable-pitch :foreground "cadet blue" :weight bold :height 1.2))))
 '(erc-fool-face ((t (:foreground "#ffffee"))))
 '(erc-input-face ((t (:foreground "yellow3"))))
 '(erc-notice-face ((t (:foreground "LightSalmon4"))))
 '(erc-pal-face ((t (:foreground "cadetblue1" :weight bold))))
 '(fg:erc-color-face12 ((t (:foreground "cyan" :weight bold))))
 '(fg:erc-color-face2 ((t (:foreground "LightBlue1"))))
 '(font-lock-cl-function-face ((t (:foreground "DodgerBlue" :weight bold))))
 '(font-lock-cl-standard-generic-function-face ((t (:foreground "turquoise" :weight bold))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face :foreground "red")) (((class color) (min-colors 16)) nil)))
 '(font-lock-comment-face ((nil (:foreground "red"))))
 '(font-lock-string-face ((t (:foreground "Orchid"))))
 '(gnus-cite-1 ((((class color) (background light)) (:foreground "lightblue"))))
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
 '(gnus-summary-normal-read ((((class color) (background light)) (:foreground "green"))))
 '(gnus-summary-selected ((t (:foreground "green2" :underline t))))
 '(message-cited-text ((((class color) (background light)) (:foreground "blue"))))
 '(message-header-xheader ((((class color) (background dark)) (:foreground "DodgerBlue"))))
 '(message-separator ((((class color) (background dark)) (:foreground "DodgerBlue" :weight bold))))
 '(mmm-default-submode-face ((t (:foreground "cyan"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "cadetblue1" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(rst-level-1-face ((t (:background "grey20"))) t)
 '(rst-level-2-face ((t (:background "grey20"))) t)
 '(rst-level-3-face ((t (:background "grey20"))) t)
 '(rst-level-4-face ((t (:background "grey20"))) t)
 '(rst-level-5-face ((t (:background "grey20"))) t)
 '(rst-level-6-face ((t (:background "grey20"))) t)
 '(semantic-unmatched-syntax-face ((((class color) (background dark)) nil))))


(.EMACS "custom variables")
(custom-set-variables
 '(ad-redefinition-action (quote accept))
 '(all-christian-calendar-holidays t t)
 '(all-hebrew-calendar-holidays nil t)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-image-file-mode t)
 '(auto-save-interval 2500)
 '(auto-save-timeout 60)
 '(backup-by-copying-when-linked t)
 '(backup-by-copying-when-mismatch t)
 '(backup-by-copying-when-privileged-mismatch 1000)
 '(bcc-user-mail-address "pjb@informatimago.com" t)
 '(blink-matching-paren t)
 '(boxquote-bottom-corner "+")
 '(boxquote-top-and-tail "----------------------------------------------------------------------------")
 '(boxquote-top-corner "+")
 '(c-argdecl-indent 4 t)
 '(c-auto-newline nil t)
 '(c-backslash-column 72)
 '(c-basic-offset 4)
 '(c-brace-imaginary-offset 0 t)
 '(c-brace-offset 0 t)
 '(c-cleanup-list (quote (brace-else-brace brace-elseif-brace brace-catch-brace list-close-comma scope-operator)))
 '(c-comment-continuation-stars "" t)
 '(c-comment-only-line-offset (quote (0 . 0)))
 '(c-continued-brace-offset 0 t)
 '(c-continued-statement-offset 4 t)
 '(c-default-style "gnu")
 '(c-echo-syntactic-information-p t)
 '(c-hanging-braces-alist (quote ((defun-open before after) (defun-close before) (class-open before after) (inline-open before after) (inline-close before) (block-open) (block-close . c-snug-do-while) (substatement-open after) (statement-case-open after) (extern-lang-open after) (extern-lang-close before after) (brace-list-open after) (brace-list-close before after) (brace-list-intro before after) (brace-entry-open before after) (inexpr-class-open after) (inexpr-class-close before))))
 '(c-hanging-comment-ender-p nil t)
 '(c-hanging-comment-starter-p nil t)
 '(c-indent-level 4 t)
 '(c-label-minimum-indentation 2)
 '(c-label-offset -4 t)
 '(c-macro-shrink-window-flag t)
 '(c-offsets-alist (quote ((topmost-intro . 0) (inclass . c-basic-offset) (cpp-macro . [0]) (objc-method-intro . [0]) (defun-block-intro . c-basic-offset) (statement-block-intro . c-basic-offset) (access-label . -4) (defun-open . 0) (topmost-intro . 0) (member-init-cont . 0) (statement-cont . 0) (inclass . 4) (cpp-macro . -1000) (objc-method-intro . 0))))
 '(c-tab-always-indent t)
 '(calendar-christian-all-holidays-flag t)
 '(calendar-date-display-form (quote ((if dayname (format "%4s-%2s-%2s  %-9s %2s %-9s" year month day monthname day dayname) (format "%4s-%2s-%2s  %-9s %2s %-9s" year month day monthname day "")))))
 '(calendar-hebrew-all-holidays-flag nil)
 '(calendar-mark-holidays-flag t)
 '(calendar-view-holidays-initially-flag t)
 '(canlock-password "87f2de14edf31a9ebb2c5b5619c818a49937a47b")
 '(case-fold-search t)
 '(chess-default-engine (quote (chess-gnuchess chess-crafty chess-phalanx)) t)
 '(chess-images-directory "/usr/share/pixmaps/chess/xboard" t)
 '(chess-sound-directory "/usr/share/sounds/chess" t)
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
 '(emms-info-mp3info-coding-system (quote utf-8))
 '(emms-player-started-hook (quote (emms-show)))
 '(emms-show-format "NP %s")
 '(emms-source-file-default-directory "/d5/music/")
 '(emms-source-playlist-formats (quote (native pls m3u)))
 '(enable-recursive-minibuffers t)
 '(erc-auto-query (quote window))
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#scheme" "#emacs" "#lisp") ("irc.oftc.net" "#uml"))))
 '(erc-away-timestamp-format "<%H:%M:%S>")
 '(erc-default-coding-system (quote (utf-8 . undecided)) t)
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
 '(erc-insert-away-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-interpret-mirc-color t)
 '(erc-minibuffer-ignored t)
 '(erc-minibuffer-notice t)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols log match netsplit readonly replace ring scrolltobottom services stamp track truncate)))
 '(erc-nick (quote ("pjb")))
 '(erc-notice-prefix "   *** ")
 '(erc-pals (quote ("bolet.*" "dmiles")))
 '(erc-port 6667)
 '(erc-prompt (lambda nil (buffer-name (current-buffer))))
 '(erc-prompt-for-password t)
 '(erc-quit-reason-various-alist nil)
 '(erc-server "irc.freenode.org")
 '(erc-server-coding-system (quote (utf-8 . undecided)))
 '(erc-timestamp-format nil)
 '(erc-timestamp-intangible nil)
 '(erc-user-full-name "Pascal J. Bourguignon")
 '(eval-expression-debug-on-error t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(file-precious-flag t)
 '(focus-follows-mouse nil)
 '(font-lock-extra-types (quote ("FILE" "\\sw+_t" "[A-Z][A-Za-z]+[A-Z][A-Za-z0-9]+" "bool" "INT8" "INT16" "INT32" "CARD8" "CARD16" "CARD32" "SignT" "CHAR" "UNICODE" "DECIMAL" "ADDRESS" "CSTRING255" "CSTRING63" "CSTRING31" "BOOLEAN")) t)
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-article-loose-mime t)
 '(gnus-article-sort-functions (quote (gnus-article-sort-by-score)))
 '(gnus-cacheable-groups "*")
 '(gnus-carpal nil)
 '(gnus-default-charset (quote iso-8859-15))
 '(gnus-default-posting-charset (quote utf-8) t)
 '(gnus-group-posting-charset-alist (quote (("^\\(no\\|fr\\)\\.[^,]*\\(,[ 	
]*\\(no\\|fr\\)\\.[^,]*\\)*$" iso-8859-15 (iso-8859-15)) ("^\\(fido7\\|relcom\\)\\.[^,]*\\(,[ 	
]*\\(fido7\\|relcom\\)\\.[^,]*\\)*$" koi8-r (koi8-r)) (message-this-is-mail nil nil) (message-this-is-news iso-8859-15 (iso-8859-15)))))
 '(gnus-ignored-headers (quote ("^Path:" "^Expires:" "^Date-Received:" "^References:" "^Xref:" "^Lines:" "^Relay-Version:" "^Approved:" "^Sender:" "^Received:" "^X-UIDL:" "^MIME-Version:" "^Return-Path:" "^In-Reply-To:" "^Content-Type:" "^Content-Transfer-Encoding:" "^X-WebTV-Signature:" "^X-MimeOLE:" "^X-MSMail-Priority:" "^X-Priority:" "^X-Loop:" "^X-Authentication-Warning:" "^X-MIME-Autoconverted:" "^X-Face:" "^X-Attribution:" "^X-Originating-IP:" "^Delivered-To:" "^NNTP-[-A-Za-z]+:" "^Distribution:" "^X-no-archive:" "^X-Trace:" "^X-Complaints-To:" "^X-NNTP-Posting-Host:" "^X-Orig.*:" "^Abuse-Reports-To:" "^Cache-Post-Path:" "^X-Article-Creation-Date:" "^X-Poster:" "^X-Mail2News-Path:" "^X-Server-Date:" "^X-Cache:" "^Originator:" "^X-Problems-To:" "^X-Auth-User:" "^X-Post-Time:" "^X-Admin:" "^X-UID:" "^Resent-[-A-Za-z]+:" "^X-Mailing-List:" "^Precedence:" "^Original-[-A-Za-z]+:" "^X-filename:" "^X-Orcpt:" "^Old-Received:" "^X-Pgp:" "^X-Auth:" "^X-From-Line:" "^X-Gnus-Article-Number:" "^X-Majordomo:" "^X-Url:" "^X-Sender:" "^MBOX-Line:" "^Priority:" "^X400-[-A-Za-z]+:" "^Status:" "^X-Gnus-Mail-Source:" "^Cancel-Lock:" "^X-FTN:" "^X-EXP32-SerialNo:" "^Encoding:" "^Importance:" "^Autoforwarded:" "^Original-Encoded-Information-Types:" "^X-Ya-Pop3:" "^X-Face-Version:" "^X-Vms-To:" "^X-ML-NAME:" "^X-ML-COUNT:" "^Mailing-List:" "^X-finfo:" "^X-md5sum:" "^X-md5sum-Origin:" "^X-Sun-Charset:" "^X-Accept-Language:" "^X-Envelope-Sender:" "^List-[A-Za-z]+:" "^X-Listprocessor-Version:" "^X-Received:" "^X-Distribute:" "^X-Sequence:" "^X-Juno-Line-Breaks:" "^X-Notes-Item:" "^X-MS-TNEF-Correlator:" "^x-uunet-gateway:" "^X-Received:" "^Content-length:" "^X-precedence:" "^X-Authenticated-User:" "^X-Comment:" "^X-Report:" "^X-Abuse-Info:" "^X-HTTP-Proxy:" "^X-Mydeja-Info:" "^X-Copyright:" "^X-No-Markup:" "^X-Abuse-Info:" "^X-From_:" "^X-Accept-Language:" "^Errors-To:" "^X-BeenThere:" "^X-Mailman-Version:" "^List-Help:" "^List-Post:" "^List-Subscribe:" "^List-Id:" "^List-Unsubscribe:" "^List-Archive:" "^X-Content-length:" "^X-Posting-Agent:" "^Original-Received:" "^X-Request-PGP:" "^X-Fingerprint:" "^X-WRIEnvto:" "^X-WRIEnvfrom:" "^X-Virus-Scanned:" "^X-Delivery-Agent:" "^Posted-Date:" "^X-Gateway:" "^X-Local-Origin:" "^X-Local-Destination:" "^X-UserInfo1:" "^X-Received-Date:" "^X-Hashcash:" "^Face:" "^X-DMCA-Notifications:" "^X-Abuse-and-DMCA-Info:" "^X-Postfilter:" "^X-Gpg-.*:" "^X-Disclaimer:")))
 '(gnus-message-setup-hook (quote (pjb-gnus-message-setup-meat)))
 '(gnus-nntp-server nil)
 '(gnus-play-startup-jingle nil)
 '(gnus-secondary-select-methods (quote ((nnml "") (nntp "news.gmane.org") (nnimap "voyager.informatimago.com"))))
 '(gnus-secondary-servers (quote ("news.gmane.org")))
 '(gnus-select-method (quote (nntp "news.individual.net")))
 '(gnus-spam-process-newsgroups (quote (("nnml:*" ((spam spam-use-stat))))))
 '(gnus-subscribe-newsgroup-method (quote gnus-subscribe-zombies))
 '(gnus-treat-display-x-face (quote head))
 '(gnus-use-nocem nil)
 '(gnus-uu-post-encode-method (quote gnus-uu-post-encode-mime))
 '(gnus-visible-headers (quote ("^From:" "^Newsgroups:" "^Subject:" "^Date:" "^Followup-To:" "^Reply-To:" "^Organization:" "^Summary:" "^Keywords:" "^To:" "^[BGF]?Cc:" "^Posted-To:" "^Mail-Copies-To:" "^Mail-Followup-To:" "^Apparently-To:" "^Gnus-Warning:" "^Resent-From:" "^Message-ID:" "^X-Sent:")))
 '(grep-command "grep -niH -e ")
 '(holiday-other-holidays (quote ((holiday-fixed 10 28 "Frédérique Saubot") (holiday-fixed 10 11 "Henri Bourguignon") (holiday-fixed 6 10 "Désirée Mayer") (holiday-fixed 3 23 "Françoise Keller") (holiday-fixed 11 25 "Joëlle Bourguignon") (holiday-fixed 12 16 "Agathe De Robert") (holiday-fixed 5 12 "Guillaume De Robert") (holiday-fixed 1 4 "Isabelle Saubot") (holiday-fixed 10 23 "Marc Moini") (holiday-fixed 2 10 "Anne-Marie Castel") (holiday-fixed 6 28 "Jean-François Gaillon") (holiday-fixed 6 28 "Sylvie Gaillon") (holiday-fixed 8 27 "Jean-Philippe Capy") (holiday-fixed 1 25 "Raoul Fruhauf") (holiday-fixed 3 15 "Pascal Bourguignon") (holiday-fixed 4 12 "Jalal Adamsah") (holiday-fixed 5 3 "Samy Karsenty") (holiday-fixed 8 17 "Alain Pierre") (holiday-fixed 1 14 "Bernard Bourguignon") (holiday-fixed 3 3 "Emmanuelle Chaize") (holiday-fixed 12 12 "Nicoleta Reinald") (holiday-fixed 1 3 "Florence Petit") (holiday-fixed 11 16 "Wei Van Chi") (holiday-fixed 12 6 "Marie Lecomte") (holiday-fixed 7 3 "Alain Bourguignon") (holiday-fixed 4 15 "André Reinald") (holiday-fixed 12 13 "Michelle Keller") (holiday-fixed 5 27 "Grégoire Saubot") (holiday-fixed 3 27 "Olivia De Robert") (holiday-fixed 11 18 "Vincent De Robert") (holiday-fixed 7 23 "Gabriel De Robert") (holiday-fixed 3 18 "Claire De Robert") (holiday-fixed 10 26 "Maxime De Robert") (holiday-fixed 3 26 "Edward-Amadeus Reinald") (holiday-fixed 3 4 "Louise Akiko Poullain") (holiday-fixed 8 26 "Iris-Alea Reinald") (holiday-fixed 9 4 "Baptiste Rouit") (holiday-fixed 2 22 "Camille Saubot") (holiday-fixed 8 2 "Clémence Saubot-Fiant") (holiday-fixed 5 29 "François Saubot") (holiday-fixed 1 2 "Henry Saubot") (holiday-fixed 2 8 "Jean-Pierre Baccache") (holiday-fixed 10 28 "Lucia (fille de Camille)") (holiday-fixed 11 26 "Marine Rouit") (holiday-fixed 3 13 "Mathias Fiant") (holiday-fixed 4 8 "Mathilde Rouit") (holiday-fixed 2 2 "Olivier Scmidt Chevalier") (holiday-fixed 2 23 "PtiDoigt Deamon") (holiday-fixed 8 10 "Kiteri (fille de Camille)") (holiday-fixed 9 10 "Remy Rouit") (holiday-fixed 8 7 "Valerie Saubot-Rouit") (holiday-fixed 1 6 "Los Reyes") (holiday-fixed 6 9 "Santa Murcia") (holiday-fixed 7 25 "Fiesta?") (holiday-fixed 10 12 "Los Reyes") (holiday-fixed 12 6 "Fiesta de la Consitución") (holiday-fixed 7 14 "Fête Nationale France"))) t)
 '(indent-tabs-mode nil)
 '(inferior-lisp-filter-regexp "\\`\\s*\\'")
 '(inihibit-default-init t)
 '(initial-major-mode (quote emacs-lisp-mode))
 '(ispell-choices-win-default-height 4)
 '(ispell-highlight-p t)
 '(ispell-local-dictionary "francais")
 '(ispell-local-dictionary-alist (quote (("francais" "[A-Za-zÀ-ÖØ-öø-ÿ]" "[^A-Za-zÀ-ÖØ-öø-ÿ]" "[-']" t nil "~latin9" iso-8859-15))) t)
 '(ispell-message-dictionary-alist (quote (("\"^Newsgroups:[ \\t]*fr\\\\.\"" . "\"francais\"") ("\"^To:[^\\n,]+\\\\.fr[ \\t\\n,>]\"" . "\"francais\"") ("\"^Newsgroups:[ \\t]*(es|mx|ar)\\\\.\"" . "\"castillano\"") ("\"^To:[^\\n,]+\\\\.(es|mx|ar)[ \\t\\n,>]\"" . "\"castillano\"") ("\"^Newsgroups:[ \\t]*uk\\\\.\"" . "\"english\"") ("\"^To:[^\\n,]+\\\\.uk[ \\t\\n,>]\"" . "\"english\"") ("\".*\"" . "\"american\""))))
 '(ispell-query-replace-choices nil)
 '(kept-new-versions 9)
 '(kept-old-versions 0)
 '(kill-whole-line t)
 '(line-number-mode t)
 '(lpr-page-header-switches (quote ("-F" "-t")))
 '(mail-archive-file-name nil)
 '(mail-bury-selects-summary t)
 '(mail-default-headers "Organization: InformatiMago.
X-PGP-Key-ID:      0xEF5E9966
X-PGP-fingerprint: 00 F5 7B DB CA 51 8A AD 04 5B 6C DE 32 60 16 8E EF 5E 99 66
X-PGP-Public-Key:  http://www.informatimago.com/pgpkey.asc
X-URL:             http://www.informatimago.com/index
X-Face: \":yO)Vk=vFU3)FL&2#7gT_G=KUuNv*BEOo+Shubl.V4Whu&;A.>.+&yEVB5I5vrpZIJ{yOW
 >CgV%jD]GHL6rp:.OCM~_YO&aY34]|`{yNq79\\x=g:7XSboBUj]1ULpA;v>-bS3veufw-rB!N0kZW!
 @A4i?z|
X-Accept-Language:         fr, es, en
MIME-Version: 1.0
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: 8bit
")
 '(mail-default-reply-to "<pjb@informatimago.com>")
 '(mail-host-address "informatimago.com")
 '(mail-interactive t)
 '(mail-mode-hook (quote (mail-abbrevs-setup (lambda nil (set-buffer-file-coding-system (quote utf-8) t t) (set-input-method default-input-method) (local-set-key "	" (quote expand-mail-aliases))))))
 '(mail-self-blind t)
 '(mail-setup-hook (quote ((lambda nil (local-set-key "	" (quote expand-mail-aliases))))))
 '(mail-signature t)
 '(mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^reply-to:\\|^return-path:\\|^mailing-list:\\|^precedence:\\|^x-\\|^content-\\|^cc:\\|^list-\\|^resent\\|^organization:\\|^sender:\\|^user-agent:\\|^mime-version:\\|^delivered-to:\\|^references:")
 '(mail-yank-prefix "> ")
 '(mark-even-if-inactive t)
 '(mark-holidays-in-calendar t t)
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
X-Disabled: X-No-Archive: no
")
 '(message-directory "~/mail/")
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
 '(mew-use-other-frame-for-draft t)
 '(mew-use-text/html t)
 '(mm-coding-system-priorities (quote (ascii iso-latin-1 iso-latin-9 utf-8)))
 '(mspools-update t)
 '(next-screen-context-lines 0)
 '(nntp-authinfo-file "~/.authinfo")
 '(org-agenda-files (quote ("~/notes.txt" "~/firms/wizards/notes.txt" "~/firms/willcom/notes.txt" "~/firms/secur.net/notes.txt" "~/firms/ravenpack/notes.txt" "~/firms/osii/notes.txt" "~/firms/medicalis/notes.txt" "~/firms/mappy/notes.txt" "~/firms/joellegymtonic/notes.txt" "~/firms/jem/notes.txt" "~/firms/intergruas/notes.txt" "~/firms/hf/notes.txt" "~/firms/hbedv/notes.txt" "~/firms/hamster-s-fabric-inc/notes.txt" "~/firms/camille/notes.txt" "~/firms/afaa/notes.txt")))
 '(ph-server "localhost" t)
 '(pjb-test-var 2 t)
 '(pop-up-frames nil)
 '(pr-faces-p t)
 '(printer-name "normal_gray" t)
 '(prolog-program-name "/usr/bin/pl")
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
 '(rmail-confirm-expunge nil)
 '(rmail-display-summary t)
 '(rmail-dont-reply-to-names "info-\\|\\(pjb\\|pascal\\)@triton.afaa.asso.fr\\|\\(pjb\\|pascal\\)@thalassa.afaa.asso.fr\\|669155386@correo.movistar.net\\|pjb@imaginet.fr\\|\\(pjb\\|pascal\\).bourguignon@afaa.asso.fr\\|\\(pjb\\|pascal\\)@afaa.asso.fr\\|pjb@afaa.asso.fr\\|pbourguignon@jazzfree.com\\|pbourguignon@jazzcyber.com\\|pajabou@worldonline.fr\\|pbo21957@worldonline.fr\\|\\(pjb\\|pascal\\)@informatimago.com\\|pjb@informatimago.com\\|informatimago@yahoo.es\\|informatimago@terra.es\\|informatimago@free.fr\\|pjb@larural.es\\|tradymago@etrademail.com\\|informatimago@users.sourceforge.net\\|pbourgui@afaa.asso.fr\\|grozilla@offcampus.es\\|latymer@jazzcyber.com\\|latymer_designs@yahoo.com\\|latymer@afaa.asso.fr\\|latymer.designs@afaa.asso.fr\\|latymer.designs@worldonline.fr\\|dla68836@worldonline.fr\\|latymer@worldonline.fr\\|idrv8338@worldonline.fr\\|\\(pjb\\|pascal\\|pascal.bourguignon\\)@informatimago.com")
 '(rmail-enable-mime nil)
 '(rmail-enable-multibyte t t)
 '(rmail-ignored-headers "^user-agent:\\|^\\(importa\\|precede\\)nce:\\|^priority:\\|^list-\\|^mailing-list\\|^via:\\|^mail-\\(from:\\|follow\\)\\|^\\(in-\\)?reply-to:\\|^sender:\\|^origin:\\|^references:\\|^status:\\|^received:\\|^summary-line:\\|^resent-\\|^\\(resent-\\)?message-id:\\|^nntp-posting-host:\\|^path:\\|^delivered-to:\\|^lines:\\|^mime-version:\\|^content-\\|^return-path:\\|^errors-to:\\|^return-receipt-to:\\|^x400-\\|^x-\\|^x-attribution:\\|^x-char.*:\\|^x-coding-system:\\|^x-face:\\|^x-mailer:\\|^x-disclaimer:\\|phone:")
 '(rmail-output-file-alist nil t)
 '(rmail-pop-password nil t)
 '(rmail-pop-password-required nil t)
 '(rmail-preserve-inbox nil)
 '(rmail-redisplay-summary t)
 '(rmail-remote-password nil)
 '(rmail-remote-password-required nil)
 '(rmail-secondary-file-directory "~/mail")
 '(rmail-summary-line-decoder (quote identity))
 '(rmail-summary-window-size 12)
 '(safe-local-variable-values (quote ((Package . SYSTEM) (Package . modlisp) (package . asdf) (Syntax . ansi-COMMON-LISP) (Package . cl-user) (Package . CYC-DEFSYS) (Patch-file . T) (Syntax . ANSI-COMMON-LISP) (Package . future-common-lisp-user) (Syntax . ansi-Common-lisp) (Package . SUBLISP) (Package . SUBLISP-INTERNALS) (Syntax . ANSI-Common-lisp) (No-Style-Shift . t) (Package . PTTP) (show-trailing-whitespace . t) (pretty-greek) (Package . CL-FAD) (Package . com\.ravenpack\.econoraven\.database) (Package . com\.ravenpack\.econoraven\.prediction) (Package . com\.ravenpack\.econoraven\.predictor) (Package . common-lisp-user) (Lowercase . T) (Package . Xlib) (Log . clx\.log) (Package . XLIB) (Lowercase . Yes) (show-nonbreak-escape) (Package . CL-WHO) (Package . CL-PPCRE) (Package . PS) (Package . UFFI) (Package . CLEVER-LOAD) (Package . REVISED^4-SCHEME) (Package . Memoization) (Package . DEMO-MENU) (Package . COMMON-LISP-USER) (egoge-buffer-language . english) (package . net\.aserve\.client) (Syntax . COMMON-LISP) (Package . CL-GD) (package . net\.html\.generator) (package . net\.aserve) (Eval cl-indent (quote with-item) 2) (package . pjb-cl) (Syntax . ansi-common-lisp) (Package . ALIEN) (Package . CL-USER) (coding-system . iso-8859-1-dos) (comment-start . ";") (pbook-heading-regexp . "^;;;\\(;+\\)") (pbook-commentary-regexp . "^;;;\\($\\|[^;]\\)") (Syntax . Common-lisp) (Package . DWIM) (byte-compile-warnings redefine callargs free-vars unresolved obsolete noruntime) (Syntax . Common-Lisp) (Package . HEMLOCK-EXT) (Syntax . ANSI-Common-Lisp) (Base . 10) (comment-start . "#") (package . COM\.INFORMATIMAGO\.COMMON-LISP\.VIRTUAL-FILE-SYSTEM) (package . COM\.INFORMATIMAGO\.COMMON-LISP\.SOURCE) (package . COM\.INFORMATIMAGO\.PJB) (standard-indent . 4) (Package . DTRACE) (unibyte . t))))
 '(sh-indent-after-case 0)
 '(sh-indent-after-switch 0)
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch t)
 '(slime-compilation-finished-hook (quote (slime-maybe-show-xrefs-for-notes)))
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
 '(slime-space-information-p nil)
 '(slime-startup-animation nil)
 '(spam-autodetect-recheck-messages t)
 '(stack-trace-on-error nil)
 '(tab-stop 4 t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64)))
 '(tab-width 4)
 '(tags-table-list (quote nil) t)
 '(tnt-use-timestamps t)
 '(tnt-username-alist (quote (("matimago") ("ogamita"))))
 '(tooltip-frame-parameters (quote ((nil . "tooltip") (right-fringe . 6) (left-fringe . 6) (nil . "lightyellow") (nil . 0) (nil . 1))))
 '(truncate-partial-width-windows nil)
 '(url-be-asynchronous t)
 '(url-honor-refresh-requests nil)
 '(user-full-name "Pascal J. Bourguignon")
 '(user-mail-address "pjb@informatimago.com")
 '(vc-annotate-background "snow1")
 '(vc-annotate-color-map (quote ((4.943848 . "#0000f0") (10.217285 . "#0000e0") (15.490723 . "#0000d0") (20.76416 . "#0000c0") (26.037598 . "#0000b0") (31.311035 . "#0000a0") (36.584473 . "#000090") (41.85791 . "#000080") (47.131348 . "#000070") (52.404785 . "#000060") (57.678223 . "#000050") (62.95166 . "#000040") (68.225098 . "#000030") (73.498535 . "#000020") (78.771973 . "#000010"))))
 '(vc-annotate-very-old-color "#000000")
 '(vc-make-backup-files t)
 '(version-control t)
 '(view-calendar-holidays-initially t t)
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
 '(w3-display-frames t)
 '(w3-do-incremental-display t)
 '(w3-honor-stylesheets nil)
 '(w3-horizontal-rule-char 45)
 '(w3-source-file-hook nil)
 '(w3-use-terminal-characters nil)
 '(w3-use-terminal-characters-on-tty nil)
 '(w3-user-colors-take-precedence t)
 '(w3-user-fonts-take-precedence t)
 '(w3m-coding-system (quote utf-8))
 '(w3m-default-display-inline-images t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(x-select-enable-clipboard t))


;; '(vm-spool-files '(("~/INBOX"
;;                     "/var/spool/mail/pjb"
;;                     "~/INBOX.local.crash")
;;                    ;; pop-ssl (nad pop) don't work, they merge emails.
;;                    ("~/INBOX"
;;                     "pop-ssl:voyager.informatimago.com:995:pass:pjb:g1ekquar"
;;                     "~/INBOX-voyager.crash")
;;                    ("~/INBOX"
;;                     "pop-ssl:correo.intergruas.com:995:pass:pjb:a-los-bel"
;;                     "~/INBOX-intergruas.crash")
;;                    ))



(.EMACS "enabling features")
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'mh-rmail         'disabled t)
(put 'scroll-left      'disabled nil)
(put 'set-goal-column  'disabled t)
(put 'erase-buffer     'disabled nil)




(setf tetris-score-file "~/.tetris-scores")

(setf fancy-splash-text
      '(
        (:face (variable-pitch :weight bold) "
WELCOME TO EMACS!
"
               ;; :face variable-pitch "Text"
               ;; :face (variable-pitch :weight bold :slant oblique) "Text"
               ;; :face variable-pitch function
               )
        (:face (variable-pitch :weight bold) "
-%- WELCOME TO EMACS -%-
")))
(setf fancy-splash-text nil)

(case system-type
  ((darwin)
   (setf browse-url-netscape-program "/sw/bin/mozilla"
         browse-url-firefox-program  "/opt/local/bin/firefox"))
  ((gnu/linux)
   (setf browse-url-netscape-program "/usr/local/apps/netscape-7.02/netscape"
         browse-url-firefox-program  "/usr/bin/firefox")))



;;;----------------------------------------------------------------------------
;; (spam-initialize)
;; (gnus-registry-initialize)
;; (setq spam-split-group "mail.spamgate"
;;       spam-use-spamassassin-headers t
;;       spam-use-bogofilter t
;; 
;;       gnus-spam-process-newsgroups
;;       '(("mail\\.*" ((spam spam-use-bogofilter))))
;; 
;;       gnus-spam-newsgroup-contents
;;       '(("mail.spamgate" gnus-group-spam-classification-spam))
;; 
;;       spam-mark-only-unseen-as-spam t
;;       spam-mark-ham-unread-before-move-spam-from-group t
;;       gnus-ham-process-destinations '(("mail\\.spamgate"
;; "mail.inbox"))
;;       gnus-spam-process-destinations '(("mail\\..*"
;; "mail.spam.expired"))
;;       spam-log-to-registry t
;;       gnus-registry-max-entries 4000)


;; (setf spam-use-stat t
;;       spam-use-spamoracle nil
;;       spam-split-group "mail.junk"
;;       spam-log-to-registry nil
;;       gnus-registry-max-entries 4000
;;       gnus-spam-process-newsgroups '(("mail\\.*" ((spam spam-use-stat)))))
;; (spam-initialize)




;;;----------------------------------------------------------------------------
;;; cl-like functions
;;;----------------------------------------------------------------------------

(define-modify-macro appendf (&rest args) append "Append onto list")

(defmacro defconstant (symbol initvalue &optional docstring)
  `(defconst ,symbol ,initvalue ,docstring))

(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))


(defun string* (x) (etypecase x
                     (integer (string x))
                     (symbol (symbol-name x))
                     (string  x)))
(defun string-downcase (x) (downcase (string* x)))
(defun string-upcase   (x) (upcase   (string* x)))
(defun character (x) (etypecase x
                       (integer x)
                       (string (aref x 0))
                       (symbol (aref (symbol-name x) 0))))
(defun char= (x y) (= x y))
(defun* string-equal* (str1 str2 &key (start1 0) (end1) (start2 0) (end2))
  (string= (string-upcase
            (if (and (= 0 start1) (or (null end1) (= end1 (length str1))))
                str1
              (subseq str1 start1 (or end1 (length str1)))))
           (string-upcase
            (if (and (= 0 start2) (or (null end2) (= end2 (length str2))))
                str2
              (subseq str2 start2 (or end2 (length str2)))))))


(defun user-homedir-pathname ()
  (if user-init-file
      (dirname user-init-file)
    (dirname (first (file-expand-wildcards "~/.emacs")))))
(defun namestring (path) path)
(defun pathname-name (path)
  (let ((path (basename path)))
    (if (string-match "^\\(.*\\)\\.[^.]*$" path)
        (match-string 1 path)
      path)))
;; (mapcar (lambda (x) (list (dirname x) (basename x) (pathname-name x)))
;;         '("abc" "abc.def" "abc.def.ghi"
;;           "/abc" "/abc.def" "/abc.def.ghi"
;;           "./abc" "./abc.def" "./abc.def.ghi"
;;           "ddd/abc" "ddd/abc.def" "ddd/abc.def.ghi"
;;           "eee/ddd/abc" "eee/ddd/abc.def" "eee/ddd/abc.def.ghi"
;;           "/eee/ddd/abc" "/eee/ddd/abc.def" "/eee/ddd/abc.def.ghi"))


(defun dirname (path)
  (if (string-match "^\\(.*/\\)\\([^/]*\\)$" path)
      (match-string 1 path)
    "./"))
(defun basename (path)
  (if (string-match "^\\(.*/\\)\\([^/]*\\)$" path)
      (match-string 2 path)
    path))


(defun prefixp (prefix string)
  "
PREFIX:  A sequence.
STRING:  A sequence.
RETURN:  Whether PREFIX is a prefix of the STRING.
"
  (string= prefix (subseq string 0 (min (length string) (length prefix)))))


;;;----------------------------------------------------------------------------
;;; File access rights
;;;----------------------------------------------------------------------------

(defun octal (n)
  "N is a decimal numbers whose digits are taken as octal digits
and converted as such."
  (loop
   for d across (format "%d" n)
   for r = (digit-char-p d) then (+ (* 8 r) (digit-char-p d))
   finally (return r)))

(defun chmod (file mode)
  (interactive "fFile path: \nXMode: ")
  (set-file-modes file mode))

(set-default-file-modes (octal 755))


;;;----------------------------------------------------------------------------
;;; File and directory stuff
;;;----------------------------------------------------------------------------

(defun first-existing-file (list-of-files)
  "Finds the first file in LIST-OF-FILES that exists.
"
  (find-if (function file-exists-p) list-of-files))

(defun map-existing-files (function list-of-files)
  "Calls FUNCTION on each file in LIST-OF-FILES that exists, and returns the list of results.
"
  (let ((result '()))
    (dolist (file list-of-files (nreverse result))
      (when (file-exists-p file)
        (push (funcall function file) result)))))


(defun remove-non-existing-files (list-of-files)
  "Returns the LIST-OF-FILES with non-existing files removed.
"
  (remove-if-not (function file-exists-p) list-of-files))


(defmacro* with-file (file-and-options &body body)
  "Processes BODY with a buffer on the given file.
DO:              find-file or find-file-literally, process body, and
                 optionally save the buffer and kill it.
                 save is not done if body exits exceptionnaly.
                 kill is always done as specified.
FILE-AND-OPTION: either an atom evaluated to a path,
                 or (path &key (save t) (kill t) (literal nil))
"
  (if (atom file-and-options)
      `(with-file (,file-and-options) ,@body)
    ;; destructuring-bind is broken, we cannot give anything else than nil
    ;; as default values:
    (destructuring-bind (path &key (save nil savep) (kill nil killp)
                              (literal nil literalp))
        file-and-options
      (unless savep (setf save t))
      (unless killp (setf kill t))
      `(unwind-protect
           (progn
             (,(if literal 'find-file-literally 'find-file) ,path)
             (prog1 (save-excursion ,@body)
               ,(when save `(save-buffer 1))))
         ,(when kill
            `(kill-buffer (current-buffer)))))))


(defvar *directories* '() "A cache for the ~/directories.txt dictionary.")
;; (setf  *directories* '())


(defun load-directories (&optional directories-file)
  "Loads ~/directories.txt (or the given DIRECTORIES-FILE),
and stores it in `*directories*'.
"
  (let ((directories-file (or directories-file "~/directories.txt")))
    (setf *directories*
          (progn
            (find-file directories-file)
            (prog1
                (loop
                 for (k v)
                 on (split-string (buffer-substring-no-properties
                                   (point-min) (point-max)))
                 by (function cddr)
                 nconc (list (intern (format ":%s" (substitute ?- ?_ (downcase k))))
                             v))
              (kill-buffer (current-buffer)))))))


(defun get-directory (key &optional subpath)
  "
RETURN: The directory in ~/directories.txt for the key, concatenated with the subpath.
NOTE:   ~/directories.txt is cached in *directories*.
"
  (unless *directories*
    (load-directories))
  (unless  (getf *directories* key)
    (error "get-directory: No directory keyed %s" key))
  (let ((dir (getf *directories* key)))
    (if (or (null subpath) (string= "" subpath))
        dir
      (flet ((lastchar (str) (and (< 0 (length str)) (aref str (1- (length str)))))
             (firstchar (str) (and (< 0 (length str)) (aref str 0)))
             (xor (a b) (or (and a (not b)) (and (not a) b))))
        (if (xor (eql ?/ (lastchar dir)) (eql ?/ (firstchar subpath)))
            (concat dir subpath)
          (concat dir "/" subpath))))))


;;;----------------------------------------------------------------------------
;;; Setting up load-path
;;;----------------------------------------------------------------------------
;;
;;
;; When we start, emacs has already filled load-path with
;; installation-local directories.
;;
;; So we only need to add the directories of specific packages (that
;; could be used in the various emacs installations).  It also means
;; that installing an emacs package must occur either in an emacs
;; specific installation (notably if .elc are compiled for this
;; specific version), or in  the package specific directory.
;;
;; If any of these directories contain one of the site or subdir el
;; files, then it is loaded too.
;;

(defun dump-load-path ()
  (interactive)
  (dolist (x load-path)
    (princ x)
    (terpri)))

(message "old load-path = %S" (with-output-to-string (dump-load-path)))

(let ((new-paths '())
      (base-load-path (copy-list load-path)))
  (flet ((add-if-good (site-lisp)
                      (when (file-exists-p site-lisp)
                        (pushnew site-lisp new-paths)
                        (mapc (lambda (file)
                                (let ((file (concat site-lisp "/" file)))
                                  (when (file-exists-p file)
                                    (.EMACS "%s FOUND" file)
                                    (let ((default-directory site-lisp))
                                      (load file *pjb-load-noerror* *pjb-load-silent*)))))
                              '("site-start.el" "site-gentoo.el" "subdirs.el"))
                        t)))
    (dolist (directories
             ;; When several directories are listed in a sublist, only
             ;; the first found directory will be added.
             (append
              (case emacs-major-version
                ((20 21 22)
                 (append '("/opt/lisp/emacs"
                           "/opt/local/share/emacs/site-lisp"
                           "/usr/local/share/emacs/site-lisp")
                         '("/opt/clisp-2.48/share/emacs/site-lisp"
                           "/opt/clisp-2.48-newclx/share/emacs/site-lisp"
                           "/opt/clisp-2.48-mitclx/share/emacs/site-lisp"
                           "/opt/clisp-2.47/share/emacs/site-lisp"
                           "/opt/clisp-2.46/share/emacs/site-lisp"
                           "/opt/clisp-2.41-pjb1-regexp/share/emacs/site-lisp")
                         '("/opt/smalltalk-3.0.4/share/emacs/site-lisp")
                         ))
                ((23)
                 '())
                (otherwise
                 (.EMACS "WARNING: No load-paths for emacs version %d" emacs-major-version)
                 '()))
              (list
               ;; -----------------
               ;; PJB emacs sources
               ;; -----------------
               ;; Since we may have several emacs version running
               ;; on the same system, for now we will avoid
               ;; compiling pjb sources, and we will load them
               ;; directly from ~/src/public/emacs/.  Later we
               ;; will see how we can install elc in version
               ;; specific directories, but keeping references to
               ;; the same source directory.
               (concat (getenv "HOME") "/src/public/emacs/")
               ;; (get-directory :share-lisp "packages/com/informatimago/emacs/")
               )))
      (if (listp directories)
          (find-if (function add-if-good) directories)
        (add-if-good directories)))
    (setf load-path (append base-load-path
                            new-paths
                            (set-difference load-path base-load-path :test (function equal))))))



(unless (prefixp "mdi-development-" *hostname*)
 (setf load-path (list* "~/opt/share/emacs/site-lisp/slime/contribs/"
                        "~/opt/share/emacs/site-lisp/slime/"
                        load-path)))

;; (message "new load-path = %S" (with-output-to-string (dump-load-path)))

(map-existing-files (lambda (dir) (pushnew dir exec-path))
                    '("/sw/sbin/" "/sw/bin/" "/opt/local/sbin" "/opt/local/bin"))


;;;----------------------------------------------------------------------------
;;; CEDET / EIEIO
;;;----------------------------------------------------------------------------
(when (or (require 'cedet nil t)
          (require 'eieio nil t))
  ;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
  ;; Select one of the following:

  ;; * This enables the database and idle reparse engines
  ;;(semantic-load-enable-minimum-features)

  ;; * This enables some tools useful for coding, such as summary mode
  ;;   imenu support, and the semantic navigator
  ;; (semantic-load-enable-code-helpers)

  ;; * This enables even more coding tools such as the nascent intellisense mode
  ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
  ;; (semantic-load-enable-guady-code-helpers)

  ;; * This turns on which-func support (Plus all other code helpers)
  ;; (semantic-load-enable-excessive-code-helpers)

  ;; This turns on modes that aid in grammar writing and semantic tool
  ;; development.  It does not enable any other features such as code
  ;; helpers above.
  ;; (semantic-load-enable-semantic-debugging-helpers)
  (setf eieio-skip-typecheck t))


;;;----------------------------------------------------------------------------
;;; Emacs-CL
;;;----------------------------------------------------------------------------
(defvar *pjb-emacs-cl-present-p* nil)
(when (load "load-cl" t)
  (setf *pjb-emacs-cl-present-p* t)
  (message "emacs-cl streams = %S" (list  *STANDARD-INPUT*  
                                          *STANDARD-OUTPUT* 
                                          *TERMINAL-IO*))
  (let ((stream (make-buffer-output-stream "*scratch*")))
    (setf *STANDARD-INPUT*  stream
          *STANDARD-OUTPUT* stream
          *TERMINAL-IO*     stream)))


;;;----------------------------------------------------------------------------
(defvar *milliways* '())
(defun milliways-run ()
  (interactive)
  (dolist (function *milliways*) (funcall function)))
(defun milliways-activate (&optional delay)
  "Called at the end of ~/.emacs"
  (run-at-time (or delay 5)
               5
               (function milliways-run)
               nil))
(defun milliways-schedule (function)
  "Schedule the function to be called after emacs started."
  (push function *milliways*))

(milliways-schedule (lambda () (message "Welcome to the Restaurant at the End of the Universe!")))




;;;----------------------------------------------------------------------------
;;; Language & character encoding stuff:
;;;----------------------------------------------------------------------------

;; (standard-display-european 1) ;;is semi-obsolete, but it works better than:
;; (standard-display-8bit 128 255)

(set-input-mode nil nil t nil) ;; INTERRUPT FLOW META [QUIT]
;; (setq meta-prefix-char nil) ; to split ESC from M- 

(when (fboundp 'unify-8859-on-encoding-mode)
  (unify-8859-on-encoding-mode 1))
(when (fboundp 'unify-8859-on-decoding-mode)
  (unify-8859-on-decoding-mode 1))

(setq default-enable-multibyte-characters      t
      unibyte-display-via-language-environment nil)

;; (setq my-latin (if (assoc-ignore-case "Latin-9" language-info-alist) 9 1))
;; ;; For coding-system we don't specify *-unix to allow it to load DOS files.
;; (cond
;;   ((= my-latin 1) (setq my-lenv     "Latin-1"
;;                         my-encoding 'iso-8859-1
;;                         x-encoding  "iso8859-1"))
;;   ((= my-latin 9) (setq my-lenv     "Latin-9"
;;                         my-encoding 'iso-8859-15
;;                         x-encoding  "iso8859-15"))
;;   (t (error "Invalid value for my-latin variable.")))

(progn
  (case system-type
    (darwin 

     (set-language-environment                "utf-8")
     (prefer-coding-system                    'utf-8-unix)

     ;; (set-buffer-file-coding-system           'utf-8-unix)
     ;; (set-buffer-process-coding-system        'utf-8-unix)
     (set-clipboard-coding-system             'utf-8-unix)
     (set-file-name-coding-system             'utf-8-unix)
     (set-keyboard-coding-system              'utf-8-unix)
     (set-next-selection-coding-system        'utf-8-unix)
     (set-selection-coding-system             'utf-8-unix)
     (set-terminal-coding-system              'utf-8-unix)

     
     (setq default-buffer-file-coding-system  'utf-8-unix
           default-file-name-coding-system    'utf-8-unix
           default-terminal-coding-system     'utf-8-unix
           default-keyboard-coding-system     'utf-8-unix
           default-sendmail-coding-system     'utf-8-unix
           default-process-coding-system      '(utf-8-unix . utf-8-unix))
     (modify-coding-system-alist 'process ".*shell\\'"     'utf-8-unix)
     (modify-coding-system-alist 'process "\\*shell\\*\\'" 'utf-8-unix)
     (modify-coding-system-alist 'process ".*lisp\\'"      'utf-8-unix))
    (otherwise
     (set-language-environment                "utf-8")
     (prefer-coding-system                    'utf-8-unix)
     (set-default-coding-systems              'utf-8-unix)
     (set-keyboard-coding-system              'iso-8859-1-unix)
     (set-terminal-coding-system              'iso-8859-1-unix)
     (set-clipboard-coding-system             'utf-8-unix)
     (set-selection-coding-system             'utf-8-unix)
     (setq default-buffer-file-coding-system  'utf-8-unix
           default-file-name-coding-system    'utf-8-unix
           default-terminal-coding-system     'iso-8859-1-unix
           default-keyboard-coding-system     'iso-8859-1-unix
           default-sendmail-coding-system     'utf-8-unix
           default-process-coding-system      '(utf-8-unix . utf-8-unix))
     (modify-coding-system-alist 'process ".*shell\\'"     'utf-8-unix)
     (modify-coding-system-alist 'process "\\*shell\\*\\'" 'utf-8-unix)
     (modify-coding-system-alist 'process ".*lisp\\'"      'utf-8-unix)))

  (dolist (cs coding-system-list nil)
    (modify-coding-system-alist 'file (format "\\.%s\\'" cs) cs)))

;; and/or set locales like LC_ALL, LC_CTYPE, LANG to contain UTF-8 as for
;; example: LANG=de_DE.UTF-8. Modern Emacsen, I think 21.3 at least,
;; derive their mode of operation from this.

;; You can start your text files à la: ;;; -*- mode: Text; coding: utf-8 -*-
;; Once you've done that you can C-x RET r:
;; revert-buffer-with-coding-system.


;; 	(set-language-environment		'German)
;; 	(setq default-file-name-coding-system	'utf-8)
;; 	(setq file-name-coding-system		'utf-8)
;; 	(setq default-buffer-file-coding-system 'iso-latin-9-unix))
;; 	(set-default-coding-systems		'mac-roman-unix)
;; 	;(setq mac-keyboard-text-encoding	 kTextEncodingISOLatin1)
;; 	(set-keyboard-coding-system		'sjis-mac)
;; 	(set-clipboard-coding-system		'sjis-mac)
;; 	(prefer-coding-system			'mac-roman-unix)
;; 	(modify-coding-system-alist	 'file "\\.tex\\'" 'iso-latin-9-unix)
;; 	(modify-coding-system-alist	 'process
;; "\\*[Ss][Hh][Ee][Ll][Ll].*\\'"  'utf-8-unix)
;; 	;(set-buffer-process-coding-system	'utf-8 'utf8)



;;;----------------------------------------------------------------------------
;;; On Macintosh
;;;----------------------------------------------------------------------------

(when (or (boundp 'aquamacs-version)
          (eq window-system 'ns))
  (setf initial-frame-alist '((background-color . "#ddffee")
                              (left . 76)
                              (top . 20)
                              (width . 80)
                              (height . 60))
        default-frame-alist (append initial-frame-alist default-frame-alist)
        cursor-type 'box)
  (when (fboundp 'smart-frame-positioning-mode)
    (smart-frame-positioning-mode nil))
  (when (load "scroll-bar" nil t)
    (defun scroll-bar-columns (side)
      "Return the width, measured in columns, of the vertical scrollbar on SIDE.
SIDE must be the symbol `left' or `right'."
      (let* ((wsb   (window-scroll-bars))
             (vtype (nth 2 wsb))
             (cols  (nth 1 wsb)))
        (cond
         ((not (memq side '(left right nil)))
          (error "`left' or `right' expected instead of %S" side))
         ((and (eq vtype side) cols))
         ((eq (frame-parameter nil 'vertical-scroll-bars) side)
          ;; nil means it's a non-toolkit scroll bar, and its width in
          ;; columns is 14 pixels rounded up.
          (ceiling (or (frame-parameter nil 'scroll-bar-width) 14)
                   (frame-char-width)))
         (0))))))




;;;----------------------------------------------------------------------------
;;; Global key map
;;;----------------------------------------------------------------------------
(.EMACS "Setting global key map")


;; feature simple is not provided on emacs < 22, so we use load-library:
(load-library "simple") (define-key ctl-x-map              "." nil)
(require 'iso-transl)   (define-key iso-transl-ctl-x-8-map "E" [342604])



;; For control, there's no distinction between shift and plain!
;; The only control keys are: @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
;; \C-^ is free.
;; Cannot use \C-i because it's TAB
;; Cannot use \C-m because it's CR
;; Cannot use \C-[ because it's ESC!
;; Cannot use \C-{ or \C-}  (no distinction between shift and plain)
;;
;; [?\C-c f5]
;; [(control c) f5]
;; (kbd "C-c <f5>")
;;
;; C-z is used now by elscreen.



(defun disabled ()
  (interactive)
  (beep))

(defun insert-sharp-brace ()
  (interactive)
  (insert "#[]")
  (forward-char -1))

(defun reset-movement-keypad ()
  "Locally set the keys <insert>, <suppr>, <home>, <end>, <prior> and <next>."
  (interactive)
  (local-set-key (kbd "<home>")        'beginning-of-buffer)
  (local-set-key (kbd "<end>")         'end-of-buffer)
  (local-set-key (kbd "<prior>")       'scroll-down)
  (local-set-key (kbd "<next>")        'scroll-up))

(defun swap-brackets-parens ()
  (interactive)
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\] ?\)))

(defun normal-brackets-parens ()
  (interactive)
  (keyboard-translate ?\( ?\()
  (keyboard-translate ?\) ?\))
  (keyboard-translate ?\[ ?\[)
  (keyboard-translate ?\] ?\]))

(defun translate-powerbook-keyboard ()
  (interactive)
  (keyboard-translate ?\§ ?\`)
  (keyboard-translate ?\± ?\~))


(defmacro define-force-justification (direction)
  `(defun ,(intern (format "force-set-justification-%s" direction)) (start end)
     (interactive "r")
     (let ((mode major-mode))
       (text-mode)
       (,(intern (format "set-justification-%s" direction))  start end)
       (funcall mode))))



(defun pjb-terminal-key-bindings ()
  (interactive)
  (global-set-key "OF"    (function end-of-buffer))
  (global-set-key "OH"    (function beginning-of-buffer))
  (global-unset-key "[")
  (global-set-key "[15~"  (function set-justification-left)) ; <f5>
  (global-set-key "[17~"  (function set-justification-center)) ; <f6>
  (global-set-key "[18~"  (function set-justification-right)) ; <f7>
  (global-set-key "[19~"  (lambda()(interactive)(beep)))  ; <f8>
  (global-set-key "[20~"  (lambda()(interactive)(beep)))  ; <f9>
  (global-set-key "[21~"  (lambda()(interactive)(beep)))  ; <f10>
  (global-set-key "[23~"  (lambda()(interactive)(beep)))  ; <f11>
  (global-set-key "[24~"  (lambda()(interactive)(beep)))  ; <f12>
  )


(defun pjb-global-key-bindings ()
  (interactive)

  (define-force-justification left)
  (define-force-justification center)
  (define-force-justification right)
  (define-force-justification full)

  ;; Advance key map setting: get a sane keyboard when loading this file fails.
  (global-set-key (kbd "C-x RET C-h")   'describe-prefix-bindings)
  ;; (global-set-key (kbd "C-x 5 o")     'other-frame-non-excluded)

  (global-set-key (kbd "<home>")        'beginning-of-buffer)
  (global-set-key (kbd "<end>")         'end-of-buffer)
  (global-set-key (kbd "<prior>")       'scroll-down)
  (global-set-key (kbd "<next>")        'scroll-up)

  (global-set-key (kbd "C-c C-s")       'search-forward-regexp)
  (global-set-key (kbd "C-c C-r")       'search-backward-regexp)

  (global-set-key (kbd "<f5>")          'set-justification-left)
  (global-set-key (kbd "<f6>")          'set-justification-full)
  (global-set-key (kbd "<f7>")          'set-justification-right)
  (global-set-key (kbd "C-c <f5>")      'force-set-justification-left)
  (global-set-key (kbd "C-c <f6>")      'force-set-justification-full)
  (global-set-key (kbd "C-c <f7>")      'force-set-justification-right)
  (global-set-key (kbd "C-c .")         'forward-sexp)
  (global-set-key (kbd "C-c ,")         'backward-sexp)
  (global-set-key (kbd "C-x DEL")       'disabled)
  (global-set-key (kbd "C-<delete>")    'disabled)
  (global-set-key (kbd "C-<backspace>") 'disabled)
  (global-set-key (kbd "A-x")           'insert-sharp-brace)
  (global-set-key (kbd "M-[")           'insert-parentheses)
  (global-set-key (kbd "M-]")           'move-past-close-and-reindent)
  (global-set-key "\M-["                'insert-parentheses)
  (global-set-key "\M-]"                'move-past-close-and-reindent)

  (global-set-key (kbd "C-<f9>")  (lambda()(interactive)(set-input-method 'chinese-py-b5)))
  (global-set-key (kbd "C-<f10>") (lambda()(interactive)(set-input-method 'cyrillic-yawerty)))
  (global-set-key (kbd "C-<f11>") (lambda()(interactive)(set-input-method 'greek)))
  (global-set-key (kbd "C-<f12>") (lambda()(interactive)(set-input-method 'hebrew)))
  ;; (autoload 'hebr-switch  "hebwork"  "Toggle Hebrew mode.")
  ;; (global-set-key (kbd "C-<f12>") 'hebr-switch)

  (when (and (fboundp 'pjb-search-backward-region)
             (fboundp 'pjb-search-forward-region))
    (global-set-key (kbd "<f9>")   'pjb-search-backward-region)
    (global-set-key (kbd "<f10>")  'pjb-search-forward-region))

  (when (and (fboundp 's2p-calculette)
             (fboundp 's2p-calculette-to-lisp))
    (global-set-key (kbd "C-c =")  's2p-calculette)
    (global-set-key (kbd "C-c +")  's2p-calculette-to-lisp))

  (global-set-key (kbd "C-c _")    'google-search-region)
  (when (fboundp 'invoke-ding-dictionary)
    (global-set-key (kbd "C-c -")   'invoke-ding-dictionary))

  ;; (delete-selection-mode t)
  (if (fboundp 'delete-region-and-yank)
      (global-set-key (kbd "C-y")  'delete-region-and-yank) 
    (global-set-key (kbd "C-y")  'yank))

  ;; A strange configuration with a narrow frame...
  (when (< (frame-parameter (selected-frame) 'width) 42)
    (global-set-key (kbd "C-h") 'backward-delete-char-untabify))

  (case window-system
    ((nil)
     (.EMACS "Setting terminal keyboard")
     (pjb-terminal-key-bindings)
     (set-keyboard-coding-system 'iso-8859-15)
     (normal-erase-is-backspace-mode 0)
     (.EMACS "C-h = %S" (key-binding "\C-h"))
     (.EMACS "DEL = %S" (key-binding "\C-?")))
    ((x)
     (.EMACS "Setting X keyboard")
     (define-key global-map [(delete)]    "\C-d")
     (make-face-bold 'bold-italic))
    ((mac)
     (.EMACS "Setting Macintosh keyboard")
     (setq *window-manager-y-offset* (+ 24 24))
     (set-keyboard-coding-system 'mac-roman)
     (setq mac-command-key-is-meta t
           mac-reverse-ctrl-meta   nil)
     (translate-powerbook-keyboard)))
  nil)


(pjb-global-key-bindings)





(defvar scroll-page-delimiter "")
(make-local-variable 'scroll-page-delimiter)
(setf scroll-page-delimiter "Software Design Notes")

(defun scroll-page-up ()
  (interactive)
  (if (re-search-forward scroll-page-delimiter nil t)
      (progn
        (goto-char (match-beginning 0))
        (recenter 0)
        (forward-line 1))
    (message ".EMACS: Last page")))

(defun scroll-page-down ()
  (interactive)
  (if (re-search-backward scroll-page-delimiter nil t 2)
      (progn
        (goto-char (match-beginning 0))
        (recenter 0)
        (forward-line 1))
    (message ".EMACS: First page")))

(defvar scroll-page-mode nil)
(make-local-variable 'scroll-page-mode)

(defun scroll-page-mode ()
  (interactive)
  (if scroll-page-mode
      (progn
        (local-set-key (kbd "<next>")  'scroll-up)
        (local-set-key (kbd "<prior>") 'scroll-down)
        (setf scroll-page-mode nil))
    (progn
      (local-set-key (kbd "<next>")  'scroll-page-up)
      (local-set-key (kbd "<prior>") 'scroll-page-down)
      (setf scroll-page-mode t))))





(standard-display-ascii #o200 (vector (decode-char 'ucs #x253c)))
(standard-display-ascii #o201 (vector (decode-char 'ucs #x251c)))
(standard-display-ascii #o202 (vector (decode-char 'ucs #x252c)))
(standard-display-ascii #o203 (vector (decode-char 'ucs #x250c)))
(standard-display-ascii #o204 (vector (decode-char 'ucs #x2524)))
(standard-display-ascii #o205 (vector (decode-char 'ucs #x2502)))
(standard-display-ascii #o206 (vector (decode-char 'ucs #x2510)))
(standard-display-ascii #o210 (vector (decode-char 'ucs #x2534)))
(standard-display-ascii #o211 (vector (decode-char 'ucs #x2514)))
(standard-display-ascii #o212 (vector (decode-char 'ucs #x2500)))
(standard-display-ascii #o214 (vector (decode-char 'ucs #x2518)))
(standard-display-ascii #o220 [? ])
(standard-display-ascii #o221 [?\` ])
(standard-display-ascii #o222 [?\'])
(standard-display-ascii #o223 [?\"])
(standard-display-ascii #o224 [?\"])
(standard-display-ascii #o225 "* ")
(standard-display-ascii #o226 "--")
(standard-display-ascii #o227 " -- ")


;; some more global key map are defined after loading my personal files below.


;;;----------------------------------------------------------------------------
(.EMACS "Loading my personal files -- My own stuff.")
(unless (load "pjb-loader.el" t)
  (.EMACS "WARNING WARNING WARNING: Could not find and load 'My own stuff'!"))


;;;----------------------------------------------------------------------------
(.EMACS "setting up fonts")
;; See also:
;; (info "(emacs)Defining Fontsets")

(when (< emacs-major-version 22)
  (require 'font nil t)

  (defun font-spatial-to-canonical (spec &optional device)
    "Convert SPEC (in inches, millimeters, points, or picas) into points"
    ;; 1 in = 6 pa = 25.4 mm = 72 pt
    (cond
      ((numberp spec)
       spec)
      ((null spec)
       nil)
      (t
       (let ((num nil)
             (type nil)
             ;; If for any reason we get null for any of this, default
             ;; to 1024x768 resolution on a 17" screen
             (pix-width (float (or (device-pixel-width device) 1024)))
             (mm-width (float (or (device-mm-width device) 293)))
             (retval nil))
         (cond
           ((string-match "^ *\\([-+*/]\\) *" spec) ; math!  whee!
            (let ((math-func (intern (match-string 1 spec)))
                  (other (font-spatial-to-canonical
                          (substring spec (match-end 0) nil)))
                  (default (font-spatial-to-canonical
                            (font-default-size-for-device device))))
              (if (and default (fboundp math-func))
                  (setq type "px"
                        spec (int-to-string (funcall math-func default other)))
                  (setq type "px"
                        spec (int-to-string other)))))
           ((string-match "[^0-9.]+$" spec)
            (setq type (substring spec (match-beginning 0))
                  spec (substring spec 0 (match-beginning 0))))
           (t
            (setq type "px"
                  spec spec)))
         (setq num (string-to-number spec))
         (cond
           ((member type '("pixel" "px" "pix"))
            (setq retval (* num (/ pix-width mm-width) (/ 25.4 72.0))))
           ((member type '("point" "pt"))
            (setq retval num))
           ((member type '("pica" "pa"))
            (setq retval (* num 12.0)))
           ((member type '("inch" "in"))
            (setq retval (* num 72.0)))
           ((string= type "mm")
            (setq retval (* num (/ 72.0 25.4))))
           ((string= type "cm")
            (setq retval (* num 10 (/ 72.0 25.4))))
           (t
            (setq retval num)))
         retval))))


  (when  (boundp 'x-font-alist)
    ;; Correct the font menu.
    (setf x-font-alist
          (let ((monop (find "monospaced fonts" (rest x-font-alist)
                             :test (function string=)
                             :key (function first))))
            (cons (first x-font-alist)
                  (loop for (a b) on (rest x-font-alist)
                     unless (equalp a b)
                     collect (cond
                               (monop a)
                               ((string= (first a) "proportional fonts")
                                '("monospaced fonts"   nil))
                               ((string= (first a) "non-proportional fonts")
                                '("proportional fonts" nil))
                               (t a)))))))
  );; when emacs-major-version < 23



(defparameter *pjb-font-list*
  '(
    "-b&h-lucidatypewriter-medium-r-normal-sans-8-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-10-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-11-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-12-*-*-*-m-*-*-*"
    "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
    "-b&h-lucidatypewriter-bold-r-normal-sans-12-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-medium-r-normal-sans-14-*-*-*-m-*-*-*"
    "-b&h-lucidatypewriter-bold-r-normal-sans-14-*-*-*-m-*-*-*"
    ))

(defvar *pjb-current-font-index* 0)

(defun sign (number)
  (cond ((< number 0) -1)
        ((> number 0) +1)
        (t             0)))

(defun* forward-font (&optional (increment 1))
  (interactive "p")
  (setf *pjb-current-font-index* (mod (+ *pjb-current-font-index* increment)
                                      (length *pjb-font-list*)))
  (loop
     for try below (length *pjb-font-list*)
     do (ignore-errors
          (return
            (progn (set-frame-font (elt *pjb-font-list* *pjb-current-font-index*))
                   (message "Set frame font %S" (elt *pjb-font-list* *pjb-current-font-index*)))))
     do (message "Failed to set frame font %S" (elt *pjb-font-list* *pjb-current-font-index*))
     do (setf *pjb-current-font-index* (mod (+ *pjb-current-font-index* (sign increment))
                                            (length *pjb-font-list*)))))

(forward-font 3)

;; (when (eq window-system 'x)
;;   (set-frame-font 
;;    (if (fboundp 'font-exists-p)
;;      (cond
;;       ((font-exists-p  "7x13") "7x13")
;;       ((font-exists-p (make-font-pattern :foundry "lispm" :family "fixed"))
;;        (create-fontset-from-fontset-spec
;;         "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-fontset-lispm,
;; ascii:,
;; latin-iso8859-1:-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*,
;; latin-iso8859-15:-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*")
;;        ;; once the fontset has been defined, it can be invoked :
;;        "fontset-lispm")
;;       ((font-exists-p  "lucidasanstypewriter-12") "lucidasanstypewriter-12")
;;       (t *default-font*))
;;      *default-font*))
;;   (when (fboundp 'single-frame) (single-frame)))

;;;----------------------------------------------------------------------------

(defmacro string-case (string-expression &body clauses)
  (let ((value (gensym)))
    `(let ((,value ,string-expression))
       (cond
         ,@(mapcar (lambda (clause)
                     (destructuring-bind (constants &rest body) clause
                       (if (member* constant '(t otherwise) :test (function string-equal*))
                           `(t ,@body)
                           `((member* ,value ',(ensure-list constants))
                             ,@body))))
                   clauses)))))

;;;----------------------------------------------------------------------------
(when (and (not *pjb-pvs-is-running*) (member window-system '(x mac)))
  ;; By default turn on colorization.

  ;; ----------------------------------------
  (.EMACS "defining palettes")
  
  (defvar *palettes* '())
  (defvar *current-palette* nil)

  (defstruct palette
    name foreground background cursor region mouse)


  (defun set-palette (palette)
    (interactive
     (list (completing-read
            "Palette: "
            (mapcar (lambda (pal) (cons (symbol-name pal) pal)) *palettes*)
            nil  t  nil nil *current-palette*)))
    (typecase palette
      (string (set-palette (intern palette)))
      (symbol (if (boundp palette)
                (let ((palval (symbol-value palette)))
                  (if (and (palette-p palval) (eq palette (palette-name palval)))
                    (set-palette palval)
                    (error "%S is not a palette name." palette)))
                (error "%S is not a palette name." palette)))
      (palette
       (setf *current-palette* (palette-name palette))
       ;;        (set-default-frame-parameter 'foreground-color (palette-foreground palette))
       ;;        (set-default-frame-parameter 'background-color (palette-background palette))
       ;;        (set-default-frame-parameter 'cursor-color     (palette-cursor palette))
       (set-face-background 'region (palette-region palette))
       (when (getenv "EMACS_WM")
         (set-face-background 'border (palette-background palette)))
       (set-foreground-color (palette-foreground palette))
       (set-background-color (palette-background palette))
       (set-cursor-color     (palette-cursor palette))
       (when (fboundp 'set-mouse-color)
         (set-mouse-color     (palette-mouse palette))))
      (otherwise (error "%S is not a palette" palette))))

(defmacro defpalette (name foreground background cursor region mouse)
    `(progn
       (defparameter ,name (make-palette :name ',name
                                         :foreground ,foreground
                                         :background ,background
                                         :cursor ,cursor
                                         :region ,region
                                         :mouse ,mouse))
       (pushnew ',name *palettes*)
       (when (eq ',name *current-palette*)
         (set-palette ',name))
       ',name))

  ;;          name              foreground     background      cursor   region           mouse
  (defpalette pal-default       "White"        "Black"         "Red"     "blue3"         "#444444")
  (defpalette pal-white         "#000000"      "#ffffff"       "#555555" "#aaaaaa"       "#444444")
  (defpalette pal-ltgray        "#000000"      "#aaaaaa"       "#ffffff" "#555555"       "#444444")
  (defpalette pal-dkgray        "#ffffff"      "#555555"       "#000000" "#aaaaaa"       "#444444")
  (defpalette pal-black         "#ffffff"      "#000000"       "#aaaaaa" "#555555"       "#444444")
  (defpalette pal-lukhas        "#fff8dc"      "#537182"       "Red"     "#ddd"          "#444444")
  (defpalette pal-thalassa      "MidnightBlue" "#e0f8ff"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-larissa       "DarkOrchid4"  "#f8e8ff"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-lassell       "green"        "black"         "yellow"  "grey19"        "#444444")
  (defpalette pal-naiad         "MidnightBlue" "DarkSeaGreen1" "Pink3"   "orchid1"       "#444444")
  (defpalette pal-galatea       "#3080ff"      "#030828"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-galatea-light "#60c0ff"      "#030828"       "Pink4"   "orchid1"       "#444444")
  (defpalette pal-green         "green"        "black"         "yellow"  "grey50"        "#444444")
  (defpalette pal-dark          "White"        "#055045"       "yellow"  "grey40"        "#444444")
  (defpalette pal-dark-cyan     "#11eef2"      "black"         "yellow"  "grey80"        "#444444")
  (defpalette pal-dark-blue     "#1199f2"      "black"         "yellow"  "grey80"        "#444444")
  (defpalette pal-dark-amber    "#e0d010"      "black"         "cyan"    "grey40"        "#444444")
  (defpalette pal-dark-galatea  "#60f0c0"      "#0c2040"       "green"   "gray60"        "#444444")
  (defpalette pal-irc           "MidnightBlue" "light yellow"  "blue"    "light green"   "#444444")
  (defpalette pal-anevia        "white"        "#081040"       "green"   "cadetblue4"    "yellow")
  (defpalette pal-blueprint     "white"        "#392b8d"       "yellow"  "cadetblue4"    "yellow")
  (defpalette pal-blueprint2    "white"        "#06104d"       "yellow"  "cadetblue4"    "yellow")
  (defpalette pal-blueprint3    "white"        "#080635"       "yellow"  "cadetblue4"    "yellow")
  
  (set-palette  pal-default)


  ;; ----------------------------------------
  (.EMACS "set-default-frame-alist")


  


(defun set-default-frame-alist (&optional font)
    "Sets default-frame-alist depending on the current environment (host, display, etc)."
    (interactive)
    (let* (
           ;; ---------------------
           (display  (let* ((display (getenv "DISPLAY"))
                            (colon   (and display (string-match ":" display))))
                       (if (or (not display) (zerop colon))
                           system-name
                         (substring display 0 colon))))
           ;; --- default values ---
           ;; (font                 (or font (frame-font)))
           (width                (frame-width))
           (height               (frame-height))
           (top                  1)
           (left                 1)
           (cursor-type            'box)
           (horizontal-scroll-bars 'nil)
           (vertical-scroll-bars   'nil) ; or left or right
           (palette              pal-default)
           (hname                (subseq *hostname* 0 (position (character ".") *hostname*)))
           ;; (name (format "emacs: %s@%s" (user-real-login-name) host-name))
           (name "EMACS")
           ;; ---------------------
           (fringe-background nil))

      
            (setf default-cursor-type cursor-type)
      (string-case (hname :test (function string-equal*))
        (("mdi-development-1" "mdi-development-2")
         (setf fringe-background "yellow"))

        (("simias")
         (setq palette            pal-anevia))
        
        (("thalassa" "despina")
         (setq palette            pal-thalassa
               width              81
               height             70))

        (("larissa") 
         (setq palette            pal-larissa
               Width              81
               height             70))

        (("galatea") 
         (setq palette            pal-naiad
               width              81
               height             54
               font   (let ((fixed (make-font-pattern :foundry "Misc"
                                                      :family "Fixed"
                                                      :weight "Medium"
                                                      :slant "R"
                                                      :width "SemiCondensed"
                                                      :style ""
                                                      :pixel-size "13"
                                                      :point-size "120"
                                                      :resolution-x "75"
                                                      :resolution-y "75"
                                                      :spacing "C"
                                                      :average-width "60"
                                                      :registry "ISO8859"
                                                      :encoding "1")))
                        (if (font-exists-p fixed) fixed font))))

        (("naiad")
         (setq palette            pal-naiad
               width              81
               height             54))

        (("lassell")
         (setq palette            pal-lassel
               width              81
               height             54))

        (("triton" "proteus")
         (setq palette            pal-galatea
               width              86
               height             52))
        (("mini")
         (setq palette            pal-white
               width              86
               height             52)))

      (if (getenv "EMACS_WM")
          (progn
            (setq
             width    140
             height   58
             top      2
             left     2
             font     (make-font-pattern :foundry "Adobe"
                                         :family "Courier"
                                         :weight "Medium"
                                         :slant "R"
                                         :width "Normal"
                                         :style ""
                                         :pixel-size "12"
                                         :point-size "120"
                                         :resolution-x "75"
                                         :resolution-y "75"
                                         :spacing "M"
                                         :average-width "70"
                                         :registry "ISO8859"
                                         :encoding "1"))
            (set-face-background 'border (palette-background palette))
            (shell-command (format "xsetroot -solid %s" (palette-background palette))))
          (setq initial-frame-alist  `((left  . -64))))

      (when (getenv "EMACS_OLD")
        (setq palette            pal-green)
        (setq font
              "-Adobe-Courier-Bold-R-Normal--12-120-75-75-M-70-ISO8859-*"
              background-color "black"
              foreground-color "green"
              region-color     "navyblue"
              cursor-color     "yellow"))

      (when (getenv "EMACS_BG")
        (setq palette (copy-palette palette))
        (setf (palette-background palette) (getenv "EMACS_BG")))

      (when(= (user-uid) 0)
        (setq palette (copy-palette palette))
        (setf (palette-foreground palette) "Red"))

      (when (fboundp 'max-frame-line-number)
        (setf height (- (max-frame-line-number (car (frame-list))) 2)))

      (setq default-frame-alist
            `(
              (tool-bar-lines       . 0)
              (menu-bar-lines       . 0) ;; window-system 'mac
              (font                 . ,font)
              ,@(unless (getenv "RATPOISON")
                      `((width                . ,width)
                        (height               . ,height)
                        (top                  . ,top)
                        (left                 . ,left)))
              (cursor-type          . ,cursor-type)
              (cursor-color         . ,(palette-cursor palette))
              (foreground-color     . ,(palette-foreground palette))
              (background-color     . ,(palette-background palette))
              (vertical-scroll-bars . ,vertical-scroll-bars)
              (name                 . ,name)))

      (when (and (string= "21.3.1" emacs-version)
                 (not (getenv "EMACS_WM"))
                 (not (getenv "RATPOISON")))
        (set-frame-position (car (frame-list)) -64 top)
        (set-frame-size     (car (frame-list)) width height)
        (setq frame-initial-frame nil))

      (set-face-background 'region (palette-region palette))
      (when (facep 'fringe)
        (if fringe-background
            (set-face-background 'fringe fringe-background)
            (set-face-background 'fringe (palette-background palette))))
      (set-palette palette)
      (set-frame-name name)
      (when (zerop (user-uid))
        (set-foreground-color "Red"))))

  
  ;; (set-default-frame-alist *default-font*)
  (.EMACS "set-default-frame-alist done"))


;;;----------------------------------------------------------------------------
(when (and (boundp 'elscreen-display-tab) elscreen-display-tab)
  (elscreen-toggle-display-tab))


;;------------------------------
(.EMACS "Miscellaneous patches")

(when (< emacs-major-version 22)
  (unless (fboundp 'called-interactively-p)
    (defun called-interactively-p () (interactive-p))))




;;;----------------------------------------------------------------------------
(.EMACS "debug")
(require 'debug)
(define-key debugger-mode-map "\C-r" 'debugger-record-expression)
(define-key debugger-mode-map "\C-m" 'help-follow)
(define-key debugger-mode-map "B" 'debugger-frame)
(define-key debugger-mode-map "C" 'debugger-continue)
(define-key debugger-mode-map "J" 'debugger-jump)
(define-key debugger-mode-map "R" 'debugger-return-value)
(define-key debugger-mode-map "U" 'debugger-frame-clear)
(define-key debugger-mode-map "D" 'debugger-step-through)
(define-key debugger-mode-map "L" 'debugger-list-functions)
(define-key debugger-mode-map "H" 'describe-mode)
(define-key debugger-mode-map "Q" 'top-level)
(define-key debugger-mode-map "E" 'debugger-eval-expression)
(define-key debugger-mode-map "\C-R" 'debugger-record-expression)
(define-key debugger-mode-map "\C-M" 'help-follow)



;;;----------------------------------------------------------------------------
(.EMACS "eshell")
(unless (featurep 'eshell-auto)
  (load "eshell-auto" *pjb-load-noerror* *pjb-load-silent*))
(defun pjb-eshell-load-meat ()
  (defun string (&rest chars)
    (do ((s (make-string (length chars) 0))
         (ch chars (cdr ch))
         (i 0 (1+ i)))
        ((null ch) s)
      (setf (aref s i) (car ch)))))
(add-hook 'eshell-load-hook (function pjb-eshell-load-meat))




;;;----------------------------------------------------------------------------
(.EMACS "shell")

(defun colorize-buffer ()
  "Interprete les codes ASCII de couleur dans ce buffer."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (ansi-color-apply-on-region (point-min) (point-max))
    (set-buffer-modified-p modified)))

(defun pjb-shell-mode-hook ()
  (set-variable 'tab-width 8)
  (setf comint-process-echoes nil)
  (when (fboundp 'ansi-color-for-comint-mode-on)
    (ansi-color-for-comint-mode-on))
  ;; (cond
  ;;   ((let ((shell (getenv "ESHELL")))
  ;;       (or (null shell)
  ;;        (not (or (search "clash" shell)
  ;;              (search "scsh" shell)))))
  ;; Moved to ~/.emacs-bash:
  ;;    (process-send-string (get-buffer-process (current-buffer))
  ;;                      "alias less=cat ; alias more=cat ; ")))
  )
(add-hook 'shell-mode-hook (function pjb-shell-mode-hook))

(defun pjb-comint-filter-meat/erase-screen (string)
  (let ((pos (search "c" string :from-end t)))
    (if pos
        (progn
          (erase-buffer)
          (subseq string (+ 2 pos)))
        string)))
(defun ecma-048-cuu ()
  (backward-line 1))
(defun ecma-048-cuf (offset)
  (let ((new-column (+ (point) offset)))
    (if (< (save-excursion (end-of-line) (point))  new-column)
        (progn (end-of-line)
               ;; (insert (make-string (- new-column (point)) 32))
               )
        (forward-char offset))))
(defun ecma-048-crlf ()
  (insert (make-string (forward-line 1) 10)))
(defun pjb-comint-filter-meat/position (string)
  (let ((commands '(("\nA"            beginning-of-line)
                    ("\\(\\[0-9\\]+\\)C"  ecma-048-cuf 1)
                    ("\\(\\[0-9;\\]*\\)H" ignore)))
        (start 0))
    (while (let ((cmd (find-if (lambda (cmd) (eql start (string-match (first cmd) string start))) commands)))
             (when cmd
               (setf start (match-end 0))
               (apply (second cmd) (mapcar (lambda (i) (parse-integer (match-string i string))) (cddr cmd)))
               t)))
    (if (zerop start)
        string
        (subseq string start))))
(defun pjb-comint-filter-meat/color (string)
  "Remove color ansi codes."
  (with-temp-buffer
    (insert string)
    (goto-char 0)
    (let ((changed nil))
      (while (re-search-forward "[\\[0-9;\\]*m" (point-max) t)
        (setf changed t)
        (delete-region (match-beginning 0) (match-end 0)))
      (if changed
          (buffer-substring-no-properties (point-min) (point-max))
          string))))
(add-hook 'comint-preoutput-filter-functions 'pjb-comint-filter-meat/position)
(add-hook 'comint-preoutput-filter-functions 'pjb-comint-filter-meat/erase-screen)
(add-hook 'comint-preoutput-filter-functions 'pjb-comint-filter-meat/color)
;; comint-preoutput-filter-functions
(setf comint-preoutput-filter-functions nil)

;; (setf (getenv "ESHELL") (concatenate 'string  (USER-HOMEDIR-PATHNAME)
;;                          "bin/clash"))
;; (setf (getenv "ESHELL") "/bin/bash")



;;;----------------------------------------------------------------------------
(.EMACS "caps-mode")
;;;(autoload 'caps-mode "caps-mode" "Toggle caps mode." t)

(defun caps-mode-self-insert-command (&optional n)
  "Like `self-insert-command', but uppercase the the typed character."
  (interactive "p")
  (insert-char (upcase last-command-char) n))

(defvar caps-mode-map nil)

(when (fboundp 'define-minor-mode)
  (define-minor-mode caps-mode
      "Toggle caps mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When caps mode is enabled, all letters are inserted in their
capitalized form."
    :init-value nil
    :lighter " Caps"
    (setq caps-mode-map
          (let ((map (make-sparse-keymap)))
            (substitute-key-definition 'self-insert-command
                                       'caps-mode-self-insert-command
                                       map global-map)
            map))
    (if caps-mode
        (add-to-list 'minor-mode-map-alist (cons 'caps-mode caps-mode-map))
        (setq minor-mode-map-alist
              (delete (assoc 'caps-mode minor-mode-map-alist)
                      minor-mode-map-alist)))))

;;;----------------------------------------------------------------------------
(.EMACS "ORG-MODE")
(require 'org)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(setf org-log-done      t
      org-agenda-files  (append
                         '("~/notes.txt")
                         (file-expand-wildcards "~/firms/*/notes.txt"))
      org-todo-keywords '((sequence "TODO" "|" "DONE(d)")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                          (sequence "|" "CANCELED(c)"))
      org-todo-keywords '("TODO" "|" "DONE")
      org-enforce-todo-dependencies t
      org-log-done 'note)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)



;;;----------------------------------------------------------------------------
(.EMACS "INFERIOR LISP")

;; slime-net-valid-coding-systems
;; (map nil (lambda (n)
;;            (format t "(~A-unix~VA~:[nil~;t  ~]  ~4:*:~A-unix)~%"
;;                    n (- 32 (length n)) ""
;;                    (ignore-errors (/= 1 (length (ext:convert-string-to-bytes
;;                                   "A" (ext:make-encoding :charset n)))))))
;;      (let ((l '()));;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))

;; swank-clisp.lisp find-encoding
;; (map nil (lambda (n) (format t "(:~A-unix~VA\"~A\")~%" n (- 32 (length n)) "" n))
;;      (let ((l '()))
;;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))


;; This is about the easiest profiling I've seen in any language. In  
;; fact, I think it's the only time I been able to make significant  
;; improvements based on the report.  
;; 
;;     
;;     M-x slime-toggle-profile-fdefinition
;; 
;; on all the functions you want to  
;; profile, 
;;     
;;     M-x slime-profile-reset
;; 
;; to clear any existing data, and  
;; 
;;     
;;     M-x slime-profile-report
;; 
;; to see the report after running.  


;; slime-net-valid-coding-systems
;; (map nil (lambda (n)
;;            (format t "(~A-unix~VA~:[nil~;t  ~]  ~4:*:~A-unix)~%"
;;                    n (- 32 (length n)) ""
;;                    (ignore-errors (/= 1 (length (ext:convert-string-to-bytes
;;                                   "A" (ext:make-encoding :charset n)))))))
;;      (let ((l '()))
;;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))

;; swank-clisp.lisp find-encoding
;; (map nil (lambda (n) (format t "(:~A-unix~VA\"~A\")~%" n (- 32 (length n)) "" n))
;;      (let ((l '()))
;;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))

(.EMACS " define-lisp-implementation")
(defvar slime-lisp-implementations    nil)
(defvar *default-lisp-implementation* nil)
(defvar lisp-implementation nil
  "Buffer local variable indicating what lisp-implementation is used here.")

(defstruct lisp-implementation
  name command prompt coding
  (function-documentation-command
   "(let ((fn '%s))
     (format t \"Documentation for ~a:~&~a\"
	     fn (documentation fn 'function))
     (values))\n")
  (variable-documentation-command
   "(let ((v '%s))
     (format t \"Documentation for ~a:~&~a\"
	     v (documentation v 'variable))
     (values))\n")
  (argument-list-command
   "(let ((fn '%s))
     (format t \"Arglist for ~a: ~a\" fn (arglist fn))
     (values))\n")
  (describe-symbol-command "(describe '%s)\n"))


(defmacro define-lisp-implementation (name command prompt coding &rest rest)
  `(let* ((command (let ((command ,command))
                     (if (stringp command) (list command) command)))
          (li (make-lisp-implementation
               :name     ',name
               :command  (apply (function concat)
                                (cdr (loop for word in command
                                        collect " " collect word)))
               :prompt   ,prompt
               :coding  ',coding
               ,@rest))
          (sli (assoc ',name slime-lisp-implementations)))
     (setf (get ',name :lisp-implementation) li)
     (if (null sli)
         (push (list ',name command
                     :coding-system  (intern (format "%s-unix" ',coding)))
               slime-lisp-implementations)
         (setf (cdr sli)
               (list command
                     :coding-system (intern (format "%s-unix" ',coding)))))
     ',name))



(define-lisp-implementation scheme
    "mzscheme"
  "^> "
  iso-8859-15)
(define-lisp-implementation mzscheme
    "mzscheme"
  "^> "
  iso-8859-15)
(define-lisp-implementation mit-scheme
    "/usr/local/languages/mit-scheme/bin/scheme"
  "^\[[0-9]*\]> "
  iso-8859-15)
(define-lisp-implementation umb-scheme
    "/usr/bin/scheme"
  "^==> "
  iso-8859-15)
(define-lisp-implementation allegro-express
    "/local/languages/acl80_express/alisp"
  "^\[[0-9]*\]> "
  iso-8859-15)
(define-lisp-implementation allegro
    "/local/languages/acl80/alisp"
  "^\[[0-9]*\]> "
  iso-8859-15)
(define-lisp-implementation sbcl
    (list (first-existing-file '("/opt/local/bin/sbcl"
                                 "/usr/local/bin/sbcl"
                                 "/usr/bin/sbcl"))
          "--noinform")
  "^\[[0-9]*\]> "
  iso-8859-1)
(define-lisp-implementation cmucl
    (first-existing-file '("/usr/local/bin/lisp"
                           "/usr/bin/lisp"))
  "^\* "
  iso-8859-1)
(define-lisp-implementation openmcl
    "/usr/local/bin/openmcl"
  "^\[[0-9]*\]> "
  iso-8859-15)


(define-lisp-implementation clisp
    (list*
     (cond
       ((eq system-type 'cygwin)  "/usr/bin/clisp")
       (t  (first-existing-file '("/opt/local/bin/clisp"
                                  "/usr/local/bin/clisp"
                                  "/usr/bin/clisp"))))
     "-ansi""-q""-K""full""-m""32M""-I"
     (cond
       ((eq system-type 'darwin)
        (list "-Efile"     "ISO-8859-15"
              "-Epathname" "UTF-8"
              "-Eterminal" "UTF-8"
              "-Emisc"     "UTF-8" ; better be same as terminal
              "-Eforeign"  "ISO-8859-1")) ; must be 1-1.
       (t
        (list "-Efile"     "ISO-8859-15"
              "-Epathname" "ISO-8859-1"
              "-Eterminal" "UTF-8"
              "-Emisc"     "UTF-8" ; better be same as terminal
              "-Eforeign"  "ISO-8859-1")
        (list "-E"         "UTF-8"
              "-Epathname" "ISO-8859-1"
              "-Eforeign"  "ISO-8859-1")))) ; must be 1-1.
  "^\[[0-9]*\]> "
  utf-8
  :argument-list-command
  "(let ((fn '%s))
     (cond
       ((not (fboundp fn))      (format t \"~A is not a function\" fn))
       ((special-operator-p fn) (format t \"~A is a special operator\" fn))
       ((macro-function fn)     (format t \"~A is a macro\" fn))
       (t  (format t \"Arglist for ~a: ~a\" fn (ext:arglist fn))))
     (values))\n")


(defun set-inferior-lisp-implementation (impl)
  (interactive "SImplementation: ")
  (let ((impl (get impl :lisp-implementation)))
    (if impl
        (progn
          (message ".EMACS: inferior-lisp implementation: %s"
                   (lisp-implementation-name impl))
          (setf *default-lisp-implementation* impl
                inferior-lisp-program (lisp-implementation-command impl)
                inferior-lisp-prompt  (lisp-implementation-prompt impl)
                lisp-function-doc-command
                (lisp-implementation-function-documentation-command impl)
                lisp-var-doc-command
                (lisp-implementation-variable-documentation-command impl)
                lisp-arglist-command
                (lisp-implementation-argument-list-command impl)
                lisp-describe-sym-command
                (lisp-implementation-describe-symbol-command impl)
                default-process-coding-system
                (let ((coding (lisp-implementation-coding impl)))
                  (cons coding coding))))
        (error "%S not a lisp implementation." impl)))
  impl)



;; (prog1 nil
;;   (set-inferior-lisp-implementation 'clisp)
;;   (print `(clisp --> ,(symbol-plist 'clisp)))
;;   (print `(inferior-lisp-program --> ,inferior-lisp-program))
;;   (print `(inferior-lisp-prompt --> ,inferior-lisp-prompt)))


;; system-type          darwin   gnu/linux  cygwin
;; system-configuration "i686-pc-linux-gnu" "i686-pc-cygwin"

;; Used both by ilisp and slime:
(case system-type
  ((darwin)     (set-inferior-lisp-implementation 'clisp)) ; openmcl))
  ((gnu/linux)  (set-inferior-lisp-implementation 'clisp)) ; sbcl))
  ((cygwin)     (set-inferior-lisp-implementation 'clisp))
  (otherwise    (warn "unexpected system-type for inferior-lisp-program")
                (set-inferior-lisp-implementation 'clisp)))


(defun set-inferior-lisp-buffer (buffer-name)
  (interactive "bInferior Lisp Buffer: ")
  (make-local-variable 'inferior-lisp-buffer)
  (make-local-variable 'lisp-function-doc-command)
  (make-local-variable 'lisp-var-doc-command)
  (make-local-variable 'lisp-arglist-command)
  (make-local-variable 'lisp-describe-sym-command)
  (make-local-variable 'lisp-implementation)
  (setf
   inferior-lisp-buffer buffer-name
   lisp-implementation  (or (buffer-local-value
                             'lisp-implementation
                             (get-buffer inferior-lisp-buffer))
                            (lisp-implementation-name
                             *default-lisp-implementation*)))
  (let ((limpl (get lisp-implementation :lisp-implementation)))
    (when limpl
      (setf
       lisp-function-doc-command
       (lisp-implementation-function-documentation-command limpl)
       lisp-var-doc-command
       (lisp-implementation-variable-documentation-command limpl)
       lisp-arglist-command
       (lisp-implementation-argument-list-command limpl)
       lisp-describe-sym-command
       (lisp-implementation-describe-symbol-command limpl)))))



(defun %lisp-buffer-name (n impl) (format "%dlisp-%s" n impl))
(defun %lisp-buffer-name-match-p (buffer-name &optional number)
  (string-match (if number (format "^%dlisp" number) "^[0-9]+lisp") buffer-name))
(defun %lisp-buffer-name-number (buffer-name)
  (when (string-match "^\\([0-9]+\\)lisp" buffer-name)
    (parse-integer (match-string 1 buffer-name))))
(defun inferior-lisp-buffers-list ()
  "RETURN: a list of the inferior-lisp buffers."
  (delete-if (lambda (name) (not (%lisp-buffer-name-match-p name)))
             (mapcar (function buffer-name) (buffer-list))))
(defun %lisp-buffer-next-number ()
  (loop
     with i = 0
     with numbers = (sort (mapcar (function  %lisp-buffer-name-number)
                                  (inferior-lisp-buffers-list))
                          (function <=))
     while numbers
     do (if (= i (car numbers))
            (progn (incf i) (pop numbers))
            (return i))
     finally (return i)))

(defvar *lisp-command-history* '())

(defun inferior-lisp-other-window (cmd)
  "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run lisp: " inferior-lisp-program)
		       inferior-lisp-program)))
  (if (not (comint-check-proc "*inferior-lisp*"))
      (let ((cmdlist (split-string cmd)))
	(set-buffer (apply (function make-comint)
			   "inferior-lisp" (car cmdlist) nil (cdr cmdlist)))
	(inferior-lisp-mode)))
  (setq inferior-lisp-buffer "*inferior-lisp*")
  (pop-to-buffer "*inferior-lisp*" t))


(defun nlisp (&optional ask-command)
  "Create a new inferior-lisp buffer."
  (interactive "P")
  (let* ((impl-or-cmd
          (if ask-command
              (read-from-minibuffer
                 "Lisp implementation or command: "
                 (format "%s" (lisp-implementation-name
                               *default-lisp-implementation*))
                 nil nil '*lisp-command-history*)
              (format "%s" (lisp-implementation-name
                            *default-lisp-implementation*))))
         (impl  (unless (position (character " ") impl-or-cmd
                                  :test (function char=))
                  (intern-soft impl-or-cmd)))
         (limpl (and impl (get impl :lisp-implementation)))
         (cmd   (if limpl (lisp-implementation-command limpl) impl-or-cmd)))
    (inferior-lisp-other-window cmd)
    (make-local-variable 'lisp-implementation)
    (setf lisp-implementation
          (or impl (lisp-implementation-name *default-lisp-implementation*)))
    (rename-buffer
     (setf inferior-lisp-buffer
           (%lisp-buffer-name
            (%lisp-buffer-next-number)
            (cond
              (impl)
              ((string= cmd (lisp-implementation-command
                             *default-lisp-implementation*))
               (lisp-implementation-name *default-lisp-implementation*))
              ('custom)))))))


(defun lisp (&optional ask-command)
  "Create a new inferior-lisp when none exist,
   or switch to the last created one."
  (interactive "P")
  (if (and (boundp 'inferior-lisp-buffer) inferior-lisp-buffer
           (get-buffer inferior-lisp-buffer))
      (switch-to-buffer inferior-lisp-buffer)
      (let ((lisp-buffers (inferior-lisp-buffers-list)))
        (if lisp-buffers
            (switch-to-buffer
             (setf inferior-lisp-buffer (first lisp-buffers)))
            (nlisp ask-command)))))


(defvar package 'common-lisp-user)

(defun symbol-value-in-buffer (symbol buffer)
  (save-excursion
    (set-buffer buffer)
    (when (boundp symbol)
      (symbol-value symbol))))

(defun set-symbol-value-in-buffer (symbol buffer value)
  (save-excursion
    (set-buffer buffer)
    (make-local-variable symbol)
    (setf (symbol-value symbol) value)))

(defsetf symbol-value-in-buffer set-symbol-value-in-buffer)

;; (symbol-value-in-buffer 'inferior-lisp-buffer "a.lisp")
;; (local-variable-p 'package)
;; (inferior-lisp-package)
;; (local-variable-p 'package)

;; Interfers with slime:
;;
;; (defun inferior-lisp-buffer (&optional process)
;;   (if (boundp 'inferior-lisp-buffer)
;;       inferior-lisp-buffer
;;       (process-buffer (or process (inferior-lisp-proc)))))
;; 
;; (defun inferior-lisp-package (&optional process)
;;   (symbol-value-in-buffer 'package (inferior-lisp-buffer process)))
;; 
;; ;; (defun lisp-eval-region (start end &optional and-go)
;; ;;   "Send the current region to the inferior Lisp process.
;; ;; Prefix argument means switch to the Lisp buffer afterwards."
;; ;;   (interactive "r\nP")
;; ;;   (comint-send-region (inferior-lisp-proc) start end)
;; ;;   (comint-send-string (inferior-lisp-proc) "\n")
;; ;;   (if and-go (switch-to-lisp t)))
;; 
;; (defadvice lisp-eval-region (before ler-in-package activate) 
;;   (when (and (boundp 'package) (not (eq package (inferior-lisp-package))))
;;     (comint-send-string (inferior-lisp-proc)
;;                         (upcase (format "(CL:IN-PACKAGE #:%s)\n" package)))
;;     (setf (symbol-value-in-buffer 'package (inferior-lisp-buffer)) package)))


(defun lisp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))


(appendf auto-mode-alist '(("\\.lisp$" . lisp-mode)
                           ("\\.fas$"  . lisp-mode)
                           ("\\.lsp$"  . lisp-mode)
                           ("\\.cl$"   . lisp-mode)
                           ("\\.acl2$" . lisp-mode)
                           ("\\.LISP$" . lisp-mode)
                           ("\\.FAS$"  . lisp-mode)
                           ("\\.LSP$"  . lisp-mode)
                           ("\\.CL$"   . lisp-mode)
                           ("\\.ACL2$" . lisp-mode)))

(appendf auto-mode-alist '(("\\.scm$"    . scheme-mode)
                           ("\\.ss$"     . scheme-mode)
                           ("\\.stk$"    . scheme-mode)
                           ("\\.stklos$" . scheme-mode)))

(appendf auto-mode-alist '(("\\.jmf$"    . java-mode)
                           ("\\.j$"      . java-mode)))


(defun show-inferior-lisp-buffer ()
  (interactive)
  (let ((lisp-buffer (get-buffer inferior-lisp-buffer)))
    (when lisp-buffer
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer lisp-buffer)
      (other-window 1))))

(defun indent-defun ()
  (interactive)
  (save-excursion
    (indent-region (progn (beginning-of-defun) (point))
                   (progn (end-of-defun) (point)))))

(defun pjb-lisp-comment-region (beg end &optional arg)
  (let* ((numarg (prefix-numeric-value arg))
         (style (cdr (assoc comment-style comment-styles)))
         (lines (nth 2 style))
         (block (nth 1 style))
         (multi (nth 0 style)))
    ;; we use `chars' instead of `syntax' because `\n' might be
    ;; of end-comment syntax rather than of whitespace syntax.
    ;; sanitize BEG and END
    (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
    (setq beg (max beg (point)))
    (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
    (setq end (min end (point)))
    (if (>= beg end) (error "Nothing to comment"))

    ;; sanitize LINES
    (setq lines
          (and
           lines ;; multi
           (progn (goto-char beg) (beginning-of-line)
                  (skip-syntax-forward " ")
                  (>= (point) beg))
           (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
                  (<= (point) end))
           (or block (not (string= "" comment-end)))
           (or block (progn (goto-char beg) (search-forward "\n" end t)))))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
      ((consp arg) (uncomment-region beg end))
      ((< numarg 0) (uncomment-region beg end (- numarg)))
      (t
       (setq numarg (comment-add arg))
       (comment-region-internal
        beg end
        (let ((s (comment-padright comment-start numarg)))
          (if (string-match comment-start-skip s) s
              (comment-padright comment-start)))
        (let ((s (comment-padleft comment-end numarg)))
          (and s (if (string-match comment-end-skip s) s
                     (comment-padright comment-end))))
        (if multi (comment-padright comment-continue numarg))
        (if multi
            (comment-padleft (comment-string-reverse comment-continue) numarg))
        block
        lines
        (nth 3 style))))))

(defun pjb-lisp-meat ()
  (interactive)
  (.EMACS "running pjb-lisp-meat on %S" (buffer-name))
  (unless (eq 'emacs-lisp-mode major-mode)
    (local-set-key [f8] (function  show-inferior-lisp-buffer)))
  (local-set-key (kbd "RET") 'newline-and-indent)
  ;; (local-set-key (kbd "RET") 'indent-defun)
  ;; (setq blink-matching-paren t)
  (setf skeleton-pair         nil
        comint-process-echoes nil)
  (setf comment-style 'indent)
  ;; (setf comment-region-function 'pjb-lisp-comment-region)
  (local-set-key (kbd "<A-up>")      (function backward-up-list))
  (local-set-key (kbd "<A-down>")    (function down-list))
  (when (load "paredit" t t)
    (paredit-mode +1))
  (local-set-key (kbd "<s-A-left>")  (function paredit-backward-barf-sexp))
  (local-set-key (kbd "<s-A-right>") (function paredit-backward-slurp-sexp))
  (local-set-key (kbd "<A-right>")   (function paredit-forward-slurp-sexp))
  (local-set-key (kbd "<A-left>")    (function paredit-forward-barf-sexp))
  (local-set-key (kbd "A-s")         (function paredit-backward-barf-sexp))
  (local-set-key (kbd "A-d")         (function paredit-backward-slurp-sexp))
  (local-set-key (kbd "A-f")         (function paredit-forward-slurp-sexp))
  (local-set-key (kbd "A-g")         (function paredit-forward-barf-sexp))
  ;;   (setq skeleton-pair t)
  ;;   (local-set-key "("  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "["  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "{"  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "|"  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "\"" 'skeleton-pair-insert-maybe)
  (when (fboundp 'column-marker-1) (column-marker-1 80))
  (add-hook 'comint-preoutput-filter-functions (function pjb-comint-preoutput-insert-image))
  (.EMACS "pjb-lisp-meat on %S done" (buffer-name))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               insert-image.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A patch to emacs to be able to insert images in a comint buffer
;;;;    such as inferior-lisp REPL.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-04-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(require 'cl)

(defun splice (new-list old list)
  "Like substitute but replace the old by the elements in the new-list."
  (loop
     with result = '()
     for item in list
     do (if (eql old item)
            (loop
               for item in new-list
               do (push item result))
            (push item result))
     finally (return (nreverse result))))

(defun ensure-list (x) (if (listp x) x (list x)))
(defun comint-output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    ;; First check for killed buffer or no input.
    (when (and string oprocbuf (buffer-name oprocbuf))
      (with-current-buffer oprocbuf
        ;; Run preoutput filters
        (let ((functions (splice (default-value 'comint-preoutput-filter-functions)
                                 t
                                 comint-preoutput-filter-functions))
              (strings (list string)))
          
          (while (and functions strings)
            (setf strings (loop
                             with result = ()
                             for string in strings
                             do (setf result (revappend (ensure-list (funcall (car functions) string)) result))
                             finally (return (nreverse result))))
            (setq functions (cdr functions)))
          (setf string strings))
        
        ;; Insert STRING
        (let ((inhibit-read-only t)
              ;; The point should float after any insertion we do.
              (saved-point (copy-marker (point) t)))

          ;; We temporarly remove any buffer narrowing, in case the
          ;; process mark is outside of the restriction
          (save-restriction
            (widen)

            (goto-char (process-mark process))
            (set-marker comint-last-output-start (point))

            ;; insert-before-markers is a bad thing. XXX
            ;; Luckily we don't have to use it any more, we use
            ;; window-point-insertion-type instead.
            (loop
                 for item in string
                 do (cond
                      ((stringp item) (insert item))
                      ((consp   item) (insert-image (first item) (second item)))
                      (t (error "Unexpected kind of insert %S" item))))

            
            ;; Advance process-mark
            (set-marker (process-mark process) (point))
            (setf string (buffer-substring comint-last-output-start (point)))
            (unless comint-inhibit-carriage-motion
              ;; Interpret any carriage motion characters (newline, backspace)
              (comint-carriage-motion comint-last-output-start (point)))

            ;; Run these hooks with point where the user had it.
            (goto-char saved-point)
            (run-hook-with-args 'comint-output-filter-functions string)
            (set-marker saved-point (point))

            (goto-char (process-mark process)) ; in case a filter moved it

            (unless comint-use-prompt-regexp
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (add-text-properties comint-last-output-start (point)
                                     '(front-sticky
                                       (field inhibit-line-move-field-capture)
                                       rear-nonsticky t
                                       field output
                                       inhibit-line-move-field-capture t))))

            ;; Highlight the prompt, where we define `prompt' to mean
            ;; the most recent output that doesn't end with a newline.
            (let ((prompt-start (save-excursion (forward-line 0) (point)))
                  (inhibit-read-only t)
                  (inhibit-modification-hooks t))
              (when comint-prompt-read-only
                (or (= (point-min) prompt-start)
                    (get-text-property (1- prompt-start) 'read-only)
                    (put-text-property
                     (1- prompt-start) prompt-start 'read-only 'fence))
                (add-text-properties
                 prompt-start (point)
                 '(read-only t rear-nonsticky t front-sticky (read-only))))
              (unless (and (bolp) (null comint-last-prompt-overlay))
                ;; Need to create or move the prompt overlay (in the case
                ;; where there is no prompt ((bolp) == t), we still do
                ;; this if there's already an existing overlay).
                (if comint-last-prompt-overlay
                    ;; Just move an existing overlay
                    (move-overlay comint-last-prompt-overlay
                                  prompt-start (point))
                    ;; Need to create the overlay
                    (setq comint-last-prompt-overlay
                          (make-overlay prompt-start (point)))
                    (overlay-put comint-last-prompt-overlay
                                 'font-lock-face 'comint-highlight-prompt))))
            (goto-char saved-point)))))))


(defun pjb-comint-preoutput-insert-image (string)
  (let ((case-fold-search t))
    (loop
         with result = '()
         while (and (plusp (length string))
                    (string-match "\\(.*\\)(EMACS:INSERT-IMAGE[ \t\n]+#P\"\\(\\([^\\\"]\\|\\.\\)*\\)\")\\(.*\\)"
                                  string))
         do (let ((before (match-string 1 string))
                  (path   (match-string 2 string))
                  (after  (match-string 4 string)))
              (push before result)
              (push (list (create-image path) " ") result)
              (setf string after))
         finally (push string result) (return (nreverse result)))))

;;;; THE END ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; (defun find-definition-site-of-preloaded-function (function)
;;   ;; Find the real def site of the preloaded function.
;;   ;; This is necessary only for defaliases.
;;   (let ((location
;;          (condition-case nil
;;              (find-function-search-for-symbol function nil "loaddefs.el")
;;            (error nil))))
;;     (when location
;;       (with-current-buffer (car location)
;;         (goto-char (cdr location))
;;         (when (re-search-backward
;;                "^;;; Generated autoloads from \\(.*\\)" nil t)
;;           (match-string 1))))))
;; 
;; 
;; (defun find-definition-site-of-subr-function (function)
;;   ;; Find the C source file name.
;;   nil)
;; 
;; 
;; (defun find-function-source (function)
;;   "Find the source of the emacs lisp FUNCTION (a symbol)."
;;   (interactive
;;    (let ((fn (function-called-at-point))
;;          (enable-recursive-minibuffers t)
;;          val)
;;      (setq val (completing-read
;;                 (if fn
;;                     (format "Find source of function  (default %s): " fn)
;;                     "Find source of function: ")
;;                 obarray 'fboundp t nil nil
;;                 (and fn (symbol-name fn))))
;;      (list (if (equal val "")
;;                fn (intern val)))))
;;   (if (null function)
;;       (.EMACS "You didn't specify a function")
;;       (let* ((def (if (symbolp function)
;;                       (symbol-function function)
;;                       function))
;;              file-name string
;;              (beg (if (commandp def) "an interactive " "a ")))
;;         (setq file-name (if (eq (car-safe def) 'autoload)
;;                             (nth 1 def)
;;                             (symbol-file function 'defun)))
;;         (when (and file-name
;;                    (equal (describe-simplify-lib-file-name file-name)
;;                           "loaddefs.el")) 
;;           (setf file-name (find-definition-site-of-preloaded-function function)))
;;         (when (and (null file-name) (subrp def))
;;           (setq file-name (find-definition-site-of-subr-function function)))
;;         (if file-name
;;             (progn
;;               (find-file file-name)
;;               (re-search-forward (format "^ *(def.* %s" function) nil t))
;;             (error "No source file for %s" function)))))


(defun hide-brackets ()
  "Show brackets as parens.
From: Jorgen Schaefer <forcer@forcix.cx>
Message-ID: <87irohiw7u.fsf@forcix.kollektiv-hamburg.de>
"
  (interactive)
  (font-lock-add-keywords
   nil '(("\\["
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    "(")
                    nil)))
         ("\\]"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ")")
                    nil)))))
  (font-lock-fontify-buffer))

(defun fix-brackets ()
  "Replace brackets outside of strings and comments with parens.
From: Jorgen Schaefer <forcer@forcix.cx>
Message-ID: <87irohiw7u.fsf@forcix.kollektiv-hamburg.de>
"
  (interactive)
  ;; This can be called in a hook before font lock mode has a chance
  ;; to run, but we need its information. So we enforce a font lock
  ;; run.
  (font-lock-fontify-buffer)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[][]" nil t)
      (let* ((bracket (match-string 0))
             (face (get-text-property 0 'face bracket)))
        (cond
          ((or (looking-back "#\\\\[][]") ; character literal
               (eq face 'font-lock-comment-face)
               (eq face 'font-lock-string-face))
           ;; Do nothing
           nil)
          ((string= "[" bracket)
           (replace-match "("))
          ((string= "]" bracket)
           (replace-match ")"))
          (t
           (error "Bad token: %s (This is a CAN'T-HAPPEN type of error)"
                  bracket)))))))




;; (load-library "cl")
;; (setq indent-region-function (function lisp-indent-function))
;; (setq lisp-indent-function   (function common-lisp-indent-function))
;; (setq lisp-indent-function   (function lisp-indent-function))
;; (put 'let  'lisp-indent-function        '(&lambda &body))
;; (put 'let* 'lisp-indent-function        '(&lambda &body))
;; (put 'let  'common-lisp-indent-function '(&lambda &body))
;; (put 'let* 'common-lisp-indent-function '(&lambda &body))


;; (defun test (var var var
;;                  var var var)
;;   body)

;; (let (var var var
;;           var
;;           var)
;;   var)

;; (TRACE lisp-indent-function common-lisp-indent-function
;;        common-lisp-indent-function-1)

;; (setq lisp-simple-loop-indentation   1
;;       lisp-loop-keyword-indentation  6
;;       lisp-loop-forms-indentation    6)


;; M-( insert-parentheses
;; M-) move-past-close-and-reindent

(defun sexp-diff (s1 s2)
  (cond
    ((and (atom s1) (atom s2))
     (if (equal s1 s2)
         s1
         (list :DIFFERENCE s1 s2)))
    ((and (consp s1) (consp s2))
     (cons (sexp-diff (car s1) (car s2))
           (sexp-diff (cdr s1) (cdr s2))))
    (t
     (list :DIFFERENCE s1 s2))))

(defun collect-sexps (count)
  (when (< count 0)
    (setf count (- count))
    (backward-sexp count))
  (forward-sexp)
  (backward-sexp)
  (let ((b (point))
        (e (point))
        (l '()))
    (dotimes (n count)
      (forward-sexp)
      (backward-sexp)
      (push (sexp-at-point) l)
      (forward-sexp)
      (setf e (point)))
    (list b e (nreverse l))))

(defun ex-parenthese (&optional arg)
  (interactive "p")
  (let ((bel (collect-sexps arg)))
    (when bel
      (goto-char (first bel))
      (dolist (sexp (third bel))
        (forward-sexp)
        (when (listp sexp)
          (let ((e (point)))
            (backward-sexp)
            (delete-char 1)
            (goto-char (- e 1))
            (backward-delete-char 1))))
      (goto-char (if (< 0 arg) (first bel) (- (second bel) 2))))))

(defun in-parenthese (&optional arg)
  (interactive "p")
  (let ((bel (collect-sexps arg)))
    (when bel
      (goto-char (second bel))
      (insert ")")
      (goto-char (first bel))
      (insert "(")
      (goto-char (1+ (first bel))))))

(global-set-key (kbd "A-[")   (function in-parenthese))
(global-set-key (kbd "A-]")   (function ex-parenthese))


(require 'inf-lisp)
(defun sexp-movement ()
  "Binds locally some keys to sexp movement commands."
  (interactive)
  (define-key inferior-lisp-mode-map  (kbd "C-c .") (function forward-sexp))
  (define-key inferior-lisp-mode-map  (kbd "C-c ,") (function backward-sexp))
  (local-set-key (kbd "C-c .") (function forward-sexp))
  (local-set-key (kbd "C-c ,") (function backward-sexp))
  (define-key inferior-lisp-mode-map  (kbd "A-.") (function forward-sexp))
  (define-key inferior-lisp-mode-map  (kbd "A-,") (function backward-sexp))
  (local-set-key (kbd "A-.") (function forward-sexp))
  (local-set-key (kbd "A-,") (function backward-sexp))
  (local-set-key [M-up]        'up-list)
  (local-set-key [M-down]      'down-list)
  (local-set-key [M-right]     'forward-sexp)
  (local-set-key [M-left]      'backward-sexp)
  (values))

(defun clisp-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (macrolet ((cmd (string)
               `(lambda ()
                   (interactive)
                   (comint-send-string (inferior-lisp-proc)
                                       ,(format "%s\n" string)))))
    (local-set-key (kbd "<f5>") (cmd ":s"))
    (local-set-key (kbd "<f6>") (cmd ":n"))
    (local-set-key (kbd "<f7>") (cmd ":o"))
    (local-set-key (kbd "<f8>") (cmd ":c"))))

(defun ecl-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (macrolet ((cmd (string)
               `(lambda ()
                   (interactive)
                   (comint-send-string (inferior-lisp-proc)
                                       ,(format "%s\n" string)))))
    
    (local-set-key (kbd "<f5>") (cmd ""))
    (local-set-key (kbd "<f6>") (cmd ""))
    (local-set-key (kbd "<f7>") (cmd ":skip"))
    (local-set-key (kbd "<f8>") (cmd ":exit"))))

(defun sbcl-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (macrolet ((cmd (string)
               `(lambda ()
                   (interactive)
                   (comint-send-string (inferior-lisp-proc)
                                       ,(format "%s\n" string)))))
    (local-set-key (kbd "<f5>") (cmd "step"))
    (local-set-key (kbd "<f6>") (cmd "next"))
    (local-set-key (kbd "<f7>") (cmd "over"))
    (local-set-key (kbd "<f8>") (cmd "out"))))

(defun allegro-debug-keys ()
  "Binds locally some keys to send allegro debugger commands to the inferior-lisp
<f5> step into
<f7> step over
<f8> continue
"
  (interactive)
  (macrolet ((cmd (string)
               `(lambda ()
                   (interactive)
                   (comint-send-string (inferior-lisp-proc)
                                       ,(format "%s\n" string)))))
    (local-set-key (kbd "<f5>") (cmd ":scont 1"))
    ;; (local-set-key (kbd "<f6>") (cmd ))
    (local-set-key (kbd "<f7>") (cmd ":sover"))
    (local-set-key (kbd "<f8>") (cmd ":continue"))))



(loop for x in '(setf common-lisp-mode-hook      nil
   inferior-lisp-load-hook    nil
   inferior-lisp-mode-hook    nil
   lisp-interaction-mode-hook nil
   lisp-mode-hook             nil
   comint-mode-hook           nil
   comint-exec-hook           nil
   ilisp-mode-hook            nil
   scheme-mode-hook           nil)
   for i from 0
     when (oddp i)
     collect x)

(message (format  "hooks=%S" 
(mapcar (lambda (h)  (if (boundp h) (list h (symbol-value h)) (list h 'unbound)))
        '(common-lisp-mode-hook inferior-lisp-load-hook inferior-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook comint-mode-hook comint-exec-hook ilisp-mode-hook scheme-mode-hook))))


(setf common-lisp-mode-hook      nil
      inferior-lisp-load-hook    nil
      inferior-lisp-mode-hook    nil
      lisp-interaction-mode-hook nil
      lisp-mode-hook             nil
      comint-mode-hook           nil
      comint-exec-hook           nil
      ilisp-mode-hook            nil
      scheme-mode-hook           nil)

(add-hook 'scheme-mode-hook      (function pjb-lisp-meat))
(add-hook 'scheme-mode-hook
          (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
(add-hook 'lisp-mode-hook        (function pjb-lisp-meat))
(add-hook 'common-lisp-mode-hook (function pjb-lisp-meat))
(add-hook 'emacs-lisp-mode-hook  (function pjb-lisp-meat))

(require 'slime)
(slime-setup '(slime-fancy))
(setf slime-net-coding-system 'utf-8-unix)
(setf slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))

;; (message (format ".EMACS:  Environment EMACS_INFERIOR_LISP = %S"
;;            (getenv "EMACS_INFERIOR_LISP")))
;; (setf (getenv "EMACS_INFERIOR_LISP")
;;       (or
;;        "inferior-lisp"
;;        (getenv "EMACS_INFERIOR_LISP")
;;        "inferior-lisp"
;;        "minimum-slime"
;;        "slime"
;;        "allegro-fi"
;;        "ILISP"))
;; ;; (setf (getenv "EMACS_INFERIOR_LISP") "slime")
;; (message (format ".EMACS:  Selected EMACS_INFERIOR_LISP = %S"
;;            (getenv "EMACS_INFERIOR_LISP")))

;; (progn
;;   (add-to-list 'load-path (get-directory :share-lisp "packages/net/common-lisp/slime/slime/"))
;;   (setf common-lisp-mode-hook      nil
;;         inferior-lisp-load-hook    nil
;;         inferior-lisp-mode-hook    nil
;;         lisp-interaction-mode-hook nil
;;         lisp-mode-hook             nil
;;         comint-mode-hook           nil
;;         comint-exec-hook           nil
;;         ilisp-mode-hook            nil
;;         scheme-mode-hook           nil)
;; 
;;   (add-hook 'scheme-mode-hook      (function pjb-lisp-meat))
;;   (add-hook 'scheme-mode-hook
;;             (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;   (add-hook 'lisp-mode-hook        (function pjb-lisp-meat))
;;   (add-hook 'common-lisp-mode-hook (function pjb-lisp-meat))
;;   (add-hook 'emacs-lisp-mode-hook  (function pjb-lisp-meat))
;;   
;;   ;; (list scheme-mode-hook lisp-mode-hook common-lisp-mode-hook emacs-lisp-mode-hook)
;; 
;;   (cond
;; 
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "allegro-fi")
;; ;;;----------------------------------------------------------------------------
;; ;;; ALLEGRO FI interface.
;;      (load "/usr/local/languages/acl80/eli/fi-site-init.el")
;;      (add-hook 'lisp-mode-hook
;;                (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;      (defun ficl-meat ()
;;        (sexp-movement))
;;      (add-hook 'fi:common-lisp-mode-hook 'ficl-meat))
;; 
;; 
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "inferior-lisp")
;;      (.EMACS "inferior-lisp")
;; ;;;----------------------------------------------------------------------------
;; ;;; INFERIOR-LISP
;;      (add-hook 'lisp-mode-hook
;;                (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;      (setf inferior-lisp-mode-hook nil)
;; 
;; 
;;      (add-hook 'inferior-lisp-mode-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "inferior-lisp-mode-hook done.")))
;;      (add-hook 'comint-mode-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "comint-mode-hook done.")))
;;      (add-hook 'comint-exec-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "comint-mode-hook done.")))
;;      );; inferior-lisp
;; 
;; 
;;     ((and (string-equal (getenv "EMACS_INFERIOR_LISP") "minimum-slime")
;;           (require 'slime nil t))
;;      (.EMACS "minimum-slime")
;; ;;;----------------------------------------------------------------------------
;; ;;; MINIMUM SLIME
;; ;;; site-lisp configuration for slime-cvs
;;      
;;      (slime-setup '(slime-repl))
;;      (setf slime-net-coding-system 'utf-8-unix)
;;      (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;      (setf slime-space-information-p t)
;;      (global-set-key (kbd "C-c s") (function slime-selector))
;;      ;; this prevents us from requiring the user get dev-lisp/hyperspec
;;      ;; (which is non-free) as a hard dependency
;; 
;;      
;;      (defun newline-and-lisp-indent (&rest rest)
;;        (interactive)
;;        (newline)
;;        (lisp-indent-line))
;;      (define-key slime-mode-map (kbd "RET") 'newline-and-lisp-indent)
;; 
;; 
;; 
;;      (defun inferior-lisp-buffer-name (name index)
;;        (format "*slime inferior-lisp %d%s*" index name))
;; 
;;      (defun slime-buffer-name (name index)
;;        (format "%d%s (slime)" index name))
;; 
;;      (defun get-next-buffer-name (name bnf)
;;        (let ((i 0))
;;          (while (get-buffer (funcall bnf name i)) (incf i))
;;          (funcall bnf name i)))
;; 
;;      (defun slime-repl-buffer (&optional create connection)
;;        "Get the REPL buffer for the current connection; optionally create."
;;        (funcall (if create
;;                     (function get-buffer-create)
;;                     (function get-buffer))
;;                 ;; (format "*slime-repl %s*" (slime-connection-name connection))
;;                 (get-next-buffer-name (slime-lisp-implementation-name connection)
;;                                       (function slime-buffer-name))))
;; 
;; 
;;      (defun slime (&optional command coding-system)
;;        "Start an inferior^_superior Lisp and connect to its Swank server."
;;        (interactive)
;;        (let* ((args (slime-read-interactive-args))
;;               (impl (get (getf args :name) :lisp-implementation)))
;;          (if impl
;;              (apply (function slime-start)
;;                     :buffer (get-next-buffer-name
;;                              (lisp-implementation-name impl)
;;                              (function inferior-lisp-buffer-name))
;;                     args)
;;              (apply (function slime-start) args))))
;;      ) ;; minimum-slime
;;     
;; 
;;     ((and (string-equal (getenv "EMACS_INFERIOR_LISP") "slime")
;;           (require 'slime nil t))
;;      (.EMACS "slime")
;; ;;;----------------------------------------------------------------------------
;; ;;; SLIME
;;      
;;      ;;(add-to-list 'load-path "/home/luke/slime")
;;      (require 'slime)
;;      (slime-setup '(slime-fancy slime-asdf slime-banner slime-repl))
;; 
;;      (add-hook 'lisp-mode-hook
;;                (lambda () (slime-mode t) (slime-autodoc-mode t)))
;;      ;;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;      ;; (modify-syntax-entry ?$ "'" lisp-mode-syntax-table)
;; 
;;      (define-key slime-mode-map (kbd "[") 'insert-parentheses)
;;      (define-key slime-mode-map (kbd "]") 'move-past-close-and-reindent)
;;      ;;(define-key slime-mode-map (kbd "(") (lambda () (interactive) (insert "[")))
;;      ;;(define-key slime-mode-map (kbd ")") (lambda () (interactive) (insert "]")))
;;      (define-key slime-mode-map (kbd "(") (function self-insert-command))
;;      (define-key slime-mode-map (kbd ")") (function self-insert-command))
;;      (define-key slime-mode-map (kbd "\e\[") (lambda () (interactive) (insert "(")))
;;      (define-key slime-mode-map (kbd "\e\]") (lambda () (interactive) (insert ")")))
;; 
;;           
;;      (defun slime-send-dwim (arg)
;;        "Send the appropriate forms to CL to be evaluated.
;; http://bc.tech.coop/blog/070424.html
;; "
;;        (interactive "P")
;;        (save-excursion
;;          (cond 
;;            ;;Region selected - evaluate region
;;            ((not (equal mark-active nil))
;;             (copy-region-as-kill-nomark (mark) (point)))
;;            ;; At/before sexp - evaluate next sexp
;;            ((or (looking-at "\s(")
;;                 (save-excursion
;;                   (ignore-errors (forward-char 1))
;;                   (looking-at "\s(")))
;;             (forward-list 1)
;;             (let ((end (point))
;;                   (beg (save-excursion
;;                          (backward-list 1)
;;                          (point))))
;;               (copy-region-as-kill-nomark beg end)))
;;            ;; At/after sexp - evaluate last sexp
;;            ((or (looking-at "\s)")
;;                 (save-excursion
;;                   (backward-char 1)
;;                   (looking-at "\s)")))
;;             (if (looking-at "\s)")
;;                 (forward-char 1))
;;             (let ((end (point))
;;                   (beg (save-excursion
;;                          (backward-list 1)
;;                          (point))))
;;               (copy-region-as-kill-nomark beg end)))
;;            ;; Default - evaluate enclosing top-level sexp
;;            (t (progn
;;                 (while (ignore-errors (progn
;;                                         (backward-up-list)
;;                                         t)))
;;                 (forward-list 1)
;;                 (let ((end (point))
;;                       (beg (save-excursion
;;                              (backward-list 1)
;;                              (point))))
;;                   (copy-region-as-kill-nomark beg end)))))
;;          (set-buffer (slime-output-buffer))
;;          (unless (eq (current-buffer) (window-buffer))
;;            (pop-to-buffer (current-buffer) t))
;;          (goto-char (point-max))
;;          (yank)
;;          (if arg (progn
;;                    (slime-repl-return)
;;                    (other-window 1)))))
;; 
;; 
;;      ;; (define-key lisp-mode-map [f7] 'slime-send-dwim)
;;      ;; (define-key lisp-mode-map [f8] (lambda ()
;;      ;;                                  (interactive)
;;      ;;                                  (slime-send-dwim 1)))
;; 
;;      
;;      (defun slime-version ()
;;        (interactive)
;;        (eval-in-cl "(swank-loader::slime-version-string)"
;;                    (lambda (values)
;;                      (if (null (cdr values))
;;                          (message (format "%s" v))
;;                          (dolist (v values)
;;                            (message (format "%s\n" v)))))))
;;   
;; 
;; 
;;      (defvar *pm* '() "process-marker alist")
;; 
;;      (defun pjb-slime-net-filter (process string)
;;        "Accept output from the socket and input all complete messages."
;;        (with-current-buffer (process-buffer process)
;;          (save-excursion
;;            (let ((pma (assoc process *pm*)))
;;              (when pma (goto-char (marker-position (cdr pma)))))
;;            (insert string))
;;          (slime-process-available-input)))
;; 
;; 
;;      (defun pjb-slime-eval-with-transcript (form &optional fn wait)
;;        "Send FROM and PACKAGE to Lisp and pass the result to FN.
;; Display the result in the message area, if FN is nil."
;;        (let* ((proc (slime-connection))
;;               (spb (process-buffer proc))
;;               (spf (process-filter proc)))
;;          (let ((pma (assoc proc *pm*))
;;                (m (let ((m (make-marker)))
;;                     (set-marker m (point) (current-buffer))
;;                     m)))
;;            (if pma
;;                (setf (cdr pma) m)
;;                (push (cons proc m) *pm*)))
;;          (set-process-buffer proc (current-buffer))
;;          (set-process-filter proc 'pjb-slime-net-filter)
;;          (unwind-protect (with-lexical-bindings (fn)
;;                            (slime-eval-async  form
;;                                               (lambda (value)
;;                                                 (cond (fn (funcall fn value))
;;                                                       (t (.EMACS "%s" value)))
;;                                                 (slime-show-last-output))))
;;            (set-process-buffer proc spb)
;;            (set-process-filter proc spf)
;;            (setf *pm* (delete (assoc proc *pm*) *pm*)))))
;; 
;; 
;;      ;;   (defun pjb-slime-eval-last-expression ()
;;      ;;     "Evaluate the expression preceding point."
;;      ;;     (interactive)
;;      ;;     (let* ((str  (slime-last-expression))
;;      ;;            (sexp (read-from-string str)))
;;      ;;       (if (and (listp sexp)
;;      ;;                (symbolp (fisrt sexp))
;;      ;;                (< 3 (LENGTH (SYMBOL-NAME (first sexp))))
;;      ;;                (STRING-EQUAL "DEF"  (SYMBOL-NAME (first sexp)) :end2 3))
;;      ;;         (slime-eval-last-expression str)
;;      ;;         (slime-eval-print-last-expression str))))
;; 
;; 
;;      (defun pjb-slime-eval-last-expression ()
;;        "Evaluate the expression preceding point."
;;        (interactive)
;;        (if buffer-read-only
;;            (slime-eval-last-expression)
;;            (let ((str  (slime-last-expression)))
;;              ;; (.EMACS "A DEF? %S" (STRING-EQUAL "(DEF"  str :end2 4))
;;              (if (string-equal* "(DEF"  str :end2 4)
;;                  (slime-interactive-eval str)
;;                  (slime-eval-print-last-expression str)))))
;; 
;; 
;;      (defun slime-restart-lisp-image ()
;;        (interactive)
;;        (when (slime-connected-p)
;;          (dolist (buf (buffer-list))
;;            (when (or (string= (buffer-name buf) slime-event-buffer-name)
;;                      (string-match "^\\*inferior-lisp*" (buffer-name buf)))
;;              (kill-buffer buf))))
;;        (call-interactively 'slime)) ;;slime-restart-lisp-image
;; 
;; 
;;      (defun pjb-slime-erase-buffer ()
;;        "Reset the slime output buffer to initial state."
;;        (interactive)
;;        (with-current-buffer (slime-output-buffer)
;;          (let ((inhibit-read-only t))
;;            (erase-buffer)
;;            (slime-repl-update-banner)))) ;;pjb-slime-erase-buffer
;; 
;; 
;;      (defun slime-kill ()
;;        (interactive)
;;        (map nil (lambda (x) (when (buffer-named x) (kill-buffer x)))
;;             '("*slime-repl[1]*" "*slime-events*" "*inferior-lisp*")))
;; 
;; 
;;      (defun slime-relaunch ()
;;        (interactive)
;;        (slime-kill)
;;        (sit-for 1)
;;        (slime)) ;;slime-relaunch
;; 
;;      (defalias 'slime-reload 'slime-relaunch)
;; 
;; 
;;      (defun pjb-slime-reset-minor-mode ()
;;        (dolist (slime-mode-vars '( slime-repl-read-mode
;;                                   slime-temp-buffer-mode
;;                                   inferior-slime-mode slime-mode))
;;          (setf minor-mode-map-alist (delete-if (lambda (x) (eq (car x) slime-mode-vars))
;;                                                minor-mode-map-alist)))
;;        ) ;;pjb-slime-reset-minor-mode
;; 
;; 
;;      (defvar *pjb-slime-keys-dynamic* nil)
;;      (defun pjb-slime-substitute-command (key command &rest keys)
;;        (unless  *pjb-slime-keys-dynamic*
;;          (setf slime-keys (mapcar (function copy-seq) (copy-seq slime-keys))
;;                *pjb-slime-keys-dynamic* t))
;;        (let ((prefixedp (cadr (member :prefixed keys)))
;;              (skeys slime-keys))
;;          (while skeys
;;            (when (and (string= key (first (car skeys)))
;;                       (equiv prefixedp (cadr (member :prefixed (car skeys)))))
;;              (setf (second (car skeys)) command
;;                    skeys nil))
;;            (pop skeys)))
;;        (pjb-slime-reset-minor-mode)
;;        (load "slime" *pjb-load-noerror* *pjb-load-silent*))
;; 
;; 
;;      ;; (pjb-slime-substitute-command "\M-." 'slime-edit-definition-other-window)
;; 
;;      ;; (pjb-slime-substitute-command "\C-e" 'pjb-slime-eval-last-expression
;;      ;;                               :prefixed t)
;; 
;; 
;;      (progn
;;        (define-key sldb-mode-map  "\M-."     'slime-edit-definition-other-window)
;;        (define-key slime-mode-map          "\C-ch"    'slime-hyperspec-lookup)
;;        (define-key inferior-slime-mode-map "\C-ch"    'slime-hyperspec-lookup)
;;        (define-key slime-mode-map  "\C-c\C-e" 'pjb-slime-eval-last-expression)
;;        (define-key slime-mode-map  "\C-x\C-e" 'pjb-slime-eval-last-expression)
;;        (define-key slime-mode-map          "\C-c\C-t" 'pjb-slime-erase-buffer)
;;        (define-key slime-mode-map          " "        'slime-space) ;'cl-magic-space)
;;        (define-key inferior-slime-mode-map "\C-c\C-t" 'pjb-slime-erase-buffer)
;;        )
;; 
;;      (defun slime-symbol-name-at-point ()
;;        "Return the name of the symbol at point, otherwise nil."
;;        (save-restriction
;;          ;; Don't be tricked into grabbing the REPL prompt.
;;          (when (and (eq major-mode 'slime-repl-mode)
;;                     (>= (point) slime-repl-input-start-mark))
;;            (narrow-to-region slime-repl-input-start-mark (point-max)))
;;          (save-excursion
;;            (skip-syntax-forward "w_")
;;            (skip-syntax-backward "-")
;;            (let ((string (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;                            (when bounds
;;                              (buffer-substring (car bounds)
;;                                                (progn
;;                                                  (goto-char (1- (cdr bounds)))
;;                                                  (if (looking-at "\\.\"")
;;                                                      (1- (cdr bounds))
;;                                                      (cdr bounds))))))))
;;              (and string
;;                   ;; In Emacs20 (thing-at-point 'symbol) returns "" instead
;;                   ;; of nil when called from an empty (or
;;                   ;; narrowed-to-empty) buffer.
;;                   (not (equal string ""))
;;                   (substring-no-properties   string)))))) ;;slime-symbol-name-at-point
;; 
;; 
;; 
;;      ;; (trace slime-init-keymaps  slime-init-keymaps  slime-define-key)
;;      ;; (trace pjb-slime-eval-last-expression)
;;      ;; (show (assoc "" slime-keys))
;; 
;;      (defun slime-hyperspec-lookup (symbol-name)
;;        "A wrapper for `hyperspec-lookup'"
;;        (interactive (list (let ((completion-ignore-case t)
;;                                 (symbol-at-point (slime-symbol-name-at-point)))
;;                             (if (and symbol-at-point
;;                                      (intern-soft (downcase symbol-at-point)
;;                                                   common-lisp-hyperspec-symbols))
;;                                 symbol-at-point
;;                                 (completing-read
;;                                  "Look up symbol in Common Lisp HyperSpec: "
;;                                  common-lisp-hyperspec-symbols #'boundp
;;                                  t symbol-at-point
;;                                  'common-lisp-hyperspec-history)))))
;;        (hyperspec-lookup symbol-name)) ;;slime-hyperspec-lookup
;; 
;;      ;; (setf sldb-hook nil)
;;      (add-hook 'sldb-hook (lambda () (toggle-truncate-lines 1)))
;; 
;; 
;;      (defun slime-macroexpand-in-place (&optional string)
;;        (interactive)
;;        (unless string
;;          (setf string (slime-sexp-at-point-or-error)))
;;        (lexical-let ((package (slime-current-package)))
;;          (insert (slime-eval `(swank:swank-macroexpand-1 ,string)))))
;;      
;;      ) ;;slime
;; 
;;     
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "ILISP")
;;      (.EMACS "ilisp")
;; ;;;----------------------------------------------------------------------------
;; ;;; ILISP
;;      (require 'ilisp)
;; 
;;      (setq ilisp-*use-fsf-compliant-keybindings*  t)
;;      (setq ilisp-*use-frame-for-arglist-output-p* nil)
;;      (setq ilisp-*arglist-message-lisp-space-p*   nil)
;;      (setq ilisp-arglist-output                   nil)
;;      (setq ilisp-motd                             nil)
;;      (setq ilisp-defpackage-command-string
;;            "([Dd][Ee][Ff][-A-Za-z]*[Pp][Aa][Cc][Kk][Aa][Gg][Ee]  *\\([^ ][^ ]*\\)")
;;      ;; ;; (setq ilisp-hash-form-regexp "\\(^[ \t]*#[+-].\\)\\|\\(^[ \t]*(\\(.*::?\\)?\\(defpackage\\|define-package\\)[ \t\n]\\)\\|\\(^[ \t]*(\\(.*::?\\)?in-package[ \t\n]*\\)")
;; 
;; 
;;      ;; (setf ilisp-mode-hook nil lisp-mode-hook nil scheme-mode-hook nil clisp-hs-hook)
;;      (let ((hook  (lambda () (require 'ilisp))))
;;        (add-hook 'lisp-mode-hook   hook)
;;        (add-hook 'ilisp-mode-hook  hook)
;;        (add-hook 'scheme-mode-hook hook))
;; 
;;      ;;(lambda () (set-buffer-process-coding-system 'mule-utf-8 'mule-utf-8)))
;;      ;;(setf common-lisp-hook nil clisp-hs-hook nil)
;;      (add-hook 'ilisp-init-hook
;;                (lambda () (set-buffer-process-coding-system 'mule-utf-8 'mule-utf-8)))
;; 
;;      (defun ilisp-eval-region (start end)
;;        (interactive "r")
;;        (let* ((form (lisp-defun-region-and-name))
;;               (result
;;                (eval-region-lisp start end  'result
;;                                  (format "Evaluating %s" (car (cdr (cdr form)))))))
;;          (goto-char end)
;;          (lisp-display-output result))) ;;ilisp-eval-region
;; 
;; 
;;      (defun pjb-output-to-current-buffer (output ilisp-output-sink)
;;        "
;; This function is used to display the output from ilisp.
;; It's hooked by `ilisp-display-output-function'.
;; "
;;        (end-of-line)
;;        (insert (if (string-match "\n" output) "\n" "\n;;"))
;;        (insert output)
;;        (unless (string-match "\n" output) (goto-char 0)))
;; 
;;      (setq ilisp-display-output-function 'pjb-output-to-current-buffer)
;; 
;; 
;;      (defadvice  ilisp-display-output-adaptively
;;          (around pjb-ilisp-display-output-adaptively last
;;                  (output ilisp-output-sink) activate)
;;        "Always display output to the echo area: 21.2 can do with multiline strings"
;;        (ilisp-display-output-in-echo-area output ilisp-output-sink)
;;        ) ;;ilisp-display-output-adaptively
;;      (ad-activate 'ilisp-display-output-adaptively)
;;      ;;end ilisp
;;      )
;;     ))


;;;----------------------------------------------------------------------------
(.EMACS "emacs<->Common Lisp RPC with slime/swank")

;;; In emacs, we can execute Common Lisp expressions:

;; (require 'slime)
;; (slime)

(setf slime-enable-evaluate-in-emacs t) 

(defun eval-in-cl (cl-expression-string process-result-values)
  (slime-eval-with-transcript
   `(swank:eval-and-grab-output ,cl-expression-string)
   (lexical-let  ((here (current-buffer))
                  (process-result-values process-result-values))
     (lambda (result-values)
       (set-buffer here)
       (funcall process-result-values result-values)))))

;; (eval-in-cl "(values 1 * (ext:! 20) (package-name *package*))"
;;             (lambda (values)
;;               (dolist (v values)
;;                 (insert (format "%s\n" v)))))
;; Returns:
;;
;; nil
;;
;; then later inserts:
;;
;; 1
;; (42 (EMACS-UNREADABLE |buffer| |*scratch*|))
;; 2432902008176640000
;; "COMMON-LISP-USER"


;; ;;; In Common Lisp, we can execute emacs lisp expressions:
;; 
;; (defparameter *emacs-readtable* (copy-readtable))
;; (setf (readtable-case *emacs-readtable*) :preserve)
;; (set-syntax-from-char #\> #\) *emacs-readtable*)
;; (set-dispatch-macro-character
;;  #\# #\<
;;  (lambda (stream subchar dispchar)
;;    `(emacs-unreadable ,@(read-delimited-list #\> stream t)))
;;  *emacs-readtable*)
;; 
;; ;; Probably more readtable patching would be in order.
;; ;;
;; ;; We could define CLOS proxies for emacs objects for a more seamless
;; ;; integration. swank::eval-in-emacs process the CL form to make it
;; ;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; ;; to emacs lisp forms returning the corresponding emacs object.
;; 
;; (defun eval-in-emacs (form &optional nowait)
;;   (let ((result (SWANK::EVAL-IN-EMACS `(format "%S" ,form) nowait))
;;         (*readtable* *emacs-readtable*))
;;     (with-input-from-string (in result)
;;       (let ((result (read in nil in)))
;;         result))))
;; 
;; 
;; (eval-in-emacs `(progn
;;                   (switch-to-buffer (buffer-named "*scratch*"))
;;                   (goto-char (point-max))
;;                   (insert ,(format nil "~%Hello~%"))
;;                   (list 42 (current-buffer))))
;; 
;; ;; Switch to the *scratch* buffer,
;; ;; goto the last position, and
;; ;; inserts \nHello\n
;; ;; then returns:
;; ;; (42 (EMACS-UNREADABLE |buffer| |*scratch*|))



;;;----------------------------------------------------------------------------
(.EMACS "Common Lisp indenting")

(require 'lisp-mode)
(load-library "cl-indent")

(setq lisp-indent-function 'common-lisp-indent-function)

(defun lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation)
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function
which has a non-nil property `lisp-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style
* an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (or (looking-at ":") (not (looking-at "\\sw\\|\\s_"))))
        (progn ; car of form doesn't seem to be a symbol, or is a keyword
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (get (intern-soft function) 'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state)))))))

;; (setq lisp-indent-function 'common-lisp-indent-function)

(defun cl-indent (symbol num-forms)
  "
Put on the SYMBOL and its lower case and upper case variants
a 'lisp-indent-function property set to NUM-FORMS.
"
  (dolist (property '(lisp-indent-function common-lisp-indent-function))
    (put symbol property num-forms)
    (put (intern (string-downcase (symbol-name symbol))) property num-forms)
    (put (intern (string-upcase   (symbol-name symbol))) property num-forms)))


(defun %batch-cl-indent (&rest indent-symbols-list)
  (dolist (item indent-symbols-list)
    (let ((indent (car item)))
      (dolist (sym (cdr item))
        (cl-indent sym indent)
        (let ((p (position (character ":") (symbol-name sym))))
          (when p
            (cl-indent (intern (subseq (symbol-name sym) (1+ p)))
                       indent)))))))


(defmacro* do-directories-up ((var dir-path &optional result) &body body)
  "
DO:     Evaluates body with var bound to dir-path, then dir-path's parent, 
        and so on up to the root directory.
RETURN: The evaluation of the result form.
"
  `(do ((,var ,dir-path
             (if (string-match "^\\(.*/\\)[^/]+/$" ,var)
                 (match-string 1 ,var)
                 "")))
      ((string-equal "" ,var) ,result)
    ,@body))


(defun* read* (stream &optional (eof-error-p t) eof-value ignored)
  (handler-case (read stream)
    (end-of-file (err)  (if eof-error-p
                            (error err)
                            eof-value))))


(defun load-lisp-indentations ()
  "Processes a lisp.indentations file, 
in the current directory, or in a parent."
  (interactive)
  (do-directories-up (dir default-directory)
    (let ((file (concat dir "lisp.indentations")))
      ;; (message "file = %S" file)
      (when (file-exists-p file)
        (save-excursion
          (let ((count (length (buffer-list)))) ; is there a better way?
            (find-file file)
            (goto-char (point-min))
            (let ((killp (/= count (length (buffer-list)))))
              (unwind-protect
                  (loop
                     for clause = (read* (current-buffer) nil (current-buffer))
                     until (eql clause (current-buffer))
                     do (message "(%%batch-cl-indent '%S)" clause)
                     do (%batch-cl-indent clause))
                (when killp (kill-buffer (current-buffer)))))))))))

;; (defmacro batch-cl-indent (&rest indent-symbols-list)
;;   `(%batch-cl-indent ,@(mapcar (lambda (x) `(quote ,x)) indent-symbols-list)))

(defun batch-cl-indent ()
  (interactive)
  (warn "The new command is load-lisp-indentations")
  (load-lisp-indentations))


(let ((html '(DOCTYPE A ABBR ACRONYM ADDRESS APPLET AREA B BASE
              BASEFONT  BDO BIG BLOCKQUOTE BODY BR BUTTON CAPTION
              CENTER CITE CODE COL COLGROUP DD DEL DFN DIR DIV DL
              DT EM FIELDSET FONT  FORM FRAME FRAMESET H1 H2 H3 H4
              H5 H6 HEAD HR HTML I  IFRAME IMG INPUT INS ISINDEX
              KBD LABEL LEGEND LI LINK MAP MENU  META NOFRAMES
              NOSCRIPT OBJECT OL OPTGROUP OPTION P PARAM PRE Q S
              SAMP SCRIPT SELECT SMALL SPAN STRIKE STRONG STYLE SUB
              SUP TABLE TBODY TD TEXTAREA TFOOT TH THEAD TITLE TR
              TT  U UL VAR)))
  (%batch-cl-indent
   (cons 1 (mapcar (lambda (sym) (intern (concat "HTML:" (symbol-name sym)))) html))
   (cons 0 (mapcar (lambda (sym) (intern (concat "<:"    (symbol-name sym)))) html))
   (cons 2 '(<:div))))

(defun eval-last-sexp-lisp ()
  (interactive)
  (forward-sexp -1)
  (let ((current-prefix-arg '-))
    (eval-next-sexp-lisp)))


(defun pjb-lisp-remove-end-comment ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\([^;\"]*\\)\n[ \t]*\\())+\\) *;.*" nil t)
    (let ((a (match-string 1))
          (b (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert a b)))
  (goto-char (point-min))
  (while (re-search-forward "^\\([^;\"]*\\)\\())+\\) *;.*" nil t)
    (let ((a (match-string 1))
          (b (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert a b))))


;;;----------------------------------------------------------------------------
(.EMACS "Common-Lisp Hyperspec")
;; common-lisp-hyperspec-symbols

(defun probe-url (url)
  (cond
    ((string= "file://" (subseq url 0 (min (length url) 7)))
     (file-readable-p (subseq url 7)))
    ((file-readable-p "/tmp/no-internet")
     nil)
    (t
     (zerop (parse-integer
             (shell-command-to-string
              (format "wget -O /dev/null %S >/dev/null 2>&1 ; echo -n $?"
                      url)))))))


;; (require 'clhs)
(require 'hyperspec)

(defvar *lw-clhs*)
(setf   *lw-clhs*          "www.lispworks.com/documentation/HyperSpec/")
(defvar *hyperspec-path*)
(setf   *hyperspec-path*   (or (ignore-errors (get-directory :hyperspec))
                               (concat "/usr/local/html/local/lisp/" *lw-clhs*))
        common-lisp-hyperspec-root
        (dolist
            (url (list
                  (concat "file://" *hyperspec-path*)
                  "file:///usr/share/doc/hyperspec/HyperSpec/"
                  ;; (concat "http://thalassa.lan.informatimago.com/lisp/" *lw-clhs*)
                  (concat "http://" *lw-clhs*)))
          (when (probe-url url)
            (return url))))

(defvar common-lisp-hyperspec-browser (function ignore))
(defvar common-lisp-hyperspec-frame   (selected-frame))
(load "extra/hyperspec" *pjb-load-noerror* *pjb-load-silent*)



(defun thing-at-point-no-properties (thing)
  "Return the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING."
  (if (get thing 'thing-at-point)
      (funcall (get thing 'thing-at-point))
      (let ((bounds (bounds-of-thing-at-point thing)))
        (if bounds
            (buffer-substring-no-properties (car bounds) (cdr bounds))))))


(when  (or t  (boundp 'common-lisp-hyperspec-symbols))

  (defun common-lisp-hyperspec-complete (string predicate allp)
    (if allp
        (let ((result '()))
          (mapatoms
           (lambda (symbol)
             (let ((name (symbol-name symbol)))
               (when (or (and (<= (length string) (length name))
                              (string-equal* string name :end2 (length string)))
                         (search (concat "-" string) name :test (function equalp)))
                 (push name result))))
           common-lisp-hyperspec-symbols)
          result)
        (try-completion string common-lisp-hyperspec-symbols predicate)))

  (defun common-lisp-hyperspec (symbol-name)
    "View the documentation on SYMBOL-NAME from the Common Lisp HyperSpec.
If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions.
The Common Lisp HyperSpec is the full ANSI Standard Common Lisp, provided
by Kent Pitman and Xanalys Inc.  By default, the Xanalys Web site is
visited to retrieve the information.  Xanalys Inc. allows you to transfer
the entire Common Lisp HyperSpec to your own site under certain conditions.
Visit http://www.xanalys.com/software_tools/reference/HyperSpec/ for more
information.  If you copy the HyperSpec to another location, customize the
variable `common-lisp-hyperspec-root' to point to that location."
    (interactive
     (list (let ((completion-ignore-case t)
                 (symbol-at-point (thing-at-point-no-properties 'symbol)))
             (completing-read
              "Look up symbol in Common Lisp HyperSpec: "
              (function common-lisp-hyperspec-complete) #'boundp
              t symbol-at-point
              'common-lisp-hyperspec-history))))
    (maplist
     (lambda (entry)
       (case system-type
         ((darwin)
          (case window-system
            ((x)
             (browse-url (concat common-lisp-hyperspec-root
                                 "Body/" (car entry))))
            ((mac ns nil)
             (let ((browse-url-browser-function 'browse-url-generic)
                   (browse-url-generic-program "/usr/bin/open"))
               (browse-url (concat common-lisp-hyperspec-root
                                   "Body/" (car entry)))) )
            (otherwise (error "Unknown window-system"))))
         ((gnu/linux)
          (let ((browse-url-browser-function common-lisp-hyperspec-browser))
            (browse-url (concat common-lisp-hyperspec-root
                                "Body/" (car entry)))) )
         (otherwise
          (error "Unknown system-type.")))
       (if (cdr entry)
           (sleep-for 1.5)))
     (delete-duplicates
      (let ((symbol (intern-soft (downcase symbol-name)
                                 common-lisp-hyperspec-symbols)))
        (if (and symbol (boundp symbol))
            (symbol-value symbol)
            (error "The symbol `%s' is not defined in Common Lisp"
                   symbol-name)))
      :test (function equal))))
  

  (defun gcl-hyperspec (symbol-name)
    (interactive
     (list (let ((completion-ignore-case t)
                 (symbol-at-point (thing-at-point-no-properties 'symbol)))
             (completing-read
              "Look up symbol in Common Lisp HyperSpec: "
              common-lisp-hyperspec-symbols #'boundp
              t symbol-at-point
              'common-lisp-hyperspec-history))))
    (maplist
     (lambda (entry)
       (info (format "(gcl)%s" (car entry)))
       (if (cdr entry)
           (sleep-for 1.5)))
     (delete-duplicates
      (let ((symbol (intern-soft (downcase symbol-name)
                                 common-lisp-hyperspec-symbols)))
        (if (and symbol (boundp symbol))
            (list symbol)
            (error "The symbol `%s' is not defined in Common Lisp"
                   symbol-name)))
      :test (function equal))))


  (defalias 'hyperspec-lookup 'common-lisp-hyperspec) ; 'gcl-hyperspec)
  (global-set-key (kbd "C-h y") (function hyperspec-lookup))
  (defalias 'clhs             'common-lisp-hyperspec)

  ) ;;(boundp 'common-lisp-hyperspec-symbols)


(defun random-hyperspec ()
  (interactive)
  (let* ((random-hyperspec-symbol
          (let ((syms '()))
            (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
            (nth (random (length syms)) syms)))
         (random-page (let ((pages (symbol-value random-hyperspec-symbol)))
                        (nth (random (length pages)) pages))))
    (browse-url (concat common-lisp-hyperspec-root "Body/" random-page))))




;;   (defun send-url-to-safari (url)
;;     "Sends URL to Safari, using Apple's Open Scripting Architecture."
;;     (with-temp-buffer
;;       (insert "tell application \"Safari\"\n")
;;       (insert "  activate\n")
;;       (insert "  make new document at the beginning of documents\n")
;;       (insert (format "  set the URL of the front document to \"%s\"\n" url))
;;       (insert "end tell\n")
;;       (call-process-region (point-min) (point-max) "/usr/bin/osascript")))

;;; (setq common-lisp-hyperspec-root
;;;       "file://Users/ayank/Documents/text/computer/lisp/HyperSpec/")
;;; (setq common-lisp-hyperspec-symbol-table
;;;       "file://Users/ayank/Documents/text/computer/lisp/HyperSpec/Data/Map_Sym.txt")

;;; (load-library
;;;  "file://Users/ayank/Documents/text/computer/lisp/ilisp/extra/hyperspec")

;;; (global-set-key [(shift f1)]
;;;                 '(lambda ()
;;;                    (interactive)
;;;                    (common-lisp-hyperspec
;;;                     (thing-at-point 'symbol))))


;;; or:
;;;       (setq browse-url-browser-function
;;; 	    '(lambda (url &optional new-win)
;;; 	       (do-applescript (concat "open location \""
;;; 				       url "\""))))




;;;----------------------------------------------------------------------------
(defstruct location buffer file-name line-number line-contents)

(defmacro ignore-errors* (&rest body)
  `(handler-case
       (progn ,@body)
     (error (err)
       (message "ignore-errors* %S" err)
       (values nil err))))


(defun %find-tag-locations-in-order ()
  (message "%%find-tag-locations-in-order enters tagname=%S next-p=%S regexp-p=%S"
           tagname next-p regexp-p)
  (loop
     with locations = '()
     for (buffer err) = (ignore-errors* (find-tag-noselect tagname nil regexp-p))
     then (ignore-errors* (find-tag-noselect tagname t regexp-p))
     initially  (message "%%find-tag-locations-in-order initially")
     do (message "buffer = %S" buffer)
     while buffer
     collect (with-current-buffer buffer
               (make-location
                :buffer (current-buffer)
                :file-name (buffer-file-name (current-buffer))
                :line-number (count-lines (point-min) (point-at-bol))
                :line-contents (buffer-substring-no-properties
                                (point-at-bol) (point-at-eol))))
     finally (message "%%find-tag-locations-in-order exists %S" locations) (return (values locations err))))


(defun pjb-find-tag-meat ()
  (message "pjb-find-tag-meat enters")
  (unless next-p
    (message "pjb-find-tag-meat 1")
    (multiple-value-bind (locations error) (%find-tag-locations-in-order)
      (message  " locations (2) = %S" locations)
      (if locations
          (progn
            (message "pjb-find-tag-meat 2")
            (save-excursion
              (message "pjb-find-tag-meat 3")
              (switch-to-buffer-other-window (get-buffer-create
                                              (format "*tags on %s*" tagname))) 
              (erase-buffer)
              (compilation-mode 1)
              (message "pjb-find-tag-meat 4")
              (dolist (loc locations)
                (insert (format "%s:%s %s\n"
                                (location-file-name loc)
                                (location-line-number loc)
                                (location-line-contents loc))))
              (message "pjb-find-tag-meat 5"))
            (message "pjb-find-tag-meat 6")
            (message "pjb-find-tag-meat exits %S" (location-buffer (first locations)))
            (location-buffer (first locations)))
          (when error
            (signal (car error) (cdr error)))))
    (error (err)
           (message "pjb-find-tag-meat 7")
           (message "%s" err))))


;; (add-hook 'find-tag-hook (function pjb-find-tag-meat))
;; (setq find-tag-hook nil)

;;;----------------------------------------------------------------------------
(.EMACS "matlab/scilab")

(autoload 'matlab-mode "matlab.el" "Enter Matlab mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab.el" "Interactive Matlab mode." t)



;;;----------------------------------------------------------------------------
(.EMACS "ocaml")

(autoload 'caml-mode "ocaml" (interactive)
          "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

(appendf auto-mode-alist '(("\\.ml[iyl]?$" .  caml-mode)))


;;;----------------------------------------------------------------------------
(.EMACS "otter")
(autoload 'otter-mode "otter-mode" "Mode for editing Otter files." t)
(autoload 'run-otter  "otter-mode" "Running an inferior Otter process." t)
(appendf auto-mode-alist '(("\\.ot$" . otter-mode)
                           ("\\.in$" . otter-mode)))

(defvar otter-program "/usr/local/src/otter-3.2/source/otter"
  "The absolute path to Otter")

(defvar otter-out-extension ".oout"
  "*The extension used to generate output files from Otter.")
(defvar otter-in-extension1 "\\.ot"
  "*The standard extension to recognize input-files for Otter.
 Note that the \\ before the period is a must !")
(defvar otter-in-extension2 "\\.in"
  "*An other extension to recognize input-files for Otter.
  Note that the \\ before the period is a must !")


;;;----------------------------------------------------------------------------
(.EMACS "COBOL mode")
(autoload 'cobol-mode "cobol")
(appendf auto-mode-alist '(("\\.cbl\\'" . cobol-mode)
                           ("\\.cobol\\'" . cobol-mode)))

(when (fboundp 'speedbar-add-supported-extension)
  (speedbar-add-supported-extension "CBL"))


;;;----------------------------------------------------------------------------
(.EMACS "PHP mode")
(autoload 'php-mode "php-mode" "Major mode for editing PHP" t)
(appendf auto-mode-alist '(("\\.php\\'" . php-mode)))

;;;----------------------------------------------------------------------------
(.EMACS "css mode")
(autoload 'css-mode "css-mode" "Major mode for editing CSS" t)
(appendf auto-mode-alist '(("\\.css\\'" . css-mode)))
(setf cssm-indent-function (function cssm-c-style-indenter))

;;;----------------------------------------------------------------------------
(.EMACS "Postscript mode")
(autoload 'ps-mode "ps-mode" "Major mode for editing PostScript" t)
(appendf auto-mode-alist '(("\\.[eE]?[pP][sS]$" . ps-mode)))

;; Use lazy-lock for large PostScript files
(add-hook 'ps-mode-hook
          '(lambda ()
            (make-local-variable 'font-lock-support-mode)
            (make-local-variable 'lazy-lock-defer-on-scrolling)
            (setq font-lock-support-mode 'lazy-lock-mode
             lazy-lock-defer-on-scrolling t)
            (turn-on-font-lock)))
(add-hook 'ps-run-mode-hook
          '(lambda ()
            (turn-on-font-lock)))

(unless (fboundp 'run-mode-hooks) (defun run-mode-hooks (&rest args)))



;;;----------------------------------------------------------------------------
;; (.EMACS "remem")
;; (when (require 'remem nil t)
;;   (setq remem-scopes-list
;;         (file-expand-wildcards "~/RA-indexes/*")
;;         '(("my-email" 6 5 500)
;;           ("my-notes" 2 10 500))))


;;;----------------------------------------------------------------------------
(.EMACS "psgml mode")
(when (require 'psgml nil t)
  ;;(define-key sgml-mode-map "\M-\C-f"   'set-justification-full)
  (appendf auto-mode-alist '(("\\.html$"   . html-mode)
                             ("\\.htm$"    . html-mode))))



;; ------------------------------------------------------------------------
;; AIM client for emacs:
;; (load"tnt") ;; Doesn't work good.

;; ------------------------------------------------------------------------
(when (require 'emms-setup nil t)
  (require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
   ;;   ;; save playlist and load at emacs start
   ;; (require 'emms-history)
   ;; (emms-history-load)
   ;; (setq emms-repeat-playlist 1)
  (emms-standard)
  (emms-default-players)
  (defalias 'np 'emms-show))



;;;----------------------------------------------------------------------------

(defun browse-url-firefox2 (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-firefox-arguments' are also passed to
Firefox.

When called interactively, if variable
`browse-url-new-window-flag' is non-nil, load the document in a
new Firefox window, otherwise use a random existing one.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

When called non-interactively, optional second argument
NEW-WINDOW is used instead of `browse-url-new-window-flag'.

On MS-Windows systems the optional `new-window' parameter is
ignored.  Firefox for Windows does not support the \"-remote\"
command line parameter.  Therefore, the
`browse-url-new-window-flag' and `browse-url-firefox-new-window-is-tab'
are ignored as well.  Firefox on Windows will always open the requested
URL in a new window."
  (interactive (browse-url-interactive-arg "URL: "))
  ;; URL encode any `confusing' characters in the URL.  This needs to
  ;; include at least commas; presumably also close parens.
  (while (string-match "[[-` -$&-,;->{-~]" url)
    (setq url (replace-match
               (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply 'start-process
                         (concat "firefox " url)
                         " *start-browser*"
                         browse-url-firefox-program
                         (append
                          browse-url-firefox-arguments
                          (list url)))))
    (set-process-sentinel process
                          `(lambda (process change)
                             (browse-url-firefox-sentinel process ,url)))))

(setf common-lisp-hyperspec-browser (function browse-url-firefox2)
      browse-url-browser-function   (function browse-url-firefox2))



(when (and (or (<= 23 emacs-major-version) (require 'mime-parse nil t))
           (ignore-errors (require 'w3m        nil t))
           (or (<= 23 emacs-major-version) (require 'mime-w3m   nil t)))
  (.EMACS "w3m mode")

  (defvar *browse-frame-name* "*w3m*")
  (pushnew *browse-frame-name* special-display-buffer-names :test (function equal))

  (defun pjb-w3m-browse-url-in-another-frame (url &rest args)
    (save-excursion
      (raise-frame
       (select-frame
        (or (find-if (lambda (frame) (equalp (frame-name frame) +browse-frame-name+))
                     (frame-list))
            (make-frame (list (cons 'name *browse-frame-name*))))))
      (w3m-goto-url url)))

  ;; (setf common-lisp-hyperspec-browser (function pjb-w3m-browse-url-in-another-frame))
  (setf common-lisp-hyperspec-browser (function w3m-browse-url)
        browse-url-browser-function   (function w3m-browse-url))
  
  ) ;;when


;;;----------------------------------------------------------------------------
(when (require 'column-marker nil t)
  (.EMACS "columnmarker")
  (column-marker-1 80))


;;;----------------------------------------------------------------------------
(.EMACS "wiki")
(autoload 'wiki-remote-get "wiki-remote" "Edit a wiki page." t)


;; Need modifications to use french mediapedias:
(when (load "~/src/wikipedia-el/wikipedia.el" t)
  (.EMACS "wikipedia")
  ;; (.EMACS "wikipedia-mode")
  ;; (autoload 'wikipedia-mode "wikipedia-mode.el"
  ;;   "Major mode for editing documents in Wikipedia markup." t)
  )

;;;----------------------------------------------------------------------------
;; Rmime:
;;(load "rmime"  *pjb-load-noerror* *pjb-load-silent*)
;;(when (fboundp 'rmime-format)
;; (add-hook 'rmail-show-message-hook 'rmime-format))
;;(when (fboundp 'rmime-cancel)
;;  (add-hook 'rmail-edit-mode-hook    'rmime-cancel))
;;(remove-hook 'rmail-show-message-hook 'rmime-format)
;;(remove-hook 'rmail-edit-mode-hook    'rmime-cancel)

;;;----------------------------------------------------------------------------
;; (.EMACS "mailcrypt")
;; ;; (load-library "mailcrypt")
;; ;; (mc-setversion "gpg")
;; (autoload 'mc-install-write-mode "mailcrypt" nil t)
;; (autoload 'mc-install-read-mode  "mailcrypt" nil t)
;; (add-hook 'mail-mode-hook 'mc-install-write-mode)
;;
;; (setf mc-default-scheme 'mc-scheme-gpg
;;       mc-gpg-user-id     "E9350DE9")

;; (when (string-lessp "23" emacs-version)
;;   (require 'epa-file)
;;   (epa-file-enable)
;;   (setf epa-armor t))

;;;----------------------------------------------------------------------------
(.EMACS "mew")

(autoload 'mew      "mew" "Start Mew." t)
(autoload 'mew-send "mew" "Compose a new message." t)
;;(defalias 'mail 'mew-send)

(setq mew-mailbox-type 'mbox)
(setq mew-mbox-command "incm")
(setq mew-mbox-command-arg "-d /var/spool/mail/pjb")


;; (autoload 'x-face-decode-message-header "x-face-e21")
;; (autoload 'x-face-insert "x-face-e21" nil t)
;; (autoload 'x-face-save "x-face-e21" nil t)
;; (autoload 'x-face-show "x-face-e21" nil t)
;; (autoload 'x-face-turn-off "x-face-e21")
;; (setq x-face-auto-image t)



;;;----------------------------------------------------------------------------
;; (require 'nnmail)
;; (defadvice nnmail-process-babyl-mail-format
;;     (before nnmail-process-babyl-mail-format/log (func artnum-func) activate)
;;   (message "Found a BABYL mailbox!"))
;; (defadvice nnmail-process-mmdf-mail-format
;;     (before nnmail-process-mmdf-mail-format/log (func artnum-func) activate)
;;   (message "Found a MMDF mailbox!"))
;; (defadvice nnmail-process-maildir-mail-format
;;     (before nnmail-process-maildir-mail-format/log (func artnum-func) activate)
;;   (message "Found a MAILDIR mailbox!"))
;; (defadvice nnmail-process-unix-mail-format
;;     (before nnmail-process-unix-mail-format/log (func artnum-func) activate)
;;   (message "Found a UNIX mailbox!"))

;;;----------------------------------------------------------------------------
(when (require 'vm nil t)
  (.EMACS "vm")
  (require 'vm-vars)
  (ignore-errors (load-library "vm-w3m"))


  (defun pjb-mail-mode-meat ()
    (message "mail-mode-meat")
    (set-buffer-file-coding-system   'utf-8)
    ;; (setf buffer-file-coding-system  'utf-8)
    ;; (inactivate-input-method)
    (local-set-key (kbd "TAB") (quote expand-mail-aliases)))

  (add-hook 'mail-setup-hook           'pjb-mail-mode-meat)

  ;; (add-hook 'vm-mode-hook              'mc-install-read-mode)
  ;; (add-hook 'vm-summary-mode-hook      'mc-install-read-mode)
  ;; (add-hook 'vm-virtual-mode-hook      'mc-install-read-mode)
  ;; (add-hook 'vm-mail-mode-hook         'mc-install-write-mode)
  ;; (add-hook 'vm-presentation-mode-hook 'mc-install-write-mode)
  (defun pjb-vm-summary-meat         () (when nil (set-frame-name "MAIL")))
  (defun pjb-vm-mail-meat            () (when nil (set-frame-name "COMPOSE")))
  (defun pjb-vm-reply-meat           () (inactivate-input-method))
  (defun pjb-vm-arrived-message-meat () (pjb-vm-kill-subject-regexp "\\[SPAM\\]"))
  (add-hook 'vm-summary-mode-hook    'pjb-vm-summary-meat)
  (add-hook 'vm-mail-mode-hook       'pjb-vm-mail-meat)
  (add-hook 'vm-reply-hook           'pjb-vm-reply-meat)
  (add-hook 'vm-arrived-message-hook 'pjb-vm-arrived-message-meat)


  (defun pjb-vm-delete-spam (count)
    (interactive "p")
    (vm-save-message "~/mail/spam.mbox" count)
    (pjb-vm-delete-message count))

  (defun pjb-vm-delete-message (count)
    (interactive "p")
    (vm-delete-message count)
    (vm-next-message))

  (defun pjb-vm-visit-folder-meat ()
    (define-key vm-mode-map (kbd "d")     'pjb-vm-delete-message)
    (define-key vm-mode-map (kbd "M-d")   'pjb-vm-delete-spam)
    (define-key vm-mode-map (kbd "o")     'vm-save-message)
    (define-key vm-mode-map (kbd "r")     'vm-followup-include-text)
    (define-key vm-mode-map (kbd "s")     'vm-save-folder)
    (local-set-key          (kbd "c")     'vm-save-message))

  (add-hook 'vm-visit-folder-hook 'pjb-vm-visit-folder-meat)

  (unless (<= 23 emacs-major-version)
    (keyboard-translate (aref (kbd "M-S-d") 0) (aref (kbd "M-S-d") 0))
    (keyboard-translate (aref (kbd "M-D")   0) (aref (kbd "M-D")   0)))

  ;; (defun vm-from-biff ()
  ;;   (interactive)
  ;;   (select-frame (make-frame))
  ;;   (vm-register-frame (vm-selected-frame))
  ;;   (when vm-warp-mouse-to-new-frame
  ;;     (vm-warp-mouse-to-frame-maybe (vm-selected-frame)))
  ;;   (vm))

  (when (load "vm-sort" *pjb-load-noerror* *pjb-load-silent*)
    (defun vm-sort-compare-author (m1 m2)
      "Let's sort by domain first"
      (let ((s1 (vm-su-from m1))
            (s2 (vm-su-from m2))
            l1 d1 l2 d2)
        (let ((@-pos (position (character "@") s1)))
          (if @-pos
              (setf d1 (subseq s1 (1+ @-pos))
                    l1 (subseq s1 0 @-pos))
              (setf d1 ""
                    l1 s1)))
        (let ((@-pos (position (character "@") s2)))
          (if @-pos
              (setf d2 (subseq s2 (1+ @-pos))
                    l2 (subseq s2 0 @-pos))
              (setf d2 ""
                    l2 s2)))
        (cond ((string-equal s1 s2) '=)
              ((string-equal d1 d2)
               (cond ((string-lessp l1 l2) t)
                     ((string-equal l1 l2)
                      (let ((f1 (vm-su-full-name m1))
                            (f2 (vm-su-full-name m2)))
                        (cond ((string-lessp f1 f2) t)
                              ((string-lessp f1 f2) '=)
                              (t nil))))
                     (t nil)))
              ((string-lessp d1 d2) t)
              (t nil))))
    ) ;;when vm-sort

  

  ;; (catch :found
  ;;   (let ((version emacs-version)
  ;;         (next
  ;;          (lambda ()
  ;;            (cond
  ;;              ((null version)          (throw :found :default))
  ;;              ((= 0 (length version))
  ;;               (setf version nil)
  ;;               (concatenate 'string (NAMESTRING (USER-HOMEDIR-PATHNAME))
  ;;                            "bin/movemail"))
  ;;              (t (prog1
  ;;                     (format "/usr/local/libexec/emacs/%s/%s/movemail"
  ;;                       version system-configuration)
  ;;                   (string-match "^\\(\\([0-9][.0-9]*\\)\\.\\)?[0-9]+$" version)
  ;;                   (setq version (or (match-string 2 version) ""))))))))
  ;;     (do ((path (funcall next) (funcall next)))
  ;;         (nil)
  ;;       (when (file-exists-p path)
  ;;         (setq vm-movemail-program  path)
  ;;         (throw :found :one)))))

  ;; ;; movemail: No locks available for /larissa//var/spool/mail/pjb
  ;; ;; /usr/local/libexec/emacs/21.3/i686-pc-linux-gnu/movemail exited with code 1
  ;; (setq vm-movemail-program
  ;;       (concatenate 'string  (NAMESTRING (USER-HOMEDIR-PATHNAME)) "bin/movemail"))

;;; '(vm-imap-server-list (quote ("imap:imap.afaa.asso.fr:143:inbox:login:pjb:pari-fle")))



  ;; rmail -> vm
  ;;(defalias 'rmail 'vm)
  ;;(defalias 'rmail-input 'vm-visit-folder)
  ;;(defun rmail       () (interactive) (error "Use mail in a shell!"))
  ;;(defun vm          () (interactive) (error "Use mail in a shell!"))
  ;;(defun rmail-input () (interactive) (error "Use mail in a shell!"))

  ;; (defmacro advise-replace (fname parameters body)
  ;;   (let ((aname (intern (format "pjb-adrep-%s" fname))))
  ;;     `(progn
  ;;        (defadvice ,fname
  ;;            (around ,aname  first  ,parameters  activate)
  ;;          ,body)
  ;;        (ad-activate (quote ,fname)))
  ;;     )) ;;advise-replace
  ;; (put 'advise-replace      'lisp-indent-function 2)
  ;; 
  ;; 
  ;; (advise-replace rmail-sort-by-correspondent (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-author" "author")))
  ;; 
  ;; (advise-replace rmail-sort-by-date          (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-date" "date")))
  ;; 
  ;; (advise-replace rmail-sort-by-labels        (reverse)
  ;;   (error "Not implemented with VM."))
  ;; 
  ;; (advise-replace rmail-sort-by-lines         (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-line-count" "line-count")))
  ;; 
  ;; (advise-replace rmail-sort-by-recipient     (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-recipients" "recipients")))
  ;; 
  ;; (advise-replace rmail-sort-by-subject       (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-subject" "subject")))


  ;; (defadvice vm-mime-attach-object
  ;;     (before pjb-removemime-vm-mime-attach-object nil activate)
  ;;   (save-restriction
  ;;     (pjb-mail-narrow-to-headers)
  ;;     (pjb-mail-remove-header "^\\(MIME-Version:\\|Content-\\)" t))
  ;;   )
  ;; (ad-activate 'vm-mime-attach-object)



  (when (require 'vm-pop nil t)
    (defun vm-pop-cleanup-region (start end)
      (setq end (vm-marker end))
      (save-excursion
        (goto-char start)
        ;; CRLF -> LF
        (while (and (< (point) end) (search-forward "\r\n"  end t))
          (replace-match "\n" t t))
        (goto-char start)
        (while (and (< (point) end) (search-forward "^\\(From .*\\)" end t))
          (message "inserting a new line before %S" (buffer-substring (match-beginning 0) (match-end 0)))
          (goto-char (match-beginning 0))
          (insert "\n\n")
          (forward-line))
        ;; (goto-char start)
        ;; chop leading dots
        ;; (while (and (< (point) end) (re-search-forward "^\\."  end t))
        ;;   (replace-match "" t t)
        ;;   (forward-char))
        )
      (set-marker end nil)))


  (defun vm (&optional folder read-only access-method)
    "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, message additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox normally causes any contents of the system mailbox to
be moved and appended to the resulting buffer.  You can disable this automatic fetching of mail by setting `vm-auto-get-new-mail' to nil.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' saves the buffered folder to disk, but does not expunge
deleted messages.  Use `###' to expunge deleted messages.

See the documentation for vm-mode for more information."
    (interactive (list nil current-prefix-arg))
    (vm-session-initialization)
    ;; set inhibit-local-variables non-nil to protect
    ;; against letter bombs.
    ;; set enable-local-variables to nil for newer Emacses
    (catch 'done
      ;; deduce the access method if none specified
      (if (null access-method)
          (let ((f (or folder vm-primary-inbox)))
            (cond ((and vm-recognize-imap-maildrops
                        ;; f could be a buffer
                        (stringp f)
                        (string-match vm-recognize-imap-maildrops f))
                   (setq access-method 'imap
                         folder f))
                  ((and vm-recognize-pop-maildrops
                        ;; f could be a buffer
                        (stringp f)
                        (string-match vm-recognize-pop-maildrops f))
                   (setq access-method 'pop
                         folder f)))))
      (let ((full-startup (not (bufferp folder)))
            (did-read-index-file nil)
            folder-buffer first-time totals-blurb
            folder-name remote-spec
            preserve-auto-save-file)
        (cond ((eq access-method 'pop)
               (setq remote-spec (vm-pop-find-spec-for-name folder))
               (if (null remote-spec)
                   (error "No such POP folder: %s" folder))
               (setq folder-name folder)
               ;; Prior to VM 7.11, we computed the cache filename
               ;; based on the full POP spec including the password
               ;; if it was in the spec.  This meant that every
               ;; time the user changed his password, we'd start
               ;; visiting the wrong (and probably nonexistent)
               ;; cache file.
               ;;
               ;; To fix this we do two things.  First, migrate the
               ;; user's caches to the filenames based in the POP
               ;; sepc without the password.  Second, we visit the
               ;; old password based filename if it still exists
               ;; after trying to migrate it.
               ;;
               ;; For VM 7.16 we apply the same logic to the access
               ;; methods, pop, pop-ssh and pop-ssl and to
               ;; authentication method and service port, which can
               ;; also change and lead us to visit a nonexistent
               ;; cache file.  The assumption is that these
               ;; properties of the connection can change and we'll
               ;; still be accessing the same mailbox on the
               ;; server.
               (let ((f-pass (vm-pop-make-filename-for-spec remote-spec))
                     (f-nopass (vm-pop-make-filename-for-spec remote-spec t))
                     (f-nospec (vm-pop-make-filename-for-spec remote-spec t t)))
                 (cond ((or (string= f-pass f-nospec)
                            (file-exists-p f-nospec))
                        nil )
                       ((file-exists-p f-pass)
                        ;; try to migrate
                        (condition-case nil
                            (rename-file f-pass f-nospec)
                          (error nil)))
                       ((file-exists-p f-nopass)
                        ;; try to migrate
                        (condition-case nil
                            (rename-file f-nopass f-nospec)
                          (error nil))))
                 ;; choose the one that exists, password version,
                 ;; nopass version and finally nopass+nospec
                 ;; version.
                 (cond ((file-exists-p f-pass)
                        (setq folder f-pass))
                       ((file-exists-p f-nopass)
                        (setq folder f-nopass))
                       (t
                        (setq folder f-nospec)))))
              ((eq access-method 'imap)
               (setq remote-spec folder
                     folder-name (or (nth 3 (vm-imap-parse-spec-to-list
                                             remote-spec))
                                     folder)
                     folder (vm-imap-make-filename-for-spec remote-spec))))
        (setq folder-buffer
              (if (bufferp folder)
                  folder
                  (let ((file (or folder (expand-file-name vm-primary-inbox
                                                           vm-folder-directory))))
                    (if (file-directory-p file)
                        ;; MH code perhaps... ?
                        (error "%s is a directory" file)
                        (or (vm-get-file-buffer file)
                            (let ((default-directory
                                   (or (and vm-folder-directory
                                            (expand-file-name vm-folder-directory))
                                       default-directory))
                                  (inhibit-local-variables t)
                                  (enable-local-variables nil)
                                  (enable-local-eval nil)
                                  ;; for Emacs/MULE
                                  (default-enable-multibyte-characters nil)
                                  ;; for XEmacs/Mule
                                  (coding-system-for-read
                                   (vm-line-ending-coding-system)))
                              (message "Reading %s..." file)
                              (prog1 (find-file-noselect file)
                                ;; update folder history
                                (let ((item (or remote-spec folder
                                                vm-primary-inbox)))
                                  (if (not (equal item (car vm-folder-history)))
                                      (setq vm-folder-history
                                            (cons item vm-folder-history))))
                                (message "Reading %s... done" file))))))))
        (set-buffer folder-buffer)
        (cond ((memq access-method '(pop imap))
               (if (not (equal folder-name (buffer-name)))
                   (rename-buffer folder-name t))))
        (if (and vm-fsfemacs-mule-p enable-multibyte-characters)
            (set-buffer-multibyte nil))
        ;; for MULE
        ;;
        ;; If the file coding system is not a no-conversion variant,
        ;; make it so by encoding all the text, then setting the
        ;; file coding system and decoding it.  This situation is
        ;; only possible if a file is visited and then vm-mode is
        ;; run on it afterwards.
        ;;
        ;; There are separate code blocks for FSF Emacs and XEmacs
        ;; because the coding systems have different names.
        (defvar buffer-file-coding-system)
        (if (and (or vm-xemacs-mule-p vm-xemacs-file-coding-p)
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-unix)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-dos)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-mac)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'binary))))
            (let ((buffer-read-only nil)
                  (omodified (buffer-modified-p)))
              (unwind-protect
                   (progn
                     (encode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system)
                     (set-buffer-file-coding-system 'no-conversion nil)
                     (decode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system))
                (set-buffer-modified-p omodified))))
        (if (and vm-fsfemacs-mule-p (null buffer-file-coding-system))
            (set-buffer-file-coding-system 'raw-text nil))
        (if (and vm-fsfemacs-mule-p
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-unix)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-mac)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-dos)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'no-conversion))))
            (let ((buffer-read-only nil)
                  (omodified (buffer-modified-p)))
              (unwind-protect
                   (progn
                     (encode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system)
                     (set-buffer-file-coding-system 'raw-text nil)
                     (decode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system))
                (set-buffer-modified-p omodified))))
        (vm-check-for-killed-summary)
        (vm-check-for-killed-presentation)
        ;; If the buffer's not modified then we know that there can be no
        ;; messages in the folder that are not on disk.
        (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
        (setq first-time (not (eq major-mode 'vm-mode))
              preserve-auto-save-file (and buffer-file-name
                                           (not (buffer-modified-p))
                                           (file-newer-than-file-p
                                            (make-auto-save-file-name)
                                            buffer-file-name)))
        ;; Force the folder to be read only if the auto
        ;; save file contains information the user might not
        ;; want overwritten, i.e. recover-file might be
        ;; desired.  What we want to avoid is an auto-save.
        ;; Making the folder read only will keep
        ;; subsequent actions from modifying the buffer in a
        ;; way that triggers an auto save.
        ;;
        ;; Also force the folder read-only if it was read only and
        ;; not already in vm-mode, since there's probably a good
        ;; reason for this.
        (setq vm-folder-read-only (or preserve-auto-save-file read-only
                                      (default-value 'vm-folder-read-only)
                                      (and first-time buffer-read-only)))
        ;; If this is not a VM mode buffer then some initialization
        ;; needs to be done 
        (if first-time
            (progn
              (buffer-disable-undo (current-buffer))
              (abbrev-mode 0)
              (auto-fill-mode 0)
              ;; If an 8-bit message arrives undeclared the 8-bit
              ;; characters in it should be displayed using the
              ;; user's default face charset, rather than as octal
              ;; escapes.
              (vm-fsfemacs-nonmule-display-8bit-chars)
              (vm-mode-internal access-method)
              (cond ((eq access-method 'pop)
                     (vm-set-folder-pop-maildrop-spec remote-spec))
                    ((eq access-method 'imap)
                     (vm-set-folder-imap-maildrop-spec remote-spec)))
              ;; If the buffer is modified we don't know if the
              ;; folder format has been changed to be different
              ;; from index file, so don't read the index file in
              ;; that case.
              (if (not (buffer-modified-p))
                  (setq did-read-index-file (vm-read-index-file-maybe)))))

        ;; builds message list, reads attributes if they weren't
        ;; read from an index file.
        (vm-assimilate-new-messages nil (not did-read-index-file) nil t)

        (if (and first-time (not did-read-index-file))
            (progn
              (vm-gobble-visible-header-variables)
              (vm-gobble-bookmark)
              (vm-gobble-pop-retrieved)
              (vm-gobble-imap-retrieved)
              (vm-gobble-summary)
              (vm-gobble-labels)))

        (if first-time
            (vm-start-itimers-if-needed))

        ;; make a new frame if the user wants one.  reuse an
        ;; existing frame that is showing this folder.
        (if (and full-startup
                 ;; this so that "emacs -f vm" doesn't create a frame.
                 this-command)
            (apply 'vm-goto-new-folder-frame-maybe
                   (if folder '(folder) '(primary-folder folder))))

        ;; raise frame if requested and apply startup window
        ;; configuration.
        (if full-startup
            (let ((buffer-to-display (or vm-summary-buffer
                                         vm-presentation-buffer
                                         (current-buffer))))
              (vm-display buffer-to-display buffer-to-display
                          (list this-command)
                          (list (or this-command 'vm) 'startup))
              (if vm-raise-frame-at-startup
                  (vm-raise-frame))))

        ;; say this NOW, before the non-previewers read a message,
        ;; alter the new message count and confuse themselves.
        (if full-startup
            (progn
              ;; save blurb so we can repeat it later as necessary.
              (set-buffer folder-buffer)
              (setq totals-blurb (vm-emit-totals-blurb))
              (and buffer-file-name
                   (vm-store-folder-totals buffer-file-name (cdr vm-totals)))))

        (vm-thoughtfully-select-message)
        (vm-update-summary-and-mode-line)
        ;; need to do this after any frame creation because the
        ;; toolbar sets frame-specific height and width specifiers.
        (vm-toolbar-install-or-uninstall-toolbar)

        (and vm-use-menus (vm-menu-support-possible-p)
             (vm-menu-install-visited-folders-menu))

        (if full-startup
            (progn
              (if (and (vm-should-generate-summary)
                       ;; don't generate a summary if recover-file is
                       ;; likely to happen, since recover-file does
                       ;; not work in a summary buffer.
                       (not preserve-auto-save-file))
                  (vm-summarize t nil))
              ;; raise the summary frame if the user wants frames
              ;; raised and if there is a summary frame.
              (if (and vm-summary-buffer
                       vm-mutable-frames
                       vm-frame-per-summary
                       vm-raise-frame-at-startup)
                  (vm-raise-frame))
              ;; if vm-mutable-windows is nil, the startup
              ;; configuration can't be applied, so do
              ;; something to get a VM buffer on the screen
              (if vm-mutable-windows
                  (vm-display nil nil (list this-command)
                              (list (or this-command 'vm) 'startup))
                  (save-excursion
                    (switch-to-buffer (or vm-summary-buffer
                                          vm-presentation-buffer
                                          (current-buffer)))))))

        (if vm-message-list
            ;; don't decode MIME if recover-file is
            ;; likely to happen, since recover-file does
            ;; not work in a presentation buffer.
            (let ((vm-auto-decode-mime-messages
                   (and vm-auto-decode-mime-messages
                        (not preserve-auto-save-file))))
              (vm-preview-current-message)))

        (run-hooks 'vm-visit-folder-hook)

        ;; Warn user about auto save file, if appropriate.
        (if (and full-startup preserve-auto-save-file)
            (message 
             (substitute-command-keys
              "Auto save file is newer; consider \\[recover-file].  FOLDER IS READ ONLY.")))
        ;; if we're not doing a full startup or if doing more would
        ;; trash the auto save file that we need to preserve,
        ;; stop here.
        (if (or (not full-startup) preserve-auto-save-file)
            (throw 'done t))
      
        (if full-startup
            (message totals-blurb))

        (if (and vm-auto-get-new-mail
                 (not vm-block-new-mail)
                 (not vm-folder-read-only))
            (progn
              (message "Checking for new mail for %s..."
                       (or buffer-file-name (buffer-name)))
              (if (vm-get-spooled-mail t)
                  (progn
                    (setq totals-blurb (vm-emit-totals-blurb))
                    (if (vm-thoughtfully-select-message)
                        (vm-preview-current-message)
                        (vm-update-summary-and-mode-line))))
              (message totals-blurb)))

        ;; Display copyright and copying info.
        (if (and (interactive-p) (not vm-startup-message-displayed))
            (progn
              (vm-display-startup-message)
              (if (not (input-pending-p))
                  (message totals-blurb)))))))

  ) ;;when


;;;----------------------------------------------------------------------------
;;; GNUS

;; (defadvice gnus-summary-mark-as-expirable
;;     (after gnus-summary-mark-as-expirable+next-line activate)
;;   (next-line))
;; (ad-disable-advice 'gnus-summary-mark-as-expirable 'after 'gnus-summary-mark-as-expirable+next-line)


;; (local-set-key (kbd "e") (function gnus-summary-mark-as-expirable))

(setf *pjb-gnus-trash-mailbox* "nnimap+voyager.informatimago.com:INBOX.Trash")
(setf *pjb-gnus-junk-mailbox*  "nnimap+voyager.informatimago.com:INBOX.Junk")

 	
	

(define-key gnus-summary-mode-map (kbd "v DEL") 'pjb-gnus-summary-move-article-to-trash)
(define-key gnus-article-mode-map (kbd "v DEL") 'pjb-gnus-summary-move-article-to-trash)

(define-key gnus-summary-mode-map (kbd "v j")   'pjb-gnus-summary-move-article-to-junk)
(define-key gnus-article-mode-map (kbd "v j")   'pjb-gnus-summary-move-article-to-junk)

;; (define-key gnus-group-mode-map   (kbd "v j d")
;;   (lambda ()
;;     (interactive)
;;     (gnus-group-jump-to-group "nndraft:drafts")))


(defun pjb-gnus-message-setup-meat ()
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (while (re-search-forward "anevia.com" (point-max) t)
;;       (delete-region (match-beginning 0) (match-end 0))
;;       (insert "informatimago.com")))
  )



;;;----------------------------------------------------------------------------
;;; pjb

(setq pgp-command 'pgp-gpg-command)
(setq pgp-signer  "0xEF5E9966") ;; "pjb@informatimago.com"
(setq *pjb-sources-initials* "PJB")

(require 'message)
(defalias 'rot13-region 'message-caesar-region)

(defalias 'scratch      'pjb-scratch)
(defalias 'eurotunnel   'pjb-eurotunnel)
(defalias 'address      'pjb-address)
(defalias 'attach-file  'pjb-mail-attach-file)
(defalias 'ff           'full-frame)
(defalias 'make         'compile)

(remove-hook 'mail-send-hook 'mime-edit-maybe-translate)


;; Server for emacsclient:
(setf mm-content-transfer-encoding-defaults '(("text/.*" 8bit)
                                              ("message/rfc822" 8bit)
                                              ("application/emacs-lisp" 8bit)
                                              ("application/x-emacs-lisp" 8bit)
                                              ("application/x-patch" 8bit)
                                              (".*" base64))
      mm-body-charset-encoding-alist '((iso-8859-1  . 8bit)
                                       (iso-8859-15 . 8bit)))
;;(add-to-list 'mm-charset-synonym-alist '(iso8859-15 . iso-8859-15))
;;(add-to-list 'mm-charset-synonym-alist '(iso885915 . iso-8859-15))




;;(erc-select :server "localhost" :nick "pjb")
;;(erc-send-command  (format "PRIVMSG &bitlbee :identify %s" "popo"))

(defun pjb/erc-insert-timestamp-left (string)
  "Insert timestamps at the beginning of the line."
  (goto-char (point-min))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
                        (string-equal string erc-timestamp-last-inserted)))
         (len (length string))
         (s (if ignore-p "" string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (unless (string= s "")
      (erc-put-text-property 0 len 'field 'erc-timestamp s)
      (erc-put-text-property 0 len 'invisible 'timestamp s))
    (insert s)))


(defun pjb/erc-fill-static ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (save-match-data
    (goto-char (point-min))
    (when (looking-at "^\\(\\S-+\\)")
      (let ((nick (match-string 1)))
        (let ((fill-column (- erc-fill-column (erc-timestamp-offset)))
              (fill-prefix (make-string erc-fill-static-center 32)))
          (insert (make-string (max 0 (- erc-fill-static-center
                                         (length nick) 1))
                               32))
          (erc-fill-regarding-timestamp))
        (erc-restore-text-properties)))))


(defun pjb/erc-meat ()
  (interactive)
  (reset-movement-keypad)
  (setf erc-insert-timestamp-function 'pjb/erc-insert-timestamp-left
        erc-fill-function 'pjb/erc-fill-static)
  (remove-hook 'erc-insert-modify-hook 'erc-unmorse))


(add-hook 'erc-insert-post-hook 'pjb/erc-meat)

;; (add-hook 'erc-join-hook 'pjb-erc-join-meat)  
;; (pjb-set-erc-nickserv-passwords)
;; (setf erc-timestamp-format "%Y-%m-%d %H:%M\n")
;; (erc-match-mode 1)
;; (global-set-key (kbd "C-y") (function erc-yank))


;;(erc-select :server "localhost" :nick "pjb")
;;(erc-send-command  (format "PRIVMSG &bitlbee :identify %s" "popo"))
 
;; (setf erc-hide-list '())
;; (setf erc-hide-list  (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))

;;;----------------------------------------------------------------------------

(defvar *galatea-frame* nil)


(defun open-frame-on-galatea ()
  (interactive)
  (unless *galatea-frame*
    (setq *galatea-frame*
          (make-frame-on-display "galatea.informatimago.com:0.0")))
  (set-frame-size  *galatea-frame* 96 40)
  (let ((current-frame (selected-frame)))
    (select-frame *galatea-frame*)
    (set-background-color "#102040")
    (set-foreground-color "#80f0f0")
    ;;(set-face-foreground 'font-lock-comment-face "Green")
    ;;(set-face-foreground 'font-lock-function-name-face "Yellow")
    (select-frame current-frame))
  (setq common-lisp-hyperspec-frame *galatea-frame*))


(defun reopen-frame-on-galatea ()
  (interactive)
  (when *galatea-frame*
    (delete-frame *galatea-frame*)
    (setq *galatea-frame* nil))
  (open-frame-on-galatea))

;;;----------------------------------------------------------------------------
(.EMACS "server")

(setf server-socket-dir (format "/tmp/emacs%s" (user-uid))
      server-name       (format "server-%d" (emacs-pid)))


(defparameter *frame-server-job-ticket* "~/frame-emacs"
  "Path to the job-ticket file.")


(defun frame-server (&optional token-path)
  (setf token-path (or token-path *frame-server-job-ticket*))
  (when (file-exists-p token-path)
    (find-file token-path)
    (make-frame-on-display
     (delete ?\n (prog1 (buffer-string)
                   (kill-buffer (current-buffer))
                   (delete-file token-path)))
     (list (cons 'name (format "n%s" (frame-parameter nil 'name)))))))

(defun frame-server-start ()
  (interactive)
  (run-at-time nil 5 (function frame-server) nil))

(frame-server-start)



(cond
  (*pjb-pvs-is-running*)
  ((member "(gnus)"  command-line-args)
   (setf *activity-tag* "GNUS")
   (push '(name . "GNUS") default-frame-alist)
   (set-background-color "#ccccfefeebb7")
   (when (fboundp 'set-default-frame-alist)
     ;; (set-default-frame-alist *default-font*)
     )
   (setf *frame-server-job-ticket* "~/frame-gnus"))
  ((member "(irc)"  command-line-args)
   (setf *activity-tag* "ERC")
   (push '(name . "ERC") default-frame-alist)
   (setf *frame-server-job-ticket* "~/frame-erc")
   (when (fboundp 'set-default-frame-alist)
     ;; (set-default-frame-alist *default-font*)
     ))
  (t
   (setf *activity-tag* "EMACS")
   (push '(name . "PGM") default-frame-alist)

   (server-start)
   
   (setf (getenv "CVSEDITOR")  "emacsclient"
         (getenv "EDITOR")     "emacsclient"
         (getenv "VISUAL")     "emacsclient")
   (setf *frame-server-job-ticket* "~/frame-emacs")
   (when (fboundp 'set-default-frame-alist)
     ;; (set-default-frame-alist *default-font*)
     )))

;;;----------------------------------------------------------------------------


;; (defvar pjb-save-buffer-skip nil
;;   "*(buffer-local) Skip the update-def-names and pjb-update-eof
;; when saving this buffer.")
;; (make-variable-buffer-local 'pjb-save-buffer-skip)
;; (defun psb-xor (p q) "RETURN:  p xor q" (not (eq (not p) (not q))))
;; (defun pjb-save-buffer (&optional args)
;;   "This advice updates the eof comment before saving the buffer."
;;   (interactive "p")
;;   (when (and (not pjb-save-buffer-skip)
;;              (psb-xor current-prefix-arg
;;                       (string-match
;;                        "/pascal/\|/pjb/"
;;                        ;;(format  "^\\(%s\\|/local/users/pascal\\)/"
;;                        ;;      (regexp-quote (USER-HOMEDIR-PATHNAME)))
;;                        (or (buffer-file-name (current-buffer)) ""))))
;;     (unwind-protect
;;          (when (memq major-mode  '(lisp-mode emacs-lisp-mode))
;;            (.EMACS "Updating definition names...")
;;            (update-def-names))
;;       (.EMACS "Updating EOF tag...")
;;       (pjb-update-eof t)))
;;   (.EMACS "Saving buffer...")
;;   (save-buffer args)) ;;pjb-save-buffer

;; (when (and (fboundp 'update-def-names) (fboundp 'pjb-update-eof))
;;   (global-set-key "\C-x\C-s"    'pjb-save-buffer))
(defalias 'pjb-save-buffer 'save-buffer)


;; c modes

(when (fboundp 'pjb-c-todo-hook)
  (mapc (lambda (hook) (add-hook hook (function pjb-c-todo-hook)))
        '(c-mode-hook c++-mode-hook objc-mode-hook )))


;; (push  '(c . pjb-lineup-C-comments)              c-offsets-alist)
;; (push  '(comment-intro . pjb-lineup-C-comments)  c-offsets-alist)

(defun c-indent-or-tab ()
  (interactive)
  (if (string-match "^[ \t]*$"
                    (buffer-substring-no-properties
                     (save-excursion (beginning-of-line) (point))
                     (point)))
      (c-indent-command)
      (let ((indent-line-function 'indent-relative))
        (indent-for-tab-command))))


;; (setf c-mode-hook nil c++-mode-hook nil objc-mode-hook nil )
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map "{" 'self-insert-command)
            (local-set-key (kbd "TAB") (quote c-indent-or-tab))))

;;(add-hook 'c++-mode-hook (function pjb-c++-mode-hook))
;;(setf c++-mode-hook (delete (function pjb-c++-mode-hook) c++-mode-hook))



;; lisp modes

;; (setq emacs-lisp-mode-hook nil lisp-mode-hook nil)
(when (fboundp 'common-lisp-font-lock-hook)
  (add-hook 'common-lisp-mode-hook 'common-lisp-font-lock-hook))

(when (and (< emacs-major-version 23) (and (eq window-system 'x) (fboundp 'pretty-greek)))
  (add-hook 'emacs-lisp-mode-hook 'pretty-greek))
;;(add-hook mode 'show-paren-mode)))

(when (fboundp 'common-lisp-font-lock-hook)
  (add-hook 'lisp-mode-hook 'common-lisp-font-lock-hook))

(dolist (hooks  '(lisp-mode-hook emacs-lisp-mode-hook common-lisp-mode-hook
                  c-mode-hook c++-mode-hook))
  (add-hook hooks 'sexp-movement))


;;(setq emacs-lisp-mode-hook nil lisp-mode-hook nil)

(setq open-paren-in-column-0-is-defun-start nil)
(setq minibuffer-max-depth nil)
(setq print-circle t)


(autoload 'd-mode "/usr/local/src/languages/clisp/clisp-cvs/clisp/emacs/d-mode"
  "Mode to edit clisp sources." t)

;; (setq auto-mode-alist (append '(("\\.c\\'" . c-mode)) auto-mode-alist))
(appendf auto-mode-alist  '(("\\.pp\\'"                     . pascal-mode)
                            ("\\.\\(m[id]\\|mod\\|def\\)$"  . modula-2-mode)
                            ("-MIB$\\|-SMI$"                . snmp-mode)
                            ("\\.bison\\'"                  . c-mode)
                            ("\\.lex\\'"                    . c-mode)
                            ("\\.d\\'"                      . d-mode)))


;;;----------------------------------------------------------------------------
;; X-GPG-Key-ID: 0xAC23A821
;; X-GPG-fingerprint: CA53 7C90 2052 3484 51A2  8B3A E10A 8A44 AC23 A821
;; Disposition-Notification-To: <pjb@informatimago.com>
;; Return-Receipt-To:           <pjb@informatimago.com>
;; X-PGP-Key-URL:     http://www.informatimago.com/pgpkey
;;;----------------------------------------------------------------------------
;; Generate vc-annotate-color-map:
;; (let ((x 240))
;;     (while (< 0 x)
;;         (insert (format "(%f . \"#0000%02x\")\n" (* (/ 86400.0 65536.0 4.0)
;;                                                     (- 255 x)) x))
;;         (setq x (- x 16))))
;;;----------------------------------------------------------------------------

;; blank-mode.el to examine tabs and spaces in interior positions in the text
;; http://www.cpqd.com.br/~vinicius/emacs/Emacs.html



;;;----------------------------------------------------------------------------
(.EMACS "Info-directory-list")

(defun find-subdirs-with-info (base-path)
  (let ((result '())
        (add-base nil)
        (items (DIRECTORY (concatenate 'string base-path "/*"))))
    (unless (find-if (lambda (item) (string-match "/\\.nosearch$" item)) items)
      (dolist (item items)
        (cond
          ((string-match "\\.info\\(.gz\\)?$" item)
           (setq add-base t))
          ((file-directory-p item)
           (setq result (append result (find-subdirs-with-info item))))))
      (when add-base
        (push base-path result)))
    result))


(defun find-subdirs-with-dir (base-path)
  (when (file-exists-p base-path)
    (mapcar
     (function NAMESTRING)
     (mapcar (lambda (path)  (MAKE-PATHNAME :NAME nil :TYPE nil :VERSION nil
                                       :DEFAULTS path))
             (remove-if-not (function NULL)
                            (DIRECTORY (CONCATENATE 'STRING base-path "**/dir"))
                            :key (function PATHNAME-TYPE))))))


(defun find-subdirs-with-dir (base-path)
  (let ((result '())
        (add-base nil)
        (items (file-expand-wildcards (concat base-path "/*") t)))
    (unless (find-if (lambda (item) (string-match "/\\.nosearch$" item)) items)
      (dolist (item items)
        (cond
          ((string-match "/dir$" item)
           (setq add-base t))
          ((file-directory-p item)
           (setq result (append result (find-subdirs-with-dir item))))))
      (when add-base
        (push base-path result))
      result)))


(setq Info-default-directory-list
      (remove
       "/usr/share/info/emacs-21"
       (labels ((flatten
                    (tree)
                  "collect in a simple list all the non-nil atoms in the tree."
                  (cond
                    ((null tree) nil)
                    ((atom tree) (list tree))
                    (t (nconc (flatten (car tree))
                              (and (cdr tree) (flatten (cdr tree))))))))
         (flatten (mapcar (function find-subdirs-with-dir)
                          '("/usr/local/share/emacs/"
                            "/usr/local/share/info/"
                            "/usr/local/info/"
                            "/usr/share/info/"
                            "/usr/share/gcc-data/i486-pc-linux-gnu/4.1.2/info/")))))
      Info-directory-list Info-default-directory-list)



;;;----------------------------------------------------------------------------
(.EMACS "Miscellaneous commands")


(defun* notes ()
  (interactive)
  (do-directories-up (dir default-directory)
    (dolist (file '("NOTES.txt" "notes.txt" "NOTES.*[a-z]" "notes.*[a-z]"
                    ".notes.utf-8" ".notes*[a-z]"))
      (let ((files (file-expand-wildcards (concat dir file) t)))
        (when files
          (find-file (first files))
          (return-from notes))))))


(defun afaire ()
  "Jump to my TODO list."
  (interactive)
  (unless (zerop (user-uid))
    (notes)
    (goto-char (point-min))
    (search-forward "AFAIRE:" nil t 2)
    (recenter 1)
    (when (string-match "^thalassa" system-name)
      (vm-visit-folder "~/mail/todo.mbox"))))

(defun acheter ()
  "Jump to my TODO list."
  (interactive)
  (notes)
  (goto-char (point-min))
  (search-forward "ACHETER:" nil t 2)
  (recenter 1))


(when t
  (unless (intersection
           '("-f" "-funcall" "--funcall" "-e" "-eval" "--eval" "-execute"
             "--execute" "-insert" "--insert") command-line-args
           :test (function string=))
    (afaire)))


(defun doing (what)
  (interactive "sWhat are you doing? ")
  (find-file "~/doing.txt")
  (goto-char (point-max))
  (insert (shell-command-to-string "date")  what "\n#\n")
  (save-buffer)
  (bury-buffer))



;;;----------------------------------------------------------------------------
(.EMACS "google")

;; (require 'google)
;; (setq google-license-key "dF18sc1QFHLPxvBVqwv/WxCbYR18GHbp")
;; ;; Then M-x google-search RET
;; ;; or M-x google-search-region RET
;; (defalias 'url-retrieve-synchronously 'url-retrieve)

(defun google-search (search-string)
  "Search a string with Google."
  (interactive "sGoogle Search: ")
  (setf search-string
        (shell-command-to-string
         (format "echo %s|iconv -f ISO8859-1 -t UTF-8"
           (shell-quote-argument search-string))))
  (browse-url
   (format "http://www.google.com/search?as_q=%s&num=50&hl=en&ie=ISO8869-1&btnG=Google+Search&as_epq=&as_oq=&as_eq=&lr=&as_ft=i&as_filetype=&as_qdr=all&as_nlo=&as_nhi=&as_occt=any&as_dt=i&as_sitesearch=&safe=images"
     (apply (function concatenate) 'string
            (mapcar (lambda (ch)
                      (if (or (and (<= ?0 ch) (<= ch ?9))
                              (and (<= ?A ch) (<= ch ?Z))
                              (and (<= ?a ch) (<= ch ?z)))
                          (format "%c" ch)
                          (format "%%%02x" ch)))
                    (string-to-sequence search-string 'list)))))) ;;google-search


(defun google-search-region (start end)
  "Search the text in the region with Google."
  (interactive "r")
  (google-search (buffer-substring-no-properties start end)))


  
(defun acronym ()
  (interactive)
  (browse-url 
   (if (or (not mark-active) (eql (point) (mark)))
       (format "http://www.cygwin.com/acronyms/#%s"
               (read-from-minibuffer "Acronym: "))
       (buffer-substring-no-properties (min (point) (mark))
                                       (max (point) (mark))))))


;;;----------------------------------------------------------------------------

(defun grep-hyperspec (&optional string)
  (interactive "sString: ")
  (grep (format "find '%s' -type f -print|while read f ; do /opt/local/bin/lynx -dump -nolist \"$f\" | grep -i '%s' && echo \"$f:1:-\" ; done #" (shell-quote-argument *hyperspec-path*) string)))

(defun grep-includes (&optional string)
  (interactive "sString: ")
  (grep (format "find /usr/include/ /usr/local/include/ -type f -exec grep -n -i %s {} /dev/null \\; #" (shell-quote-argument string))))



;;;----------------------------------------------------------------------------
(.EMACS "emacs-uptime")

;;;----------------------------------------------------------------------------
;;; emacs-uptime.el
;;;
;;; Copyright (C) 1998, 2000, 2002, 2004, 2007, 2008 Thien-Thi Nguyen
;;;
;;; This file is part of ttn's personal elisp library, released under
;;; the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.  There is NO WARRANTY.  See file COPYING for details.

;;; Description: Give Emacs' uptime and some other stats in the modeline.

(defvar *emacs-start-time*   (current-time) "For (emacs-uptime);")

;;;###autoload
(defun emacs-uptime ()
  "Gives Emacs' uptime, based on global var `*emacs-start-time*'."
  (interactive)
  (let* ((st *emacs-start-time*)                ; set in do-it-now.el
         (cur (current-time))
         (hi-diff (- (car cur) (car st)))
         (tot-sec (+ (ash hi-diff 16) (- (cadr cur) (cadr st))))
         (days (/ tot-sec (* 60 60 24)))
         (hrs  (/ (- tot-sec (* days 60 60 24)) (* 60 60)))
         (mins (/ (- tot-sec (* days 60 60 24) (* hrs 60 60)) 60))
         (secs (/ (- tot-sec (* days 60 60 24) (* hrs 60 60) (* mins 60)) 1)))
    (message "Up %dd %dh %dm %ds (%s), %d buffers, %d files"
             days hrs mins secs
             (format-time-string "%a %Y-%m-%d %T" st)
             (length (buffer-list))
             (count t (buffer-list)
                    :test-not
                    (lambda (ignore buf)
                      (null (cdr (assoc 'buffer-file-truename
                                        (buffer-local-variables buf)))))))))

(provide 'emacs-uptime)

;;; emacs-uptime.el ends here
;;;----------------------------------------------------------------------------

(defalias 'uptime 'emacs-uptime)

(when (require 'uptimes nil t)
  (defun uptimes-read-uptimes ()
    "Read the uptimes database into `uptimes-last-n' and `uptimes-top-n'."
    (when (file-exists-p uptimes-database) ; doesn't mean the file contains anything
      (with-temp-buffer
        (let ((inhibit-clash-detection t)) ; For the benefit of XEmacs.
          ;; we don't want to visit the file, to avoid locking the file.
          (insert-file-contents uptimes-database nil))
        (setq uptimes-last-n 
              (ignore-errors            ; eat end-of-file errors
                (read (current-buffer))))
        (setq uptimes-top-n
              (or (ignore-errors        ; eat end-of-file errors
                    (read (current-buffer)))
                  uptimes-last-n))))))


;;;----------------------------------------------------------------------------
(.EMACS "Other patches")
(ignore-errors
  (require 'newcomment)
  (defun comment-region-internal (beg end cs ce
                                &optional ccs cce block lines indent)
  "Comment region BEG..END.
CS and CE are the comment start resp end string.
CCS and CCE are the comment continuation strings for the start resp end
of lines (default to CS and CE).
BLOCK indicates that end of lines should be marked with either CCE, CE or CS
\(if CE is empty) and that those markers should be aligned.
LINES indicates that an extra lines will be used at the beginning and end
of the region for CE and CS.
INDENT indicates to put CS and CCS at the current indentation of the region
rather than at left margin."
  ;;(assert (< beg end))
  (let ((no-empty nil ; PJB: always no-empty.
          ;;  (not (or (eq comment-empty-lines t)
          ;;           (and comment-empty-lines (zerop (length ce)))))
          ))
    ;; Sanitize CE and CCE.
    (if (and (stringp ce) (string= "" ce)) (setq ce nil))
    (if (and (stringp cce) (string= "" cce)) (setq cce nil))
    ;; If CE is empty, multiline cannot be used.
    (unless ce (setq ccs nil cce nil))
    ;; Should we mark empty lines as well ?
    (if (or ccs block lines) (setq no-empty nil))
    ;; Make sure we have end-markers for BLOCK mode.
    (when block (unless ce (setq ce (comment-string-reverse cs))))
    ;; If BLOCK is not requested, we don't need CCE.
    (unless block (setq cce nil))
    ;; Continuation defaults to the same as CS and CE.
    (unless ccs (setq ccs cs cce ce))

    (save-excursion
      (goto-char end)
      ;; If the end is not at the end of a line and the comment-end
      ;; is implicit (i.e. a newline), explicitly insert a newline.
      (unless (or ce (eolp)) (insert "\n") (indent-according-to-mode))
      (comment-with-narrowing
          beg end
          (let ((min-indent (point-max))
                (max-indent 0))
            (goto-char (point-min))
            ;; Quote any nested comment marker
            (comment-quote-nested comment-start comment-end nil)
            
            ;; Loop over all lines to find the needed indentations.
            (goto-char (point-min))
            (while
                (progn
                  (unless (looking-at "[ \t]*$")
                    (setq min-indent (min min-indent (current-indentation))))
                  (end-of-line)
                  (setq max-indent (max max-indent (current-column)))
                  (not (or (eobp) (progn (forward-line) nil)))))

            (setq max-indent
                  (+ max-indent (max (length cs) (length ccs))
                     ;; Inserting ccs can change max-indent by (1- tab-width)
                     ;; but only if there are TABs in the boxed text, of course.
                     (if (save-excursion (goto-char beg)
                                         (search-forward "\t" end t))
                         (1- tab-width) 0)))
            ;; ;; Inserting ccs can change max-indent by (1- tab-width).
            ;; (setq max-indent
            ;;   (+ max-indent (max (length cs) (length ccs)) tab-width -1))
            (unless indent (setq min-indent 0))
            
            ;; make the leading and trailing lines if requested
            (when lines
              (let ((csce
                     (comment-make-extra-lines
                      cs ce ccs cce min-indent max-indent block)))
                (setq cs (car csce))
                (setq ce (cdr csce))))

            (goto-char (point-min))
            ;; Loop over all lines from BEG to END.
            (while
                (progn
                  (unless (and no-empty (looking-at "[ \t]*$"))
                    (move-to-column min-indent t)
                    (insert cs) (setq cs ccs) ;switch to CCS after the first line
                    (end-of-line)
                    (if (eobp) (setq cce ce))
                    (when cce
                      (when block (move-to-column max-indent t))
                      (insert cce)))
                  (end-of-line)
                  (not (or (eobp) (progn (forward-line) nil))))))))))
  );;patch


;;;----------------------------------------------------------------------------
;; (.EMACS "indent buffer on find-file")
(defun pjb-indent-meat ()
  ;; If pjb-indent-meat is not in last position,
  ;; then move it over to last position.
  (let ((p (position (function pjb-indent-meat) find-file-hook)))
    (when (and p (< (1+ p) (length find-file-hook)))
      (setf find-file-hook
            (append (remove (function pjb-indent-meat) find-file-hook)
                    (list (function pjb-indent-meat))))))
  ;; Work only on some modes:
  (when (member major-mode '(lisp-mode common-lisp-mode emacs-lisp-mode
                             perl-mode shell-script-mode
                             scheme-mode c-mode c++-mode objective-c-mode))
    (.EMACS "indenting %S" (buffer-name))
    (if buffer-read-only
        (unwind-protect
             (progn (toggle-read-only)
                    (indent-region (point-min) (point-max))
                    (pop-mark))
          (toggle-read-only))
        (progn (indent-region (point-min) (point-max))
               (pop-mark)))
    (.EMACS "indenting %S done" (buffer-name))
    (set-buffer-modified-p nil)))
;; (setf find-file-hook
;;       (append (remove (function pjb-indent-meat) find-file-hook)
;;               (list (function pjb-indent-meat))))

;; (setf find-file-hook  (remove (function pjb-indent-meat) find-file-hook))


;;;----------------------------------------------------------------------------
(.EMACS "vc")
(require 'vc-hooks)
(defadvice vc-registered (around vc-registered/bug-on-empty-string-filename
                                 first (file) activate)
  (unless (and (stringp file) (string= "" file))
    ad-do-it))

;;;----------------------------------------------------------------------------
(.EMACS "darcs")
(load "vc-darcs" t nil)

(defun jump-to-real-file-from-darcs ()
  (interactive)
  (let* ((f (buffer-file-name (current-buffer)))
         (match (string-match "_darcs/current" f)))
    (and f match
         (find-alternate-file
          (concat (substring f 0 (match-beginning 0))
                  (substring f (match-end 0)))))))

(defun warn-if-darcs-file ()
  (let ((f (buffer-file-name (current-buffer))))
    (and f (string-match "_darcs" f)
         (if (y-or-n-p "This is a _darcs file, open the real file? ")
             (jump-to-real-file-from-darcs)
             (push '(:propertize "_DARCS-FILE:" face font-lock-warning-face)
                   mode-line-buffer-identification)))))

(add-hook 'find-file-hooks 'warn-if-darcs-file)


;;;----------------------------------------------------------------------------
(.EMACS "balance windows")

(defun horizontal-offset ()
  "Number of columns taken by the fringe and vertical scroll bar"
  ;; TODO: Implement in function of the effective fringe and vertical scroll bar.
  5)

(defun pjb-balance-windows-vertically ()
  "Make all visible windows the same width (approximately)."
  (interactive)
  (let ((count -1) levels newsizes level-size
        (last-window (previous-window (frame-first-window (selected-frame))))
        ;; Don't count the columns that are past the lowest main window.
        total)
    ;; Rightmost edge of last window determines what size we have to work with.
    (setq total
          (+ (window-width last-window) (horizontal-offset)
             (nth 0 (window-edges last-window))))
    ;; Find all the different hpos's at which windows start,
    ;; then count them.  But ignore levels that differ by only 1.
    (let (lefts (prev-left -2))
      (walk-windows (function (lambda (w)
                      (setq lefts (cons (nth 0 (window-edges w))
                                        lefts))))
                    'nomini)
      (setq lefts (sort lefts '<))
      (while lefts
        (if (> (car lefts) (1+ prev-left))
            (setq prev-left (car lefts)
                  count (1+ count)))
        (setq levels (cons (cons (car lefts) count) levels))
        (setq lefts (cdr lefts)))
      (setq count (1+ count)))
    ;; Subdivide the frame into desired number of vertical levels.
    (setq level-size (/ total count))
    (.EMACS "levels=%S" levels)
    (.EMACS "level-size=%S" level-size)
    (save-selected-window
      ;; Set up NEWSIZES to map windows to their desired sizes.
      ;; If a window ends at the rightmost level, don't include
      ;; it in NEWSIZES.  Those windows get the right sizes
      ;; by adjusting the ones above them.
      (walk-windows (function
                     (lambda (w)
                      (let ((newleft (cdr (assq (nth 0 (window-edges w))
                                                levels)))
                            (newright (cdr (assq (+ (window-width w)
                                                    (horizontal-offset)
                                                    (nth 0 (window-edges w)))
                                                 levels))))
                        (message ".EMACS: newleft=%S newright=%S"
                                 newleft newright)
                        (if newright
                            (setq newsizes
                                  (cons (cons w (* level-size
                                                   (- newright newleft)))
                                        newsizes))))))
                    'nomini)
      (.EMACS "newsizes=%S" newsizes)
      ;; Make walk-windows start with the leftmost window.
      (select-window (previous-window (frame-first-window (selected-frame))))
      (let (done (count 0))
        ;; Give each window its precomputed size, or at least try.
        ;; Keep trying until they all get the intended sizes,
        ;; but not more than 3 times (to prevent infinite loop).
        (while (and (not done) (< count 3))
          (setq done t)
          (setq count (1+ count))
          (walk-windows (function (lambda (w)
                          (select-window w)
                          (let ((newsize (cdr (assq w newsizes))))
                            (when newsize
                              (apply (function enlarge-window)
                                     (- newsize
                                        (horizontal-offset)
                                        (window-width))
                                     t
                                     (if (= 2 (cdr (function-argument-counts
                                                    (function enlarge-window))))
                                         '()
                                         '(preserve)))
                              (unless (= (window-width)
                                         (- newsize (horizontal-offset)))
                                (setq done nil))))))
                        'nomini))))))



(defun pjb-balance-windows (&optional horizontally)
  "Make all visible windows on the current frame the same size (approximately).
If optional prefix arg is not given, \"same size\" is same height.
When prefix arg is given,  \"same size\" is same width."
  (interactive "P")
  (let* (count size w cmjr resize
               (edge (if horizontally 0 1)) ;; Minor field to sort by 0=LEFT, 1=TOP
               (mjr (- 1 edge))             ;; Major field to sort
               (far (+ 2 edge)) ;; far edge (right/bottom) - for current size
               (windows nil)    ;; list of windows
               (ix 0)
               nwin                   ;; number of windows
               (curw (selected-window)) ;; selected window (to return to)
               )
    ;; Build and sort list of all windows on frame
    (save-window-excursion
      (walk-windows (function (lambda (w)
                      (let ((ltrb (window-edges w)))
                        (setq windows (cons (list
                                             (nth mjr  ltrb)
                                             (nth edge ltrb)
                                             (nth far  ltrb)
                                             w) windows)))))
                    'nomini)
      (setq windows (sort windows (lambda (e1 e2)
                                    (if (< (nth 0 e1) (nth 0 e2))
                                        t
                                        (if (= (nth 0 e1) (nth 0 e2))
                                            (if (< (nth 1 e1) (nth 1 e2))
                                                t)))))))
    (setq nwin (length windows))
    ;; add 1 extra entry (for while check)
    (appendf windows '((-1 -1 -1 nil)))

    (while (< ix nwin)                  ; walk on all (sorted) windows
      (setq count ix)         ; count the windows in 1 column (or row)
      (setq cmjr (car (nth ix windows))) ; column / raw identification
      (while (= cmjr (car (nth ix windows)))   ; same column / row
        (setq ix (1+ ix)))                     ; next window
      (setq count (- ix count))
      (if (/= count 1) ; do only if more than one window in this column/row
          (let ((gix (- ix count)))
            (setq size (- (nth far (window-edges (nth 3 (nth (1- ix) windows))))
                          (nth edge (window-edges
                                     (nth 3 (nth (- ix count) windows))))))
            (setq size (/ (+ size count -1) count)) ; average window size

            ;; (.EMACS "Size=%d" size)

            (while (< gix ix)
              (setq w (nth 3 (nth gix windows)))
              (setq resize (- size (- (nth far (window-edges w))
                                      (nth edge (window-edges w)))))

              ;; (.EMACS "Window=%s  resize=%d" w resize)
                                        ; don't resize by 1 character/line
              (if (or (> resize 1)
                      (< resize -1))
                  (progn

                    ;; (sit-for 2)

                    (select-window w)   ; window to work on
                    (apply (function enlarge-window)
                           resize horizontally
                           (if (= 2 (cdr (function-argument-counts
                                          (function enlarge-window))))
                               '()
                               '(preserve)))
                    ;; (sit-for 2)
                    ))
              (setq gix (1+ gix))))))

    ;; (.EMACS "")
    (select-window curw)))


(defun align-cols (start end max-cols)
  "Align text between point and mark as columns.
Columns are separated by whitespace characters.
Prefix arg means align that many columns. (default is all)
Attribution: ?"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
          pos
          end-of-line
          word
          count
          (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
          (pos-list nil)
          (ref-list nil))
      ;; find the positions
      (goto-char start)
      (while (< p end)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (setq pos (current-column))     ;start of first word
        (if (null (car ref-list))
            (setq pos-list (list pos))
            (setq pos-list (list (max pos (car ref-list))))
            (setq ref-list (cdr ref-list)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq word (- (current-column) pos))
          ;; length of next word including following whitespaces
          (setq pos (current-column))
          (if (null (car ref-list))
              (setq pos-list (cons word pos-list))
              (setq pos-list (cons (max word (car ref-list)) pos-list))
              (setq ref-list (cdr ref-list))))
        (while ref-list
          (setq pos-list (cons (car ref-list) pos-list))
          (setq ref-list (cdr ref-list)))
        (setq ref-list (nreverse pos-list))
        (forward-line)
        (setq p (point)))
      ;; align the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start
            (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (goto-char (match-end 0))
        (setq pos (nth count pos-list))
        (while (< (current-column) pos)
          (insert-char ?\040 1))
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq pos   (+  pos (nth count pos-list)))
          (goto-char (match-end 0))
          (while (< (current-column) pos)
            (insert-char ?\040 1))
          (setq end-of-line (save-excursion (end-of-line) (point))))
        (forward-line -1)
        (if (= p (point-min)) (setq p (1- p))
            (setq p (point))))))) ;;align-cols



;;;----------------------------------------------------------------------------
(defun remove-meat-from-all-hook (meat)
  (let ((c 0))
    (do-symbols (s)
      (when (and (boundp s)
                 (listp (symbol-value s))
                 (< 5 (length (symbol-name s)))
                 (string= (subseq (symbol-name s) (- (length (symbol-name s)) 5))
                          "-hook"))
        (set s (remove meat (symbol-value s)))
        (incf c)))
    c))
(remove-meat-from-all-hook 'semantic-default-elisp-setup)
(remove-meat-from-all-hook 'semantic-default-c-setup)
(remove-meat-from-all-hook 'semantic-make)

;;;----------------------------------------------------------------------------
;; (load "/opt/smalltalk-3.0.4/share/emacs/site-lisp/gst-mode.el") 
;; (load "/opt/smalltalk-3.0.4/share/emacs/site-lisp/smalltalk-mode.el") 

;;;----------------------------------------------------------------------------
(defvar *compile-and-run-cflags*
  (let ((prefix "."))
    (format  "-I%s -L%s" prefix prefix)))

(defun compile-and-run (mode)
  (interactive "p")
  (flet ((name (path)
           (when (string-match "^.*/\\([^./]*\\)\\.[^/.]*$" path)
             (match-string 1 path)))
         (type (path)
           (when (string-match "^.*/[^./]*\\.\\([^/.]*\\)$" path)
             (match-string 1 path))))
    (let* ((src (buffer-file-name (current-buffer)))
           (compiler (or (cdr (assoc* (type src)
                                      '(("c++" . "g++")
                                        ("cpp" . "g++")
                                        ("cxx" . "g++")
                                        ("C" . "g++"))
                                      :test (function string=)))
                         "gcc")))
      (message "src=%S" src)
      (message "exe=%S"  (name src))
      (compile
       (format "SRC=%S ; EXE=%S ; %s %s -g3 -ggdb3 -o ${EXE} ${SRC} && %s ./${EXE} && echo status = $?"
               src (name src) compiler *compile-and-run-cflags*
               (case mode
                 ((4) "valgrind")
                 (otherwise "")))))))

;;;----------------------------------------------------------------------------
(when (require 'psql-mode nil t)
  (modify-syntax-entry ?/   "<14>" psql-mode-syntax-table)
  (modify-syntax-entry ?*   "<23>" psql-mode-syntax-table)
  
  (modify-syntax-entry ?-   "<12"  psql-mode-syntax-table)
  (modify-syntax-entry ?\n  ">"    psql-mode-syntax-table))

;; A little patch:
(require 'canlock)
(defun canlock-sha1 (message)
  "Make a SHA-1 digest of MESSAGE as a unibyte string of length 20 bytes."
  (let (sha1-maximum-internal-length)
    (sha1 message nil nil )))


(defun alert ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*compilation*"))

(setf (getenv "ANT_ARGS") "")



;; MacOSX / Xcode

(when (require 'gnuserv-compat nil t)
  (autoload 'gnuserv-start "gnuserv-compat"
    "Allow this Emacs process to be a server for client processes." t)
  (gnuserv-start))

(when (eq system-type 'darwin)
  ;; (shell-command "defaults write com.apple.Xcode PBXEmacsPath /opt/local/bin/emacsclient")
  (shell-command "defaults write com.apple.Xcode PBXEmacsPath /usr/bin/true")
  (defun bh-compile ()
    (interactive)
    (let ((df (directory-files "."))
          (has-proj-file nil))
      (while (and df (not has-proj-file))
        (let ((fn (car df)))
          (if (> (length fn) 10)
              (if (string-equal (substring fn -10) ".xcodeproj")
                  (setq has-proj-file t))))
        (setq df (cdr df)))
      (if has-proj-file
          (compile "xcodebuild -configuration Debug")
          (compile "make")))))

(appendf auto-mode-alist '(("\\.m$"  . objc-mode)
                           ("\\.mm$" . objc-mode)))


(setf grep-find-command "find $HOME/src/manager2/trunk \\( -name release -prune \\) -o -type f  \\(  -name \\*.h -o -name \\*.c -name \\*.hh -o -name \\*.hxx -o -name \\*.cc  -o -name \\*.cxx -o -name \\*.lisp -o -name \\*.rb -o -name \\*.logs \\) -print0 | xargs -0 -e grep -niH -e "
      grep-host-defaults-alist nil)
(setf grep-find-command "find $HOME/firms/medicalis/src/amd/subprojects/incident-tracker/sources/siam \\( \\( -name release -o -name .git \\) -prune \\) -o -type f  \\( -name \\*.php -o -name \\*.inc -o -name \\*.txt \\) -print0 | xargs -0  grep -niH -e "
      grep-host-defaults-alist nil)
(setf grep-find-command "find . \\( \\( -name release -o -name .git \\) -prune \\) -o -type f  -print0 | xargs -0  grep -niH -e "
      grep-host-defaults-alist nil)


(defun next-day (date)
  "Returns the next day.
DATE: (YYYY MM DD [DOW])
RETURN: (YYYY MM DD DOW)  next day."
  (destructuring-bind (y m d &rest ignored) date
    (declare (ignore ignored))
    (let ((next-cursor-day (calendar-gregorian-from-absolute
                            (1+ (calendar-absolute-from-gregorian  (list m d y))))))
      (destructuring-bind (m d y) next-cursor-day
        
        (list y m d (aref [Do Lu Ma Mi Ju Vi Sa]
                          (calendar-day-of-week next-cursor-day)))))))

(defun* insert-calendar (start-date &optional (count 30))
  "Inserts a calendar from start-date up to count days."
  (destructuring-bind (year month day &rest ignored) start-date
    (declare (ignore ignored))
    (let ((date (next-day (list year month (1- day)))))
      (destructuring-bind (year month day dow) date
        (insert (format "%04d-%02d-%02d %2s : \n" year month day dow))
        (when (eq 'Do dow) (insert "\n"))
        (when (plusp count)
          (insert-calendar (next-day date) (1- count)))))))



;;;----------------------------------------------------------------------------
(defvar *from-regexp*
  "^From [^ ]*@[^ ]*  \\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) [ 0-3][0-9] [ 0-2][0-9]:[0-5][0-9]:[0-5][0-9] [0-9][0-9][0-9][0-9]$")


(defun sfn (&optional hostnamep)
  "Set the name of all frames as \"EMACS\" with a prefix,
or as \"emacs at <hostname>\"."
  (interactive "P")
  (let ((c   (selected-frame))
        (use (or (getenv "EMACS_USE") "emacs")))
    (unwind-protect
         (dolist (f (frame-list))
           (select-frame f)
           (set-frame-name (if hostnamep
                               (format "%6s at %s" use *hostname*)
                               (string-upcase use))))
      (select-frame c))))

;; (setf (getenv "EMACS_USE") "erc")
;; (setf (getenv "EMACS_USE") "gnus")
;; (setf (getenv "EMACS_USE") "pgm")


(cond
  ((string= (getenv "EMACS_USE") "erc")
   (when (fboundp 'set-palette) (set-palette pal-dark-blue))
   (set-frame-name "ERC")
   (erc-select))
  ((string= (getenv "EMACS_USE") "gnus")
   (when (fboundp 'set-palette) (set-palette pal-dark-amber))
   (gnus))
  (t
   (when (fboundp 'set-palette) (set-palette pal-green))))

(sfn t)

(defun current-minor-modes (&optional buffer)
  "The list of the minor modes currently active in the buffer (or current buffer)."
  (let ((result '()))
    (with-current-buffer (or buffer (current-buffer))
      (dolist (mode minor-mode-list result)
        (when (and (boundp mode) (symbol-value mode))
          (push mode result))))))



;;;----------------------------------------------------------------------------
(.EMACS "epilogue")
(milliways-activate) (.EMACS "milliways activated!")
(.EMACS "DONE")

;; (setf inhibit-splash-screen t)
;; (switch-to-buffer (get-buffer-create "emtpy"))
;; (delete-other-windows)


;; Local Variables:
;; eval: (cl-indent 'string-case 1)
;; End
;;;; THE END ;;;;
