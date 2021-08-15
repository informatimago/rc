(require 'erc)
(require 'erc-join)
(require 'cl)
(require 'pjb-cl)
(require 'tls)

(defparameter *pjb-autojoin-channels-alist*
  '(

    ("irc.libera.chat"
     "##kernel" "#linux-kernel"

     "#lisp"
     "#commonlisp"
     "##lispm"

     "#clschool"  "#lisp-es"
     "#abcl" "#ccl" "#ecl" "#sicl"
     "#clim" "#clcs" "#slime"
     "#lispcafe"  "#lispweb" "#lispgames"
     "#common-lisp.net" "#nyxt"
     "#cl-naive"

     "#clergo"
     
     ;; "#emacs"
     "#emacs-beginners" "#org-mode" "#erc" "#gnus"

     "#hn" "#space"
     "#emacsfr-off" "##coding"

     "#linaro" "#linaro-tcwg" "#linaro-qa" "#linaro-watercooler" "#linaro-kernel")

    ("freenode.org"

     "##Freelancers" "##binutils" "##cinema" "##coders" "##coding" "##france"
     "#lisp" "##lisp" "##lispm"
     "##sci-fi" "##smalltalk" "##tesla" "##teslamotors" "##teslamotors-staging"
     "##trains" "##ufo" "##workingset"

     "#abcl" "#bourguinux" "#ccl" "#cl" "#cl-bodge" "#clcs" "#clim" "#clschool"
     "#clus" "#cobol" "#cpp" "#cuda" "#ecl" "#emacs" "#emacsconf"
     "#emacsfr-off"
     ;; "#emacsfr"
     ;; "#ghc" "#ghc-mod" "#hackage"
     "#haskell"
     ;; "#haskell-apple" "#haskell-beginners"
     ;; "#haskell-blah" "#haskell-emacs" "#haskell-embedded"
     "#haskell-fr"
     ;; "#haskell-lang" "#haskell-overflow" "#haskell-stack" "#haskell.nix"

     "#hn"
     "#iphonedev" "#iphonedev-chat"

     "#lisp-es" "#lispcafe" "#lispgames"  "#lispweb" "#lispx"
     ;; "#lispm"
     "#macintosh" "#macos9lives"
     "#qlisp" "#sicl" "#sicp" "#slime" "#space"
     "#swift-lang" "#synthesizers" "#tesla")



    ("disabled.freenode.org" "#macports")

    ("esper.net" "#SpaceX")
    ("irc.sbde.fr" "#laboite" "#sbde")
    ("irc.oftc.net" "#uml")))

(setf erc-autojoin-channels-alist *pjb-autojoin-channels-alist*)

(defun pjb-join-channels ()
  (interactive)
  ;; (when (buffer-named "irc.freenode.org:6667")
  ;;   (with-current-buffer "irc.freenode.org:6667"
  ;;     (mapcar 'erc-join-channel
  ;;             (cdr (assoc "freenode.org" *pjb-autojoin-channels-alist*)))))
  (let ((irc (buffer-named "irc.libera.chat:6697")))
   (when irc
       (with-current-buffer irc
         (mapcar 'erc-join-channel
                 (cdr (assoc "irc.libera.chat" *pjb-autojoin-channels-alist*)))))))


(defvar *erc-cmd-BR-line* (make-string 72 ?=))

(defun erc-cmd-BR (&optional filler)
  "Inserts a line."
  (let ((inhibit-read-only t)
        (line (cond
                ((characterp filler) (make-string 72 filler))
                ((stringp filler)    (with-output-to-string
                                         (loop repeat (ceiling 72 (length filler))
                                               do (write-string filler))))
                (t *erc-cmd-BR-line*))))
    (erc-display-line (erc-make-notice line) 'active))
  t)

(put 'erc-cmd-BR 'process-not-needed t)

(defun pjb-erc-clear-all-buffers  ()
  (interactive)
  (dolist (buf (erc-buffer-list))
    (with-current-buffer buf
      (erc-cmd-CLEAR)
      (bury-buffer))))


(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697
           :nick "pjb" :full-name "Pascal J. Bourguignon")
  ;; (erc     :server "irc.freenode.net" :port 6667
  ;;          :nick "pjb" :full-name "Pascal J. Bourguignon")
  )

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
