(require 'erc)
(require 'cl)

(defparameter *pjb-autojoin-channels-alist*
  '(("freenode.org"

     "##Freelancers" "##binutils" "##cinema" "##coders" "##coding" "##france"
     "##lisp" "##lispm"
     "##sci-fi" "##smalltalk" "##tesla" "##teslamotors" "##teslamotors-staging"
     "##trains" "##ufo" "##workingset"

     "#abcl" "#bourguinux" "#ccl" "#cl" "#cl-bodge" "#clcs" "#clim" "#clschool"
     "#clus" "#cobol" "#cpp" "#cuda" "#ecl" "#emacs" "#emacsconf"
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

(defun pjb-join-channels ()
  (interactive)
  (with-current-buffer "irc.freenode.org:6667"
    (mapcar 'erc-join-channel
            (cdr (assoc "freenode.org" *pjb-autojoin-channels-alist*)))))


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


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
