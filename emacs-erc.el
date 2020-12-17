(require 'erc)

(defparameter *pjb-autojoin-channels-alist*
  '(("freenode.org" "#lispweb" "##Freelancers" "##binutils"
     "##cinema" "##coders" "##coding" "##france" "##lisp" "##lispm"
     "##sci-fi" "##smalltalk" "##tesla" "##teslamotors"
     "##teslamotors-staging" "##trains" "##ufo" "##workingset" "#abcl"
     "#bourguinux" "#ccl" "#cl" "#cl-bodge" "#clcs" "#clim" "#clschool"
     "#clus" "#cobol" "#cpp" "#cuda" "#ecl" "#emacs" "#emacsfr" "#emacsconf"
     "#ghc" "#ghc-mod" "#hackage" "#haskell" "#haskell-apple"
     "#haskell-beginners" "#haskell-blah" "#haskell-emacs"
     "#haskell-embedded" "#haskell-fr" "#haskell-lang"
     "#haskell-overflow" "#haskell-stack" "#haskell.nix" "#hn"
     "#iphonedev" "#iphonedev-chat" "#lisp-es" "#lispcafe" "#lispgames"
     "#lispm" "#macintosh" "#macos9lives" "#qlisp" "#sicl" "#sicp"
     "#slime" "#space" "#swift-lang" "#synthesizers" "#tesla")

    ("disabled.freenode.org" "#macports")

    ("esper.net" "#SpaceX")
    ("irc.sbde.fr" "#laboite" "#sbde")
    ("irc.oftc.net" "#uml")))

(defun pjb-join-channels ()
  (interactive)
  (with-current-buffer "irc.freenode.org:6667"
    (mapcar 'erc-join-channel
            (cdr (assoc "freenode.org" *pjb-autojoin-channels-alist*)))))


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
