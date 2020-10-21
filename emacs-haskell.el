(require 'haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; (add-hook 'haskell-mode-hook 'haskell-indent-mode)
;; (add-hook 'haskell-mode-hook 'haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


(require 'lsp)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-haskell)


(setf lsp-haskell-server-path "/opt/haskell-language-server/bin/haskell-language-server-wrapper")
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
(add-hook 'haskell-mode-hook 'lsp)


(push '(ghc "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): error:"  1 2 3 nil nil)
      compilation-error-regexp-alist-alist)

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
