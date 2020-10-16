
(require 'lsp)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-haskell)

(setq lsp-haskell-server-path "haskell-language-server-wrapper")
(setq lsp-log-io t)
(setq lsp-enable-snippet nil)

(add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
;;(add-hook 'haskell-mode-hook #'haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'lsp)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
