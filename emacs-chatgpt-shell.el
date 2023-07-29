
;; git@github.com:xenodium/chatgpt-shell.git
(pushnew (expand-file-name "~/emacs/chatgpt-shell") load-path)

(require 'chatgpt-shell) 
(require 'ob-chatgpt-shell)
(require 'dall-e-shell)
(require 'ob-dall-e-shell)


;; in ~/.authinfo:
;; machine api.openai.com login your@email password sk-....................

(setf chatgpt-shell-api-url-base "https://localhost:9443")
(setf chatgpt-shell-api-url-base "https://api.opeanai.com")

(setq chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))
(setq dall-e-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))

(ob-chatgpt-shell-setup)
(ob-dall-e-shell-setup)

