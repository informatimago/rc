
;; git@github.com:xenodium/chatgpt-shell.git
;; (push "~/emacs/chatgpt-shell" load-path)

(require 'chatgpt-shell) 
(require 'ob-chatgpt-shell)
(require 'dall-e-shell)
(require 'ob-dall-e-shell)


;; in ~/.authoinfo:
;; machine api.openai.com login your@email password sk-....................

(setq chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))
(setq dall-e-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))


(ob-chatgpt-shell-setup)
(ob-dall-e-shell-setup)
