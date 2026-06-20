
;; git@github.com:xenodium/chatgpt-shell.git
(pushnew (home "emacs/chatgpt-shell") load-path)

(require 'shell-maker nil t)
(require 'chatgpt-shell nil t) 
(require 'ob-chatgpt-shell nil t)
(require 'dall-e-shell nil t)
(require 'ob-dall-e-shell nil t)


;; in ~/.authinfo:
;; machine api.openai.com login your@email password sk-....................

(setf chatgpt-shell-api-url-base "https://localhost:9443")
(setf chatgpt-shell-api-url-base "https://api.openai.com")

(setq chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com" :port "emacs"))

(setq dall-e-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com" :port "emacs"))

(when (fboundp 'ob-chatgpt-shell-setup)
   (ob-chatgpt-shell-setup))
(when (fboundp 'ob-dall-e-shell-setup)
  (ob-dall-e-shell-setup))


(auth-source-pick-first-password :host "fabrik.sncf.fr" :port "gitlab")
