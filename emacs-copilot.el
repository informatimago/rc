(add-to-list 'load-path "~/emacs/copilot")
(add-to-list 'load-path "~/emacs/s")
(add-to-list 'load-path "~/emacs/editorconfig")

(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

;; Customize variables:
;; copilot-enable-predicates
;; copilot-disable-predicates

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(push "~/opt/lib/nodejs/node-v16.16.0-linux-x64/bin" exec-path)

