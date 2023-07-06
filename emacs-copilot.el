(add-to-list 'load-path "~/emacs/copilot")            ;; git@github.com:zerolfx/copilot.el.git       
(add-to-list 'load-path "~/emacs/s")                  ;; git@github.com:magnars/s.el.git         
(add-to-list 'load-path "~/emacs/editorconfig-emacs") ;; git@github.com:editorconfig/editorconfig-emacs.git
;; (add-to-list 'load-path "~/emacs/editorconfig")       ;; git@github.com:editorconfig/editorconfig.git

(require 'copilot)

;; sudo port install nodejs16
;; (push "~/opt/lib/nodejs/node-v16.16.0-linux-x64/bin" exec-path)
(add-hook 'prog-mode-hook 'copilot-mode)

;; Customize variables:
;; copilot-enable-predicates
;; copilot-disable-predicates

(progn
  (define-key copilot-completion-map (kbd "<f12>") #'copilot-accept-completion-by-paragraph)
  (define-key global-map             (kbd "<f12>") #'copilot-accept-completion-by-paragraph)
  (define-key copilot-completion-map (kbd "C-<f12>") #'rk/copilot-quit)
  (define-key global-map             (kbd "C-<f12>") #'rk/copilot-quit)
  (define-key copilot-completion-map (kbd "M-<f12>") #'copilot-accept-completion)
  (define-key global-map             (kbd "M-<f12>") #'copilot-accept-completion))

;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; (define-key copilot-completion-map (kbd "<tab>") nil)
;; (define-key copilot-completion-map (kbd "TAB") nil)
;; (define-key copilot-mode-map (kbd "<tab>") nil)
;; (define-key copilot-mode-map (kbd "TAB") nil)
;; (define-key global-map (kbd "<tab>") nil)
;; (define-key global-map (kbd "TAB") nil)

;; (lookup-key copilot-completion-map (kbd "<tab>"))
;; (lookup-key copilot-completion-map (kbd "TAB"))
;; (lookup-key copilot-mode-map (kbd "<tab>"))
;; (lookup-key copilot-mode-map (kbd "TAB"))
;; (lookup-key global-map (kbd "<tab>"))
;; (lookup-key global-map (kbd "TAB"))



;;; From
;;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;;; with patches.

(defvar rk/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

 (defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (when (fboundp 'company--active-p) (company--active-p))))

(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)




(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "C-M-<escape>") #'rk/copilot-change-activation)



(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "C-M-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "C-M-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "C-M-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-M-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "C-M-<return>") #'rk/copilot-complete-or-accept)

;; (defun rk/copilot-tab ()
;;   "Tab command that will complet with copilot if a completion is
;; available. Otherwise will try company, yasnippet or normal
;; tab-indent."
;;   (interactive)
;;   (or (copilot-accept-completion)
;;       (minibuffer-complete)
;;       (when (fboundp 'company-yasnippet-or-completion)
;;         (company-yasnippet-or-completion))
;;       (indent-for-tab-command)))
;; 
;; (define-key copilot-mode-map (kbd "TAB") #'rk/copilot-tab)
;; (define-key copilot-mode-map (kbd "<tab>") #'rk/copilot-tab)
;; (define-key global-map (kbd "TAB") #'rk/copilot-tab)
;; (define-key global-map (kbd "<tab>") #'rk/copilot-tab)
;; (define-key global-map (kbd "<tab>") nil)


(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)
