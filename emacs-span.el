(message "Hi SPAN!")
;; (load "~/src/public/emacs/freerdp-c-style.el")
(load "~/rc/emacs-pjb.el")
(ignore-errors (progn (setf *pjb-current-font-index* 4) (set-current-font)))
(setf *pjb-intervention-firm* '((minint)))

(defun turn-off-mouse (&optional frame)
  (interactive)
  (shell-command "xinput disable \"ImPS/2 Generic Wheel Mouse\""))

(defun turn-on-mouse (&optional frame)
  (interactive)
  (shell-command "xinput enable \"ImPS/2 Generic Wheel Mouse\""))

(add-hook 'focus-in-hook 'turn-off-mouse)
(add-hook 'focus-out-hook 'turn-on-mouse)
(add-hook 'delete-frame-functions 'turn-on-mouse)
(setf browse-url-browser-function 'browse-url-firefox2)

(global-set-key (kbd "s-s")   'git-search-symbol-at-point)
(global-set-key (kbd "C-s-s") 'git-search)
(global-set-key (kbd "M-s-s") 'git-search-region)


