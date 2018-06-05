(message "Hi SPAN!")
;; (load "~/src/public/emacs/freerdp-c-style.el")
(load "~/rc/emacs-pjb.el")
(ignore-errors (progn (setf *pjb-current-font-index* 4) (set-current-font)))
(setf *pjb-intervention-firm* '((minint)))

(defun mice ()
  (split-string
   (string-trim
    (shell-command-to-string  "xinput list  --name-only|grep -i mouse"))
   "\n"))

(defun touchpads ()
  (split-string
   (string-trim
    (shell-command-to-string  "xinput list  --name-only|grep -i touchpad"))
   "\n"))

(defun turn-off-trackpad (&optional frame)
  (interactive)
  (dolist (pad (touchpads))
    (shell-command (format "xinput disable %S" pad))))

(defun turn-on-trackpad (&optional frame)
  (interactive)
  (dolist (pad (touchpads))
    (shell-command (format "xinput enable %S" pad))))

(add-hook 'focus-in-hook          'turn-off-trackpad)
(add-hook 'focus-out-hook         'turn-on-trackpad)
(add-hook 'delete-frame-functions 'turn-on-trackpad)
(setf browse-url-browser-function 'browse-url-firefox2)

(global-set-key (kbd "s-s")   'git-search-symbol-at-point)
(global-set-key (kbd "C-s-s") 'git-search)
(global-set-key (kbd "M-s-s") 'git-search-region)

