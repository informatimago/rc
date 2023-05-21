(require 'shell)

(defun redbend--comint-output-filter--remove-esc-b (string)
  "Remove ESC(B sequences between `comint-last-output-start’ and `process-mark’."
  (save-excursion
   (goto-char comint-last-output-start)
   (let ((end (process-mark (get-buffer-process (current-buffer)))))
     (while (search-forward "\e(B" end t)
       (delete-region (match-beginning 0) (match-end 0))))))

(defun redbend--shell-mode-meat ()
  (set-variable 'tab-width 8)
  (setf comint-process-echoes nil)
  (add-to-list 'comint-output-filter-functions #'redbend--comint-output-filter--remove-esc-b)
  ;; (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (when (fboundp 'ansi-color-for-comint-mode-on) (ansi-color-for-comint-mode-on))
  (when (fboundp 'bash-completion-setup) (bash-completion-setup))
  (set-default 'shell-dirstack-query "pwd"))

(add-hook 'shell-mode-hook 'redbend--shell-mode-meat)

(setf org-agenda-files '("~/pjb/notes.org"))
