
(defun mode-line--kill-buffer (event)
  (interactive "e")
  (kill-buffer))


(defvar mode-line-kill-buffer
  '("["
    (:propertize ("Kill")
     mouse-face mode-line-highlight
     help-echo "mouse-1: Kill the current buffer"
     local-map (keymap
                (mode-line keymap
                           (down-mouse-1 . mode-line--kill-buffer))))

    "]"
    " "))


(set-default 'mode-line-format (append mode-line-format mode-line-kill-buffer))
