(add-to-invisibility-spec '(pjb-fold . t))   ; the . t gives the ellipsis

(defun pjb-fold-region (beg end)
  "Coalesce the region BEG..END into an ellipsis."
  (interactive "r")
  (let ((o (make-overlay beg end)))
    (overlay-put o 'invisible 'pjb-fold)
    (overlay-put o 'pjb-fold t)
    (overlay-put o 'evaporate t)))

(defun pjb-unfold-all ()
  "Reveal everything folded by `pjb-fold-region'."
  (interactive)
  (remove-overlays nil nil 'pjb-fold t))
