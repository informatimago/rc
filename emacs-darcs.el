(error "Obsolete")

;;;----------------------------------------------------------------------------
(.EMACS "darcs")
(load "vc-darcs" t nil)


(defun jump-to-real-file-from-darcs ()
  (interactive)
  (let* ((f (buffer-file-name (current-buffer)))
         (match (string-match "_darcs/current" f)))
    (and f match
         (find-alternate-file
          (concat (substring f 0 (match-beginning 0))
                  (substring f (match-end 0)))))))

(defun warn-if-darcs-file ()
  (let ((f (buffer-file-name (current-buffer))))
    (and f (string-match "_darcs" f)
         (if (y-or-n-p "This is a _darcs file, open the real file? ")
             (jump-to-real-file-from-darcs)
             (push '(:propertize "_DARCS-FILE:" face font-lock-warning-face)
                   mode-line-buffer-identification)))))

(add-hook 'find-file-hooks 'warn-if-darcs-file)


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
