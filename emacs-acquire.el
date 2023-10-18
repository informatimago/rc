;;; Nothing yet.


(defun pjb-wrap-latex-block (start end)
  (interactive "r")
  (goto-char end)
  (end-of-line)
  (let ((end (point)))
    (beginning-of-line)
    (when (< (point) end)
      (end-of-line)
      (insert "\n")))
  (insert "#+LATEX: }\n")
  (goto-char start)
  (beginning-of-line)
  (insert "#+LATEX: {\\small\n"))
