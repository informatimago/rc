(defun goto-asdf-form ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "^(\\(defsystem\\|asdf:defsystem\\) ")
    (goto-char (match-beginning 0))))

(defun goto-asdf-version ()
  (interactive)
  (goto-asdf-form)
  (forward-char)
  (forward-sexp) (backward-sexp)
  (while (not (looking-at ":version"))
    (forward-sexp) (forward-sexp) (backward-sexp))
  (forward-sexp) (forward-sexp) (backward-sexp))

(defun next-date-version (version)
  "version is a list of 3 or 4 integers,
representing (YEAR MONTH DAY [INCREMENT]
If it's the current date, then the increment is added or augmented,
otherwise the current date is returned."
  (let ((date )))
)

(defun bump-asdf-version ()
  (goto-asdf-version)
  (unless (looking-at "\"\\(\\([0-9]+\\.\\)*[0-9]+\\)\"")
    (error "Not a valid ASDF version"))
  (let* ((start   (match-beginning 1))
         (end     (match-end 1))
         (version (mapcar (lambda (item)
                            (car (read-from-string item)))
                          (split-string (buffer-substring start end) ".")))
         (new-version (cond
                          ((and (<= 3 (length version) 4)
                                (< 1990 (first version) 2100)
                                (<= 1 (second version) 12)
                                (<= 1 (third version) 31))
                           (next-date-version version))
                          (t
                           (next-usual-version version)))))
    (delete-region start end)
    (goto-char start)
    (insert (mapconcat (function prin1-to-string) new-version "."))))
