;;;----------------------------------------------------------------------------
;;; PAREDIT: essential!
;;;----------------------------------------------------------------------------
(require 'paredit)

(defvar pjb-paredit-space-for-delimiter-predicates '()
  "A list of predicates taking a buffer start end range indicating
whether no space should be inserted at the end before an opening
parenthesis.
The disjonction of all predicates is used.")

(defun pjb-dispatching-reader-macros-p (start end)
  "Whether there is a dispatching reader macro instance from `start' to `end'."
  (message "previous: %S" (buffer-substring-no-properties start end))
  (goto-char start)
  (and (looking-at "\\(#[0-9]*[^0-9]\\)")
       (= end (match-end 0))))

(defun pjb-comma-at-p (start end)
  "Whether there is ` ,@' just before `end'."
  (message "previous: %S" (buffer-substring-no-properties start end))
  (when (<= (point-min) (- end 3))
    (goto-char (- end 3))
    (looking-at " ,@")))

(defun pjb-at-p (start end)
  "Whether there is `@' just before `end'.
Useful for Objective-CL reader macros."
  (message "previous: %S" (buffer-substring-no-properties start end))
  (when (<= (point-min) (- end 3))
    (goto-char (- end 1))
    (looking-at "@")))

(defun pjb-colon-p (start end)
  "Whether there is `:' just before `end'.
Useful for Objective-CL reader macros."
  (message "previous: %S" (buffer-substring-no-properties start end))
  (when (<= (point-min) (- end 3))
    (goto-char (- end 1))
    (looking-at ":")))


(defun pjb-paredit-space-for-delimiter-p/predicates (endp delimiter)
  (not (and (not endp)
            (save-excursion
             (let ((end (point))
                   (start (progn (backward-sexp) (point))))
               (some (lambda (predicate)
                       (funcall predicate start end))
                     pjb-paredit-space-for-delimiter-predicates)))
            t)))

(push 'pjb-dispatching-reader-macros-p          pjb-paredit-space-for-delimiter-predicates)
(push 'pjb-comma-at-p                           pjb-paredit-space-for-delimiter-predicates)
(push 'pjb-at-p                                 pjb-paredit-space-for-delimiter-predicates)
(push 'pjb-colon-p                              pjb-paredit-space-for-delimiter-predicates)
(push 'pjb-paredit-space-for-delimiter-p/predicates paredit-space-for-delimiter-predicates)
;; (setf  paredit-space-for-delimiter-predicates '(pjb-paredit-space-for-delimiter-p/predicates))

;; (defun bagger-lambda-p (start end)
;;   (goto-char start)
;;   (and (looking-at "λ")
;;        (= end (match-end 0))))
;; (push 'bagger-lambda-p pjb-paredit-space-for-delimiter-predicates)



