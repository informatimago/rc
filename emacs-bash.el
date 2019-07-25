;;; emacs-bash -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(defun python-shell-send-string-no-output (string &optional process)
  "Send STRING to PROCESS and inhibit output.
Return the output."
  (let ((process (or process (python-shell-get-or-create-process)))
        (comint-preoutput-filter-functions
         '(python-shell-output-filter))
        (python-shell-output-filter-in-progress t)
        (inhibit-quit t))
    (or
     (with-local-quit
       (python-shell-send-string string process)
       (while python-shell-output-filter-in-progress
         ;; `python-shell-output-filter' takes care of setting
         ;; `python-shell-output-filter-in-progress' to NIL after it
         ;; detects end of output.
         (accept-process-output process))
       (prog1
           python-shell-output-filter-buffer
         (setq python-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer process)
       (comint-interrupt-subjob)))))



(defun send-invisible (&optional prompt)
  "Read a string without echoing.
Then send it to the process running in the current buffer.
The string is sent using `comint-input-sender'.
Security bug: your string can still be temporarily recovered with
\\[view-lossage]; `clear-this-command-keys' can fix that."
  (interactive "P")			; Defeat snooping via C-x ESC ESC
  (let ((proc (get-buffer-process (current-buffer)))
	(prefix
	 (if (eq (window-buffer) (current-buffer))
	     ""
	   (format "(In buffer %s) "
		   (current-buffer)))))
    (if proc
	(let ((str (read-passwd (concat prefix
					(or prompt "Non-echoed text: ")))))
	  (if (stringp str)
	      (progn
		(comint-snapshot-last-prompt)
		(funcall comint-input-sender proc str))
	    (message "Warning: text will be echoed")))
      (error "Buffer %s has no process" (current-buffer)))))


(defun comint-delete-output ()
  "Delete all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	(replacement nil)
	(inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
			  (forward-line 0)
			  (point-marker))))
	(delete-region comint-last-input-end pmark)
	(goto-char (process-mark proc))
	(setq replacement (concat "*** output flushed ***\n"
				  (buffer-substring pmark (point))))
	(delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

;; Local Variables:
;; coding: utf-8
;; End Variables:
