(require 'cc-styles)
(require 'cc-defs)
(require 'cc-vars)
(require 'pjb-cl)

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos     c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun linux-c-mode-common-meat ()
  ;; Add kernel style
  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist (arglist-cont-nonempty
                               c-lineup-gcc-asm-reg
                               c-lineup-arglist-tabs-only)))))

(add-hook 'c-mode-common-hook 'linux-c-mode-common-meat)

(defparameter *pjb-force-linux-tabulation*
  '("/pjb/works/qorvo/")
  "A list of path regexps to include.")

(defun pjb-insert-newline-command (repeat)
  (interactive "p")
  (loop repeat repeat do (insert "\n")))

(defun pjb-find-file-meat/force-linux-tabulation ()
  "Meat for find-file-hook: force linux tabulation; no indent."
  (let ((file-name (buffer-file-name)))
    (when (and file-name
               (find-if (lambda (re) (string-match re file-name))
                        *pjb-force-linux-tabulation*))
	  (local-set-key (kbd "TAB") 'self-insert-command)
      (local-set-key (kbd "RET") 'pjb-insert-newline-command))))

(add-hook 'find-file-hook 'pjb-find-file-meat/force-linux-tabulation)


(defvar *pjb-c-mode-meat-blacklist* '())
(pushnew (format "^%s" (expand-file-name "~/works/qorvo/"))
         *pjb-c-mode-meat-blacklist*
         :test (function equal))

(defun linux-c-mode-meat ()
  (let ((filename (buffer-file-name)))
    (message "linux-c-mode-meat filename %S" filename)
    ;; Enable kernel mode for the appropriate files
    (message "linux-c-mode-meat match %S" (and filename (string-match (expand-file-name "~/works/qorvo/") filename)))
    (when (and filename (string-match (expand-file-name "~/works/qorvo/") filename))
      (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
      (c-set-style "linux-tabs-only")
      (setq tab-width 8)
      (setq indent-tabs-mode t)
      (setq show-trailing-whitespace t)
      ;; (ggtags-mode 1)
      (c-set-style "linux-tabs-only"))))

(add-hook 'c-mode-hook   'linux-c-mode-meat)

(defun linux-asm-mode-meat ()
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (when (and filename (string-match (expand-file-name "~/works/qorvo/") filename))
      (setq tab-width 8)
      (setq indent-tabs-mode t)
      (setq show-trailing-whitespace t))))

(add-hook 'asm-mode-hook   'linux-asm-mode-meat)

