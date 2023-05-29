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


(defvar *pjb-force-linux-tabulation* '()
  "A list of path regexps to include, or (include \"regexp\") or (exclulde \"regexp\")")
(pushnew '(exclude "/src/public/")    *pjb-force-linux-tabulation* :test (function equal))
(pushnew '(exclude "/src/c-tidbits/") *pjb-force-linux-tabulation* :test (function equal))
(pushnew '(include "/src/")           *pjb-force-linux-tabulation* :test (function equal))
(pushnew '(include "^/build/")        *pjb-force-linux-tabulation* :test (function equal))

(defun pjb-force-linux-tabulation-file-p (filename)
  (let ((excludes (remove-if-not (lambda (re) (and (consp re) (eq 'exclude (first re))))
                                 *pjb-force-linux-tabulation*))
        (includes (remove-if (lambda (re) (and (consp re) (eq 'exclude (first re))))
                             *pjb-force-linux-tabulation*)))
    ;; (message "excludes %S %S" excludes
    ;;          (every (lambda (re)
    ;;                   (not (string-match (second re) filename)))
    ;;                 excludes))
    ;; (message "includes %S %S" includes
    ;;          (some (lambda (re)
    ;;                  (if (stringp re)
    ;;                      (string-match re filename)
    ;;                      (string-match (second re) filename)))
    ;;                includes))
    (and filename
         (every (lambda (re)
                  (not (string-match (second re) filename)))
                excludes)
         (some (lambda (re)
                 (if (stringp re)
                     (string-match re filename)
                     (string-match (second re) filename)))
               includes)
         t)))

(defun pjb-insert-newline-command (repeat)
  (interactive "p")
  (loop repeat repeat do (insert "\n")))

(defun pjb-find-file-meat/force-linux-tabulation ()
  "Meat for find-file-hook: force linux tabulation; no indent."
  (when (pjb-force-linux-tabulation-file-p (buffer-file-name))
	(local-set-key (kbd "TAB") 'self-insert-command)
    (local-set-key (kbd "RET") 'pjb-insert-newline-command)))

(add-hook 'find-file-hook 'pjb-find-file-meat/force-linux-tabulation)

(defun linux-c-mode-meat ()
  (message "linux-c-mode-meat %S %s" (buffer-file-name)
           (if (pjb-force-linux-tabulation-file-p (buffer-file-name)) "yes" "nope"))
  ;; Enable kernel mode for the appropriate files
  (when (pjb-force-linux-tabulation-file-p (buffer-file-name))
    (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
    (c-set-style "linux-tabs-only")
    (setq tab-width 8)
    (setq indent-tabs-mode t)
    (setq show-trailing-whitespace t)
    ;; (ggtags-mode 1)
    (c-set-style "linux-tabs-only")))

(add-hook 'c-mode-hook   'linux-c-mode-meat)

(defun linux-asm-mode-meat ()
  ;; Enable kernel mode for the appropriate files
  (when (pjb-force-linux-tabulation-file-p (buffer-file-name))
    (setq tab-width 8)
    (setq indent-tabs-mode t)
    (setq show-trailing-whitespace t)))

(add-hook 'asm-mode-hook   'linux-asm-mode-meat)

