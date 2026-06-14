;;; emacs-utils.el --- Reusable bits salvaged from old per-host configs  -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;
;;; Commentary:
;;;
;;; These functions and commands were gathered from the now-obsolete
;;; per-host configuration files (emacs-harman, emacs-trustonic,
;;; emacs-ubudu, emacs-dxo, emacs-span), which were retired in commit
;;; 6eea2db and removed in e56350a.  Only the generic, reusable bits
;;; were kept here; host-specific paths, hooks and keybindings were
;;; left behind (they remain in git history if ever needed).
;;;
;;; Light clean-up only: firm-specific prefixes dropped, duplicates
;;; merged.  Generalising/perfecting these is a later job.
;;;
;;; External dependencies still assumed (provided elsewhere in pjb's init):
;;;   - the `cl' compatibility aliases: subseq, coerce, loop, remove-if
;;;   - pjb utilities: set-palette, *current-palette*, with-files, with-file
;;;
;;; Code:

(require 'cl-lib)

;;;============================================================================
;;; Trackpad / pointing devices (Linux, xinput)         [from emacs-span]
;;;============================================================================
;;; Suggested hooks (left to the host config):
;;;   (add-hook 'focus-in-hook  'turn-off-trackpad)
;;;   (add-hook 'focus-out-hook 'turn-on-trackpad)

(defun mice ()
  "Return the list of xinput pointing devices that look like mice."
  (split-string
   (string-trim
    (shell-command-to-string "xinput list --name-only|grep -i mouse"))
   "\n"))

(defun touchpads ()
  "Return the list of xinput pointing devices that look like touchpads."
  (split-string
   (string-trim
    (shell-command-to-string "xinput list --name-only|grep -i -e 'touchpad\\|DLL07A0:01\\|VirtualBox USB Tablet'"))
   "\n"))

(defun turn-off-trackpad (&optional _frame)
  "Disable every xinput touchpad (see `touchpads')."
  (interactive)
  (dolist (pad (touchpads))
    (shell-command (format "xinput disable %S" pad))))

(defun turn-on-trackpad (&optional _frame)
  "Enable every xinput touchpad (see `touchpads')."
  (interactive)
  (dolist (pad (touchpads))
    (shell-command (format "xinput enable %S" pad))))

;;;============================================================================
;;; ASN.1 OCTET STRING <-> Emacs string                 [from emacs-span]
;;;============================================================================

(defun string-to-asn1-octet-string (string)
  "Encode STRING as an ASN.1 hex OCTET STRING literal ('....'H)."
  (format "'%s'H" (mapconcat
                   (lambda (ch)
                     (format "%02X%02X"
                             (mod ch 256)
                             (truncate ch 256)))
                   string
                   "")))

(defun string-to-asn1-octet-string-at-point ()
  "Replace the string sexp at point with its ASN.1 OCTET STRING literal."
  (interactive)
  (let ((string (sexp-at-point)))
    (if (stringp string)
        (progn
          (kill-sexp)
          (insert (string-to-asn1-octet-string string)))
        (error "Not a string at point: %S" string))))

(defun string-from-asn1-octet-string (os)
  "Decode an ASN.1 hex OCTET STRING literal OS ('....'H) back to a string."
  (let ((os (if (and (string= "'"  (subseq os 0 1))
                     (string= "'H" (subseq os (- (length os) 2))))
                (subseq os 1 (- (length os) 2))
                os)))
    (coerce (loop
              for i below (length os) by 4
              for l = (car (read-from-string (concat "#x" (subseq os i (+ i 2)))))
              for h = (car (read-from-string (concat "#x" (subseq os (+ i 2) (+ i 4)))))
              for c = (+ (* h 256) l)
              collect c)
            'string)))

(defun convert-all-asn1-octet-string-to-string (start end)
  "Replace every ASN.1 hex OCTET STRING literal in the region by its string."
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "'[0-9A-Fa-f]*'H" end t)
    (let ((os (match-string 0)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert (format "%S" (string-from-asn1-octet-string os))))))

;;;============================================================================
;;; comint / shell output cleanup                       [from emacs-harman]
;;;============================================================================

(defun comint-remove-esc-b-filter (_string)
  "Remove ESC(B sequences between `comint-last-output-start' and the process mark.
Suitable for `comint-output-filter-functions'."
  (save-excursion
   (goto-char comint-last-output-start)
   (let ((end (process-mark (get-buffer-process (current-buffer)))))
     (while (search-forward "\e(B" end t)
       (delete-region (match-beginning 0) (match-end 0))))))

;;;============================================================================
;;; Log-file viewing modes                       [from emacs-trustonic/dxo]
;;;============================================================================

(defface gherkin-error   '((default (:foreground "red")))    "Error in Gherkin logs.")
(defface gherkin-simple  '((default (:foreground "yellow"))) "Simple lines in Gherkin logs.")
(defface gherkin-command '((default (:foreground "blue")))   "Command lines in Gherkin logs.")
(defface gherkin-build   '((default (:foreground "grey")))   "Build lines in Gherkin logs.")

(defun gherkin-log-meat ()
  "Set up font-locking for a Gherkin/Cucumber log buffer."
  (interactive)
  (toggle-truncate-lines 1)
  (font-lock-mode 1)
  (setf font-lock-maximum-size 10000000)
  (setf font-lock-keywords '())
  (font-lock-add-keywords
   nil
   '(("^\\(error .*\\)"                (1 gherkin-error))
     ("^\\(build .*error on line.*\\)" (1 gherkin-error))
     ("^\\(.*\\.m:[0-9]+: error:.*\\)" (1 gherkin-error))
     ("^\\(simple .*\\)"               (1 gherkin-simple))
     ("^\\(command .*\\)"              (1 gherkin-command))
     ("^\\(build .*\\)"                (1 gherkin-build))))
  (font-lock-fontify-buffer))

(define-derived-mode gherkin-log-mode view-mode "Gherkin"
  "Mode for Gherkin logs."
  (gherkin-log-meat))

(define-derived-mode valgrind-mode compilation-mode "Valgrind"
  "Mode to read valgrind log files."
  (setf font-lock-keywords nil)
  (font-lock-add-keywords
   nil
   '(("^{[^}]*}"
      (0 font-lock-preprocessor-face))
     ("^\\(==[0-9]*==\\) \\([^ \n].*\\)$"
      (1 font-lock-variable-name-face)
      (2 font-lock-warning-face))
     ("^\\(==[0-9]*==\\)   \\([^ \n].*\\)$"
      (1 font-lock-variable-name-face)
      (2 font-lock-comment-face))
     ("^\\(==[0-9]*==\\)    [a-z]+ \\(0x[0-9A-F]*\\): \\([^( \n]*\\)(\\(.*\\)) (\\([^:]+:[0-9]+\\))$"
      (1 font-lock-variable-name-face)
      (2 font-lock-constant-face)
      (3 font-lock-function-name-face)
      (4 font-lock-type-face)
      (5 font-lock-comment-face)))))

;;;============================================================================
;;; Region / line editing                        [from emacs-trustonic/dxo]
;;;============================================================================

(defun re-delete-lines-between (start end start-re end-re)
  "In the region START..END, delete whole lines from a match of START-RE
through the line of the next match of END-RE."
  (interactive "rsStart regexp: \nsEnd regexp: ")
  (save-excursion
    (narrow-to-region start end)
    (while (re-search-forward start-re nil t)
      (let ((end (match-end 0)))
        (goto-char (match-beginning 0))
        (beginning-of-line)
        (let ((start (point)))
          (if (re-search-forward end-re nil t)
              (progn
                (goto-char (match-end 0))
                (end-of-line)
                (forward-char)
                (delete-region start (point)))
              (goto-char end)))))
    (widen)))

;;;============================================================================
;;; hideshow for lua-mode and nxml-mode          [from emacs-trustonic/dxo]
;;;============================================================================

(defun my-nxml-forward-element ()
  "Forward over one balanced nxml element, for hideshow."
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))

(with-eval-after-load 'hideshow
  (setf hs-special-modes-alist
        (append '((lua-mode
                   "\\([[{(]\\|\\<\\(do\\|\\(?:functio\\|the\\)n\\)\\>\\)"
                   "\\([]})]\\|\\<\\(end\\)\\>\\)"
                   nil
                   lua-forward-sexp
                   nil)
                  (nxml-mode
                   "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                   ""
                   "<!--" ;; won't work on its own; uses syntax table
                   (lambda (_arg) (my-nxml-forward-element))
                   nil))
                (remove-if (lambda (key) (member key '(lua-mode nxml-mode)))
                           hs-special-modes-alist :key 'car))))

;;;============================================================================
;;; Outline minor-mode easy bindings                    [from emacs-dxo]
;;;============================================================================

(defun outline-easy-bindings-meat ()
  "Load the outline-mode-easy-bindings package for the current outline buffer."
  (interactive)
  (require 'outline-mode-easy-bindings))

;;;============================================================================
;;; Dated TODO comment inserter                   [from emacs-trustonic]
;;;============================================================================
;;; Suggested bindings (left to the host config), e.g.:
;;;   (global-set-key (kbd "C-c t ;") 'insert-dated-todo-comment)
;;;   (global-set-key (kbd "H-;")     'insert-dated-todo-comment)

(defvar *todo-comment-user-name* nil
  "User name stamped in TODO comments; defaults to `user-login-name' when nil.")

(defun insert-dated-todo-comment (start end)
  "Insert a dated TODO comment: TODO-[YYYY-MM-DD]-[user] .
If the region is active, comment it as the TODO text.
START and END are the region bounds."
  (interactive "*r")
  (let* ((user (or *todo-comment-user-name* user-login-name))
         (date (calendar-current-date))
         (head (format "TODO-[%04d-%02d-%02d]-[%s] "
                       (third date) (first date) (second date)
                       user))
         (endm (make-marker)))
    (unwind-protect
         (progn
           (if (and mark-active start end)
               (progn
                 (goto-char start)
                 (set-marker endm end)
                 (insert head))
               (progn
                 (setf start (point))
                 (insert head)
                 (set-marker endm (point))))
           (set-mark start)
           (goto-char endm)
           (comment-region start endm)
           (goto-char endm))
      (set-marker endm nil))))

;;;============================================================================
;;; Trailing whitespace checks                    [from emacs-trustonic]
;;;============================================================================
;;; Suggested hooks (left to the host config):
;;;   (add-hook 'find-file-hook   'pjb-check-trailing-whitespace-meat)
;;;   (add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun pjb-alert-trailing-whitespace ()
  "Flash the frame red and wait for a keypress (trailing-whitespace alert)."
  (set-background-color "#600c0c")
  (read-char "ALERT! Trailing whitespaces!     (Type the Any key)")
  (set-palette *current-palette*))

(defun pjb-check-trailing-whitespace-meat ()
  "Alert (`pjb-alert-trailing-whitespace') on trailing whitespace in the buffer."
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (when (re-search-forward " $" nil t)
     (pjb-alert-trailing-whitespace))))

(defun check-trailing-whitespace-vc-meat ()
  "Warn via a message box if a version-controlled buffer has trailing whitespace."
  (interactive)
  (when (vc-backend (buffer-file-name)) ; only files under version control
    (save-excursion
     (goto-char (point-min))
     (when (re-search-forward " $" nil t)
       (message-box "*** WARNING: There are Trailing White Spaces in %s ***"
                    (buffer-name))))))

;;;============================================================================
;;; C comment helpers                             [from emacs-trustonic]
;;;============================================================================

(defun looking-at-c-comment ()
  "Non-nil when looking at the start of a C comment (after optional whitespace)."
  (looking-at "[ \n\t]*/[*/]"))

(defun c-comment-block-to-line (comment)
  "Convert a /* ... */ block COMMENT string into // line comments."
  (if (and (string= "/*" (subseq comment 0 2))
           (string= "*/" (subseq comment (- (length comment) 2))))
      (concat "// " (mapconcat (function identity)
                               (split-string (subseq comment 2 (- (length comment) 2)) "\n")
                               "\n// "))
      comment))

;;;============================================================================
;;; Bulk reindent of source trees                       [from emacs-ubudu]
;;;============================================================================

(defun reindent-all-java (dirpath)
  "Untabify and reindent every .java file under DIRPATH (recursively)."
  (interactive "DDirectory: ")
  (with-files (file dirpath 'recursive)
              (when (string-match "\.java$" file)
                (with-file (file :save t :kill t :literal nil)
                           (untabify (point-min) (point-max))
                           (indent-region (point-min) (point-max))))))

(provide 'emacs-utils)
;;;; THE END ;;;;
