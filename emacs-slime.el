;;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               emacs-slime.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Load slime and other CL stuff.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-22 <PJB> Extracted from emacs-common.el
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(.EMACS "emacs-slime.el")

(when (require 'slime nil t)
  
  (defun slime-eval-print (string)
    "Eval STRING in Lisp; insert any output and the result at point."
    (message "current-prefix-arg = %S" current-prefix-arg)
    (let ((commentp (and (listp current-prefix-arg)
                         (integerp (first current-prefix-arg))
                         (< 4 (first current-prefix-arg)))))
      (slime-eval-async `(swank:eval-and-grab-output ,string)
        `(lambda (result)
           (destructuring-bind (output value) result
             (push-mark)
             (if ,commentp
                 (progn
                   (insert output)
                   (let ((lines (split-string value "\n")))
                     (insert "\n;; --> " (pop lines) "\n")
                     (dolist (line lines)
                       (insert ";;     " line "\n"))))
                 (insert output value)))))))

  
  (or (ignore-errors
        (progn (slime-setup '(slime-fancy
                              slime-xref-browser
                              slime-asdf
                              slime-banner
                              slime-repl
                              slime-indentation
                              slime-fuzzy
                              slime-autodoc
                              slime-presentations
                              slime-presentation-streams))
               (setf slime-complete-symbol*-fancy   t
                     slime-complete-symbol-function 'slime-fuzzy-complete-symbol
                     slime-load-failed-fasl         'never)
               t))
      (ignore-errors
        (progn (slime-setup '(slime-fancy slime-indentation))
               t))
      (ignore-errors
        (progn (slime-setup :autodoc t :typeout-frame t :highlight-edits t)
               t))
      (ignore-errors
        (progn (slime-setup)
               t))
      (error ".EMACS: Cannot setup slime :-("))

  (defun reload-swank ()
    (interactive)
    (load (expand-file-name "~/quicklisp/slime-helper.el") t)
    (load-library "slime")
    (slime-setup '(slime-fancy
                   slime-asdf
                   slime-sprof
                   slime-compiler-notes-tree
                   slime-hyperdoc
                   slime-mrepl
                   slime-indentation
                   slime-repl
                   slime-media)))

  (reload-swank)


  (setf slime-net-coding-system 'utf-8-unix)
  (setf slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
  (pushnew 'paredit-mode slime-repl-mode-hook)



  (.EMACS " define-lisp-implementation")
  (defvar *lisp-implementations* '() "List of defined lisp implementations.")
  (defvar *default-lisp-implementation* nil "The default lisp implementation.")
  (defvar slime-lisp-implementations    nil)
  (defvar lisp-implementation nil
    "Buffer local variable indicating what lisp-implementation is used here.")


  (defstruct lisp-implementation
    name command prompt coding
    (function-documentation-command
     "(let ((fn '%s))
     (format t \"Documentation for ~a:~&~a\"
	     fn (documentation fn 'function))
     (values))\n")
    (variable-documentation-command
     "(let ((v '%s))
     (format t \"Documentation for ~a:~&~a\"
	     v (documentation v 'variable))
     (values))\n")
    (argument-list-command
     "(let ((fn '%s))
     (format t \"Arglist for ~a: ~a\" fn (arglist fn))
     (values))\n")
    (describe-symbol-command "(describe '%s)\n")
    (init 'slime-init-command))


  (defmacro define-lisp-implementation (name commands-expression prompt coding &rest rest)
    `(let ((command (first-existing-file (mapcar (lambda (cmd)
                                                   (if (listp cmd)
                                                       (first cmd)
                                                       cmd))
                                                 ,commands-expression))))
       (if command
           (let* ((command (ensure-list command))
                  (li (make-lisp-implementation
                       :name     ',name
                       :command  (mapconcat (function identity) command " ")
                       :prompt   ,prompt
                       :coding  ',coding
                       ,@rest))
                  (sli (assoc ',name slime-lisp-implementations)))
             (setf (get ',name :lisp-implementation) li)
             (pushnew ',name *lisp-implementations*)
             (if (null sli)
                 (push (list ',name command
                             :coding-system  (intern (format "%s-unix" ',coding))
                             :init (lisp-implementation-init li))
                       slime-lisp-implementations)
                 (setf (cdr sli)
                       (list command
                             :coding-system (intern (format "%s-unix" ',coding))
                             :init (lisp-implementation-init li))))
             ',name)
           (warn "No executable for lisp implementation: %s" ',name))))


  (define-lisp-implementation scheme
      '("mzscheme")
    "^> "
    iso-8859-1)


  (define-lisp-implementation mzscheme
      '("mzscheme")
    "^> "
    iso-8859-1)

  (define-lisp-implementation mit-scheme
      '("/usr/local/languages/mit-scheme/bin/scheme")
    "^\[[0-9]*\]> "
    iso-8859-1)

  (define-lisp-implementation umb-scheme
      '("/usr/bin/scheme")
    "^==> "
    iso-8859-1)


  
  (define-lisp-implementation abcl
      '("/data/languages/abcl/abcl")
    "^.*([0-9]+): "
    iso-8859-1)
  
  (define-lisp-implementation allegro
      '("/data/languages/acl82express/alisp")
    "^\[[0-9]*\]> "
    iso-8859-1)

  (define-lisp-implementation ccl
      '("/data/languages/ccl/bin/ccl"
        "/usr/local/bin/ccl"
        "/opt/local/bin/ccl"
        "/usr/bin/ccl")
    "^? "
    utf-8)



  (defun windoize-pathname (path)
    ;; "/home/pjb/quicklisp/dists/quicklisp/software/slime-20120208-cvs/swank-loader.lisp"
    (let ((home (expand-file-name "~/")))
      (if (prefixp home path)
          (format "HOME:%s"      (substitute (character ";") (character "/") (subseq path (length home))))
          (format "C:\\cygwin%s" (substitute (character "\\") (character "/") path)))))
  
  (defun slime-init-ccl-win-cygwin (port-filename coding-system)
    "Return a string to initialize Lisp."
    (let ((loader (if (file-name-absolute-p slime-backend)
                      slime-backend
                      (concat slime-path slime-backend))))
      ;; Return a single form to avoid problems with buffered input.
      (format "%S\n\n"
              `(progn
                 (load ,(windoize-pathname (expand-file-name loader)) 
                       :verbose t)
                 (funcall (read-from-string "swank-loader:init"))
                 (funcall (read-from-string "swank:start-server")
                          ,(windoize-pathname port-filename))))))

  (define-lisp-implementation ccl-win-cygwin
      ;; This is a Windows CCL run thru cygwin emacs…
      '("/usr/local/bin/ccl")
    "^? "
    utf-8
    :init  'slime-init-ccl-win-cygwin)


  
  (define-lisp-implementation openmcl
      '("/usr/local/bin/openmcl")
    "^\[[0-9]*\]> "
    iso-8859-1)


  (define-lisp-implementation clisp
      (list* (cond
               ((eq system-type 'cygwin)  "/usr/bin/clisp")
               (t  (first-existing-file '("/data/languages/clisp/bin/clisp"
                                          "/opt/local/bin/clisp"
                                          "/usr/local/bin/clisp"
                                          "/opt/clisp-2.41-pjb1-regexp/bin/clisp"
                                          "/usr/bin/clisp"))))
             "-ansi""-q";"-m""32M""-I""-K""full"
             (cond
               ((eq system-type 'darwin)
                (list "-Efile"     "UTF-8"
                      "-Epathname" "UTF-8"
                      "-Eterminal" "UTF-8"
                      "-Emisc"     "UTF-8" ; better be same as terminal
                      "-Eforeign"  "ISO-8859-1")) ; must be 1-1.
               (t
                (list "-Efile"     "UTF-8"
                      "-Epathname" "ISO-8859-1"
                      "-Eterminal" "UTF-8"
                      "-Emisc"     "UTF-8" ; better be same as terminal
                      "-Eforeign"  "ISO-8859-1")))) ; must be 1-1.
    "^\[[0-9]*\]> "
    utf-8
    :argument-list-command
    "(let ((fn '%s))
     (cond
       ((not (fboundp fn))      (format t \"~A is not a function\" fn))
       ((special-operator-p fn) (format t \"~A is a special operator\" fn))
       ((macro-function fn)     (format t \"~A is a macro\" fn))
       (t  (format t \"Arglist for ~a: ~a\" fn (ext:arglist fn))))
     (values))\n")

  ;; (lisp-implementation-coding(get 'clisp :lisp-implementation))
  ;; utf-8
  ;; slime-net-coding-system

  (define-lisp-implementation cmucl
      '("/data/languages/cmucl/bin/lisp"
        "/usr/local/bin/lisp"
        "/opt/local/bin/lisp"
        "/usr/bin/lisp")
    "^\* "
    utf-8)

  
  (define-lisp-implementation sbcl
      (mapcar (lambda (cmd) (list cmd "--noinform"))
              '("/data/languages/sbcl/bin/sbcl" 
                "/usr/local/bin/sbcl" 
                "/opt/local/bin/sbcl"
                "/usr/bin/sbcl"))
    "^\[[0-9]*\]> "
    utf-8)


  (define-lisp-implementation ecl
      '("/data/languages/ecl/bin/ecl"
        "/usr/local/bin/ecl"
        "/opt/local/bin/ecl"
        "/usr/bin/ecl")
    "^> "
    utf-8)

  
  (define-lisp-implementation gcl
      '("/data/languages/gcl/bin/gcl"
        "/usr/local/bin/gcl"
        "/opt/local/bin/gcl"
        "/usr/bin/gcl")
    "^> "
    utf-8)

  

  


  (defun set-inferior-lisp-implementation (impl)
    "Set the default lisp implementation used by inferior-lisp and slime."
    (interactive "SImplementation: ")
    (when (member impl *lisp-implementations*)
      (let ((limpl (get impl :lisp-implementation)))
        (if limpl
            (progn
              (message ".EMACS: inferior-lisp implementation: %s"
                       (lisp-implementation-name limpl))
              (let ((coding (lisp-implementation-coding limpl)))
                (setf *default-lisp-implementation* limpl
                      inferior-lisp-program         (lisp-implementation-command limpl)
                      inferior-lisp-prompt          (lisp-implementation-prompt limpl)
                      lisp-function-doc-command     (lisp-implementation-function-documentation-command limpl)
                      lisp-var-doc-command          (lisp-implementation-variable-documentation-command limpl)
                      lisp-arglist-command          (lisp-implementation-argument-list-command limpl)
                      lisp-describe-sym-command     (lisp-implementation-describe-symbol-command limpl)
                      default-process-coding-system (cons coding coding)
                      slime-net-coding-system       (intern (format "%s-unix" coding))
                      slime-default-lisp            impl)))
            (error "%S not a lisp implementation." impl))
        impl)))

  (defalias 'set-default-lisp-implementation 'set-inferior-lisp-implementation)

  (defun set-inferior-lisp-buffer (buffer-name)
    (interactive "bInferior Lisp Buffer: ")
    (make-local-variable 'inferior-lisp-buffer)
    (make-local-variable 'lisp-function-doc-command)
    (make-local-variable 'lisp-var-doc-command)
    (make-local-variable 'lisp-arglist-command)
    (make-local-variable 'lisp-describe-sym-command)
    (make-local-variable 'lisp-implementation)
    (setf inferior-lisp-buffer buffer-name
          lisp-implementation  (or (buffer-local-value 'lisp-implementation
                                                       (get-buffer inferior-lisp-buffer))
                                   (lisp-implementation-name *default-lisp-implementation*)))
    (let ((limpl (get lisp-implementation :lisp-implementation)))
      (when limpl
        (setf lisp-function-doc-command (lisp-implementation-function-documentation-command limpl)
              lisp-var-doc-command      (lisp-implementation-variable-documentation-command limpl)
              lisp-arglist-command      (lisp-implementation-argument-list-command limpl)
              lisp-describe-sym-command (lisp-implementation-describe-symbol-command limpl)))))


  ;; Used both by ilisp and slime:
  ;; (case system-type
  ;;   ((darwin)     (set-default-lisp-implementation 'clisp)) ; openmcl))
  ;;   ((gnu/linux)  (set-default-lisp-implementation 'clisp)) ; sbcl))
  ;;   ((cygwin)     (set-default-lisp-implementation 'clisp))
  ;;   (otherwise    (warn "unexpected system-type for inferior-lisp-program")
  ;;                 (set-default-lisp-implementation 'clisp)))

  (loop
     for impl in '(ccl clisp sbcl ecl abcl)
     when (set-default-lisp-implementation impl)
     do (progn
          (message "Default Lisp implementations is %s" impl)
          (return impl)))
  
  (defun %lisp-buffer-name (n impl) (format "%dlisp-%s" n impl))
  (defun %lisp-buffer-name-match-p (buffer-name &optional number)
    (string-match (if number (format "^%dlisp" number) "^[0-9]+lisp") buffer-name))
  (defun %lisp-buffer-name-number (buffer-name)
    (when (string-match "^\\([0-9]+\\)lisp" buffer-name)
      (cl:parse-integer (match-string 1 buffer-name))))
  (defun inferior-lisp-buffers-list ()
    "RETURN: a list of the inferior-lisp buffers."
    (delete-if (lambda (name) (not (%lisp-buffer-name-match-p name)))
               (mapcar (function buffer-name) (buffer-list))))
  (defun %lisp-buffer-next-number ()
    (loop
       with i = 0
       with numbers = (sort (mapcar (function  %lisp-buffer-name-number)
                                    (inferior-lisp-buffers-list))
                            (function <=))
       while numbers
       do (if (= i (car numbers))
              (progn (incf i) (pop numbers))
              (return i))
       finally (return i)))

  (defvar *lisp-command-history* '())

  (defun inferior-lisp-other-window (cmd)
    "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
    (interactive (list (if current-prefix-arg
                           (read-string "Run lisp: " inferior-lisp-program)
                           inferior-lisp-program)))
    (if (not (comint-check-proc "*inferior-lisp*"))
        (let ((cmdlist (split-string cmd)))
          (set-buffer (apply (function make-comint)
                             "inferior-lisp" (car cmdlist) nil (cdr cmdlist)))
          (inferior-lisp-mode)))
    (setq inferior-lisp-buffer "*inferior-lisp*")
    (pop-to-buffer "*inferior-lisp*" t))


  (defun nlisp (&optional ask-command)
    "Create a new inferior-lisp buffer."
    (interactive "P")
    (let* ((impl-or-cmd
            (if ask-command
                (read-from-minibuffer
                 "Lisp implementation or command: "
                 (format "%s" (lisp-implementation-name
                               *default-lisp-implementation*))
                 nil nil '*lisp-command-history*)
                (format "%s" (lisp-implementation-name
                              *default-lisp-implementation*))))
           (impl  (unless (position (character " ") impl-or-cmd
                                    :test (function char=))
                    (intern-soft impl-or-cmd)))
           (limpl (and impl (get impl :lisp-implementation)))
           (cmd   (if limpl (lisp-implementation-command limpl) impl-or-cmd)))
      (inferior-lisp-other-window cmd)
      (make-local-variable 'lisp-implementation)
      (setf lisp-implementation
            (or impl (lisp-implementation-name *default-lisp-implementation*)))
      (rename-buffer
       (setf inferior-lisp-buffer
             (%lisp-buffer-name
              (%lisp-buffer-next-number)
              (cond
                (impl)
                ((string= cmd (lisp-implementation-command
                               *default-lisp-implementation*))
                 (lisp-implementation-name *default-lisp-implementation*))
                ('custom)))))))


  (defun lisp (&optional ask-command)
    "Create a new inferior-lisp when none exist,
   or switch to the last created one."
    (interactive "P")
    (if (and (boundp 'inferior-lisp-buffer) inferior-lisp-buffer
             (get-buffer inferior-lisp-buffer))
        (switch-to-buffer inferior-lisp-buffer)
        (let ((lisp-buffers (inferior-lisp-buffers-list)))
          (if lisp-buffers
              (switch-to-buffer
               (setf inferior-lisp-buffer (first lisp-buffers)))
              (nlisp ask-command))))))



(defvar package 'common-lisp-user)

(defun symbol-value-in-buffer (symbol buffer)
  (save-excursion
    (set-buffer buffer)
    (when (boundp symbol)
      (symbol-value symbol))))

(defun set-symbol-value-in-buffer (symbol buffer value)
  (save-excursion
    (set-buffer buffer)
    (make-local-variable symbol)
    (setf (symbol-value symbol) value)))

(defsetf symbol-value-in-buffer set-symbol-value-in-buffer)

;; (symbol-value-in-buffer 'inferior-lisp-buffer "a.lisp")
;; (local-variable-p 'package)
;; (inferior-lisp-package)
;; (local-variable-p 'package)

;; Interfers with slime:
;;
;; (defun inferior-lisp-buffer (&optional process)
;;   (if (boundp 'inferior-lisp-buffer)
;;       inferior-lisp-buffer
;;       (process-buffer (or process (inferior-lisp-proc)))))
;; 
;; (defun inferior-lisp-package (&optional process)
;;   (symbol-value-in-buffer 'package (inferior-lisp-buffer process)))
;; 
;; ;; (defun lisp-eval-region (start end &optional and-go)
;; ;;   "Send the current region to the inferior Lisp process.
;; ;; Prefix argument means switch to the Lisp buffer afterwards."
;; ;;   (interactive "r\nP")
;; ;;   (comint-send-region (inferior-lisp-proc) start end)
;; ;;   (comint-send-string (inferior-lisp-proc) "\n")
;; ;;   (if and-go (switch-to-lisp t)))
;; 
;; (defadvice lisp-eval-region (before ler-in-package activate) 
;;   (when (and (boundp 'package) (not (eq package (inferior-lisp-package))))
;;     (comint-send-string (inferior-lisp-proc)
;;                         (upcase (format "(CL:IN-PACKAGE #:%s)\n" package)))
;;     (setf (symbol-value-in-buffer 'package (inferior-lisp-buffer)) package)))


(defun lisp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))


(appendf interpreter-mode-alist '(("sbcl" . lisp-mode)
                                  ("abcl" . lisp-mode)
                                  ("gcl" . lisp-mode)
                                  ("ecl" . lisp-mode)
                                  ("cmucl" . lisp-mode)
                                  ("alisp" . lisp-mode)))

(appendf auto-mode-alist '(("\\.lisp$" . lisp-mode)
                           ("\\.fas$"  . lisp-mode)
                           ("\\.lsp$"  . lisp-mode)
                           ("\\.cl$"   . lisp-mode)
                           ("\\.acl2$" . lisp-mode)
                           ("\\.LISP$" . lisp-mode)
                           ("\\.FAS$"  . lisp-mode)
                           ("\\.LSP$"  . lisp-mode)
                           ("\\.CL$"   . lisp-mode)
                           ("\\.ACL2$" . lisp-mode)))

(appendf auto-mode-alist '(("\\.scm$"    . scheme-mode)
                           ("\\.ss$"     . scheme-mode)
                           ("\\.stk$"    . scheme-mode)
                           ("\\.stklos$" . scheme-mode)))



(defun pjb-show-lisp-repl (jump-in)
  "Switches to a repl buffer, depending on the major mode and what's available.
If `jump-in' is true (ie. a prefix is given), we switch to the repl too."
  (interactive "P")
  (labels ((show-buffer (buffer)
             (delete-other-windows)
             (split-window-horizontally)
             (other-window 1)
             (etypecase buffer
               (buffer (switch-to-buffer buffer))
               (function (funcall buffer)))
             (unless jump-in (other-window 1)))
           (inferior-lisp-repl ()
             (when inferior-lisp-buffer
               (let ((lisp-buffer (get-buffer inferior-lisp-buffer)))
                 (when lisp-buffer
                   (show-buffer lisp-buffer)
                   (return-from inferior-lisp-repl))))
             ;; No inferior-lisp buffer, let's start a lisp.
             (if (fboundp 'slime)
                 (show-buffer (function slime))
                 (show-buffer (function inferior-lisp)))))
    (case major-mode
      ((emacs-lisp-mode)
       (show-buffer (function ielm)))
      ((lisp-mode)
       (if (and (boundp 'slime-mode) slime-mode)
           (show-buffer (function slime-switch-to-output-buffer))
           (inferior-lisp-repl)))
      ((slime-repl-mode inferior-emacs-lisp-mode)
       (message "Already there."))
      (t
       (inferior-lisp-repl)))))

(defun indent-defun ()
  (interactive)
  (save-excursion
    (indent-region (progn (beginning-of-defun) (point))
                   (progn (end-of-defun) (point)))))

(defun pjb-lisp-comment-region (beg end &optional arg)
  (let* ((numarg (prefix-numeric-value arg))
         (style (cdr (assoc comment-style comment-styles)))
         (lines (nth 2 style))
         (block (nth 1 style))
         (multi (nth 0 style)))
    ;; we use `chars' instead of `syntax' because `\n' might be
    ;; of end-comment syntax rather than of whitespace syntax.
    ;; sanitize BEG and END
    (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
    (setq beg (max beg (point)))
    (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
    (setq end (min end (point)))
    (if (>= beg end) (error "Nothing to comment"))

    ;; sanitize LINES
    (setq lines
          (and
           lines ;; multi
           (progn (goto-char beg) (beginning-of-line)
                  (skip-syntax-forward " ")
                  (>= (point) beg))
           (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
                  (<= (point) end))
           (or block (not (string= "" comment-end)))
           (or block (progn (goto-char beg) (search-forward "\n" end t)))))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
      ((consp arg) (uncomment-region beg end))
      ((< numarg 0) (uncomment-region beg end (- numarg)))
      (t
       (setq numarg (comment-add arg))
       (comment-region-internal
        beg end
        (let ((s (comment-padright comment-start numarg)))
          (if (string-match comment-start-skip s) s
              (comment-padright comment-start)))
        (let ((s (comment-padleft comment-end numarg)))
          (and s (if (string-match comment-end-skip s) s
                     (comment-padright comment-end))))
        (if multi (comment-padright comment-continue numarg))
        (if multi
            (comment-padleft (comment-string-reverse comment-continue) numarg))
        block
        lines
        (nth 3 style))))))


(defun paredit-beginning-of-toplevel-form ()
  (interactive)
  (ignore-errors (paredit-backward-up 1000)))

(defun paredit-end-of-toplevel-form ()
  (interactive)
  (ignore-errors (paredit-forward-up 1000)))


(defun pjb-lisp-meat ()
  (interactive)
  (.EMACS "pjb-lisp-meat on %S starts" (buffer-name))
  (when (fboundp 'auto-complete-mode) (auto-complete-mode 1))
  (local-set-key (kbd "RET")  'newline-and-indent)
  ;; (local-set-key (kbd "RET") 'indent-defun)
  ;; (setq blink-matching-paren t)
  (setf skeleton-pair         nil
        comint-process-echoes nil)
  (setf comment-style 'indent)
  ;; (setf comment-region-function 'pjb-lisp-comment-region)
  (local-set-key (kbd "<A-up>")      'backward-up-list)
  (local-set-key (kbd "<A-down>")    'down-list)
  (paredit-mode +1)
  (local-set-key (kbd "s-A-<left>")  'paredit-backward-barf-sexp)
  (local-set-key (kbd "s-A-<right>") 'paredit-backward-slurp-sexp)
  (local-set-key (kbd "A-<right>")   'paredit-forward-slurp-sexp)
  (local-set-key (kbd "A-<left>")    'paredit-forward-barf-sexp)
  (local-set-key (kbd "A-s")         'paredit-backward-barf-sexp)
  (local-set-key (kbd "A-d")         'paredit-backward-slurp-sexp)
  (local-set-key (kbd "A-f")         'paredit-forward-slurp-sexp)
  (local-set-key (kbd "A-g")         'paredit-forward-barf-sexp)
  (local-set-key (kbd "C-M-U")       'paredit-beginning-of-toplevel-form)
  (local-set-key (kbd "C-M-N")       'paredit-end-of-toplevel-form)
  (local-set-key (kbd "C-M-<backspace>") 'backwards-kill-sexp)
  (local-set-key (kbd "C-x C-r g")   'redshank-make-defgeneric-from-defmethod)
  (local-set-key (kbd "H-e")         'pjb-cl-export-definition-at-point)
  (local-set-key (kbd "H-s")         'pjb-cl-export-symbol-at-point)
  ;;   (setq skeleton-pair t)
  ;;   (local-set-key "("  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "["  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "{"  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "|"  'skeleton-pair-insert-maybe)
  ;;   (local-set-key "\"" 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "M-[") 'paredit-wrap-square)
  (local-set-key (kbd "M-{") 'paredit-wrap-curly)
  (modify-syntax-entry ?\[ "()" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")(" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "()" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} ")(" lisp-mode-syntax-table)
  (when (fboundp 'column-marker-1) (column-marker-1 80))
  (add-hook 'comint-preoutput-filter-functions (function pjb-comint-preoutput-insert-image))
  (font-lock-add-keywords nil '(("\\<[Rr][Kk]:\\sw\\sw+\\>" 0 font-lock-builtin-face)
                                ("(\\(\\<[-A-Za-z0-9]+-define-[-A-Za-z0-9]+\\>\\)" 1 font-lock-keyword)))
  (.EMACS "pjb-lisp-meat on %S done" (buffer-name))
  (values))






;; (load-library "cl")
;; (setq indent-region-function (function lisp-indent-function))
;; (setq lisp-indent-function   (function common-lisp-indent-function))
;; (setq lisp-indent-function   (function lisp-indent-function))
;; (put 'let  'lisp-indent-function        '(&lambda &body))
;; (put 'let* 'lisp-indent-function        '(&lambda &body))
;; (put 'let  'common-lisp-indent-function '(&lambda &body))
;; (put 'let* 'common-lisp-indent-function '(&lambda &body))


;; (defun test (var var var
;;                  var var var)
;;   body)

;; (let (var var var
;;           var
;;           var)
;;   var)

;; (TRACE lisp-indent-function common-lisp-indent-function
;;        common-lisp-indent-function-1)

;; (setq lisp-simple-loop-indentation   1
;;       lisp-loop-keyword-indentation  6
;;       lisp-loop-forms-indentation    6)


;; M-( insert-parentheses
;; M-) move-past-close-and-reindent

(defun sexp-diff (s1 s2)
  (cond
    ((and (atom s1) (atom s2))
     (if (equal s1 s2)
         s1
         (list :DIFFERENCE s1 s2)))
    ((and (consp s1) (consp s2))
     (cons (sexp-diff (car s1) (car s2))
           (sexp-diff (cdr s1) (cdr s2))))
    (t
     (list :DIFFERENCE s1 s2))))

(defun collect-sexps (count)
  (when (< count 0)
    (setf count (- count))
    (backward-sexp count))
  (forward-sexp)
  (backward-sexp)
  (let ((b (point))
        (e (point))
        (l '()))
    (dotimes (n count)
      (forward-sexp)
      (backward-sexp)
      (push (sexp-at-point) l)
      (forward-sexp)
      (setf e (point)))
    (list b e (nreverse l))))

(defun ex-parenthese (&optional arg)
  (interactive "p")
  (let ((bel (collect-sexps arg)))
    (when bel
      (goto-char (first bel))
      (dolist (sexp (third bel))
        (forward-sexp)
        (when (listp sexp)
          (let ((e (point)))
            (backward-sexp)
            (delete-char 1)
            (goto-char (- e 1))
            (backward-delete-char 1))))
      (goto-char (if (< 0 arg) (first bel) (- (second bel) 2))))))

(defun in-parenthese (&optional arg)
  (interactive "p")
  (let ((bel (collect-sexps arg)))
    (when bel
      (goto-char (second bel))
      (insert ")")
      (goto-char (first bel))
      (insert "(")
      (goto-char (1+ (first bel))))))

(global-set-key (kbd "A-[")   'in-parenthese)
(global-set-key (kbd "A-]")   'ex-parenthese)


(require 'inf-lisp)
(defun sexp-movement ()
  "Binds locally some keys to sexp movement commands."
  (interactive)
  (define-key inferior-lisp-mode-map  (kbd "C-c .") 'forward-sexp)
  (define-key inferior-lisp-mode-map  (kbd "C-c ,") 'backward-sexp)
  (local-set-key (kbd "C-c .") 'forward-sexp)
  (local-set-key (kbd "C-c ,") 'backward-sexp)
  (define-key inferior-lisp-mode-map  (kbd "A-.") 'forward-sexp)
  (define-key inferior-lisp-mode-map  (kbd "A-,") 'backward-sexp)
  (local-set-key (kbd "A-.") 'forward-sexp)
  (local-set-key (kbd "A-,") 'backward-sexp)
  (local-set-key [M-up]        'up-list)
  (local-set-key [M-down]      'down-list)
  (local-set-key [M-right]     'forward-sexp)
  (local-set-key [M-left]      'backward-sexp)
  (values)) 




(defun make-lisp-command-sender (string)
  (byte-compile `(lambda ()
                   (interactive)
                   (cond
                     ((and (boundp 'slime-inferior-process:connlocal)
                           slime-inferior-process:connlocal)
                      (slime-repl-send-string ,(format "%s\n" string)))
                     ((and inferior-lisp-buffer
                           (inferior-lisp-proc))
                      (comint-send-string (inferior-lisp-proc)
                                          ,(format "%s\n" string)))
                     ((get-buffer-process (current-buffer))
                      (comint-send-string (get-buffer-process (current-buffer))
                                          ,(format "%s\n" string)))
                     (t (error "No process to send debugging command to."))))))

(defun clisp-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender ":s"))
  (local-set-key (kbd "<f6>") (make-lisp-command-sender ":n"))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender ":o"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender ":c"))
  (message "<f5> step into  <f6> next       <f7> step over  <f8> continue"))

(defun ecl-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender ""))
  (local-set-key (kbd "<f6>") (make-lisp-command-sender ""))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender ":skip"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender ":exit"))
  (message "<f5> step into  <f6> next       <f7> step over  <f8> continue"))

(defun sbcl-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender "step"))
  (local-set-key (kbd "<f6>") (make-lisp-command-sender "next"))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender "over"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender "out"))
  (message "<f5> step into  <f6> next       <f7> step over  <f8> continue"))

(defun allegro-debug-keys ()
  "Binds locally some keys to send allegro debugger commands to the inferior-lisp
<f5> step into
<f7> step over
<f8> continue
"
  (interactive)
  (local-set-key (kbd "<f5>") (make-lisp-command-sender ":scont 1"))
  ;; (local-set-key (kbd "<f6>") (make-lisp-command-sender ))
  (local-set-key (kbd "<f7>") (make-lisp-command-sender ":sover"))
  (local-set-key (kbd "<f8>") (make-lisp-command-sender ":continue"))
  (message "<f5> step into                  <f7> step over  <f8> continue"))



(loop for x in '(setf common-lisp-mode-hook      nil
                 inferior-lisp-load-hook    nil
                 inferior-lisp-mode-hook    nil
                 lisp-interaction-mode-hook nil
                 lisp-mode-hook             nil
                 comint-mode-hook           nil
                 comint-exec-hook           nil
                 ilisp-mode-hook            nil
                 scheme-mode-hook           nil)
   for i from 0
   when (oddp i)
   collect x)

(message (format  "hooks=%S" 
                  (mapcar (lambda (h)  (if (boundp h) (list h (symbol-value h)) (list h 'unbound)))
                          '(common-lisp-mode-hook inferior-lisp-load-hook inferior-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook comint-mode-hook comint-exec-hook ilisp-mode-hook scheme-mode-hook))))


(setf common-lisp-mode-hook      nil
      inferior-lisp-load-hook    nil
      inferior-lisp-mode-hook    nil
      lisp-interaction-mode-hook nil
      lisp-mode-hook             nil
      comint-mode-hook           nil
      comint-exec-hook           nil
      ilisp-mode-hook            nil
      scheme-mode-hook           nil)

(add-hook 'scheme-mode-hook      'pjb-lisp-meat)
(add-hook 'scheme-mode-hook
          (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
(add-hook 'lisp-mode-hook        'pjb-lisp-meat)
(add-hook 'common-lisp-mode-hook 'pjb-lisp-meat)
(add-hook 'emacs-lisp-mode-hook  'pjb-lisp-meat)
(add-hook 'emacs-lisp-mode-hook  'eldoc-mode)





;; slime-net-valid-coding-systems
;; (map nil (lambda (n)
;;            (format t "(~A-unix~VA~:[nil~;t  ~]  ~4:*:~A-unix)~%"
;;                    n (- 32 (length n)) ""
;;                    (ignore-errors (/= 1 (length (ext:convert-string-to-bytes
;;                                   "A" (ext:make-encoding :charset n)))))))
;;      (let ((l '()))
;;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))


;; swank-clisp.lisp find-encoding
;; (map nil (lambda (n) (format t "(:~A-unix~VA\"~A\")~%" n (- 32 (length n)) "" n))
;;      (let ((l '()))
;;        (do-external-symbols (s :charset) (push (string-downcase  s) l))
;;        (sort l (function string<=))))



;; This is about the easiest profiling I've seen in any language. In  
;; fact, I think it's the only time I been able to make significant  
;; improvements based on the report.  
;; 
;;     
;;     M-x slime-toggle-profile-fdefinition
;; 
;; on all the functions you want to  
;; profile, 
;;     
;;     M-x slime-profile-reset
;; 
;; to clear any existing data, and  
;; 
;;     
;;     M-x slime-profile-report
;; 
;; to see the report after running.  

(when (fboundp 'slime-repl-bol)
  (defvar *slime-repl-bol* (symbol-function 'slime-repl-bol))
  (defun slime-repl-bol ()
    (interactive)
    (if (eql 'home last-input-event)
        (beginning-of-buffer) 
        (funcall *slime-repl-bol*))))


;; (message (format ".EMACS:  Environment EMACS_INFERIOR_LISP = %S"
;;            (getenv "EMACS_INFERIOR_LISP")))
;; (setf (getenv "EMACS_INFERIOR_LISP")
;;       (or
;;        "inferior-lisp"
;;        (getenv "EMACS_INFERIOR_LISP")
;;        "inferior-lisp"
;;        "minimum-slime"
;;        "slime"
;;        "allegro-fi"
;;        "ILISP"))
;; ;; (setf (getenv "EMACS_INFERIOR_LISP") "slime")
;; (message (format ".EMACS:  Selected EMACS_INFERIOR_LISP = %S"
;;            (getenv "EMACS_INFERIOR_LISP")))

;; (progn
;;   (add-to-list 'load-path (get-directory :share-lisp "packages/net/common-lisp/slime/slime/"))
;;   (setf common-lisp-mode-hook      nil
;;         inferior-lisp-load-hook    nil
;;         inferior-lisp-mode-hook    nil
;;         lisp-interaction-mode-hook nil
;;         lisp-mode-hook             nil
;;         comint-mode-hook           nil
;;         comint-exec-hook           nil
;;         ilisp-mode-hook            nil
;;         scheme-mode-hook           nil)
;; 
;;   (add-hook 'scheme-mode-hook      (function pjb-lisp-meat))
;;   (add-hook 'scheme-mode-hook
;;             (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;   (add-hook 'lisp-mode-hook        (function pjb-lisp-meat))
;;   (add-hook 'common-lisp-mode-hook (function pjb-lisp-meat))
;;   (add-hook 'emacs-lisp-mode-hook  (function pjb-lisp-meat))
;;   
;;   ;; (list scheme-mode-hook lisp-mode-hook common-lisp-mode-hook emacs-lisp-mode-hook)
;; 
;;   (cond
;; 
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "allegro-fi")
;; ;;;----------------------------------------------------------------------------
;; ;;; ALLEGRO FI interface.
;;      (load "/usr/local/languages/acl80/eli/fi-site-init.el")
;;      (add-hook 'lisp-mode-hook
;;                (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;      (defun ficl-meat ()
;;        (sexp-movement))
;;      (add-hook 'fi:common-lisp-mode-hook 'ficl-meat))
;; 
;; 
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "inferior-lisp")
;;      (.EMACS "inferior-lisp")
;; ;;;----------------------------------------------------------------------------
;; ;;; INFERIOR-LISP
;;      (add-hook 'lisp-mode-hook
;;                (lambda () (local-set-key (kbd "C-x C-e") 'lisp-eval-last-sexp)))
;;      (setf inferior-lisp-mode-hook nil)
;; 
;; 
;;      (add-hook 'inferior-lisp-mode-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "inferior-lisp-mode-hook done.")))
;;      (add-hook 'comint-mode-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "comint-mode-hook done.")))
;;      (add-hook 'comint-exec-hook
;;                (lambda ()
;;                  (sexp-movement)
;;                  (.EMACS "comint-mode-hook done.")))
;;      );; inferior-lisp
;; 
;; 
;;     ((and (string-equal (getenv "EMACS_INFERIOR_LISP") "minimum-slime")
;;           (require 'slime nil t))
;;      (.EMACS "minimum-slime")
;; ;;;----------------------------------------------------------------------------
;; ;;; MINIMUM SLIME
;; ;;; site-lisp configuration for slime-cvs
;;      
;;      (slime-setup '(slime-repl))
;;      (setf slime-net-coding-system 'utf-8-unix)
;;      (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;      (setf slime-space-information-p t)
;;      (global-set-key (kbd "C-c s") 'slime-selector)
;;      ;; this prevents us from requiring the user get dev-lisp/hyperspec
;;      ;; (which is non-free) as a hard dependency
;; 
;;      
;;      (defun newline-and-lisp-indent (&rest rest)
;;        (interactive)
;;        (newline)
;;        (lisp-indent-line))
;;      (define-key slime-mode-map (kbd "RET") 'newline-and-lisp-indent)
;; 
;; 
;; 
;;      (defun inferior-lisp-buffer-name (name index)
;;        (format "*slime inferior-lisp %d%s*" index name))
;; 
;;      (defun slime-buffer-name (name index)
;;        (format "%d%s (slime)" index name))
;; 
;;      (defun get-next-buffer-name (name bnf)
;;        (let ((i 0))
;;          (while (get-buffer (funcall bnf name i)) (incf i))
;;          (funcall bnf name i)))
;; 
;;      (defun slime-repl-buffer (&optional create connection)
;;        "Get the REPL buffer for the current connection; optionally create."
;;        (funcall (if create
;;                     (function get-buffer-create)
;;                     (function get-buffer))
;;                 ;; (format "*slime-repl %s*" (slime-connection-name connection))
;;                 (get-next-buffer-name (slime-lisp-implementation-name connection)
;;                                       (function slime-buffer-name))))
;; 
;; 
;;      (defun slime (&optional command coding-system)
;;        "Start an inferior^_superior Lisp and connect to its Swank server."
;;        (interactive)
;;        (let* ((args (slime-read-interactive-args))
;;               (impl (get (getf args :name) :lisp-implementation)))
;;          (if impl
;;              (apply (function slime-start)
;;                     :buffer (get-next-buffer-name
;;                              (lisp-implementation-name impl)
;;                              (function inferior-lisp-buffer-name))
;;                     args)
;;              (apply (function slime-start) args))))
;;      ) ;; minimum-slime
;;     
;; 
;;     ((and (string-equal (getenv "EMACS_INFERIOR_LISP") "slime")
;;           (require 'slime nil t))
;;      (.EMACS "slime")
;; ;;;----------------------------------------------------------------------------
;; ;;; SLIME
;;      
;;      ;;(add-to-list 'load-path "/home/luke/slime")
;;      (require 'slime)
;;      (slime-setup '(slime-fancy slime-asdf slime-banner slime-repl))
;; 
     (add-hook 'lisp-mode-hook
               (lambda () (slime-mode t) (slime-autodoc-mode t)))
;;      ;;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;      ;; (modify-syntax-entry ?$ "'" lisp-mode-syntax-table)
;; 
;;      (define-key slime-mode-map (kbd "[") 'insert-parentheses)
;;      (define-key slime-mode-map (kbd "]") 'move-past-close-and-reindent)
;;      ;;(define-key slime-mode-map (kbd "(") (lambda () (interactive) (insert "[")))
;;      ;;(define-key slime-mode-map (kbd ")") (lambda () (interactive) (insert "]")))
;;      (define-key slime-mode-map (kbd "(") (function self-insert-command))
;;      (define-key slime-mode-map (kbd ")") (function self-insert-command))
;;      (define-key slime-mode-map (kbd "\e\[") (lambda () (interactive) (insert "(")))
;;      (define-key slime-mode-map (kbd "\e\]") (lambda () (interactive) (insert ")")))
;; 
;;           
;;      (defun slime-send-dwim (arg)
;;        "Send the appropriate forms to CL to be evaluated.
;; http://bc.tech.coop/blog/070424.html
;; "
;;        (interactive "P")
;;        (save-excursion
;;          (cond 
;;            ;;Region selected - evaluate region
;;            ((not (equal mark-active nil))
;;             (copy-region-as-kill-nomark (mark) (point)))
;;            ;; At/before sexp - evaluate next sexp
;;            ((or (looking-at "\s(")
;;                 (save-excursion
;;                   (ignore-errors (forward-char 1))
;;                   (looking-at "\s(")))
;;             (forward-list 1)
;;             (let ((end (point))
;;                   (beg (save-excursion
;;                          (backward-list 1)
;;                          (point))))
;;               (copy-region-as-kill-nomark beg end)))
;;            ;; At/after sexp - evaluate last sexp
;;            ((or (looking-at "\s)")
;;                 (save-excursion
;;                   (backward-char 1)
;;                   (looking-at "\s)")))
;;             (if (looking-at "\s)")
;;                 (forward-char 1))
;;             (let ((end (point))
;;                   (beg (save-excursion
;;                          (backward-list 1)
;;                          (point))))
;;               (copy-region-as-kill-nomark beg end)))
;;            ;; Default - evaluate enclosing top-level sexp
;;            (t (progn
;;                 (while (ignore-errors (progn
;;                                         (backward-up-list)
;;                                         t)))
;;                 (forward-list 1)
;;                 (let ((end (point))
;;                       (beg (save-excursion
;;                              (backward-list 1)
;;                              (point))))
;;                   (copy-region-as-kill-nomark beg end)))))
;;          (set-buffer (slime-output-buffer))
;;          (unless (eq (current-buffer) (window-buffer))
;;            (pop-to-buffer (current-buffer) t))
;;          (goto-char (point-max))
;;          (yank)
;;          (if arg (progn
;;                    (slime-repl-return)
;;                    (other-window 1)))))
;; 
;; 
;;      ;; (define-key lisp-mode-map [f7] 'slime-send-dwim)
;;      ;; (define-key lisp-mode-map [f8] (lambda ()
;;      ;;                                  (interactive)
;;      ;;                                  (slime-send-dwim 1)))
;; 
;;      
;;      (defun slime-version ()
;;        (interactive)
;;        (eval-in-cl "(swank-loader::slime-version-string)"
;;                    (lambda (values)
;;                      (if (null (cdr values))
;;                          (message (format "%s" v))
;;                          (dolist (v values)
;;                            (message (format "%s\n" v)))))))
;;   
;; 
;; 
;;      (defvar *pm* '() "process-marker alist")
;; 
;;      (defun pjb-slime-net-filter (process string)
;;        "Accept output from the socket and input all complete messages."
;;        (with-current-buffer (process-buffer process)
;;          (save-excursion
;;            (let ((pma (assoc process *pm*)))
;;              (when pma (goto-char (marker-position (cdr pma)))))
;;            (insert string))
;;          (slime-process-available-input)))
;; 
;; 
;;      (defun pjb-slime-eval-with-transcript (form &optional fn wait)
;;        "Send FROM and PACKAGE to Lisp and pass the result to FN.
;; Display the result in the message area, if FN is nil."
;;        (let* ((proc (slime-connection))
;;               (spb (process-buffer proc))
;;               (spf (process-filter proc)))
;;          (let ((pma (assoc proc *pm*))
;;                (m (let ((m (make-marker)))
;;                     (set-marker m (point) (current-buffer))
;;                     m)))
;;            (if pma
;;                (setf (cdr pma) m)
;;                (push (cons proc m) *pm*)))
;;          (set-process-buffer proc (current-buffer))
;;          (set-process-filter proc 'pjb-slime-net-filter)
;;          (unwind-protect (with-lexical-bindings (fn)
;;                            (slime-eval-async  form
;;                                               (lambda (value)
;;                                                 (cond (fn (funcall fn value))
;;                                                       (t (.EMACS "%s" value)))
;;                                                 (slime-show-last-output))))
;;            (set-process-buffer proc spb)
;;            (set-process-filter proc spf)
;;            (setf *pm* (delete (assoc proc *pm*) *pm*)))))
;; 
;; 
;;      ;;   (defun pjb-slime-eval-last-expression ()
;;      ;;     "Evaluate the expression preceding point."
;;      ;;     (interactive)
;;      ;;     (let* ((str  (slime-last-expression))
;;      ;;            (sexp (read-from-string str)))
;;      ;;       (if (and (listp sexp)
;;      ;;                (symbolp (fisrt sexp))
;;      ;;                (< 3 (LENGTH (SYMBOL-NAME (first sexp))))
;;      ;;                (STRING-EQUAL "DEF"  (SYMBOL-NAME (first sexp)) :end2 3))
;;      ;;         (slime-eval-last-expression str)
;;      ;;         (slime-eval-print-last-expression str))))
;; 
;; 
;;      (defun pjb-slime-eval-last-expression ()
;;        "Evaluate the expression preceding point."
;;        (interactive)
;;        (if buffer-read-only
;;            (slime-eval-last-expression)
;;            (let ((str  (slime-last-expression)))
;;              ;; (.EMACS "A DEF? %S" (STRING-EQUAL "(DEF"  str :end2 4))
;;              (if (string-equal* "(DEF"  str :end2 4)
;;                  (slime-interactive-eval str)
;;                  (slime-eval-print-last-expression str)))))
;; 
;; 
;;      (defun slime-restart-lisp-image ()
;;        (interactive)
;;        (when (slime-connected-p)
;;          (dolist (buf (buffer-list))
;;            (when (or (string= (buffer-name buf) slime-event-buffer-name)
;;                      (string-match "^\\*inferior-lisp*" (buffer-name buf)))
;;              (kill-buffer buf))))
;;        (call-interactively 'slime)) ;;slime-restart-lisp-image
;; 
;; 
;;      (defun pjb-slime-erase-buffer ()
;;        "Reset the slime output buffer to initial state."
;;        (interactive)
;;        (with-current-buffer (slime-output-buffer)
;;          (let ((inhibit-read-only t))
;;            (erase-buffer)
;;            (slime-repl-update-banner)))) ;;pjb-slime-erase-buffer
;; 
;; 
;;      (defun slime-kill ()
;;        (interactive)
;;        (map nil (lambda (x) (when (buffer-named x) (kill-buffer x)))
;;             '("*slime-repl[1]*" "*slime-events*" "*inferior-lisp*")))
;; 
;; 
;;      (defun slime-relaunch ()
;;        (interactive)
;;        (slime-kill)
;;        (sit-for 1)
;;        (slime)) ;;slime-relaunch
;; 
;;      (defalias 'slime-reload 'slime-relaunch)
;; 
;; 
;;      (defun pjb-slime-reset-minor-mode ()
;;        (dolist (slime-mode-vars '( slime-repl-read-mode
;;                                   slime-temp-buffer-mode
;;                                   inferior-slime-mode slime-mode))
;;          (setf minor-mode-map-alist (delete-if (lambda (x) (eq (car x) slime-mode-vars))
;;                                                minor-mode-map-alist)))
;;        ) ;;pjb-slime-reset-minor-mode
;; 
;; 
;;      (defvar *pjb-slime-keys-dynamic* nil)
;;      (defun pjb-slime-substitute-command (key command &rest keys)
;;        (unless  *pjb-slime-keys-dynamic*
;;          (setf slime-keys (mapcar (function copy-seq) (copy-seq slime-keys))
;;                *pjb-slime-keys-dynamic* t))
;;        (let ((prefixedp (cadr (member :prefixed keys)))
;;              (skeys slime-keys))
;;          (while skeys
;;            (when (and (string= key (first (car skeys)))
;;                       (equiv prefixedp (cadr (member :prefixed (car skeys)))))
;;              (setf (second (car skeys)) command
;;                    skeys nil))
;;            (pop skeys)))
;;        (pjb-slime-reset-minor-mode)
;;        (load "slime" *pjb-load-noerror* *pjb-load-silent*))
;; 
;; 
;;      ;; (pjb-slime-substitute-command "\M-." 'slime-edit-definition-other-window)
;; 
;;      ;; (pjb-slime-substitute-command "\C-e" 'pjb-slime-eval-last-expression
;;      ;;                               :prefixed t)
;; 
;; 
;;      (progn
;;        (define-key sldb-mode-map  "\M-."     'slime-edit-definition-other-window)
;;        (define-key slime-mode-map          "\C-ch"    'slime-hyperspec-lookup)
;;        (define-key inferior-slime-mode-map "\C-ch"    'slime-hyperspec-lookup)
;;        (define-key slime-mode-map  "\C-c\C-e" 'pjb-slime-eval-last-expression)
;;        (define-key slime-mode-map  "\C-x\C-e" 'pjb-slime-eval-last-expression)
;;        (define-key slime-mode-map          "\C-c\C-t" 'pjb-slime-erase-buffer)
;;        (define-key slime-mode-map          " "        'slime-space) ;'cl-magic-space)
;;        (define-key inferior-slime-mode-map "\C-c\C-t" 'pjb-slime-erase-buffer)
;;        )
;; 
;;      (defun slime-symbol-name-at-point ()
;;        "Return the name of the symbol at point, otherwise nil."
;;        (save-restriction
;;          ;; Don't be tricked into grabbing the REPL prompt.
;;          (when (and (eq major-mode 'slime-repl-mode)
;;                     (>= (point) slime-repl-input-start-mark))
;;            (narrow-to-region slime-repl-input-start-mark (point-max)))
;;          (save-excursion
;;            (skip-syntax-forward "w_")
;;            (skip-syntax-backward "-")
;;            (let ((string (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;                            (when bounds
;;                              (buffer-substring (car bounds)
;;                                                (progn
;;                                                  (goto-char (1- (cdr bounds)))
;;                                                  (if (looking-at "\\.\"")
;;                                                      (1- (cdr bounds))
;;                                                      (cdr bounds))))))))
;;              (and string
;;                   ;; In Emacs20 (thing-at-point 'symbol) returns "" instead
;;                   ;; of nil when called from an empty (or
;;                   ;; narrowed-to-empty) buffer.
;;                   (not (equal string ""))
;;                   (substring-no-properties   string)))))) ;;slime-symbol-name-at-point
;; 
;; 
;; 
;;      ;; (trace slime-init-keymaps  slime-init-keymaps  slime-define-key)
;;      ;; (trace pjb-slime-eval-last-expression)
;;      ;; (show (assoc "" slime-keys))
;; 
;;      (defun slime-hyperspec-lookup (symbol-name)
;;        "A wrapper for `hyperspec-lookup'"
;;        (interactive (list (let ((completion-ignore-case t)
;;                                 (symbol-at-point (slime-symbol-name-at-point)))
;;                             (if (and symbol-at-point
;;                                      (intern-soft (downcase symbol-at-point)
;;                                                   common-lisp-hyperspec-symbols))
;;                                 symbol-at-point
;;                                 (completing-read
;;                                  "Look up symbol in Common Lisp HyperSpec: "
;;                                  common-lisp-hyperspec-symbols #'boundp
;;                                  t symbol-at-point
;;                                  'common-lisp-hyperspec-history)))))
;;        (hyperspec-lookup symbol-name)) ;;slime-hyperspec-lookup
;; 
;;      ;; (setf sldb-hook nil)
;;      (add-hook 'sldb-hook (lambda () (toggle-truncate-lines 1)))
;; 
;; 
;;      (defun slime-macroexpand-in-place (&optional string)
;;        (interactive)
;;        (unless string
;;          (setf string (slime-sexp-at-point-or-error)))
;;        (lexical-let ((package (slime-current-package)))
;;          (insert (slime-eval `(swank:swank-macroexpand-1 ,string)))))
;;      
;;      ) ;;slime
;; 
;;     
;;     ((string-equal (getenv "EMACS_INFERIOR_LISP") "ILISP")
;;      (.EMACS "ilisp")
;; ;;;----------------------------------------------------------------------------
;; ;;; ILISP
;;      (require 'ilisp)
;; 
;;      (setq ilisp-*use-fsf-compliant-keybindings*  t)
;;      (setq ilisp-*use-frame-for-arglist-output-p* nil)
;;      (setq ilisp-*arglist-message-lisp-space-p*   nil)
;;      (setq ilisp-arglist-output                   nil)
;;      (setq ilisp-motd                             nil)
;;      (setq ilisp-defpackage-command-string
;;            "([Dd][Ee][Ff][-A-Za-z]*[Pp][Aa][Cc][Kk][Aa][Gg][Ee]  *\\([^ ][^ ]*\\)")
;;      ;; ;; (setq ilisp-hash-form-regexp "\\(^[ \t]*#[+-].\\)\\|\\(^[ \t]*(\\(.*::?\\)?\\(defpackage\\|define-package\\)[ \t\n]\\)\\|\\(^[ \t]*(\\(.*::?\\)?in-package[ \t\n]*\\)")
;; 
;; 
;;      ;; (setf ilisp-mode-hook nil lisp-mode-hook nil scheme-mode-hook nil clisp-hs-hook)
;;      (let ((hook  (lambda () (require 'ilisp))))
;;        (add-hook 'lisp-mode-hook   hook)
;;        (add-hook 'ilisp-mode-hook  hook)
;;        (add-hook 'scheme-mode-hook hook))
;; 
;;      ;;(lambda () (set-buffer-process-coding-system 'mule-utf-8 'mule-utf-8)))
;;      ;;(setf common-lisp-hook nil clisp-hs-hook nil)
;;      (add-hook 'ilisp-init-hook
;;                (lambda () (set-buffer-process-coding-system 'mule-utf-8 'mule-utf-8)))
;; 
;;      (defun ilisp-eval-region (start end)
;;        (interactive "r")
;;        (let* ((form (lisp-defun-region-and-name))
;;               (result
;;                (eval-region-lisp start end  'result
;;                                  (format "Evaluating %s" (car (cdr (cdr form)))))))
;;          (goto-char end)
;;          (lisp-display-output result))) ;;ilisp-eval-region
;; 
;; 
;;      (defun pjb-output-to-current-buffer (output ilisp-output-sink)
;;        "
;; This function is used to display the output from ilisp.
;; It's hooked by `ilisp-display-output-function'.
;; "
;;        (end-of-line)
;;        (insert (if (string-match "\n" output) "\n" "\n;;"))
;;        (insert output)
;;        (unless (string-match "\n" output) (goto-char 0)))
;; 
;;      (setq ilisp-display-output-function 'pjb-output-to-current-buffer)
;; 
;; 
;;      (defadvice  ilisp-display-output-adaptively
;;          (around pjb-ilisp-display-output-adaptively last
;;                  (output ilisp-output-sink) activate)
;;        "Always display output to the echo area: 21.2 can do with multiline strings"
;;        (ilisp-display-output-in-echo-area output ilisp-output-sink)
;;        ) ;;ilisp-display-output-adaptively
;;      (ad-activate 'ilisp-display-output-adaptively)
;;      ;;end ilisp
;;      )
;;     ))



;;;----------------------------------------------------------------------------
(.EMACS "emacs<->Common Lisp RPC with slime/swank")

;;; In emacs, we can execute Common Lisp expressions:

;; (require 'slime)
;; (slime)

(setf slime-enable-evaluate-in-emacs t) 

(defun eval-in-cl (cl-expression-string process-result-values)
  (slime-eval-with-transcript
   `(swank:eval-and-grab-output ,cl-expression-string)
   (lexical-let  ((here (current-buffer))
                  (process-result-values process-result-values))
     (lambda (result-values)
       (set-buffer here)
       (funcall process-result-values result-values)))))

;; (eval-in-cl "(values 1 * (ext:! 20) (package-name *package*))"
;;             (lambda (values)
;;               (dolist (v values)
;;                 (insert (format "%s\n" v)))))
;; Returns:
;;
;; nil
;;
;; then later inserts:
;;
;; 1
;; (42 (EMACS-UNREADABLE |buffer| |*scratch*|))
;; 2432902008176640000
;; "COMMON-LISP-USER"


;; ;;; In Common Lisp, we can execute emacs lisp expressions:
;; 
;; (defparameter *emacs-readtable* (copy-readtable))
;; (setf (readtable-case *emacs-readtable*) :preserve)
;; (set-syntax-from-char #\> #\) *emacs-readtable*)
;; (set-dispatch-macro-character
;;  #\# #\<
;;  (lambda (stream subchar dispchar)
;;    `(emacs-unreadable ,@(read-delimited-list #\> stream t)))
;;  *emacs-readtable*)
;; 
;; ;; Probably more readtable patching would be in order.
;; ;;
;; ;; We could define CLOS proxies for emacs objects for a more seamless
;; ;; integration. swank::eval-in-emacs process the CL form to make it
;; ;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; ;; to emacs lisp forms returning the corresponding emacs object.
;; 
;; (defun eval-in-emacs (form &optional nowait)
;;   (let ((result (SWANK::EVAL-IN-EMACS `(format "%S" ,form) nowait))
;;         (*readtable* *emacs-readtable*))
;;     (with-input-from-string (in result)
;;       (let ((result (read in nil in)))
;;         result))))
;; 
;; 
;; (eval-in-emacs `(progn
;;                   (switch-to-buffer (buffer-named "*scratch*"))
;;                   (goto-char (point-max))
;;                   (insert ,(format nil "~%Hello~%"))
;;                   (list 42 (current-buffer))))
;; 
;; ;; Switch to the *scratch* buffer,
;; ;; goto the last position, and
;; ;; inserts \nHello\n
;; ;; then returns:
;; ;; (42 (EMACS-UNREADABLE |buffer| |*scratch*|))

