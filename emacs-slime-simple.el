;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               emacs-slime-simple.el
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

(.EMACS "emacs-slime-simple.el")


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
               slime-media))


(setf slime-net-coding-system        'utf-8-unix)
(setf slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(pushnew 'paredit-mode slime-repl-mode-hook)


(when (fboundp 'slime-repl-bol)
  (defvar *slime-repl-bol* (symbol-function 'slime-repl-bol))
  (defun slime-repl-bol ()
    (interactive)
    (if (eql 'home last-input-event)
        (beginning-of-buffer) 
        (funcall *slime-repl-bol*))))


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



 (progn

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
              "-ansi""-q"               ;"-m""32M""-I""-K""full"
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
       (parse-integer (match-string 1 buffer-name))))
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






(defun lisp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))


(appendf interpreter-mode-alist '(("sbcl"  . lisp-mode)
                                  ("abcl"  . lisp-mode)
                                  ("gcl"   . lisp-mode)
                                  ("ecl"   . lisp-mode)
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
  
  ;; (local-set-key (kbd "C-x C-e")     'lisp-eval-last-sexp)
  
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






;;;
;;; Hooks
;;;

(setf common-lisp-mode-hook      nil
      inferior-lisp-load-hook    nil
      inferior-lisp-mode-hook    nil
      lisp-interaction-mode-hook nil
      lisp-mode-hook             nil
      comint-mode-hook           nil
      comint-exec-hook           nil
      ilisp-mode-hook            nil
      scheme-mode-hook           nil)

(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'slime-autodoc-mode)

(add-hook 'scheme-mode-hook      'pjb-lisp-meat)

(add-hook 'lisp-mode-hook        'pjb-lisp-meat)
(add-hook 'common-lisp-mode-hook 'pjb-lisp-meat)
(add-hook 'emacs-lisp-mode-hook  'pjb-lisp-meat)
(add-hook 'emacs-lisp-mode-hook  'eldoc-mode)


(dolist (h '(common-lisp-mode-hook inferior-lisp-load-hook inferior-lisp-mode-hook
             lisp-interaction-mode-hook lisp-mode-hook comint-mode-hook
             comint-exec-hook ilisp-mode-hook scheme-mode-hook))
  (message "%-30s %S"  h (if (boundp h)
                             (symbol-value h)
                             'unbound)))



;;;----------------------------------------------------------------------------
(.EMACS "emacs<->Common Lisp RPC with slime/swank")

;;; In emacs, we can execute Common Lisp expressions:

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

