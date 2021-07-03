(in-package :swank)

;; swank:*communication-style*          (member nil :fd-handler :sigio :spawn)
;; swank:*configure-emacs-indentation*  boolean t
;; swank:*globally-redirect-io*         boolean nil
;; swank:*global-debugger*              boolean t
;; swank:*macroexpand-printer-bindings* list
;; swank:*swank-pprint-bindings*        list
;; swank:*use-dedicated-output-stream*  boolean t
;; swank:*log-events*                   boolean nil

;; When there's a NAT: (setf swank:*use-dedicated-output-stream* nil)
;; (or with clisp).
;;
;; (list  swank:*communication-style* swank:*use-dedicated-output-stream* swank:*globally-redirect-io*)

(defparameter *dont-close* t)

(defparameter swank::*asdf-path* (merge-pathnames #p"quicklisp/asdf.lisp" (user-homedir-pathname)))

#+clisp (defparameter swank:*use-dedicated-output-stream*  nil)
#-clisp (defvar       swank:*use-dedicated-output-stream*  nil)

(defvar swank:*communication-style* nil)

(defparameter swank::*sldb-initial-frames*             40)
(defparameter swank::*auto-abbreviate-dotted-packages* t)
(defparameter swank:*globally-redirect-io*             nil)

(defvar *swank-bindings*
  `((*PRINT-PRETTY* . nil)
    (*PRINT-LEVEL* . nil)
    (*PRINT-LENGTH* . nil)
    (*PRINT-CIRCLE* . T)
    (*PRINT-CASE* . :downcase)
    (*PRINT-READABLY*)
    (*PRINT-GENSYM* . T)
    (*PRINT-BASE* . 10.)
    (*PRINT-RADIX* . nil)
    (*PRINT-ARRAY* . T)
    (*PRINT-LINES* . nil)
    (*PRINT-ESCAPE* . T)
    (*PRINT-RIGHT-MARGIN* . 120)
    (*random-state* . ,(make-random-state t))
    (*SLDB-BITVECTOR-LENGTH* . nil)
    (*SLDB-STRING-LENGTH* . nil)))


(defparameter *swank-bindings-code*
  `((*PRINT-PRETTY* . nil)
    (*PRINT-LEVEL* . nil)
    (*PRINT-LENGTH* . nil)
    (*PRINT-CIRCLE* . nil)
    (*PRINT-CASE* . :downcase)
    (*PRINT-READABLY*)
    (*PRINT-GENSYM* . T)
    (*PRINT-BASE* . 10.)
    (*PRINT-RADIX* . nil)
    (*PRINT-ARRAY* . T)
    (*PRINT-LINES* . nil)
    (*PRINT-ESCAPE* . T)
    (*PRINT-RIGHT-MARGIN* . 120)
    (*random-state* . ,(make-random-state t))
    (*SLDB-BITVECTOR-LENGTH* . nil)
    (*SLDB-STRING-LENGTH* . nil)))


(defparameter *swank-bindings-data*
  `((*PRINT-PRETTY* . nil)
    (*PRINT-LEVEL* . 4)
    (*PRINT-LENGTH* . 20)
    (*PRINT-CIRCLE* . T)
    (*PRINT-CASE* . :downcase)
    (*PRINT-READABLY*)
    (*PRINT-GENSYM* . T)
    (*PRINT-BASE* . 10.)
    (*PRINT-RADIX* . nil)
    (*PRINT-ARRAY* . T)
    (*PRINT-LINES* . nil)
    (*PRINT-ESCAPE* . T)
    (*PRINT-RIGHT-MARGIN* . 110)
    (*random-state* . ,(make-random-state t))
    (*SLDB-BITVECTOR-LENGTH* . 64)
    (*SLDB-STRING-LENGTH* . 64)))


(defun set-swank-binding (variable value)
  (let ((bindings (copy-tree *swank-bindings*)))
    (setf (cdr (assoc variable bindings)) value)
    (set-swank-bindings bindings)))

(defun set-swank-bindings (&optional (*swank-bindings* *swank-bindings*))
  (let ((variables '(swank:*default-worker-thread-bindings*
                     SWANK:*MACROEXPAND-PRINTER-BINDINGS*
                     SWANK::*INSPECTOR-VERBOSE-PRINTER-BINDINGS*
                     SWANK::*INSPECTOR-PRINTER-BINDINGS*
                     swank::*backtrace-printer-bindings*
                     #+#.(cl:if (cl:find-symbol "*SLDB-PRINTER-BINDINGS*" "SWANK")
                                '(:and) '(:or))
                     swank:*sldb-printer-bindings*)))
    (dolist (var variables)
      (set var *swank-bindings*))))

(defun set-swank-bindings-code () (set-swank-bindings *swank-bindings-code*))
(defun set-swank-bindings-data () (set-swank-bindings *swank-bindings-data*))
(set-swank-bindings-code)
;; (set-swank-bindings)
;; (set-swank-bindings-data)
#-(and) (
         (set-swank-bindings *swank-bindings*)
         (set-swank-bindings *swank-bindings-code*)
         (set-swank-bindings *swank-bindings-data*)

         (swank::set-swank-binding '*print-circle* nil))
#-(and) (set-swank-bindings   `((*PRINT-PRETTY* . nil)
                                (*PRINT-LEVEL* . nil)
                                (*PRINT-LENGTH* . nil)
                                (*PRINT-CIRCLE* . T)
                                (*PRINT-CASE* . :upcase)
                                (*PRINT-READABLY*)
                                (*PRINT-GENSYM* . T)
                                (*PRINT-BASE* . 10.)
                                (*PRINT-RADIX* . nil)
                                (*PRINT-ARRAY* . T)
                                (*PRINT-LINES* . nil)
                                (*PRINT-ESCAPE* . T)
                                (*PRINT-RIGHT-MARGIN* . 120)
                                (*random-state* . ,(make-random-state t))
                                (*SLDB-BITVECTOR-LENGTH* . nil)
                                (*SLDB-STRING-LENGTH* . nil)))

;; Before saving a core with swank, if slime uses options:
;; (swank:swank-require '(swank-repl swank-asdf swank-fuzzy swank-indentation swank-media))
;; otherwise swank tries to load them from source files after launching the core.


(defslimefun eval-and-grab-output-and-error (string)
  (with-buffer-syntax ()
    (let* ((s (make-string-output-stream))
           (*standard-output* s)
           (values (handler-case
                       (multiple-value-list (eval (from-string string)))
                     (error (err)
                       (format t "~&ERROR: ~A" err)
                       (values)))))
      (list (get-output-stream-string s)
            (format nil "~{~S~^~%~}" values)))))


;;;; THE END ;;;;
