(in-package :swank)

;; swank:*communication-style*          (member nil :fd-handler :sigio :spawn)
;; swank:*configure-emacs-indentation*  boolean t
;; swank:*globally-redirect-io*         boolean nil
;; swank:*global-debugger*              boolean t
;; swank:*sldb-printer-bindings*        list
;; swank:*macroexpand-printer-bindings* list
;; swank:*swank-pprint-bindings*        list
;; swank:*use-dedicated-output-stream*  boolean t
;; swank:*log-events*                   boolean nil

;; When there's a NAT: (setf swank:*use-dedicated-output-stream* nil)
;; (or with clisp).

#+clisp (defparameter swank:*use-dedicated-output-stream*  nil)

(defparameter swank::*sldb-initial-frames* 40)
(defparameter swank:*globally-redirect-io* t)

(let ((bindings '((*PRINT-PRETTY* . nil)
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
                  (*PRINT-RIGHT-MARGIN* . 1000)
                  (*SLDB-BITVECTOR-LENGTH* . nil)
                  (*SLDB-STRING-LENGTH* . nil)))
      (variables '(SWANK:*MACROEXPAND-PRINTER-BINDINGS*
                   SWANK::*INSPECTOR-VERBOSE-PRINTER-BINDINGS*
                   SWANK::*INSPECTOR-PRINTER-BINDINGS*
                   swank:*backtrace-printer-bindings*
                   #+#.(cl:if (cl:find-symbol "*SLDB-PRINTER-BINDINGS*" "SWANK")
                            '(:and) '(:or))
                   swank:*sldb-printer-bindings*)))
  (dolist (var variables)
    (set var bindings)))


;;;; THE END ;;;;
