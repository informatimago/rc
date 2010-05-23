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

(setf swank:*use-dedicated-output-stream*  nil)
(setf swank::*sldb-initial-frames*         40)
;;;; .swank.lisp                      --                     --          ;;;;
