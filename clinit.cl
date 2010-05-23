;;; $Header: /cvs/cl/sample-files/dot-clinit.cl,v 1.6 1997/02/03 22:33:43 layer Exp $
;;;
;;; This file contains examples of potentially interesting user
;;; customizations which can be done via a $HOME/.clinit.cl.

(format *terminal-io* "~%; Loading home ~a~@[.~a~] file.~%"
	(pathname-name *load-pathname*)
	(pathname-type *load-pathname*))

;;; Set a few top-level variables.
(tpl:setq-default top-level:*history* 50)
(tpl:setq-default top-level:*print-length* nil)
(tpl:setq-default top-level:*print-level* nil)
(tpl:setq-default top-level:*zoom-print-level* nil)
(tpl:setq-default top-level:*zoom-print-length* nil)
(tpl:setq-default top-level:*exit-on-eof* t)

;;; Display 10 frames on :zoom,
(tpl:setq-default top-level:*zoom-display* 10)
;;; and don't print anything but the current frame on :dn, :up and :find
(tpl:setq-default top-level:*auto-zoom* :current)

;;; Have the garbage collector print interesting stats.
(setf (sys:gsgc-switch :print) t)
(setf (sys:gsgc-switch :stats) t)

;;; To have all advice automatically compiled.
(tpl:setq-default *compile-advice* t)

;;; Have packages print with their shortest nickname instead of the package
;;; name.
(tpl:setq-default *print-nickname* t)

;;; Allow concise printing of shared structure.
(tpl:setq-default *print-circle* t)

;;; Only print "Compiling" messages for files, not for individual functions,
;;; unless there is a warning or error.
(tpl:setq-default *compile-verbose* t)
(tpl:setq-default *compile-print* nil)

;;; Set up a top-level alias.
(top-level:alias ("shell" 1 :case-sensitive) (&rest args)
  "`:sh args' will execute the shell command in `args'"
  (let ((cmd 
         (apply #'concatenate 'simple-string
                (mapcar #'(lambda (x)
                            (concatenate 'simple-string
                              (write-to-string x :escape nil) " "))
                        args))))
    (prin1 (shell cmd))))

;;; The following makes the source file recording facility compare only the
;;; names of pathnames, for the purposes of determining when a redefinition
;;; warning should be issued.
(push #'(lambda (old new fspec type)
	  (when (and old new)
	    (string= (pathname-name old) (pathname-name new))))
      *redefinition-pathname-comparison-hook*)

;;; Use the Composer package if it is available.
(eval-when (eval compile load)
  (when (find-package :wt)
    (use-package :wt)))

(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       (USER-HOMEDIR-PATHNAME)
       NIL))

;;;; .clinit.cl                       -- 2004-03-06 07:55:46 -- pascal   ;;;;
