;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               clinit.cl
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    This is the Allegro Common Lisp initialization file.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-09-11 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;  
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;  
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;  
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;  
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
;;;;  

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




;; ;; (|CL|:|SETF| |CUSTOM|:|*LOAD-ECHO*| |CL|:|T|)
;; (IN-PACKAGE "COMMON-LISP-USER")
;; 
;; ;;----------------------------------------------------------------------
;; ;; Setting environment -- Allegro Lisp specific --
;; ;; -----------------------------------------------
;; 
;; 
;; (SETF *print-length*                  nil
;;       *print-level*                   nil
;;       EXCL:*DEFAULT-EXTERNAL-FORMAT*  :utf-8
;;       EXCL:*STEP-PRINT-LENGTH*        nil
;;       EXCL:*STEP-PRINT-LEVEL*         6
;;       EXCL:*TRACE-PRINT-LENGTH*       nil
;;       EXCL:*TRACE-PRINT-LEVEL*        nil
;;       INSPECT::*INSPECT-LENGTH*       MOST-POSITIVE-FIXNUM
;;       INSPECT::*INSPECT-RAW*          NIL
;;       INSPECT::*INSPECT-LEVEL*        1
;;       TOP-LEVEL:*ZOOM-DISPLAY*        50
;;       TOP-LEVEL:*PRINT-LENGTH*        nil
;;       TOP-LEVEL:*PRINT-LEVEL*         20
;;       TOP-LEVEL:*ZOOM-PRINT-LENGTH*   nil
;;       TOP-LEVEL:*ZOOM-PRINT-LEVEL*    6
;;       TOP-LEVEL:*HELP-PAGE-LENGTH*    MOST-POSITIVE-FIXNUM
;;       TOP-LEVEL:*TIME-THRESHOLD*      5.0
;;       TOP-LEVEL:*COMMAND-CHAR*        #\: ; #\,
;;       TOP-LEVEL:*prompt*              top-level:*prompt*
;;       top-level:*reset-hook*          (lambda ()
;;                                           (format t "~%~78,,78,'-A~%" "")
;;                                           (excl:run-shell-command "fortune")
;;                                           (format t "~&~78,,78,'-A~2%" "")))
;; 
;; 
;; (defun set-prompt-label (label)
;;   "Add a fixed label to the prompt, to identify the lisp instance.
;; RETURN: LABEL"
;;   (setf top-level:*prompt*
;;         (format nil "~~&~~
;; ~~@[[Current process: ~~a]~~%~~]~~
;; ~~@[[Current process focus: ~~a]~~%~~]~~
;; ~~:[~~
;;      ~~:[~~
;;         ~~2*~~
;;      ~~;~~
;;         [~~:*~~d~~:[~~;c~~]~~:[~~;i~~]] ~~
;;      ~~]~~
;; ~~;~~
;;      [~~:*~~a] ~~3*~~
;; ~~]~~
;; ~:[~;~:*~A/~]~~a(~~d): " label))
;;   label)
;; 
;; 
;; #- (and)
;; (let ((*terminal-io* (make-broadcast-stream)))
;;    (TOP-LEVEL:do-command "zoom" :verbose t :all t)
;;    (princ #\newline)
;;    (values))
;; 
;; ;; clisp like aliases:
;; (top-level:alias "a"    ()                      "pop"      (TOP-LEVEL:do-command "pop"))
;; (top-level:alias "q"    ()                      "reset"    (TOP-LEVEL:do-command "reset"))
;; (top-level:alias "c"    (&optional (restart 0)) "continue" (TOP-LEVEL:do-command "continue" restart))
;; (top-level:alias "u"    (&optional (count 1))   "up"       (TOP-LEVEL:do-command "up" count))
;; (top-level:alias "d"    (&optional (count 1))   "dn"       (TOP-LEVEL:do-command "dn" count))
;; (top-level:alias "down" (&optional (count 1))   "dn"       (TOP-LEVEL:do-command "dn" count))
;; 
;; (top-level:alias "dl"   (&optional (count 1))
;;   "dn + local"
;;   (TOP-LEVEL:do-command "dn" count)
;;   (TOP-LEVEL:do-command "local"))
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Debugging stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;; 
;; ;;; A list with a cursor:
;; 
;; (defun make-malcolm (&key above below)
;;   "
;; RETURN: A new Malcolm in the Middle.
;; ABOVE:  If given, it's a reversed list of the elements above (bottom-most first).
;; BELOW:  A list of the elements below (top-most first).
;; NOTE:   The middle is taken as the top-most below.
;; "
;;   (cons above below))
;; 
;; (defun malcolm-above         (malcolm) (car malcolm))
;; (defun malcolm-below         (malcolm) (cdr malcolm))
;; (defun copy-malcolm          (malcolm) (cons (car malcolm) (cdr malcolm)))
;; (defun malcolm-at-top-p      (malcolm) (null (car malcolm)))
;; (defun malcolm-at-bottom-p   (malcolm) (null (cdr malcolm)))
;; (defun malcolm-in-the-middle (malcolm) (cadr malcolm))
;; 
;; 
;; (defun malcolm-move-up (malcolm)
;;   "
;; DO:     Modify the malcolm moving up (unless it's already at the top)
;; RETURN: MALCOLM.
;; "
;;   (unless (malcolm-at-top-p malcolm)
;;     (setf (cdr malcolm) (cons (caar malcolm) (cdr malcolm))
;;           (car malcolm) (cdar malcolm)))
;;   malcolm)
;; 
;; 
;; (defun malcolm-move-down (malcolm)
;;   "
;; DO:     Modify the malcolm moving down (unless it's already at the bottom).
;; RETURN: MALCOLM.
;; "
;;   (unless (malcolm-at-bottom-p malcolm)
;;     (setf (car malcolm) (cons (cadr malcolm) (car malcolm))
;;           (cdr malcolm) (cddr malcolm)))
;;   malcolm)
;; 
;; 
;; 
;; ;;; Getting the stack frames:
;; 
;; (require :sundebug)
;; 
;; (defun list-stack-frames (&key from-end only-interesting
;;                           (process (function identity)))
;;   (macrolet ((with-functions (flist &body body)
;;                (let ((vargs (gensym)))
;;                  `(flet ,(mapcar (lambda (fun) `(,fun (&rest ,vargs)
;;                                             (apply ,fun ,vargs)))
;;                                  flist)
;;                     ,@body))))
;;       (flet ((list-frames (head next lastp filter)
;;                (with-functions (head next lastp filter process)
;;                  (loop
;;                   :for frame = (head)
;;                   :then (next frame)
;;                   :until (lastp frame)
;;                   :when (and (debug:frame-reference-p frame) (filter frame))
;;                   :collect (process frame)))))
;;         (if from-end
;;           (list-frames (function debug:oldest-frame)
;;                        (function debug:next-newer-frame)
;;                        (function null)
;;                        (if only-interesting
;;                          (function debug:frame-visible-p)
;;                          (function identity)))
;;           (list-frames (function debug:newest-frame)
;;                        (function debug:next-older-frame)
;;                        (function null)
;;                        (if only-interesting
;;                          (function debug:frame-visible-p)
;;                          (function identity)))))))
;; 
;; 
;; ;; (LIST-STACK-FRAMES :process  (lambda (f) (ignore-errors (debug:frame-name f))))
;; 
;; 
;; (defmacro handling-errors (&body body)
;;   `(HANDLER-CASE (progn ,@body)
;;      (simple-condition 
;;          (ERR) 
;;        (format *error-output* "~%ERROR: ~A~%" err)
;;        (finish-output *error-output*))
;;      (condition 
;;          (ERR) 
;;        (format *error-output* "~%ERROR: ~A~%       ~A~%"
;;                (class-name (class-of err)) err)
;;        (finish-output *error-output*))))
;; 
;; 
;; ;;; A REPL to inspect stack frames:
;; 
;; (defparameter *whitespaces*
;;   #(#\space #\newline #\tab #\return #\linefeed #\vt #\ff)
;;   "White spaces.")
;; 
;; 
;; (defparameter *current-frame* nil
;;   "IDEBUG binds it to its current frame (set with :top, :up, :down, :bottom).")
;; 
;; 
;; (defun idebug ()
;;   (let ((frames
;;          (make-malcolm
;;           :below (rest
;;                   (member
;;                    'idebug
;;                    (LIST-STACK-FRAMES
;;                     :process (lambda (f)
;;                                  (list
;;                                   f
;;                                   (ignore-errors (debug:frame-type f))
;;                                   (ignore-errors (debug:frame-name f))
;;                                   (ignore-errors (debug:frame-arglist f)))))
;;                    :key (function third)))))
;;         (all-frames nil)
;;         (+eof+ (gensym)))
;;     (flet ((toggle-filter (arg)
;;              (if all-frames
;;                ;; find the current frame in the all-frames malcolm
;;                (let ((p (position (malcolm-in-the-middle frames)
;;                                   (malcolm-below all-frames))))
;;                  (if p
;;                    (dotimes (i p) (malcolm-move-down all-frames))
;;                    (dotimes (i (position (malcolm-in-the-middle frames)
;;                                          (malcolm-above all-frames)))
;;                      (malcolm-move-up all-frames)))
;;                  (setf frames all-frames
;;                        all-frames nil)
;;                  (when arg (toggle-filter arg)))
;;                (progn
;;                  (setf all-frames frames
;;                        frames(make-malcolm
;;                         :above (remove-if
;;                                 (if arg
;;                                   (lambda (x) (not (member arg x)))
;;                                   (lambda (x) (member :unknown x)))
;;                                 (malcolm-above frames))
;;                         :below (remove-if
;;                                 (if arg
;;                                   (lambda (x) (not (member arg x)))
;;                                   (lambda (x) (member :unknown x)))
;;                                 (malcolm-below frames))))
;;                  (when (malcolm-at-bottom-p frames)
;;                    ;; Assume there is always at least one interesting frame...
;;                    (malcolm-move-up frames)))))
;;            (print-frame (f &optional (n 0))
;;              (format *debug-io* "~%~3D ~A ~20S ~S"
;;                      n
;;                      (case (second f)
;;                        ((:function)    "F")
;;                        ((:eval)        "E")
;;                        ((:interpreter) "I")
;;                        ((:binding)     "B")
;;                        ((:catch)       "C")
;;                        ((:special-form :special-operator) "S")
;;                        ((nil)          " ")
;;                        (otherwise      (second f)))
;;                      (third f)
;;                      (fourth f)))
;;            (read-arg (stream &optional default)
;;              (let ((line (and (listen stream) (read-line stream nil +eof+))))
;;                (cond
;;                  ((null line)                                   default)
;;                  ((eq line +eof+)                                 +eof+)
;;                  ((string= "" (string-trim *whitespaces* line)) default)
;;                  (t                          (read-from-string line)))))
;;            (integer-or-1 (x) (if (integerp x) x 1)))
;;       #- (and)
;;       (loop
;;        :until (or (malcolm-at-bottom-p frames)
;;                   (eq 'idebug (third (first (malcolm-in-the-middle frames)))))
;;        :do (malcolm-move-down frames)
;;        :finally (when (malcolm-at-bottom-p frames)
;;                   (loop :until (malcolm-at-top-p frames)
;;                         :do (malcolm-move-up frames))))
;;       (do ((hist 1 (1+ hist)))
;;           (nil)
;;         (format *debug-io* "~%IDEBUG Current frame:")
;;         (print-frame (malcolm-in-the-middle frames))
;;         (format *debug-io* "~%IDEBUG/~A[~D]> "
;;                 (first (sort (copy-seq
;;                               (cons (package-name *package*)
;;                                     (package-nicknames *package*)))
;;                              (function <) :key (function length)))
;;                 hist)
;;         (finish-output *debug-io*)
;;         (handling-errors
;;          (let ((*current-frame* (first (malcolm-in-the-middle frames))))
;;            (setf - (read *debug-io* nil +eof+))
;;            (cond
;;              ((or (eq - +eof+)
;;                   (member - '(:q :a :quit :exit :done :continue
;;                               (quit)(exit)(continue))
;;                    :test (function equal)))
;;               (return-from idebug))
;;              ((member - '(:u :up))
;;               (dotimes (n (integer-or-1 (read-arg *debug-io* 1)))
;;                 (if (malcolm-at-top-p frames)
;;                   (progn
;;                     (format *debug-io* "~&Already at the top of the frame stack.~%")
;;                     (return))
;;                   (malcolm-move-up frames))))
;;              ((member - '(:d :do :down))
;;               (dotimes (n (integer-or-1 (read-arg *debug-io* 1)))
;;                 (malcolm-move-down frames)
;;                 (when (malcolm-at-bottom-p frames)
;;                   (format *debug-io* "~&Already at the bottom of the frame stack.~%")
;;                   (malcolm-move-up frames)
;;                   (return))))
;;              ((member - '(:t :to :top))
;;               (loop :until (malcolm-at-top-p frames)
;;                     :do (malcolm-move-up frames)))
;;              ((member - '(:b :bo :bottom))
;;               (loop :until (malcolm-at-bottom-p frames)
;;                     :do (malcolm-move-down frames)
;;                     :finally (malcolm-move-up frames)))
;;              ((member - '(:z :zo :zoom :bt :bl))
;;               (let ((n (read-arg *debug-io*))
;;                     (f (copy-malcolm frames)))
;;                 (if (integerp n)
;;                   (dotimes (i n)
;;                     (print-frame (malcolm-in-the-middle f) i)
;;                     (malcolm-move-down f))
;;                   (loop
;;                    :for i :from 0
;;                    :until (malcolm-at-bottom-p f)
;;                    :do (print-frame (malcolm-in-the-middle f) i)
;;                    :do (malcolm-move-down f)))))
;;              ((member - '(:f :fi :filter))
;;               (toggle-filter (read-arg *debug-io* nil)))
;;              ((member - '(? :h :he :help))
;;               (format *debug-io* "~%IDEBUG commands: ~
;;                                 ~:{~%   ~:[~;~:*~{~S~^ | ~}~]  ~:[~;[~:*~A]~]~
;;                                   ~%   ~A~%~}~%"
;;                       '(((:q :a :quit :exit :done :continue (quit)(exit)(continue))
;;                          nil "Exits from the IDEBUG REPL.")
;;                         ((:t :to :top)
;;                          nil "Goes to the top of the frame stack.")
;;                         ((:u :up)
;;                          "number-of-frames"
;;                          "Moves up in the frame stack.")
;;                         ((:d :do :down)
;;                          "number-of-frames"
;;                          "Moves down in the frame stack.")
;;                         ((:b :bo :bottom)
;;                          nil "Goes to the bottom of the frame stack.")
;;                         ((:z :zo :zoom :bt :bl)
;;                          "number-of-frames"
;;                          "Prints all the (or at most NUMBER-OF-FRAMES frames
;;                           below the current one.")
;;                         ((:f :fi :filter)
;;                          ":FUNCTION | :EVAL | :SPECIAL-OPERATOR | :UNKNOWN"
;;                          "Toggle filtering of :UNKNOWN stack frames.")
;;                         (("any other form")
;;                          nil
;;                          "is evaluated in the current frame environment."))))
;;              (t
;;               (let ((results (multiple-value-list
;;                               (debug:eval-form-in-context
;;                                -
;;                                 (debug:environment-of-frame
;;                                        (first (malcolm-in-the-middle frames)))))))
;;                 (shiftf +++ ++ + -)
;;                 (shiftf /// // / results)
;;                 (shiftf *** ** * (first /)))
;;               (format *debug-io* "~& --> ~{~S~^ ;~%     ~}~%" /))))
;;          (finish-output *debug-io*))))))
;; 
;; 
;; (setf *debugger-hook*
;;       (lambda (condition debugger-hook)
;;           (format *debug-io* "~2%:focus '~S~2%"
;;                   (mp:process-name mp:*current-process*))
;;         (values)))
;; 
;; 
;; 
;; 
;; 
;; 
;; ;; (defun clisp-version (&optional (version-string (LISP-IMPLEMENTATION-VERSION)))
;; ;;   (loop
;; ;;      :with r = '()
;; ;;      :with start = 0
;; ;;      :do (multiple-value-bind (n p)
;; ;;              (parse-integer version-string :start start :junk-allowed t)
;; ;;            (push n r)
;; ;;            (if (or (<= (length version-string) p)
;; ;;                    (char= #\space (aref version-string p)))
;; ;;                (return-from clisp-version (nreverse r))
;; ;;                (setf start (1+ p))))))
;; ;; 
;; ;; (defun version= (a b)
;; ;;   (equal (if (stringp a) (clisp-version a) a)
;; ;;          (if (stringp b) (clisp-version b) b)))
;; ;; 
;; ;; (defun version< (a b)
;; ;;   (setf a (if (stringp a) (clisp-version a) a)
;; ;;         b (if (stringp b) (clisp-version b) b))
;; ;;   (cond
;; ;;     ((null a)            (not (null b)))
;; ;;     ((null b)            nil)
;; ;;     ((< (car a) (car b)) t)
;; ;;     ((= (car a) (car b)) (version< (cdr a) (cdr b)))
;; ;;     (t                   nil)))
;; ;; 
;; ;; (defun version<= (a b)
;; ;;   (setf a (if (stringp a) (clisp-version a) a)
;; ;;         b (if (stringp b) (clisp-version b) b))
;; ;;   (or (version= a b) (version< a b)))
;; ;; 
;; ;; (defun rt-version=  (a b) (if (version=  a b) '(and) '(or)))
;; ;; (defun rt-version<  (a b) (if (version<  a b) '(and) '(or)))
;; ;; (defun rt-version<= (a b) (if (version<= a b) '(and) '(or)))
;; ;; 
;; ;; (export '(clisp-version
;; ;;           version=    version<    version<=
;; ;;           rt-version= rt-version< rt-version<=))
;; ;; 
;; ;; 
;; ;; (let ((counter 0))
;; ;;   (defun prompt-body ()
;; ;;     (format nil "~A[~D]"
;; ;;             (if (packagep *package*)
;; ;;                 (first (sort (cons (package-name *package*)
;; ;;                                    (package-nicknames *package*))
;; ;;                              (function <=) :key (function length)))
;; ;;                 "#<INVALID *PACKAGE*>")
;; ;;             (incf counter))))
;; 
;; 
;;    
;; ;;----------------------------------------------------------------------
;; ;; Setting environment -- clisp part --
;; ;; ------------------------------------
;; 
;; (DEFUN X-POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS (HOST)
;;   (FLET ((PSTRING (X) (IF (PATHNAMEP X) (NAMESTRING X) (STRING X))))
;;     (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
;;           (SORT
;;            (REMOVE-IF
;;             (LAMBDA (P) (LET ((P (PSTRING P)))
;;                      (AND (< 5 (LENGTH P))
;;                           (STRING= "*.*.*" P :START2 (- (LENGTH P) 5)))))
;;             (LOGICAL-PATHNAME-TRANSLATIONS HOST)
;;             :KEY (FUNCTION FIRST))
;;                 (LAMBDA (A B) (> (LENGTH (PSTRING (FIRST A)))
;;                                  (LENGTH (PSTRING (FIRST B)))))))))
;; 
;; 
;; 
;; ;;----------------------------------------------------------------------
;; ;; Setting environment -- COMMON-LISP part --
;; ;; ------------------------------------------
;; 
;; (SETF *LOAD-VERBOSE* NIL)
;; (LOAD (MERGE-PATHNAMES
;;        (MAKE-PATHNAME :NAME ".common" :TYPE "lisp") (USER-HOMEDIR-PATHNAME)))
;; 
;; (IN-PACKAGE "COM.INFORMATIMAGO.PJB")
;; ;; additional export at the end.
;; 
;; 
;; ;;----------------------------------------------------------------------
;; ;; Setting environment -- Allegro Lisp specific --
;; ;; -----------------------------------------------
;; 
;; 
;; (SETF *EDITOR* 
;;       (LAMBDA (ARG &KEY (WAIT T))
;;         (IF (OR (FUNCTIONP ARG) (SYMBOLP ARG))
;;             (ED ARG)
;;             (Excl:run-SHELL-command
;;              (FORMAT NIL "/usr/local/bin/emacsclient ~
;;                           ~:[-n~;~] ~A" WAIT ARG)))))
;; 
;; ;; in .common.lisp
;; ;; (DEFUN QUIT () (excl:exit))
;; ;; (EXPORT 'QUIT)
;; 
;; (PUSH (FUNCTION Excl:current-directory)
;;       COM.INFORMATIMAGO.COMMON-LISP.BROWSER:*CHANGE-DIRECTORY-HOOK*)
;; (CD (Excl:current-directory))
;; 
;; 
;; 
;; (import 'excl:shell)
;; (export 'shell)
;; 
;; (defmacro eval-string-in-unlocked-package (package string)
;;   `(excl:without-package-locks
;;      (let ((*package* (find-package ',package)))
;;        (with-input-from-string (input ,string)
;;          (loop
;;             :for sexp = (read input nil input)
;;             :until (eq sexp input)
;;             :do (print (eval sexp)))))))
;; (export 'eval-string-in-unlocked-package)
;; 
;; 
;; (defun ps ()
;;   (with-open-stream (in (excl:run-shell-command "ps axf" 
;;                                                 :wait nil
;;                                                 :output :stream)) 
;;     (loop for line = (read-line in nil nil) 
;;        while line do (princ line ) (terpri))))
;; (export 'ps)
;; 
;; (defun netstat (&optional (args "-tnpl"))
;;   (shell (format nil "netstat ~A" args)))
;; (export 'netstat)
;; 
;; (IN-PACKAGE "COMMON-LISP-USER")
;; (USE-PACKAGE "COM.INFORMATIMAGO.PJB")
;; (USE-PACKAGE "EXCL")

;;;; THE END ;;;;
