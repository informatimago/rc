;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               .clisprc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             CLISP
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The CLISP init file.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-05-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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
;;;;****************************************************************************

(dolist (module '("linux" "threads" "regexp"))
  (ignore-errors (require module)))

;; (|CL|:|SETF| |CUSTOM|:|*LOAD-ECHO*| |CL|:|T|)
(in-package "COMMON-LISP-USER")

;;;---------------------------------------------------------------------------
;;;

(defpackage "COM.INFORMATIMAGO.CLISP.VERSION"
  (:nicknames "CICV")
  (:use "COMMON-LISP")
  (:export
   "CLISP-VERSION"
   "VERSION="    "VERSION<"    "VERSION<="
   "RT-VERSION=" "RT-VERSION<" "RT-VERSION<="
   ))
(in-package "COM.INFORMATIMAGO.CLISP.VERSION")


(defun clisp-version (&optional (version-string (lisp-implementation-version)))
  (loop
     :with r = '()
     :with start = 0
     :do (multiple-value-bind (n p)
             (parse-integer version-string :start start :junk-allowed t)
           (push n r)
           (if (or (<= (length version-string) p)
                   (char= #\space (aref version-string p)))
               (return-from clisp-version (nreverse r))
               (setf start (1+ p))))))

(defun version= (a b)
  (equal (if (stringp a) (clisp-version a) a)
         (if (stringp b) (clisp-version b) b)))

(defun version< (a b)
  (setf a (if (stringp a) (clisp-version a) a)
        b (if (stringp b) (clisp-version b) b))
  (cond
    ((null a)            (not (null b)))
    ((null b)            nil)
    ((< (car a) (car b)) t)
    ((= (car a) (car b)) (version< (cdr a) (cdr b)))
    (t                   nil)))

(defun version<= (a b)
  (setf a (if (stringp a) (clisp-version a) a)
        b (if (stringp b) (clisp-version b) b))
  (or (version= a b) (version< a b)))

(defun rt-version=  (a b) (if (version=  a b) '(and) '(or)))
(defun rt-version<  (a b) (if (version<  a b) '(and) '(or)))
(defun rt-version<= (a b) (if (version<= a b) '(and) '(or)))


;;;---------------------------------------------------------------------------
;;;

(cl:in-package "COMMON-LISP-USER")


(let ((counter 0))
  (defun prompt-body ()
    (format nil "~A[~D]"
            (if (packagep *package*)
                (first (sort (cons (package-name *package*) (package-nicknames *package*))
                             (function <=) :key (function length)))
                "#<INVALID *PACKAGE*>")
            (incf counter))))


#+(and macos unix)
(unless (some (lambda (processor) (member processor *features*))
              '(:powerpc :ppc :x86 :x86-64 :i686 :pc386 :iapx386 :sparc :pentium3))
  (push :x86 *features*))


;; The following custom variables already existed in 2.33.83:
(setf ;; ANSI
 custom:*ansi*                             t
 custom:*coerce-fixnum-char-ansi*          t
 custom:*floating-point-contagion-ansi*    t
 custom:*merge-pathnames-ansi*             t
 custom:*parse-namestring-ansi*            t
 custom:*parse-namestring-dot-file*       :name
 custom:*print-pathnames-ansi*             t
 custom:*sequence-count-ansi*              t
      
 custom:*default-float-format*            'single-float
 custom:*warn-on-floating-point-contagion* nil
 custom:*suppress-check-redefinition*      nil
 custom:*deftype-depth-limit*              nil
 custom:*package-tasks-treat-specially*    nil

 custom:*pprint-first-newline*             t
 custom:*print-closure*                    nil
 custom:*print-indent-lists*               1
 custom:*print-pretty-fill*                nil
 custom:*print-rpars*                      nil
 custom:*prompt-start*                     "C/"
 custom:*prompt-body*                      (function prompt-body)
 ;; CUSTOM:*PROMPT-STEP*         ;; this is a function
 ;; CUSTOM:*PROMPT-BREAK*        ;; this is a function
 custom:*prompt-finish*                    "> "

 ;; CUSTOM:*SYSTEM-PACKAGE-LIST* ;; let the system set it

 custom:*load-compiling*        nil
 custom:*load-echo*             custom:*load-echo*
 custom:*load-logical-pathname-translations-database*
 (list (merge-pathnames #p "LOGHOSTS/" (user-homedir-pathname) nil))
 custom:*load-paths*           '(#p"")
 custom:*load-obsolete-action*  nil
 custom:*compile-warnings*      t
 ;; custom:*lib-directory*        used by require to load dynmods
 ;; custom:*user-lib-directory*   used by require to load dynmods -- user customization.

 custom:*source-file-types*   '("lisp" "lsp" "cl")
 custom:*compiled-file-types* '("fas")
  
 custom:*applyhook*             nil
 custom:*evalhook*              nil
 custom:*trace-indent*          t
 custom:*apropos-do-more*       nil
 custom:*apropos-matcher*       nil

 custom:*error-handler*         nil
 custom:*break-on-warnings*     nil

 custom:*inspect-browser*       nil
 custom:*inspect-frontend*     :tty
 custom:*inspect-length*        100000
 custom:*inspect-print-length*  100000
 custom:*inspect-print-level*   5
 custom:*inspect-print-lines*   5
 
 ;; HTTP
 custom:*browser*           :emacs-w3m
 custom:*browsers*
 '((:firefox             "firefox" "~A")
   (:netscape            "netscape" "~A")
   (:netscape-remote     "netscape" "-remote" "openURL(~A,new-window)")
   (:mozilla             "/usr/local/apps/mozilla-1.7/mozilla"      "~A")
   (:mozilla-remote      "usr/local/apps/mozilla-1.7/mozilla" "-remote" "openURL(~A,new-window)")
   (:konqueror           "kfmclient" "openURL" "~A")
   (:lynx                "lynx" "~A")
   (:lynx-xterm          "xterm" "-e" "lynx" "~A")
   (:links               "links" "~A")
   (:links-xterm         "xterm" "-e" "links" "~A")
   (:w3m                 "w3m"       "~A")
   (:w3m-xterm           "xterm" "-e" "w3m" "~A")
   (:mmm                 "mmm" "-external" "~A")
   (:mosaic              "xmosaic" "~A")
   (:emacs-w3            "emacsclient" "-n" "-e" "(w3-fetch \"~A\")")
   (:emacs-w3m           "emacsclient" "-n" "-e" "(w3m-browse-url \"~A\")"))
 ;; for emacs-client browser, see SET-EMACS-SERVER-PID below.
 custom:*clhs-root-default*
 "http://localhost/local/lisp/HyperSpec/"
 custom:*with-html-output-doctype*
 '("html" "PUBLIC"
   "\"-//W3C//DTD XHTML 1.0 Strict//EN\""
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"")
 
 custom:*user-mail-address*    "pjb@informatimago.com"
 custom:*editor*               "emacsclient"

 ;; CUSTOM:*CURRENT-LANGUAGE*     'I18N:ENGLISH
 )


;; (unless (string= "dumb" (ext:getenv "TERM"))
;; )


;; When launched with slime, the standard streams are replaced
;; by swank streams, and macros such as CUSTOM:*PATHNAME-ENCODING*
;; cannot work on them anymore.

(defmacro ignoring-each-error (&body body)
  `(progn
     ,@(mapcar (lambda (expression)`(ignore-errors ,expression ))
               body)))

#-(and unix macos)
(ignoring-each-error
 (setf custom:*pathname-encoding*     (ext:make-encoding :charset charset:iso-8859-1
                                                         :line-terminator :unix))
 (setf custom:*default-file-encoding* (ext:make-encoding :charset charset:utf-8
                                                         :line-terminator :unix))
 (setf custom:*terminal-encoding*     (ext:make-encoding :charset charset:utf-8
                                                         :line-terminator :unix))
 (setf custom:*misc-encoding*         (ext:make-encoding :charset charset:utf-8
                                                         :line-terminator :unix))
 #+ffi (setf custom:*foreign-encoding* (ext:make-encoding :charset charset:iso-8859-1
                                                          :line-terminator :unix)))

;; Breaks stdin/stdout...
;; #+(and unix macos)
;; (setf ;; CUSTOM:*DEFAULT-FILE-ENCODING*  charset:utf-8 
;;       #+FFI CUSTOM:*FOREIGN-ENCODING* #+FFI charset:iso-8859-15
;;       CUSTOM:*MISC-ENCODING*          charset:utf-8 ; best same as terminal
;;       CUSTOM:*TERMINAL-ENCODING*      charset:utf-8 
;;       CUSTOM:*PATHNAME-ENCODING*      charset:utf-8)

#+#.(cicv:rt-version<= "2.38" (cicv:clisp-version))
(setf
 ;; ANSI
 custom:*loop-ansi*                                 t
 custom:*phase-ansi*                                t
 custom:*print-empty-arrays-ansi*                   t
 custom:*print-space-char-ansi*                     t
 custom:*print-unreadable-ansi*                     t
 custom:*floating-point-rational-contagion-ansi*    t
 custom:*warn-on-floating-point-rational-contagion* nil

 custom:*defun-accept-specialized-lambda-list*           nil
 custom:*eq-hashfunction*                           'ext:fasthash-eq
 custom:*eql-hashfunction*                          'ext:fasthash-eql
 custom:*equal-hashfunction*                        'ext:fasthash-equal
 custom:*fill-indent-sexp*                          (function 1+)
      
 custom:*init-hooks*                                nil
 custom:*fini-hooks*                                nil
 custom:*warn-on-hashtable-needing-rehash-after-gc* nil

 ;; HTTP
 custom:*http-proxy*                               nil
 custom:*impnotes-root-default* "http://clisp.cons.org/impnotes/"
      
      
 custom:*print-symbol-package-prefix-shortest*      nil
 custom:*report-error-print-backtrace*              nil
 custom:*user-commands*                             nil
      
 ;; CUSTOM:*STRICT-MOP*                             T
 ;; CUSTOM:*FORWARD-REFERENCED-CLASS-MISDESIGN*     NIL
 )


;;----------------------------------------------------------------------
;; Setting environment -- clisp part --
;; ------------------------------------

(defun x-post-process-logical-pathname-translations (host)
  (flet ((pstring (x) (if (pathnamep x) (namestring x) (string x))))
    (setf (logical-pathname-translations host)
          (sort
           (remove-if
            (lambda (p) (let ((p (pstring p)))
                     (and (< 5 (length p))
                          (string= "*.*.*" p :start2 (- (length p) 5)))))
            (logical-pathname-translations host)
            :key (function first))
                (lambda (a b) (> (length (pstring (first a)))
                                 (length (pstring (first b)))))))))



#+#.(cicv:rt-version= "2.33.83" (cicv:clisp-version))
(ext:without-package-lock ("COMMON-LISP")
  (let ((oldload (function cl:load)))
    (fmakunbound 'cl:load)
    (defun cl:load (filespec &key (verbose *load-verbose*)
                    (print *load-print*)
                    (if-does-not-exist t)
                    (external-format :default))
      ;; (format *trace-output* "~&Will LOAD ~S;~%" filespec)
      ;; (finish-output *trace-output*)
      (handler-case (funcall oldload filespec :verbose verbose
                             :print print :if-does-not-exist if-does-not-exist
                             :external-format external-format)
        (system::simple-parse-error
            ()
          ;; (format *trace-output* "~&LOAD got parse error on ~S;~%" filespec)
          ;; (finish-output *trace-output*)
          ;; (format *trace-output*   "               Will try ~S~%"
          ;;        (translate-logical-pathname filespec))
          ;; (finish-output *trace-output*)
          (funcall oldload (translate-logical-pathname filespec)
                   :verbose verbose
                   :print print :if-does-not-exist if-does-not-exist
                   :external-format external-format))))
    (compile 'cl:load)))

;; What's the relationship between SYSCALLS and POSIX?
#+#.(cl:if (cicv:version= "2.33.83" (cicv:clisp-version))
           :syscall '(or))
(ext:without-package-lock  ("POSIX")
  (intern "HOSTENT-ADDR-TYPE" "POSIX"))
#+#.(cl:if (cicv:version= "2.33.83" (cicv:clisp-version))
           :syscall '(or))
(ext:without-package-lock  ("POSIX")
  (defun posix::hostent-addr-type (&rest args)
    (apply (function posix::hostent-addrtype) args)))

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------
(setf *load-verbose* t)
(load (merge-pathnames
       (make-pathname :directory '(:relative "RC") :name "COMMON" :type "LISP"
                      :case :common)
       (user-homedir-pathname)
       nil))

(in-package "COM.INFORMATIMAGO.PJB")
;; additional export at the end.

;; ---------------------------------------------------------------------
;; clocc defsystem is broken for clisp --
;; --------------------------------------
;; Use: (LOAD "LOADER:CLOCC")


;;; (format t "~&tr=~S~%" (logical-pathname-translations "DEFSYSTEM"))
;;; (format t "~&dir=~S~%" (directory "DEFSYSTEM:*.*"))
;;; (LOAD "DEFSYSTEM:DEFSYSTEM.LISP")
;;; (SETF MK::*FILENAME-EXTENSIONS* '("lisp" . "fas"))
;;; (MK::DEFINE-LANGUAGE :LISP
;;;                      :COMPILER #'COMPILE-FILE
;;;                      :LOADER   #'LOAD
;;;                      :SOURCE-EXTENSION (CAR MK::*FILENAME-EXTENSIONS*)
;;;                      :BINARY-EXTENSION (CDR MK::*FILENAME-EXTENSIONS*))


;;; (UNLESS (FIND-PACKAGE "MK") (DEFPACKAGE "MK"))
;;; (DEFUN LOAD-MK ()
;;;   (LOAD "DEFSYSTEM:DEFSYSTEM.LISP")
;;;   (LET ((FE '("lisp" . "fas")))
;;;     (DEFVAR MK::*FILENAME-EXTENSIONS* FE)
;;;     (MK::DEFINE-LANGUAGE :LISP
;;;                          :COMPILER #'COMPILE-FILE
;;;                          :LOADER   #'LOAD
;;;                          :SOURCE-EXTENSION (CAR FE)
;;;                          :BINARY-EXTENSION (CDR FE)))
;;;   );;LOAD-MK



;;----------------------------------------------------------------------
;; Setting environment -- CLISP specific --
;; ----------------------------------------

;; (setf *editor* 
;;       (lambda (arg &key (wait t))
;;         (if (or (functionp arg) (symbolp arg))
;;             (ed arg)
;;             (ext:shell (format nil "emacsclient ~:[-n~;~] ~A" wait arg)))))



;; We'll use clocc xlib\clx\clue
;; (WHEN (FIND-PACKAGE "XLIB") (DELETE-PACKAGE (FIND-PACKAGE "XLIB")))

;;----------------------------------------------------------------------
;; Awfull trick for com.informatimago.clisp.script:is-running:

(defun executable-reader (a b c) (sys::unix-executable-reader a b c))
(set-dispatch-macro-character #\# #\! (function executable-reader))


;; (EXT:WITHOUT-PACKAGE-LOCK ("EXT")
;;   (eval-when (:execute)
;;     (defvar *original-make-encoding* (symbol-function 'ext:make-encoding))
;;     (defun ext:make-encoding (&key (charset :default)
;;                                    (line-terminator :unix)
;;                                    (input-error-action :error)
;;                                    (output-error-action :error))
;;       (funcall *original-make-encoding*
;;                :charset (if (and (< (length charset) 3)
;;                                  (string-equal charset "iso" :end1 3)
;;                                  (digit-char-p (char charset 3)))
;;                           (concatenate 'string (subseq charset 0 3)
;;                                        "-" (subseq charset 3))
;;                           charset)
;;                :line-terminator line-terminator
;;                :input-error-action input-error-action
;;                :output-error-action output-error-action)
;;       ) ;;ext:make-encoding
;;     ))

(defun quit () (ext:quit))
(export 'quit)

(push 'ext:cd com.informatimago.common-lisp.interactive.browser:*change-directory-hook*)
(cd (ext:cd))     


(defun sh (command)
  (let ((args (delete "" (split-string command " "))))
    (with-open-stream (in (ext:run-program (first args)
                            :arguments (cdr args) :output :stream))
      (loop for line = (read-line in nil nil)
         while line do (princ line) (terpri)))))


;; (SET-MACRO-CHARACTER #\] (GET-MACRO-CHARACTER #\)))
;; (SET-DISPATCH-MACRO-CHARACTER  #\# #\[
;;   (LAMBDA (STREAM CHAR1 CHAR2)
;;     (DECLARE (IGNORE CHAR1 CHAR2))
;;     (SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
;;     (UNWIND-PROTECT
;;         `(LET ((ARGS ',(READ-DELIMITED-LIST #\] STREAM T)))
;;            (WITH-OPEN-STREAM (IN (EXT:RUN-PROGRAM (FIRST ARGS)
;;                                    :ARGUMENTS (CDR ARGS) :OUTPUT :STREAM))
;;              (LOOP FOR LINE = (READ-LINE IN NIL NIL)
;;                    WHILE LINE DO (PRINC LINE) (TERPRI))))
;;       (SETF (READTABLE-CASE *READTABLE*) :UPCASE))))


;; (setf CUSTOM:*DEFAULT-FILE-ENCODING* charset:iso-8859-1
;;       SYSTEM::FOREIGN-ENCODING       charset:iso-8859-1
;;       CUSTOM:*MISC-ENCODING*         charset:iso-8859-1
;;       CUSTOM:*PATHNAME-ENCODING*     charset:iso-8859-1
;;       CUSTOM:*TERMINAL-ENCODING*     charset:iso-8859-1)

;; ;;; You could put this in ~/.clisprc.lisp
;; 
;; ;; http://clisp.cons.org/impnotes/encoding.html#make-encoding
;; (let ((dumb-encoding
;;        (ext:make-encoding :charset charset:utf-8
;;                           :line-terminator :unix
;;                           :input-error-action :ignore
;;                           :output-error-action #\uFFFD)))
;;   (setf
;;    CUSTOM:*DEFAULT-FILE-ENCODING*   dumb-encoding 
;;    #+FFI 'CUSTOM:*FOREIGN-ENCODING* #+FFI dumb-encoding 
;;    CUSTOM:*MISC-ENCODING*           dumb-encoding 
;;    CUSTOM:*PATHNAME-ENCODING*       dumb-encoding 
;;    CUSTOM:*TERMINAL-ENCODING*       dumb-encoding 
;;    SYSTEM::*HTTP-ENCODING*          dumb-encoding))
;; 
;; ;;; But I wouldn't advise you to do so...


(defmacro define-encoding-macro (name symbol-macro)
  `(defmacro ,name (encoding &body body)
     (with-gensyms (saved-encoding)
       `(let ((,saved-encoding ,',symbol-macro))
          (unwind-protect (progn (setf ,',symbol-macro ,encoding)
                                 ,@body)
            (setf ,',symbol-macro  ,saved-encoding))))))

(define-encoding-macro with-file-encoding     custom:*default-file-encoding*)
(define-encoding-macro with-pathname-encoding custom:*pathname-encoding*)
(define-encoding-macro with-terminal-encoding custom:*terminal-encoding*)
(define-encoding-macro with-misc-encoding     custom:*misc-encoding*)
#+ffi (define-encoding-macro with-foreign-encoding  custom:*foreign-encoding*)

(defun lsencod ()
  (format t "~%~{~32A ~A~%~}~%"
          (list
           'custom:*default-file-encoding* custom:*default-file-encoding*
           #+ffi 'custom:*foreign-encoding* #+ffi custom:*foreign-encoding*
           'custom:*misc-encoding*         custom:*misc-encoding*
           'custom:*pathname-encoding*     custom:*pathname-encoding*
           'custom:*terminal-encoding*     custom:*terminal-encoding*
           'system::*http-encoding*        system::*http-encoding*))
  (values))
(export 'lsencod)


(defmacro eval-string-in-unlocked-package (package string)
  `(ext:without-package-lock (,package)
     (let ((*package* (find-package ',package)))
       (with-input-from-string (input ,string)
         (loop
            :for sexp = (read input nil input)
            :until (eq sexp input)
            :do (eval sexp))))))
(export 'eval-string-in-unlocked-package)
        
;; for a *debugger-hook*:
;; SYSTEM::*READ-LINE-NUMBER*

(defun ps ()
  (with-open-stream (in (ext:run-program "ps" :arguments '("axf") 
                                         :output :stream)) 
    (loop
       :for line = (read-line in nil nil) 
       :while line :do (princ line ) (terpri))))
(export 'ps)


(defun print-print-vars ()
  (dolist (var '(
                 *print-array*
                 *print-base*
                 *print-case*
                 *print-circle*
                 *print-escape*
                 *print-gensym*
                 *print-length*
                 *print-level*
                 *print-lines*
                 *print-miser-width*
                 *print-pprint-dispatch*
                 *print-pretty*
                 *print-radix*
                 *print-readably*
                 *print-right-margin*
                 custom:*print-closure*
                 custom:*print-empty-arrays-ansi*
                 custom:*print-indent-lists*
                 custom:*print-pathnames-ansi*
                 custom:*print-pretty-fill*
                 custom:*print-rpars*
                 custom:*print-space-char-ansi*
                 custom:*print-symbol-package-prefix-shortest*
                 custom:*print-unreadable-ansi*) (values))
    (format t "~44A ~S~%" var (symbol-value var))))
(export 'print-print-vars)

(setf *print-length*  nil
      *print-level*   nil
      *print-right-margin* 1000)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interactive commands
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *user-command-groups* (make-hash-table :test (function equal)))
  (defun ensure-list (item) (if (listp item) item (list item))))

;;(setf custom:*user-commands* nil)

(defun generate-user-commands ()
  (setf custom:*user-commands*
        (let ((commands '()))
          (maphash
           (lambda (category command-list)
             (dolist (command  command-list)
               (destructuring-bind (names docstring body) command
                 (push (coerce
                        (let ((vfun (gensym)))
                          `(lambda ()
                             (flet ((,vfun () ,@body))
                               (list
                                ,(format nil "~%~:@(~{~S ~}~) ~A" names docstring)
                                ,@(mapcar (lambda (name)
                                            `(cons ,(format nil "~(~S~)" name)
                                                   (function ,vfun)))
                                          names)))))
                        'function) commands)))
             (push (coerce
                    `(lambda () (list ,(format nil "~2%~A:" category)))
                    'function) commands))
           *user-command-groups*)
          commands))
  (values))

(defmacro define-user-commands (names category docstring &body body)
  (let ((names (ensure-list names)))
    `(let ((entry (find ',names  (gethash ',category *user-command-groups* '())
                        :test (function equal)
                        :key (function first))))
       (if entry
           (setf (second entry) ',docstring
                 (third  entry) ',body)
           (push (list ',names ',docstring ',body)
                 (gethash ',category *user-command-groups* '())))
       (generate-user-commands)
       ',(first names))))

(defun delete-user-command (name)
  (maphash (lambda (category commands)
             (setf commands (delete name commands
                                    :test (function member)
                                    :key (function first)))
             (if commands
                 (setf (gethash category *user-command-groups*) commands)
                 (remhash category *user-command-groups*)))
            *user-command-groups*)
  (generate-user-commands))


(defun read-arguments (stream &key (prompt "> ") (prompt-function nil))
  "
DO:     Reads from the stream a line and parses the arguments
        separated by spaces.  Argument characters may be quoted
        with double-quotes, single-quotes or backspaces, and may
        stand on several lines.
RETURN: A list of string, the parsed arguments.
"
  (loop
     :with prompt-function = (or prompt-function
                                 (lambda (state)
                                   (case state
                                     ((:escape-out)
                                      (format stream "~&\\~A" prompt))
                                     ((:escape-in-double :string-double)
                                      (format stream "~&\"~A" prompt))
                                     ((:escape-in-single :string-single)
                                      (format stream "~&'~A"  prompt))
                                     (otherwise
                                      (format stream "~&~A"   prompt)))))
     :with line = "\\"
     :with state = :initial
     :with arguments = '()
     :with buffer = nil
     :with i = 0
     ;; :do (print `(:line ,line :i ,i :state ,state
     ;;                                :arguments ,arguments
     ;;                                :buffer ,buffer))
     :do (if (< i (length line))
             (let ((ch (prog1 (aref line i) (incf i))))
               (case state
                 (:out
                  (case ch
                    ((#\")     (setf state :string-double))
                    ((#\')     (setf state :string-single))
                    ((#\\)     (setf state :escape-out))
                    ((#\space) (when buffer
                                 (push (coerce (nreverse buffer) 'string)
                                       arguments)
                                 (setf buffer nil)))
                    (otherwise (if buffer
                                   (push ch buffer)
                                   (setf buffer (list ch))))))
                 (:string-double
                  (case ch
                    ((#\")     (setf state :out))
                    ((#\\)     (setf state :escape-in-double))
                    (otherwise (if buffer
                                   (push ch buffer)
                                   (setf buffer (list ch))))))
                 (:string-single
                  (case ch
                    ((#\')     (setf state :out))
                    ((#\\)     (setf state :escape-in-single))
                    (otherwise (if buffer
                                   (push ch buffer)
                                   (setf buffer (list ch))))))
                 (:escape-out
                  (if buffer
                      (push ch buffer)
                      (setf buffer (list ch)))
                  (setf state :out))
                 (:escape-in-double
                  (if buffer
                      (push ch buffer)
                      (setf buffer (list ch)))
                  (setf state :string-double))
                 (:escape-in-single
                  (if buffer
                      (push ch buffer)
                      (setf buffer (list ch)))
                  (setf state :string-single))))
             (progn
               (case state
                 (:out
                  (when buffer
                    (push (coerce (nreverse buffer) 'string)
                          arguments)
                    (setf buffer nil))
                  (return (nreverse arguments)))
                 ((:initial :escape-out)
                  (funcall prompt-function state)
                  (setf state :out))
                 ((:escape-in-double :escape-in-single)
                  (funcall prompt-function state)
                  (if buffer
                      (push #\newline buffer)
                      (setf buffer (list #\newline))))
                 (otherwise
                  (funcall prompt-function state)))
               (finish-output stream)
               (setf line (read-line stream)
                     i 0)))))

;; a b c def "def" 'def' \'def "de'f\"g'hi\ " "   a b c   " 'de"f\'g"hi\ ' '   a b c   ' 

(define-user-commands (date) "interactive commands"
  "prints the date."
  (com.informatimago.common-lisp.interactive.interactive:date))

(define-user-commands (uptime) "interactive commands"
  "prints the uptime."
  (com.informatimago.common-lisp.interactive.interactive:uptime))

(define-user-commands (pwd) "interactive commands"
  "print the working directory."
  (format t "~&~a~%" (com.informatimago.common-lisp.interactive.interactive:pwd)))

(define-user-commands (cd) "interactive commands"
  "change working directory."
  (apply (function com.informatimago.common-lisp.interactive.interactive:cd)
         (read-arguments *query-io*)))

(define-user-commands (ls) "interactive commands"
  "list files."
  (apply (function com.informatimago.common-lisp.interactive.interactive:ls)
         (read-arguments *query-io*)))

(define-user-commands (cat less more) "interactive commands"
  "catenate files."
  (apply (function com.informatimago.common-lisp.interactive.interactive:cat)
         (read-arguments *query-io*)))


(define-user-commands (panic :pa) "user-defined commands"
  "hit the panic button!"
   (format t "don't panic, ~d~%" (random 42)))

(define-user-commands (swear :sw) "user-defined commands"
  "curse"
  (let ((curses  #("ouch" "yuk" "bletch")))
    (format t "~a!~%" (aref curses (random (length curses))))))


;; (SYSTEM::DESCRIBE-FRAME *standard-output* system::*debug-frame*)

;; (loop :for frame = system::*debug-frame* :then next-frame
;;       :for next-frame = (SYSTEM::FRAME-UP system::*debug-frame* 4)
;;       :do  (format t "==> ")
;;            (system::describe-frame *standard-output* frame)
;;       :until (eq frame next-frame)
;;       :finally (return (values)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+emacs (emacs-pid)
(defun set-emacs-server-pid (pid)
  (setf CUSTOM:*BROWSERS* (cons (list :emacs-w3m-socket
                                      "emacsclient" "-n" "-s" (format nil "server-~d" pid)
                                      "-e" "(w3m-browse-url \"~a\")")
                                (delete :emacs-w3m-socket CUSTOM:*BROWSERS*  :key (function car)))
        CUSTOM:*BROWSER* :emacs-w3m-socket))
(export 'set-emacs-server-pid)
;; (set-emacs-server-pid 392)
   

(pushnew :testing-script *features*)
(in-package "COMMON-LISP-USER")
(use-package "COM.INFORMATIMAGO.PJB")

;; (print ".clisprc.lisp done")
;;;; THE END ;;;;
