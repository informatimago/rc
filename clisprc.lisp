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


;; (|CL|:|SETF| |CUSTOM|:|*LOAD-ECHO*| |CL|:|T|)
(IN-PACKAGE "COMMON-LISP-USER")

(defun clisp-version (&optional (version-string (LISP-IMPLEMENTATION-VERSION)))
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

(export '(clisp-version
          version=    version<    version<=
          rt-version= rt-version< rt-version<=))


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
              '(:POWERPC :PPC :X86 :X86-64 :I686 :PC386 :IAPX386 :SPARC :PENTIUM3))
  (push :X86 *features*))



;; The following custom variables already existed in 2.33.83:
(setf ;; ANSI
 CUSTOM:*ANSI*                             T
 CUSTOM:*COERCE-FIXNUM-CHAR-ANSI*          T
 CUSTOM:*FLOATING-POINT-CONTAGION-ANSI*    T
 CUSTOM:*MERGE-PATHNAMES-ANSI*             T
 CUSTOM:*PARSE-NAMESTRING-ANSI*            T
 CUSTOM:*PARSE-NAMESTRING-DOT-FILE*       :NAME
 CUSTOM:*PRINT-PATHNAMES-ANSI*             T
 CUSTOM:*SEQUENCE-COUNT-ANSI*              T
      
 CUSTOM:*DEFAULT-FLOAT-FORMAT*            'SINGLE-FLOAT
 CUSTOM:*WARN-ON-FLOATING-POINT-CONTAGION* NIL
 CUSTOM:*SUPPRESS-CHECK-REDEFINITION*      NIL
 CUSTOM:*DEFTYPE-DEPTH-LIMIT*              NIL
 CUSTOM:*PACKAGE-TASKS-TREAT-SPECIALLY*    NIL

 CUSTOM:*PPRINT-FIRST-NEWLINE*             T
 CUSTOM:*PRINT-CLOSURE*                    NIL
 CUSTOM:*PRINT-INDENT-LISTS*               1
 CUSTOM:*PRINT-PRETTY-FILL*                NIL
 CUSTOM:*PRINT-RPARS*                      NIL
 CUSTOM:*PROMPT-START*                     "C/"
 CUSTOM:*PROMPT-BODY*                      (function prompt-body)
 ;; CUSTOM:*PROMPT-STEP*         ;; this is a function
 ;; CUSTOM:*PROMPT-BREAK*        ;; this is a function
 CUSTOM:*PROMPT-FINISH*                    "> "

 ;; CUSTOM:*SYSTEM-PACKAGE-LIST* ;; let the system set it

 CUSTOM:*LOAD-COMPILING*        NIL
 CUSTOM:*LOAD-ECHO*             CUSTOM:*LOAD-ECHO*
 CUSTOM:*LOAD-LOGICAL-PATHNAME-TRANSLATIONS-DATABASE*
 (list (merge-pathnames #P "LOGHOSTS/" (user-homedir-pathname) nil))
 CUSTOM:*LOAD-PATHS*          '(#P"")
 CUSTOM:*LOAD-OBSOLETE-ACTION*  NIL
 CUSTOM:*COMPILE-WARNINGS*      T

 CUSTOM:*SOURCE-FILE-TYPES*   '("lisp" "lsp" "cl")
 CUSTOM:*COMPILED-FILE-TYPES* '("fas")
 ;; CUSTOM:*LIB-DIRECTORY*       #P"/usr/local/languages/clisp/lib/clisp/"
  
 CUSTOM:*APPLYHOOK*             NIL
 CUSTOM:*EVALHOOK*              NIL
 CUSTOM:*TRACE-INDENT*          T
 CUSTOM:*APROPOS-DO-MORE*       NIL
 CUSTOM:*APROPOS-MATCHER*       NIL

 CUSTOM:*ERROR-HANDLER*         NIL
 CUSTOM:*BREAK-ON-WARNINGS*     NIL

 CUSTOM:*INSPECT-BROWSER*       NIL
 CUSTOM:*INSPECT-FRONTEND*     :TTY
 CUSTOM:*INSPECT-LENGTH*        100000
 CUSTOM:*INSPECT-PRINT-LENGTH*  100000
 CUSTOM:*INSPECT-PRINT-LEVEL*   5
 CUSTOM:*INSPECT-PRINT-LINES*   5

 ;; HTTP
 CUSTOM:*BROWSER*           :emacs-w3m
 CUSTOM:*BROWSERS*
 '((:firefox             "firefox" "~A")
   (:NETSCAPE            "netscape" "~A")
   (:NETSCAPE-REMOTE     "netscape" "-remote" "openURL(~A,new-window)")
   (:MOZILLA             "/usr/local/apps/mozilla-1.7/mozilla"      "~A")
   (:MOZILLA-REMOTE      "usr/local/apps/mozilla-1.7/mozilla" "-remote" "openURL(~A,new-window)")
   (:KONQUEROR           "kfmclient" "openURL" "~A")
   (:LYNX                "lynx" "~A")
   (:LYNX-XTERM          "xterm" "-e" "lynx" "~A")
   (:LINKS               "links" "~A")
   (:LINKS-XTERM         "xterm" "-e" "links" "~A")
   (:W3M                 "w3m"       "~A")
   (:W3M-XTERM           "xterm" "-e" "w3m" "~A")
   (:MMM                 "mmm" "-external" "~A")
   (:MOSAIC              "xmosaic" "~A")
   (:EMACS-W3            "emacsclient" "-n" "-e" "(w3-fetch \"~A\")")
   (:EMACS-W3M           "emacsclient" "-n" "-e" "(w3m-browse-url \"~A\")"))
 ;; for emacs-client browser, see SET-EMACS-SERVER-PID below.
 CUSTOM:*CLHS-ROOT-DEFAULT*
 "http://localhost/local/lisp/HyperSpec/"
 CUSTOM:*WITH-HTML-OUTPUT-DOCTYPE*
 '("html" "PUBLIC"
   "\"-//W3C//DTD XHTML 1.0 Strict//EN\""
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"")
 
 CUSTOM:*USER-MAIL-ADDRESS*    "pjb@informatimago.com"
 CUSTOM:*EDITOR*               "emacsclient"

 ;; CUSTOM:*CURRENT-LANGUAGE*     'I18N:ENGLISH
 )


;; (unless (string= "dumb" (ext:getenv "TERM"))
;; )

#-(and unix macos)
(setf CUSTOM:*PATHNAME-ENCODING*     (ext:make-encoding :charset CHARSET:ISO-8859-1
                                                        :line-terminator :UNIX)
      CUSTOM:*DEFAULT-FILE-ENCODING* (ext:make-encoding :charset CHARSET:utf-8
                                                        :line-terminator :UNIX)
      CUSTOM:*TERMINAL-ENCODING*     (ext:make-encoding :charset CHARSET:utf-8
                                                        :line-terminator :UNIX)
      CUSTOM:*MISC-ENCODING*         (ext:make-encoding :charset CHARSET:utf-8
                                                        :line-terminator :UNIX)
      #+FFI CUSTOM:*FOREIGN-ENCODING* #+FFI (ext:make-encoding :charset CHARSET:ISO-8859-1
                                                               :line-terminator :UNIX))
#+(and unix macos)
(setf CUSTOM:*DEFAULT-FILE-ENCODING*  charset:utf-8
      #+FFI CUSTOM:*FOREIGN-ENCODING* #+FFI charset:iso-8859-15
      CUSTOM:*MISC-ENCODING*          charset:utf-8 ; best same as terminal
      CUSTOM:*TERMINAL-ENCODING*      charset:utf-8 
      CUSTOM:*PATHNAME-ENCODING*      charset:utf-8)


#+#.(cl-user:rt-version<= "2.38" (cl-user:clisp-version))
(setf
 ;; ANSI
 CUSTOM:*LOOP-ANSI*                                 T
 CUSTOM:*PHASE-ANSI*                                T
 CUSTOM:*PRINT-EMPTY-ARRAYS-ANSI*                   T
 CUSTOM:*PRINT-SPACE-CHAR-ANSI*                     T
 CUSTOM:*PRINT-UNREADABLE-ANSI*                     T
 CUSTOM:*FLOATING-POINT-RATIONAL-CONTAGION-ANSI*    T
 CUSTOM:*WARN-ON-FLOATING-POINT-RATIONAL-CONTAGION* NIL

 CUSTOM:*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST*           NIL
 CUSTOM:*EQ-HASHFUNCTION*                           'EXT:FASTHASH-EQ
 CUSTOM:*EQL-HASHFUNCTION*                          'EXT:FASTHASH-EQL
 CUSTOM:*EQUAL-HASHFUNCTION*                        'EXT:FASTHASH-EQUAL
 CUSTOM:*FILL-INDENT-SEXP*                          (function 1+)
      
 CUSTOM:*INIT-HOOKS*                                NIL
 CUSTOM:*FINI-HOOKS*                                NIL
 CUSTOM:*WARN-ON-HASHTABLE-NEEDING-REHASH-AFTER-GC* NIL

 ;; HTTP
 CUSTOM:*HTTP-PROXY*                               NIL
 CUSTOM:*IMPNOTES-ROOT-DEFAULT* "http://clisp.cons.org/impnotes/"
      
      
 CUSTOM:*PRINT-SYMBOL-PACKAGE-PREFIX-SHORTEST*      NIL
 CUSTOM:*REPORT-ERROR-PRINT-BACKTRACE*              NIL
 CUSTOM:*USER-COMMANDS*                             NIL
      
 ;; CUSTOM:*STRICT-MOP*                             T
 ;; CUSTOM:*FORWARD-REFERENCED-CLASS-MISDESIGN*     NIL
 )


     
;;----------------------------------------------------------------------
;; Setting environment -- clisp part --
;; ------------------------------------

(DEFUN X-POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS (HOST)
  (FLET ((PSTRING (X) (IF (PATHNAMEP X) (NAMESTRING X) (STRING X))))
    (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
          (SORT
           (REMOVE-IF
            (LAMBDA (P) (LET ((P (PSTRING P)))
                     (AND (< 5 (LENGTH P))
                          (STRING= "*.*.*" P :START2 (- (LENGTH P) 5)))))
            (LOGICAL-PATHNAME-TRANSLATIONS HOST)
            :KEY (FUNCTION FIRST))
                (LAMBDA (A B) (> (LENGTH (PSTRING (FIRST A)))
                                 (LENGTH (PSTRING (FIRST B)))))))))



#+#.(cl-user:rt-version= "2.33.83" (cl-user:clisp-version))
(EXT:WITHOUT-PACKAGE-LOCK ("COMMON-LISP")
  (let ((oldload (function cl:load)))
    (fmakunbound 'cl:load)
    (defun cl:load (filespec &key (verbose *load-verbose*)
                    (print *load-print*)
                    (if-does-not-exist t)
                    (external-format :default))
      ;; (format *trace-output* "~&Will LOAD ~S;~%" filespec)
      ;; (finish-output *trace-output*)
      (HANDLER-CASE (FUNCALL OLDLOAD FILESPEC :VERBOSE VERBOSE
                             :PRINT PRINT :IF-DOES-NOT-EXIST IF-DOES-NOT-EXIST
                             :EXTERNAL-FORMAT EXTERNAL-FORMAT)
        (SYSTEM::SIMPLE-PARSE-ERROR
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
#+#.(cl:if (cl-user:version= "2.33.83" (cl-user:clisp-version))
           :syscall '(or))
(EXT:WITHOUT-PACKAGE-LOCK  ("POSIX")
  (intern "HOSTENT-ADDR-TYPE" "POSIX"))
#+#.(cl:if (cl-user:version= "2.33.83" (cl-user:clisp-version))
           :syscall '(or))
(EXT:WITHOUT-PACKAGE-LOCK  ("POSIX")
  (defun POSIX::HOSTENT-ADDR-TYPE (&rest args)
    (apply (function POSIX::HOSTENT-ADDRTYPE) args)))

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(SETF *LOAD-VERBOSE* NIL)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       (USER-HOMEDIR-PATHNAME)
       NIL))

(IN-PACKAGE "COM.INFORMATIMAGO.PJB")
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

(SETF *print-length*  nil)

(SETF *EDITOR* 
      (LAMBDA (ARG &KEY (WAIT T))
        (IF (OR (FUNCTIONP ARG) (SYMBOLP ARG))
            (ED ARG)
            (EXT:SHELL (FORMAT NIL "emacsclient ~:[-n~;~] ~A" WAIT ARG)))))



;; We'll use clocc xlib\clx\clue
;; (WHEN (FIND-PACKAGE "XLIB") (DELETE-PACKAGE (FIND-PACKAGE "XLIB")))

;;----------------------------------------------------------------------
;; Awfull trick for com.informatimago.clisp.script:is-running:

(DEFUN EXECUTABLE-READER (A B C) (SYS::UNIX-EXECUTABLE-READER A B C))
(SET-DISPATCH-MACRO-CHARACTER #\# #\! (FUNCTION EXECUTABLE-READER))


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

(DEFUN QUIT () (EXT:QUIT))
(EXPORT 'QUIT)

(PUSH (FUNCTION EXT:CD)
      COM.INFORMATIMAGO.COMMON-LISP.BROWSER:*CHANGE-DIRECTORY-HOOK*)
(CD (EXT:CD))     



(DEFUN SH (COMMAND)
  (LET ((ARGS (DELETE "" (SPLIT-STRING COMMAND " "))))
    (WITH-OPEN-STREAM (IN (EXT:RUN-PROGRAM (FIRST ARGS)
                            :ARGUMENTS (CDR ARGS) :OUTPUT :STREAM))
      (LOOP FOR LINE = (READ-LINE IN NIL NIL)
         WHILE LINE DO (PRINC LINE) (TERPRI))))) ;;SH


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
#+FFI (define-encoding-macro with-foreign-encoding  custom:*foreign-encoding*)

(DEFUN LSENCOD ()
  (FORMAT T "~%~{~32A ~A~%~}~%"
          (LIST
           'CUSTOM:*DEFAULT-FILE-ENCODING* CUSTOM:*DEFAULT-FILE-ENCODING*
           #+FFI 'CUSTOM:*FOREIGN-ENCODING* #+FFI CUSTOM:*FOREIGN-ENCODING*
           'CUSTOM:*MISC-ENCODING*         CUSTOM:*MISC-ENCODING*
           'CUSTOM:*PATHNAME-ENCODING*     CUSTOM:*PATHNAME-ENCODING*
           'CUSTOM:*TERMINAL-ENCODING*     CUSTOM:*TERMINAL-ENCODING*
           'SYSTEM::*HTTP-ENCODING*        SYSTEM::*HTTP-ENCODING*))
  (VALUES))
(EXPORT 'LSENCOD)


(defmacro eval-string-in-unlocked-package (package string)
  `(ext:without-package-lock (,package)
     (let ((*package* (find-package ',package)))
       (with-input-from-string (input ,string)
         (loop
            :for sexp = (read input nil input)
            :until (eq sexp input)
            :do (print (eval sexp)))))))
(export 'eval-string-in-unlocked-package)
        
;; for a *debugger-hook*:
;; SYSTEM::*READ-LINE-NUMBER*

;;----------------------------------------------------------------------
;;(format *trace-output* "~&.clisprc.lisp loaded~%")
;;----------------------------------------------------------------------

(defun ps ()
  (with-open-stream (in (ext:run-program "ps" :arguments '("axf") 
                                         :output :stream)) 
    (loop for line = (read-line in nil nil) 
       while line do (princ line ) (terpri))))
(export 'ps)


(defun print-print-vars ()
  (dolist (var '(
                 *PRINT-ARRAY*
                 *PRINT-BASE*
                 *PRINT-CASE*
                 *PRINT-CIRCLE*
                 *PRINT-ESCAPE*
                 *PRINT-GENSYM*
                 *PRINT-LENGTH*
                 *PRINT-LEVEL*
                 *PRINT-LINES*
                 *PRINT-MISER-WIDTH*
                 *PRINT-PPRINT-DISPATCH*
                 *PRINT-PRETTY*
                 *PRINT-RADIX*
                 *PRINT-READABLY*
                 *PRINT-RIGHT-MARGIN*
                 CUSTOM:*PRINT-CLOSURE*
                 CUSTOM:*PRINT-EMPTY-ARRAYS-ANSI*
                 CUSTOM:*PRINT-INDENT-LISTS*
                 CUSTOM:*PRINT-PATHNAMES-ANSI*
                 CUSTOM:*PRINT-PRETTY-FILL*
                 CUSTOM:*PRINT-RPARS*
                 CUSTOM:*PRINT-SPACE-CHAR-ANSI*
                 CUSTOM:*PRINT-SYMBOL-PACKAGE-PREFIX-SHORTEST*
                 CUSTOM:*PRINT-UNREADABLE-ANSI*) (values))
    (format t "~44A ~S~%" var (symbol-value var))))
(export 'print-print-vars)




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

(define-user-commands (date) "Interactive commands"
  "Prints the date."
  (com.informatimago.common-lisp.interactive:date))

(define-user-commands (uptime) "Interactive commands"
  "Prints the uptime."
  (com.informatimago.common-lisp.interactive:uptime))

(define-user-commands (pwd) "Interactive commands"
  "Print the working directory."
  (format t "~&~A~%" (com.informatimago.common-lisp.interactive:pwd)))

(define-user-commands (cd) "Interactive commands"
  "Change working directory."
  (apply (function com.informatimago.common-lisp.interactive:cd)
         (read-arguments *query-io*)))

(define-user-commands (ls) "Interactive commands"
  "List files."
  (apply (function com.informatimago.common-lisp.interactive:ls)
         (read-arguments *query-io*)))

(define-user-commands (cat less more) "Interactive commands"
  "Catenate files."
  (apply (function com.informatimago.common-lisp.interactive:cat)
         (read-arguments *query-io*)))


(define-user-commands (panic :pa) "User-defined commands"
  "Hit the panic button!"
   (format t "don't panic, ~D~%" (random 42)))

(define-user-commands (swear :sw) "User-defined commands"
  "curse"
  (let ((curses  #("ouch" "yuk" "bletch")))
    (format t "~A!~%" (aref curses (random (length curses))))))


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
                                      "emacsclient" "-n" "-s" (format nil "server-~D" pid)
                                      "-e" "(w3m-browse-url \"~a\")")
                                (delete :emacs-w3m-socket CUSTOM:*BROWSERS*  :key (function car)))
        CUSTOM:*BROWSER* :emacs-w3m-socket))
(export 'set-emacs-server-pid)
;; (set-emacs-server-pid 392)
   

(pushnew :testing-script *features*)
(IN-PACKAGE "COMMON-LISP-USER")
(USE-PACKAGE "COM.INFORMATIMAGO.PJB")
;; (print ".clisprc.lisp done")

;;;; THE END ;;;;
