;;;; -*- coding:utf-8; mode: lisp -*-
;;;;****************************************************************************
;;;;FILE:               .sbclrc
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             SBCL
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The SBCL init file.
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

(print *package*)
(in-package "COMMON-LISP-USER")

;; Those requires are required for quicklisp/asdf...
(require :sb-posix)
(require :sb-bsd-sockets)
(require :sb-cltl2)
(setf *print-circle* t
      *print-length* nil
      *print-level*  nil
      *print-lines*  nil)

(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)

;;;---------------------------------------------------------------------------
;;;

(defpackage "COM.INFORMATIMAGO.SBCL.VERSION"
  (:nicknames "VERSION")
  (:use "COMMON-LISP")
  (:export
   "SBCL-VERSION"
   "VERSION="    "VERSION<"    "VERSION<="
   "RT-VERSION=" "RT-VERSION<" "RT-VERSION<="
   ))
(in-package "COM.INFORMATIMAGO.SBCL.VERSION")


(defun sbcl-version (&optional (version-string (LISP-IMPLEMENTATION-VERSION)))
  (loop
     :with r = '()
     :with start = 0
     :do (multiple-value-bind (n p)
             (parse-integer version-string :start start :junk-allowed t)
           (if n
               (progn
                 (push n r)
                 (if (or (<= (length version-string) p)
                         (char= #\space (aref version-string p)))
                     (return-from sbcl-version (nreverse r))
                     (setf start (1+ p))))
               (return-from sbcl-version (nreverse r))))))

(defun version= (a b)
  (equal (if (stringp a) (sbcl-version a) a)
         (if (stringp b) (sbcl-version b) b)))

(defun version< (a b)
  (setf a (if (stringp a) (sbcl-version a) a)
        b (if (stringp b) (sbcl-version b) b))
  (cond
    ((null a)            (not (null b)))
    ((null b)            nil)
    ((< (car a) (car b)) t)
    ((= (car a) (car b)) (version< (cdr a) (cdr b)))
    (t                   nil)))

(defun version<= (a b)
  (setf a (if (stringp a) (sbcl-version a) a)
        b (if (stringp b) (sbcl-version b) b))
  (or (version= a b) (version< a b)))

(defun rt-version=  (a b) (if (version=  a b) '(and) '(or)))
(defun rt-version<  (a b) (if (version<  a b) '(and) '(or)))
(defun rt-version<= (a b) (if (version<= a b) '(and) '(or)))




(in-package "COMMON-LISP-USER")
;;----------------------------------------------------------------------
;; Setting environment -- SBCL part --
;; -----------------------------------

#-(and)
(SETF (LOGICAL-PATHNAME-TRANSLATIONS "target")
      '(("" "/local/src/sbcl/sbcl/src/")))
;; We put this before the COMMON-LISP part because when there's error
;; in .common.lisp, we want to get a reference to the SBCL source where
;; the error is detected.

#+#.(version:rt-version= (version:sbcl-version) '(1 0 39))
(setf (logical-pathname-translations "SYS") nil
      (logical-pathname-translations "SYS")
      '((#P"SYS:**;*.*" #P"/Users/pjb/src/sbcl/sbcl-1.0.39/**/*.*")
        (#P"SYS:**;*"   #P"/Users/pjb/src/sbcl/sbcl-1.0.39/**/*")))



(defvar *LOGHOSTS-DIRECTORY*
  (merge-pathnames (make-pathname :directory '(:relative "LOGHOSTS-SBCL")
                                  :case :common)
                   (user-homedir-pathname) nil)
  "The directory where logical host descriptions are stored.")


(defun logical-pathname-namestring (logical-pathname)
  (format nil "~A:~{~A;~}~:[~;~:*~A~:[~;.~:*~A~:[~;.~:*~A~]~]~]"
          (SB-IMPL::LOGICAL-HOST-NAME (pathname-host logical-pathname))
          (mapcar
           (lambda (item)
             (cond
               ((eq :wild-inferiors item) "**")
               ((eq :wild item) "*")
               (t item)))
           (if (eq :absolute (first (pathname-directory logical-pathname)))
               (rest (pathname-directory logical-pathname))
               (cons "" (rest (pathname-directory logical-pathname)))))
          (if (eq :wild (pathname-name logical-pathname))
              "*"
              (pathname-name logical-pathname))
          (if (eq :wild (pathname-type logical-pathname))
              "*"
              (pathname-type logical-pathname))
          (if (eq :wild (pathname-version logical-pathname))
              "*"
              (pathname-version logical-pathname))))


(defun X-post-process-logical-pathname-translations (host)
  (flet ((pstring (x)
           (typecase x
             (logical-pathname (logical-pathname-namestring x))
             (pathname         (namestring x))
             (t                (string x)))))
    (setf (logical-pathname-translations host)
          (sort (mapcar
                 (lambda (trans)
                   (let ((p (second trans)))
                     (list (first trans)
                           (make-pathname :host      (pathname-host      p)
                                          :device    (pathname-device    p)
                                          :directory (pathname-directory p)
                                          :name      (pathname-name      p)
                                          :type      (pathname-type      p)
                                          :version   nil
                                          :defaults  #P""))))
                 (logical-pathname-translations host))
                (lambda (a b) (> (length (pstring (first a)))
                                 (length (pstring (first b)))))))))


;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------
(declaim (sb-ext:muffle-conditions (or style-warning SB-EXT:COMPILER-NOTE))
         (optimize (speed 0) (space 0) (debug 3) (safety 3)))
(SETF *LOAD-VERBOSE* t)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       (USER-HOMEDIR-PATHNAME)
       NIL))

(IN-PACKAGE "COM.INFORMATIMAGO.PJB")
;; additional export at the end.
(export '(EDIT QUIT))

;;----------------------------------------------------------------------
;; Setting environment -- SBCL part --
;; -----------------------------------


(SETF *PRINT-READABLY* NIL
      *PRINT-LEVEL*    NIL
      *PRINT-LINES*    NIL
      *PRINT-LENGTH*   NIL
      *PRINT-CIRCLE*   T
      *PRINT-PRETTY*   NIL)

#+#.(cl:if(cl:find-symbol"*ERROR-PRINT-LEVEL*""SB-EXT")'(and)'(or))
(SETF SB-EXT:*ERROR-PRINT-LEVEL*  *PRINT-LEVEL*
      SB-EXT:*ERROR-PRINT-LINES*  *PRINT-LINES*
      SB-EXT:*ERROR-PRINT-LENGTH* *PRINT-LENGTH*)
(setf SB-EXT:*DEBUG-PRINT-VARIABLE-ALIST*
      `((*PRINT-READABLY* . ,*PRINT-READABLY*)
        (*PRINT-LENGTH*   . ,*PRINT-LENGTH*)
        (*PRINT-LEVEL*    . ,*PRINT-LEVEL*)
        (*PRINT-LINES*    . ,*PRINT-LINES*)
        (*PRINT-CIRCLE*   . ,*PRINT-CIRCLE*)
        (*PRINT-PRETTY*   . ,*PRINT-PRETTY*)))


(let ((counter 0))
  (defun prompt (stream)
    (format stream "~2%S/~A[~D]> "
            (if (packagep *package*)
                (first (sort (cons (package-name *package*)
                                   (package-nicknames *package*))
                             (function <=) :key (function length)))
                "#<INVALID *PACKAGE*>")
            (incf counter))))

(setf SB-INT:*REPL-PROMPT-FUN* (function prompt))

;; Add "USER" nickname to "COMMON-LISP-USER",
;; for compatibility with old packages..
(sb-ext:without-package-locks
  (com.informatimago.common-lisp.cesarum.PACKAGE:ADD-NICKNAME  "COMMON-LISP-USER" "USER")
  (com.informatimago.common-lisp.cesarum.PACKAGE:ADD-NICKNAME  "SB-PCL"           "CLOS"))


(defun edit-1 (arg)
  (sb-EXT:run-program "emacsclient" 
    (list (format nil "~A" (if (pathnamep arg)
                               (namestring arg) arg)))
    :search t :wait t :pty nil :input t :output t))
(push (function edit-1) SB-EXT:*ED-FUNCTIONS*)
(setf *editor* (lambda (arg) (if (or (functionp arg) (symbolp arg))
                                 (ed arg)
                                 (edit-1 arg))))

(defun quit () (sb-ext:quit))

;;----------------------------------------------------------------------

(in-package "COMMON-LISP-USER")
(use-package "COM.INFORMATIMAGO.PJB")

;; (require 'asdf) (require 'asdf-install) ;; done in .common.lisp
;; Now we use quicklisp.
;; http://ww.telent.net/cclan-choose-mirror
;; (setf ASDF-INSTALL:*CCLAN-MIRROR* "http://thingamy.com/cclan/")
;; (setf ASDF-INSTALL:*CCLAN-MIRROR* "http://ftp.linux.org.uk/pub/lisp/cclan/")

#+#.(cl:if (cl:find-package "SWANK") '(:and) '(:or))
(pushnew 'swank:ed-in-emacs sb-ext:*ed-functions*)


(when nil
  (dolist (host-path (directory (merge-pathnames
                                 (make-pathname :name :wild
                                                      :type "HOST"
                                                      :case :common)
                                 common-lisp-user::*LOGHOSTS-DIRECTORY* nil)))
    (let ((host (pathname-name host-path)))
      (let ((ht (reverse (with-open-file (in host-path) (read in)))))
        (with-open-file (out host-path :direction :output :if-exists :supersede)
          (format out ";; -*- mode:lisp -*- ~%")
          (print ht out))))))

(format t "~~/.sbclrc loaded~%")
;;;; .sbclrc                          --                     --          ;;;;

