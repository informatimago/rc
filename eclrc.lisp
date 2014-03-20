;;;; -*- mode: lisp -*-
;;;;****************************************************************************
;;;;FILE:               rc/eclrc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             ECL
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The ECL init file.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2005-09-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2010
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

(IN-PACKAGE "COMMON-LISP-USER")

;;----------------------------------------------------------------------
;; Setting environment -- ecl part --
;; ------------------------------------
;; #+#.(cl:if (cl:find-symbol "*DEFAULT-EXTERNAL-FORMAT*" :ext)
;;            '(:and) '(:or))
;; (setf ext:*default-external-format* '(:utf-8 :lf))

(when (string= (namestring (make-pathname :name "ABC" :case :common
                                          :defaults (make-pathname :type "ABC" :case :common
                                                                   :defaults nil)))
               "abc.ABC")
  (defvar *old-make-pathname* (function cl:make-pathname))
  (unwind-protect
       (progn
         (SI:PACKAGE-LOCK (find-package "COMMON-LISP") nil)
         (defun cl:make-pathname (&rest args &key host device directory name type version defaults case)
           (if (and (eq case :common) defaults) 
               (apply *old-make-pathname* :defaults  (string-upcase (namestring defaults)) args)
               (apply *old-make-pathname* args))))
    (SI:PACKAGE-LOCK (find-package "COMMON-LISP") t)))


#+#.(cl:if (cl:find-symbol "*TRANSLATE-PATHNAME-HOOK*" "SI") '(:and) '(:or))
(setf SI:*TRANSLATE-PATHNAME-HOOK*
      (lambda (src from to)
        (when (and (typep src 'logical-pathname)
                   (or (not (pathname-directory src))
                       (every (lambda (item) (string= (string-upcase item) item))
                              (rest (pathname-directory src))))
                   (or (not (pathname-name src))
                       (string= (string-upcase (pathname-name src)) (pathname-name src)))
                   (or (not (pathname-type src))
                       (string= (string-upcase (pathname-type src)) (pathname-type src))))
          (setf src (make-pathname :defaults src
                                   :directory (when (pathname-directory src)
                                                (cons (first (pathname-directory src))
                                                      (mapcar (function string-downcase)
                                                              (rest (pathname-directory src)))))
                                   :name (when (pathname-name src) (string-downcase (pathname-name src)))
                                   :type (when (pathname-type src) (string-downcase (pathname-type src)))))
          #-(and) (print (list 'SI:*TRANSLATE-PATHNAME-HOOK* src from to)))
      (values src from to)))


(DEFUN POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS (HOST)
  (FLET ((PSTRING (X) (IF (PATHNAMEP X) (NAMESTRING X) (STRING X))))
    (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
          (SORT (REMOVE-IF
                 (LAMBDA (P) (LET ((P (PSTRING P)))
                          (AND (< 5 (LENGTH P))
                               (STRING= "*.*.*" P :START2 (- (LENGTH P) 5)))))
                 (LOGICAL-PATHNAME-TRANSLATIONS HOST)
                 :KEY (FUNCTION FIRST))
                (LAMBDA (A B) (> (LENGTH (PSTRING (FIRST A)))
                            (LENGTH (PSTRING (FIRST B)))))))))

(defvar oldload (function cl:load))
#+#.(cl:if (cl:string=
              (cl:LISP-IMPLEMENTATION-VERSION)  "0.9"
              :end1 (cl:min (cl:length (cl:LISP-IMPLEMENTATION-VERSION)) 3))
      :ecl
      '(:or))
(let ((oldload (function cl:load)))
  (fmakunbound 'cl:load)
  (defun cl:load (filespec &key (verbose *load-verbose*)
                  (print *load-print*)
                  (if-does-not-exist t)
                  (external-format :default))
    (declare (ignore external-format))
    ;; (format *trace-output* "~&Will LOAD ~S;~%" filespec)
    ;; (finish-output *trace-output*)
    (funcall oldload (translate-logical-pathname filespec)
             :verbose verbose
             :print print :if-does-not-exist if-does-not-exist))
  (compile 'cl:load))

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(require :cmp)
(let ((C::*SUPPRESS-COMPILER-NOTES* t)
      (*LOAD-VERBOSE* nil))
  (LOAD (MERGE-PATHNAMES
         (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                        :CASE :COMMON)
         (USER-HOMEDIR-PATHNAME)
         NIL)))

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
;; Setting environment -- ERC specific --
;; ----------------------------------------

(DEFUN QUIT () (SYSTEM:QUIT))
;; (EXPORT 'QUIT)


(PUSH (function si:chdir)
      COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER:*CHANGE-DIRECTORY-HOOK*)
(cd (si:getcwd))

;;----------------------------------------------------------------------
;;(format *trace-output* "~&.clisprc.lisp loaded~%")
;;----------------------------------------------------------------------

(IN-PACKAGE "COMMON-LISP-USER")
;; ecl doesn't import packages in COMMON-LISP-USER, there's no package
;; to unuse to remove cruft.
;; Therefore we must shadow the old symbols:
(shadow 'quit)


(USE-PACKAGE "COM.INFORMATIMAGO.PJB")
