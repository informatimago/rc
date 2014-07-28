;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               ccl-init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Clozure Common Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The CCL init file.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;;;    2010-10-31 <PJB> Adapted from openmcl-init. to ccl-init.
;;;;    2003-12-18 <PJB> Adapted from .clisprc to .openmcl-init.
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


#-ccl-1.6 (defun ccl::delete-directory (path)
            (ccl::recursive-delete-directory path))

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(SETQ *LOAD-VERBOSE* NIL)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       #+windows-target (let ((home (ccl::getenv "HOME")))
                          (if home
                              (pathname (format nil "~A\\" home))
                              #P"C:\\cygwin\\home\\pjb\\"))
       #-windows-target (USER-HOMEDIR-PATHNAME)
       NIL))


(in-package "COM.INFORMATIMAGO.PJB")
(export '(EDIT QUIT))

(setf CCL:*PRINT-ABBREVIATE-QUOTE* nil)
;; (setf ccl:*save-definitions*  t) ; for function-lambda-expression

;; ---------------------------------------------------------------------
;; clocc defsystem is erroneous for clisp --
;; -----------------------------------------

;;; (format t "~&tr=~S~%" (logical-pathname-translations "DEFSYSTEM"))
;;; (format t "~&dir=~S~%" (directory "DEFSYSTEM:*.*"))
;;; (LOAD "DEFSYSTEM:DEFSYSTEM.LISP")
;;; (SETQ MK::*FILENAME-EXTENSIONS* '("lisp" . "fas"))
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
;; Setting environment -- CCL specific --
;; ------------------------------------------

;; We'll use clocc xlib\clx\clue
;; (WHEN (FIND-PACKAGE "XLIB") (DELETE-PACKAGE (FIND-PACKAGE "XLIB")))

;; (def-lp-trans "CCL"      "/usr/local/lib/" "ccl/")
;; (def-lp-trans "COCOA"    "/usr/local/lib/" "ccl/examples/")
;; (def-lp-trans "EXAMPLES" "/usr/local/lib/" "ccl/examples/")

;; (setf COMMON-LISP-USER::*default-bundle-path* "CCL:OPENMCL.APP;")
;;  ccl::*module-search-path*  ;; paths used by REQUIRE.

(defun locale-terminal-encoding ()
  "Returns the terminal encoding specified by the locale(7)."
  #+(and ccl windows-target)
  :iso-8859-1
  ;; ccl doesn't support :windows-1252.
  ;; (intern (format nil "WINDOWS-~A" (#_GetACP)) "KEYWORD")
  #-(and ccl windows-target)
  (dolist (var '("LC_ALL" "LC_CTYPE" "LANG")
               :iso-8859-1) ; some random default…
    (let* ((val (ccl::getenv var))
           (dot (position #\. val))
           (at  (position #\@ val :start (or dot (length val)))))
      (when (and dot (< dot (1- (length val))))
        (flet ((prefixp (p s)
                 (and (<= (length p) (length s))
                      (string= p s :end1 (length p)))))
         (return (intern (let ((name (string-upcase (subseq val (1+ dot)
                                                            (or at (length val))))))
                           (if (and (prefixp "ISO" name) (not (prefixp "ISO-" name)))
                               (concatenate 'string "ISO-" (subseq name 3))
                               name))
                         "KEYWORD")))))))


(defun set-terminal-encoding (encoding)
  #-(and ccl (not swank)) (declare (ignore encoding))
  #+(and ccl (not swank))
  (mapc (lambda (stream)

          (setf (ccl::stream-external-format stream)
                (ccl:make-external-format :domain nil
                                          :character-encoding encoding
                                          :line-termination
                                          #+unix :unix
                                          #+windows :windows
                                          #-(or unix windows) :unix)))
        (list (two-way-stream-input-stream  *terminal-io*)
              (two-way-stream-output-stream *terminal-io*)))
  (values))

(set-terminal-encoding  (locale-terminal-encoding))

(setf ccl:*default-external-format*           :unix
      ccl:*default-file-character-encoding*   :utf-8
      ccl:*default-line-termination*          :unix
      ccl:*default-socket-character-encoding* :utf-8)

;; #|
;; From: John DeSoi <jd@icx.net>
;; Note: Do not put any semicolon comments before this line.
;; The next forms define a semicolon reader for both #\linefeed
;; and #\return line endings so that OpenMCL can read MCL input
;; files (Mac line endings). Should also work with with MCL for unix files.
;; We could try (setf CCL::*LINEFEED-EQUALS-NEWLINE* t)
;; |#  
;; (defun semicolon-reader (stream char)
;;    (declare (ignore char))
;;    (do ((c #\null)) ((or (char= c #\linefeed) (char= c #\return)))
;;      (setf c (read-char stream nil #\newline t)))
;;    (values))
;; (set-macro-character #\; #'semicolon-reader)


;;----------------------------------------------------------------------
;; EDIT --
;; -------

;; editor-name is redefined in config.lisp to be:
;; (defun editor-name () (or (getenv "EDITOR") *editor*))

(defun get-first-word (string)
  "
RETURN:     The first word of the string, or the empty string.
"
  (do ((i 0)
       (j 0)
       (found nil)
       (done nil))
      (done (if found (subseq string i  j) ""))
    (if  (<= (length string) i)
      (setq done t found nil)
      (if (<= j i)
        (if (alpha-char-p (char string i))
          (setq j (1+ i))
          (incf i))
        (if (<= (length string) j)
          (setq done t found t)
          (if (alpha-char-p (char string j))
            (incf j)
            (setq done t found t)))))))


(defun edit (&optional (x nil x-p))
  (declare (ignore x x-p))
  (format *error-output* "~&Not implemented yet.~%"))
(defun quit ()                      (ccl:quit))
(defun really-quit () (#_kill (ccl::getpid) 9))

(defun ccl-cl (directory)
  (setf (ccl:current-directory) directory))
(push 'ccl-cl com.informatimago.common-lisp.interactive.browser:*change-directory-hook*)
(cd (ccl:current-directory))

(in-package "COMMON-LISP-USER")
(use-package "COM.INFORMATIMAGO.PJB")
(setf *print-right-margin* 110)


;; Temporarily, while developping from kuiper for galatea:
(when (string= (com.informatimago.pjb:hostname) "galatea.local")
  (ql:quickload :swank))
#+#.(cl:if (cl:find-package "SWANK") '(:and) '(:or))
(let ((swank::*loopback-interface* (ccl::primary-ip-interface-address))
      (port (+ 4005 (random 123))))
  (swank:create-server :port port))


(defun optimization ()
   (list 'optimize
         (list 'safety   ccl::*nx-safety*)
         (list 'debug    ccl::*nx-debug*)
         (list 'speed    ccl::*nx-speed*)
         (list 'space    ccl::*nx-space*)
         (list 'compilation-speed ccl::*nx-cspeed*)))

(format t "~&~S~%" (optimization))

;;----------------------------------------------------------------------
;; (format *trace-output* "~&.openmcl-init.lisp loaded~%")
;;----------------------------------------------------------------------
;;;; THE END ;;;;
