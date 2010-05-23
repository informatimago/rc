;;;; -*- coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:               .cmucl-init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             CMUCL
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The CMUCL init file.
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
;;;;***************************************************************************



;; ----------------------------------------------------------------------
;; -- Setting environment -- CMUCL part --
;; ---------------------------------------

(SETF EXTENSIONS:*GC-VERBOSE*  NIL)

(SETF (EXTENSIONS:SEARCH-LIST "TARGET:")
      '("/local/src/cmucl/cmucl-18d/src/"))
;;; ;; We put this before the COMMON-LISP part because when there's error
;;; ;; in .common.lisp, we want to get a reference to the CMUCL source where
;;; ;; the error is detected.

(SETF (EXTENSIONS:SEARCH-LIST "LIBRARY:") 
      '("/local/languages/cmucl/lib/cmucl/lib/"))
;;(EXT:SEARCH-LIST "MODULES:")


;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(SETQ *LOAD-VERBOSE* NIL)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       (USER-HOMEDIR-PATHNAME)
       NIL))




;; ----------------------------------------------------------------------
;; -- Setting environment -- CMUCL part --
;; ---------------------------------------

(IN-PACKAGE "COM.INFORMATIMAGO.PJB")
(SETf *PRINT-LENGTH*                      NIL
      DEBUG:*DEBUG-PRINT-LENGTH*          nil
      EXTENSIONS::*TRACE-PRINT-LENGTH*    nil
      EXTENSIONS:*DESCRIBE-PRINT-LENGTH*  nil
      EXTENSIONS:*ERROR-PRINT-LENGTH*     nil)



;; (require :clx)
;; (require :hemlock)
;; (in-package "HEMLOCK-INTERNALS")
;; (defun maybe-load-hemlock-init (init)
;;   (when init
;;     (let* ((switch (find "hinit" *command-line-switches*
;;                          :test #'string-equal
;;                          :key #'cmd-switch-name))
;;            (spec-name
;;             (if (not (eq init t))
;;                 init
;;                 (and switch
;;                      (or (cmd-switch-value switch)
;;                          (car (cmd-switch-words switch)))))))
;;       (if spec-name
;;           (load (merge-pathnames spec-name (user-homedir-pathname))
;;                 :if-does-not-exist nil)
;;           (load "home:hemlock-init"
;;                 :if-does-not-exist nil)))));;maybe-load-hemlock-init

(PACKAGE:LOAD-PACKAGE :COM.INFORMATIMAGO.COMMON-LISP.ED)
(SETF *EDITOR* (FUNCTION COM.INFORMATIMAGO.COMMON-LISP.ED:ED))


(DEFUN QUIT () (EXTENSIONS:QUIT))
(EXPORT 'QUIT)


(DEFUN SEARCH-LIST-PATHNAME (SL)
  (MAKE-PATHNAME
   :DIRECTORY (CONS :ABSOLUTE (CL::SEARCH-LIST-EXPANSIONS
                               (CL::FIND-SEARCH-LIST (STRING SL))))))
(EXPORT 'SEARCH-LIST-PATHNAME)

;; ----------------------------------------------------------------------
;; We should not be needing CLOS, it should all be in COMMON-LISP!
(PACKAGE::ADD-NICKNAME  "PCL" "CLOS")
(IN-PACKAGE "COMMON-LISP")
(SHADOWING-IMPORT
 '(CLOS:FIND-CLASS CLOS:CLASS-NAME CLOS:BUILT-IN-CLASS CLOS:CLASS-OF))
(IN-PACKAGE "COMMON-LISP-USER")
(USE-PACKAGE "COM.INFORMATIMAGO.PJB")


;;;; .cmucl-init.lisp                 --                     --          ;;;;
