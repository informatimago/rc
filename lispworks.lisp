;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               lispworks.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Lispworks Common Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;
;;;;    The Lispworks init file.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;;;    2013-09-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2013 - 2013
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

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------


(in-package "COMMON-LISP-USER")



(SETQ *LOAD-VERBOSE* NIL)

;; For now, let's keep using the lispworks packages in CL-USER
(let ((used-packages (package-use-list "COMMON-LISP-USER")))
  (unwind-protect

       (LOAD (MERGE-PATHNAMES
              (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                             :CASE :COMMON)
              (USER-HOMEDIR-PATHNAME)
              NIL))

    (use-package used-packages "COMMON-LISP-USER")))


(in-package "COM.INFORMATIMAGO.PJB")
(export '(EDIT QUIT))


;;----------------------------------------------------------------------
;; Setting environment -- Lispworks specific --
;; ------------------------------------------

;;----------------------------------------------------------------------
;; EDIT --
;; -------

;; editor-name is redefined in config.lisp to be:
;; (defun editor-name () (or (getenv "EDITOR") *editor*))

(DEFUN GET-FIRST-WORD (STRING)
  "
RETURN:     The first word of the string, or the empty string.
"
  (DO ((I 0)
       (J 0)
       (FOUND NIL)
       (DONE NIL))
      (DONE (IF FOUND (SUBSEQ STRING I  J) ""))
    (IF  (<= (LENGTH STRING) I)
      (SETQ DONE T FOUND NIL)
      (IF (<= J I)
        (IF (ALPHA-CHAR-P (CHAR STRING I))
          (SETQ J (1+ I))
          (INCF I))
        (IF (<= (LENGTH STRING) J)
          (SETQ DONE T FOUND T)
          (IF (ALPHA-CHAR-P (CHAR STRING J))
            (INCF J)
            (SETQ DONE T FOUND T)))))))


(defun edit (&optional (x nil x-p))
  (declare (ignore x x-p))
  (apply (function ed) x))
(defun quit ()                (lispworks:quit))
(defun really-quit ()         (lispworks:quit))

;;; (setf (current-directory) ...)


(in-package "COMMON-LISP-USER")
(use-package "COM.INFORMATIMAGO.PJB")

(setf *print-right-margin* 110)

;;----------------------------------------------------------------------
(format *trace-output* "~&~A loaded~%" *load-pathname*)
;;----------------------------------------------------------------------
;;; THE END ;;;;
