;;;;**************************************************************************
;;;;FILE:               .gclrc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             gcl
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The gcl init file.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2013-07-01 <PJB> Revised.
;;;;    2005-07-19 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2013
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
;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------




(defvar *original-make-pathname* (function make-pathname))
(defun make-pathname (&rest arguments &key (case :local) &allow-other-keys)
  (ecase case
    ((:local)  (apply *original-make-pathname* :allow-other-keys t arguments))
    ((:common) (apply *original-make-pathname* :allow-other-keys t
                      (mapcar (lambda (argument)
                                (if (string))
                                )
                              arguments))))
  )

(SETF *LOAD-VERBOSE* NIL)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       (USER-HOMEDIR-PATHNAME)
       NIL))

(IN-PACKAGE "COM.INFORMATIMAGO.PJB")
;; additional export at the end.


;;----------------------------------------------------------------------
;; Setting environment -- gcl specific --
;; --------------------------------------

(DEFUN QUIT () (EXT:QUIT))
(EXPORT 'QUIT)


;;----------------------------------------------------------------------
;;(format *trace-output* "~&.gclrc.lisp loaded~%")
;;----------------------------------------------------------------------

(IN-PACKAGE "COMMON-LISP-USER")
(USE-PACKAGE "COM.INFORMATIMAGO.PJB")

;;----------------------------------------------------------------------
