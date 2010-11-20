;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               abcl-init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Armed Bear Common Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The ABCL init file.
;;;;
;;;;    Note, abcl doesn't load it automatically, you have to load it yourself.
;;;;
;;;;    abcl -load ~/abcl-init.lisp
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

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP Part --
;; ------------------------------------------

(SETQ *LOAD-VERBOSE* NIL)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       (USER-HOMEDIR-PATHNAME)
       NIL))

;;----------------------------------------------------------------------
;; Setting environment -- ABCL Specific Part --
;; --------------------------------------------

(in-package "COM.INFORMATIMAGO.PJB")
(export '(EDIT QUIT))


(defun edit (&optional (x nil x-p))
  (apply (function ed) (when x-p (list x))))
(defun quit ()
  (extensions:quit))

(in-package "COMMON-LISP-USER")
(use-package "COM.INFORMATIMAGO.PJB")


;;;; THE END ;;;;
