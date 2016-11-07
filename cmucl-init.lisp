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

(setf extensions:*gc-verbose*  nil)

;; We put this before the COMMON-LISP part because when there's error
;; in rc/common.lisp, we want to get a reference to the CMUCL source where
;; the error is detected.

(setf (extensions:search-list "library:")  '("/data/languages/cmucl/lib/")
      (extensions:search-list "target:")   '("/data/languages/cmucl/src/"))


;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(setq *load-verbose* nil)
(load (merge-pathnames
       (make-pathname :directory '(:relative "RC") :name "COMMON" :type "LISP"
                      :case :common)
       (user-homedir-pathname)
       nil))



;; ----------------------------------------------------------------------
;; -- Setting environment -- CMUCL part --
;; ---------------------------------------

(in-package "COM.INFORMATIMAGO.PJB")
(setf *print-length*                      nil
      debug:*debug-print-length*          nil
      extensions:*describe-print-length*  nil
      extensions:*error-print-length*     nil)



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


(asdf-load :com.informatimago.common-lisp.ed)
(setf *editor* (function com.informatimago.common-lisp.ed.ed:ed))


(defun quit () (extensions:quit))
(export 'quit)


;; (defun search-list-pathname (sl)
;;   (make-pathname
;;    :directory (cons :absolute (cl::search-list-expansions
;;                                (cl::find-search-list (string sl))))))
;; (export 'search-list-pathname)

(in-package "COMMON-LISP-USER")
(use-package "COM.INFORMATIMAGO.PJB")


;;;; .cmucl-init.lisp                 --                     --          ;;;;
