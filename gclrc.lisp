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
;; Setting environment -- gcl part --
;; ----------------------------------
(in-package "USER")
(RENAME-PACKAGE "LISP" "LISP" (list*  "CL" "COMMON-LISP"
                                      (copy-seq (PACKAGE-NICKNAMES "LISP"))))
(RENAME-PACKAGE "USER" "USER" (list*  "CL-USER" "COMMON-LISP-USER"
                                      (copy-seq (PACKAGE-NICKNAMES "USER"))))
(in-package "COMMON-LISP-USER")

#||
(defun post-process-logical-pathname-translations (host)
  (flet ((pstring (x) (if (pathnamep x) (namestring x) (string x))))
    (setf (logical-pathname-translations host)
          (sort (remove-if
                 (lambda (p) (let ((p (pstring p)))
                          (and (< 5 (length p))
                               (string= "*.*.*" p :start2 (- (length p) 5)))))
                 (logical-pathname-translations host)
                 :key (function first))
                (lambda (a b) (> (length (pstring (first a)))
                            (length (pstring (first b)))))))))
||#


;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------


(defun mixed-case-p (string)
  "
RETURN:         Whether the STRING contains both upper case characters
                and lower  case characters.   If there  are characters
                that are both-case-p, then mixed-case-p returns true.
"
  (and (some (function upper-case-p) string)
       (some (function lower-case-p) string)))


(defvar *original-make-pathname* (function make-pathname))
(defun make-pathname (&rest arguments &key directory (case :local) &allow-other-keys)
  (when directory
       (ecase (first directory)
         ((:relative) (pop directory))
         ((:absolute) (pop directory) (push "" directory))))
  (ecase case
    ((:local)
     (apply *original-make-pathname* :allow-other-keys t arguments))
    ((:common)
     (let ((arguments (mapcar (lambda (argument)
                                (flet ((convert (argument)
                                         (if (stringp argument)
                                             (if (mixed-case-p argument)
                                                 (error "~S is not in :COMMON case." argument)
                                                 (string-downcase argument))
                                             argument)))
                                  (typecase argument
                                    (string  (convert argument))
                                    (list    (mapcar (function convert) argument))
                                    (t       argument))))
                              (if directory
                                  (list* :directory directory arguments)
                                  arguments))))
       (apply *original-make-pathname* :allow-other-keys t arguments)))))

(SETF *LOAD-VERBOSE* t)
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
