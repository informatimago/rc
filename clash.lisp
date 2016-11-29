;;****************************************************************************
;;FILE:               rc/clash.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             CLISP
;;USER-INTERFACE:     None
;;DESCRIPTION
;;
;;    The CLASH init file.
;;
;;AUTHORS
;;    <PJB> Pascal Bourguignon
;;MODIFICATIONS
;;    2004-03-24 <PJB> Created.
;;BUGS
;;LEGAL
;;    GPL
;;
;;    Copyright Pascal Bourguignon 2004 - 2004
;;    mailto:pjb@informatimago.com
;;
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(SETQ *LOAD-VERBOSE* nil)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "RC") :NAME "COMMON" :TYPE "LISP"
                      :CASE :COMMON)
       (USER-HOMEDIR-PATHNAME)
       NIL))


;;----------------------------------------------------------------------
;; Setting environment -- CLISP specific --
;; ----------------------------------------

(SETQ CUSTOM:*EDITOR*   "emacsclient --no-wait")

;; (SETQ CUSTOM:*BROWSER*  :MOZILLA-REMOTE)
(SETQ CUSTOM:*CLHS-ROOT-DEFAULT*
      "http://thalassa.informatimago.com/local/lisp/HyperSpec/")


;; We'll use clocc xlib\clx\clue
;; (WHEN (FIND-PACKAGE "XLIB") (DELETE-PACKAGE (FIND-PACKAGE "XLIB")))

;;----------------------------------------------------------------------
;; Awfull trick for com.informatimago.clisp.script:is-running:

(DEFUN EXECUTABLE-READER (A B C) (SYS::UNIX-EXECUTABLE-READER A B C))
(SET-DISPATCH-MACRO-CHARACTER #\# #\! (FUNCTION EXECUTABLE-READER))

;;------------------------------------------------------------------------
;; Mapping M-x to insertion of #[] in ~/.inputrc with:
;;   #for clash: prints the square brackets to run an external command
;;   "\ex": "#[]\C-b"
;; and dispatching #[] to shell..
(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (setf (readtable-case *readtable*) :preserve)
    (unwind-protect
         (let ((command-line (read-delimited-list #\] stream t)))
           (list 'ext:run-program (princ-to-string (car command-line))
                 :arguments `',(mapcar #'princ-to-string (rest command-line))))
      (setf (readtable-case *readtable*) :upcase))))


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
            (SETQ DONE T FOUND T))))))
  );;GET-FIRST-WORD


(DEFUN EDIT (FILE)
  "
DO:         Invoke the CUSTOM:*EDITOR* with the FILE argument.
"
  (IF (PROBE-FILE FILE)
    (EXT:SHELL (FORMAT NIL "~A ~A" CUSTOM:*EDITOR* (TRUENAME FILE)))
    (IF (DO ((REP NIL)
             (LINE))
            ((OR (EQ REP :YES) (EQ REP :NO)) (EQ REP :YES))
          (FORMAT *QUERY-IO* "File ~S does not exist. Should I create it? "
                  FILE)
          (SETQ LINE (STRING-UPCASE
                      (GET-FIRST-WORD (READ-LINE *QUERY-IO* NIL :NO))))
          (COND
           ((OR (STRING= LINE "YES")
                (STRING= LINE "Y")
                (STRING= LINE "JA")
                (STRING= LINE "J")
                (STRING= LINE "SI")
                (STRING= LINE "S")
                (STRING= LINE "OUI")
                (STRING= LINE "O")
                (STRING= LINE "T")
                )
            (SETQ REP :YES))
           ((OR (STRING= LINE "NO")
                (STRING= LINE "N")
                (STRING= LINE "NON")
                (STRING= LINE "NEIN")
                (STRING= LINE "NIL")
                )
            (SETQ REP :NO))))
      (PROGN
        (CLOSE (OPEN FILE :DIRECTION :OUTPUT))
        (EXT:SHELL (FORMAT NIL "~A ~A" CUSTOM:*EDITOR* (TRUENAME FILE))))
      (FORMAT *ERROR-OUTPUT* "EDIT OF ~S CANCELED." FILE)))
  );;EDIT



;; (fmakunbound 'COMMON-LISP-USER::QUIT)

(use-package "EXT")

(defmacro pwd () `(cd))
(define-symbol-macro pwd (cd))
(defmacro ls (&rest args) `(ext:run-program "ls"  :arguments ',args))
(define-symbol-macro ls #[ls])


;;;; .clash.lisp                      -- 2004-03-24 03:08:46 -- pascal   ;;;;
