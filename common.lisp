;;;; -*- coding:utf-8; mode: lisp -*-
;;;;****************************************************************************
;;;;FILE:               common.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             None
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;
;;;;    This file contains initializations common to clisp, cmucl, sbcl, and
;;;;    possibly to other Common-Lisp too.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2006-08-28 <PJB> Moved interactive functions
;;;;                     to COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.
;;;;                     This file should contains only environment
;;;;                     configuration forms (asdf, logical pathnames, etc).
;;;;    2004-11-23 <PJB> Added LIST-ALL-SYMBOLS and LIST-EXTERNAL-SYMBOLS.
;;;;    2003-05-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2003 - 2006
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
(DEFPACKAGE "COM.INFORMATIMAGO.PJB"
  (:NICKNAMES "PJB")
  (:USE "COMMON-LISP"))
;; EXPORT at the end.


;; Clean the packages imported into COMMON-LISP-USER:
(MAPC (LAMBDA (package) (UNUSE-PACKAGE package "COMMON-LISP-USER"))
      (set-difference
       (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))
       (delete nil (list ;; A list of all the "CL" packages possible:
                    (FIND-PACKAGE "COMMON-LISP")
                    (FIND-PACKAGE "IMAGE-BASED-COMMON-LISP")))))

(IN-PACKAGE "COM.INFORMATIMAGO.PJB")

(setf *print-circle* t
      *print-length* nil
      *print-level*  nil
      *print-lines*  nil)

(declaim (OPTIMIZE (SAFETY 3) (SPACE 0) (SPEED 0) (DEBUG 3)))

;; ----------------------------------------------------------------------
;; -- logical hosts -- the Common-Lisp way to PATH --
;; --------------------------------------------------

(progn 
  (defvar *directories*  '())
  (defun get-directory (key &optional (subpath ""))
    (unless *directories*
      (with-open-file (dirs (make-pathname :name "DIRECTORIES" :type "TXT"
                                           :version nil :case :common
                                           :defaults (user-homedir-pathname)))
        (loop
           :for k = (read dirs nil dirs)
           :until (eq k dirs)
           :do (push (string-trim " " (read-line dirs)) *directories*)
           :do (push (intern (substitute #\- #\_ (string k))
                             "KEYWORD") *directories*))))
    (unless (getf *directories* key)
      (error "~S: No directory keyed ~S" 'get-directory key))
    (merge-pathnames subpath (getf *directories* key) nil)))


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (DEFVAR *LOGICAL-HOSTS* '()))
(EXPORT '(*directories* *LOGICAL-HOSTS*))

(defun make-translations (host logical-dir physical-dir &optional file-type)
  (mapcar
   (lambda (item)
     (destructuring-bind (logical-tail physical-tail) item
       (list (apply (function make-pathname)
                    :host host
                    :directory `(:absolute ,@logical-dir :wild-inferiors)
                    logical-tail)
             (format nil "~A**/~A" physical-dir physical-tail))))
   
   #+clisp
   (if file-type
       `(((:name :wild :type ,file-type :version nil) ,(format nil "*.~(~A~)" file-type)))
       '(((:name :wild :type :wild :version nil)   "*.*")
         ((:name :wild :type nil   :version nil)   "*")))
   #+sbcl
   (if file-type
       `(((:name :wild :type ,file-type :version :wild) ,(format nil "*.~(~A~)" file-type)))
       '(((:name :wild :type :wild :version :wild) "*.*")))
   #+allegro
   (if file-type
       `(((:name :wild :type ,file-type :version nil) ,(format nil "*.~(~A~)" file-type)))
       '(((:name :wild :type nil   :version nil)   "*")
         ((:name :wild :type :wild :version nil) "*.*")))
   #-(OR CLISP sbcl)
   (if file-type
       `(((:name :wild :type ,file-type :version nil)   ,(format nil "*.~(~A~)" file-type))
         ((:name :wild :type ,file-type :version :wild) ,(format nil "*.~(~A~)" file-type)))
       '(((:name :wild :type nil   :version nil)   "*")
         ((:name :wild :type :wild :version nil)   "*.*")
         ((:name :wild :type :wild :version :wild) "*.*")))))


(DEFUN DEF-LP-TRANS (HOST PATH &OPTIONAL (SUBPATH ""))
  (PUSHNEW HOST *LOGICAL-HOSTS* :TEST (FUNCTION STRING-EQUAL))
  ;; If the HOST is already defined we don't change it (eg. HOME):
  (UNLESS (HANDLER-CASE (LOGICAL-PATHNAME-TRANSLATIONS HOST) (ERROR () NIL))
    (and (ignore-errors (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST) NIL) t)
         (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
               (make-translations
                host '() (format nil "~A~:[~;~:*~A~]"
                                 path (if (string= "" subpath) nil subpath)))))))

(DEFMACRO MP (PATHNAME &OPTIONAL
              (DIRECTORY NIL DIRECTORY-P)
              (NAME      NIL NAME-P)
              (TYPE      NIL TYPE-P)
              (VERSION   NIL VERSION-P))
  `(MERGE-PATHNAMES
    (MAKE-PATHNAME,@(WHEN DIRECTORY-P `(:DIRECTORY '(:RELATIVE ,@DIRECTORY)))
                  ,@(WHEN NAME-P      `(:NAME      ,NAME))
                  ,@(WHEN TYPE-P      `(:TYPE      ,TYPE))
                  ,@(WHEN VERSION-P   `(:VERSION   ,VERSION))
                  :DEFAULTS ,PATHNAME)
    ,PATHNAME
    nil))

(DEFPARAMETER *asdf-install-location*  (get-directory :asdf-install))
(DEFPARAMETER *SHARE-LISP*             (get-directory :share-lisp))
(DEFPARAMETER *PJB-COMM*               (get-directory :lisp-sources))
(DEFPARAMETER *PJB-LISP*               (get-directory :pjb-lisp))

(DEF-LP-TRANS "SHARE-LISP" *SHARE-LISP*)
(DEF-LP-TRANS "PJB-COMM"   *PJB-COMM*)
(DEF-LP-TRANS "PJB-LISP"   *PJB-LISP*)
(DEF-LP-TRANS "CMU-AI"     *SHARE-LISP* "ai/")
(DEF-LP-TRANS "CL-PDF"     *SHARE-LISP* "cl-pdf/")
(DEF-LP-TRANS "UFFI"       *SHARE-LISP* "uffi/")
(DEF-LP-TRANS "PACKAGES"   *SHARE-LISP* "packages/")
(DEF-LP-TRANS "CLOCC"      *SHARE-LISP* "packages/net/sourceforge/clocc/clocc/")
(DEF-LP-TRANS "CCLAN"      *SHARE-LISP* "packages/net/sourceforge/cclan/")
(DEF-LP-TRANS "DEFSYSTEM"  *SHARE-LISP*
  ;; We must go thru a translation for defsystem-3.x isn't a valid logical name!
  "packages/net/sourceforge/clocc/clocc/src/defsystem-3.x/")


(DEF-LP-TRANS "HOME"     (USER-HOMEDIR-PATHNAME)   "")
(DEF-LP-TRANS "LOADERS"  *PJB-COMM*        "cl-loaders/")
;;(DEF-LP-TRANS "NORVIG"   *PJB-LISP*        "norvig/")
(DEF-LP-TRANS "NORVIG"   #P"/home/pjb/src/lisp/ai/"    "norvig-paip-pjb/")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun POST-PROCESS-LOGICAL-HOST-TRANSLATIONS ()
    (WHEN (FBOUNDP 'COMMON-LISP-USER::POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS)
      (MAP NIL
           (FUNCTION COMMON-LISP-USER::POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS)
           *LOGICAL-HOSTS*)))) 

(POST-PROCESS-LOGICAL-HOST-TRANSLATIONS)

(defmacro defautoload (name arguments loader)
  `(defun ,name ,arguments
     (load ,loader)
     (eval '(,name ,@arguments))))
(defautoload scheme () "LOADERS:PSEUDO")
(export '(defautoload scheme))

;;----------------------------------------------------------------------

#+allegro
(defun lp (designator)
  (if (stringp designator)
      (let ((colon (position #\: designator)))
        (format nil "~:@(~A~)~(~A~)"
                (subseq designator 0 colon)
                (subseq designator colon)))
      designator))



#+(or ecl sbcl)
(require :asdf)

#-(or ecl sbcl)
(dolist (file (list #P"PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP"
                    #P"PACKAGES:ASDF;ASDF.LISP"
                    #P"SHARE-LISP:ASDF;ASDF.LISP"
                    (get-directory :share-lisp "packages/net/sourceforge/cclan/asdf/asdf.lisp")
                    (get-directory :share-lisp "asdf/asdf.lisp"))
         :failure)
  (handler-case
      (progn (LOAD #+allegro (lp file) #-allegro file)
             (return :success))
    #-clisp
    (FILE-ERROR (err)
      (format *error-output* "Got error ~A ; trying again.~%" err)
      (format *error-output* "Translations are: ~S~%" (logical-pathname-translations "PACKAGES"))
      (format *error-output* "Got error ~A ; trying translating the logical pathname: ~%  ~S --> ~S~%"
              err file (translate-logical-pathname file))
      (handler-case (load (translate-logical-pathname file))
        (FILE-ERROR (err)
          (format *error-output* "Got error ~A~%" err))))
    
    #+clisp
    (SYSTEM::SIMPLE-FILE-ERROR (err)
      (format *error-output* "Got error ~A ; trying again.~%" err)
      (format *error-output* "Translations are: ~S~%" (logical-pathname-translations "PACKAGES"))
      (format *error-output* "Got error ~A ; trying translating the logical pathname: ~%   ~S --> ~S~%"
              err
              (slot-value err 'system::$pathname)
              (translate-logical-pathname (slot-value err 'system::$pathname))))))



(defparameter *asdf-interval-between-rescan* (* 7 24 60 60)
  "Force rescan at leastr once this amount of seconds.")

(defparameter *asdf-registry-file*
  (make-pathname :name "ASDF-CENTRAL-REGISTRY" :type "DATA" :case :common
                 :defaults (#+allegro (lambda (x) (string-upcase (namestring x)))
                            #-allegro identity
                            (user-homedir-pathname)))
  "Cache file.")

(defparameter *original-asdf-registry* ASDF:*CENTRAL-REGISTRY*)

(defun asdf-rescan-packages (&optional (directories (list #P"PACKAGES:" *asdf-install-location*)))
  (format *trace-output* "~&;; Scanning ASDF packages...~%")
  (prog1
      (SORT 
       (DELETE-DUPLICATES 
        (MAPCAR
         (LAMBDA (P) (MAKE-PATHNAME :NAME NIL :TYPE NIL :VERSION NIL :DEFAULTS P))
         (mapcan (lambda (dir)
                   (DIRECTORY
                    (merge-pathnames
                     (make-pathname :directory (if (pathname-directory dir)
                                                   '(:relative :wild-inferiors)
                                                   '(:absolute :wild-inferiors))
                                    :name :wild
                                    :type "ASD"
                                    :case :common
                                    :defaults dir)
                     dir)))
                 directories))
        :test (function equal))
       (LAMBDA (A B) (if (= (length a) (length b))
                    (string< a b)
                    (< (length a) (length b))))
       :key (function namestring))
    (format *trace-output* "~&;; Done.~%")))


(defun update-asdf-registry (&key (force-scan nil)
                             (directories nil directoriesp))
  (length
   (setf ASDF:*CENTRAL-REGISTRY*
         (nconc
          (if (and (not force-scan)
                   (probe-file *ASDF-REGISTRY-FILE*)
                   (let ((fdate (file-write-date *ASDF-REGISTRY-FILE*)))
                     (and fdate (< (get-universal-time)
                                  (+ fdate *asdf-interval-between-rescan*)))))
              (with-open-file (in *ASDF-REGISTRY-FILE*)
                (format *trace-output* "~&;; Reading ASDF packages from ~A...~%"
                        *asdf-registry-file*)
                (let ((*read-eval* nil))
                  (read in nil nil)))
              (let ((scan (apply (function asdf-rescan-packages)
                                 (when directoriesp
                                   (list directories)))))
                (unless force-scan
                  (format *trace-output* "~&;; Writing ASDF packages to ~A...~%"
                          *asdf-registry-file*)
                  (with-open-file (out *ASDF-REGISTRY-FILE*
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
                    (print scan out)))
                scan))
          ;;(list CCLAN-GET::*CCLAN-ASDF-REGISTRY*)
          *original-asdf-registry*))))
(EXPORT 'UPDATE-ASDF-REGISTRY)

#-sbcl (update-asdf-registry)


;;--------------------
;; (asdf-load system)
;;--------------------

;; Once upon a time, there was a bug in sbcl. Not needed with >=sbcl-1.0.18
;; #+sbcl(require :sb-posix)
;; #+sbcl(defun asdf-load (system)
;;         ;; Let's try to happily round about sbcl's bugs...
;;         (let ((ASDF:*COMPILE-FILE-WARNINGS-BEHAVIOUR* :warn))
;;           (handler-bind
;;               ((SB-POSIX::FILE-ENOENT
;;                 (lambda (err)
;;                   (if (and (= 2 (slot-value err 'sb-posix::errno))
;;                            (string= (pathname-name (slot-value err 'sb-posix::pathname))
;;                                     "constants")
;;                            (or (member "sb-posix"
;;                                        (pathname-directory (slot-value err 'sb-posix::pathname))
;;                                        :test (function string=))
;;                                (member "sb-bsd-sockets"
;;                                        (pathname-directory (slot-value err 'sb-posix::pathname))
;;                                        :test (function string=))))
;;                       (invoke-restart (find-restart 'asdf:accept))
;;                       (error err))))
;;                (sb-int:simple-file-error
;;                 (lambda (err)
;;                   (if (and (string= (simple-condition-format-control err)
;;                                     "can't create directory ~A")
;;                            (let ((s (first (simple-condition-format-arguments err))))
;;                              (and (< 5 (length s))
;;                                   (string= "/usr/" s :end2 5))))
;;                       (invoke-restart (find-restart 'continue))
;;                       (error err)))))
;;             (asdf:oos 'asdf:load-op system))))

(defun asdf-load (&rest systems)
  (dolist (system systems systems)
    (asdf:operate 'asdf:load-op system)))

;;--------------------
;; (asdf-load-source system)
;;--------------------

(defun asdf-load-source (&rest systems)
  (dolist (system systems systems)
    (asdf:operate 'asdf:load-source-op system)))


;;--------------------
;; (asdf-install system)
;;--------------------

#+sbcl (require :asdf-install)
#+sbcl (defvar ASDF-INSTALL::*CONNECT-TIMEOUT* 0)      ; needed by sb-posix
#+sbcl (defvar ASDF-INSTALL::*READ-HEADER-TIMEOUT* 10) ; needed by sb-posix

#-sbcl (asdf-load :asdf-install)
#-sbcl (setf asdf-install:*preferred-location* 0)

;; (setf asdf-install:*locations* (butlast asdf-install:*locations*))
(setf asdf-install:*locations*
      (list
       (list
        (merge-pathnames "./site/"         *asdf-install-location* nil)
        (merge-pathnames "./site-systems/" *asdf-install-location* nil)
        "System-wide install")))

(defun asdf-install (&rest systems)
  (dolist (system systems systems)
    (asdf-install:install system)))

;; (invoke-debugger 't)
;; (setf f (with-open-file (in "/tmp/f") (read in)))
;; (eval f)

;;--------------------
;; (asdf-delete-system system)
;;--------------------
(defun asdf-delete-system (system)
  (remhash (string-downcase system) asdf::*defined-systems*)
  (values))

(export '(asdf-load asdf-load-source asdf-install asdf-delete-system))


(setf  asdf-install:*proxy*        asdf-install:*proxy*
       #+sbcl asdf-install:*sbcl-home* #+sbcl asdf-install:*sbcl-home*
       asdf-install:*cclan-mirror* "http://ftp.linux.org.uk/pub/lisp/cclan/"
       ASDF-INSTALL:*CCLAN-MIRROR* "http://thingamy.com/cclan/"
       ASDF-INSTALL:*CCLAN-MIRROR* "ftp://ftp.ntnu.no/pub/lisp/cclan/"
       ASDF-INSTALL:*CCLAN-MIRROR* "http://www-jcsu.jesus.cam.ac.uk/ftp/pub/cclan/"
       asdf-install:*locations*    (list (first asdf-install:*locations*)))


(setf asdf-install::*connect-timeout*      10
      asdf-install::*read-header-timeout*  10)


#-(or clc-os-debian) 
(push (second (first asdf-install:*locations*))  ASDF:*CENTRAL-REGISTRY*)
#-(or clc-os-debian) 
(push #P"/data/lisp/gentoo/systems/"      asdf:*central-registry*)


(push #P"/usr/share/common-lisp/systems/" asdf:*central-registry*) 



;; asdf-binary-locations is already loaded in sbcl.
;; asdf-binary-locations is mutually exclusive with asdf2.
#-(or sbcl asdf2 clc-os-debian)
(asdf-load :asdf-binary-locations)
#-(or sbcl asdf2 clc-os-debian)
(setf asdf:*centralize-lisp-binaries*     t
      asdf:*include-per-user-information* nil
      asdf:*default-toplevel-directory*
      (merge-pathnames
       (make-pathname :directory '(:relative ".fasls"))
       (user-homedir-pathname))
      asdf:*source-to-target-mappings* '()
      #- (and)
      '((#P"/usr/lib/sbcl/"  #P "/home/pjb/.fasls/lib/sbcl/")
        (#P"/usr/lib64/sbcl/"  #P "/home/pjb/.fasls/lib64/sbcl/"))
      #-(and)
      '((#P"/usr/share/common-lisp/sources/" nil)
        #+sbcl (#P"/usr/lib/sbcl/"   NIL)
        #+sbcl (#P"/usr/lib64/sbcl/" NIL)))



;; (in-package "ASDF")
;; (handler-bind ((warning (function muffle-warning)))
;;   ;; TODO: we should keep the error message in a string
;;   ;;       and check it's only the warnings.
;;   (FLET ((F-OUTPUT-FILES (C)
;;            (FLET ((IMPLEMENTATION-ID ()
;;                     (FLET ((FIRST-WORD (TEXT)
;;                              (LET ((POS (POSITION (CHARACTER " ") TEXT)))
;;                                (REMOVE (CHARACTER ".")
;;                                        (IF POS (SUBSEQ TEXT 0 POS) TEXT)))))
;;                       (FORMAT NIL
;;                         "~A-~A-~A"
;;                         (FIRST-WORD (LISP-IMPLEMENTATION-TYPE))
;;                         (FIRST-WORD (LISP-IMPLEMENTATION-VERSION))
;;                         (FIRST-WORD (MACHINE-TYPE))))))
;;              (LET* ((OBJECT
;;                       (COMPILE-FILE-PATHNAME (ASDF:COMPONENT-PATHNAME C)))
;;                     (PATH
;;                      (MERGE-PATHNAMES
;;                       (MAKE-PATHNAME :DIRECTORY
;;                                      (LIST :RELATIVE
;;                                            (FORMAT NIL
;;                                              "OBJ-~:@(~A~)"
;;                                              (IMPLEMENTATION-ID)))
;;                                      :NAME
;;                                      (PATHNAME-NAME OBJECT)
;;                                      :TYPE
;;                                      (PATHNAME-TYPE OBJECT))
;;                       OBJECT)))
;;                (ENSURE-DIRECTORIES-EXIST PATH)
;;                (LIST PATH)))))
;;     (DEFMETHOD OUTPUT-FILES ((OPERATION COMPILE-OP) (C CL-SOURCE-FILE))
;;       (F-OUTPUT-FILES C))
;;     (DEFMETHOD OUTPUT-FILES ((OPERATION LOAD-OP)    (C CL-SOURCE-FILE))
;;       (F-OUTPUT-FILES C))))
;; (in-package "COM.INFORMATIMAGO.PJB")



;;----------------------------------------------------------------------
;; (WHEN *LOAD-VERBOSE*
;;   (FORMAT T "~& (LOAD \"LOADER:CCLAN.LISP\") ~%")
;;   (FORMAT T "~& (LOAD \"DEFSYSTEM:DEFSYSTEM.LISP\") ~%"))



(UNLESS
    (BLOCK :DONE
      (DOLIST (FILE
                (LIST
                 (#+allegro lp #-allegro identity
                            "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE")
                 (#+allegro lp #-allegro identity
                            "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE.LISP")
                 (MP *PJB-COMM* ("common-lisp") "package")
                 (MP *PJB-COMM* ("common-lisp") "package" "lisp")))
        (HANDLER-CASE (PROGN (LOAD FILE) (RETURN-FROM :DONE T)) (ERROR ())))
      NIL)
  (ERROR "Cannot find COM.INFORMATIMAGO.COMMON-LISP.PACKAGE"))

(push 'PACKAGE:PACKAGE-SYSTEM-DEFINITION
      ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*)



(IMPORT '(COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:DEFINE-PACKAGE
          COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:LIST-ALL-SYMBOLS
          COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:LIST-EXTERNAL-SYMBOLS))


(asdf-load :COM.INFORMATIMAGO.COMMON-LISP)
;; (PACKAGE:LOAD-PACKAGE          "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE")
(USE-PACKAGE                   "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE")
(EXPORT (list-external-symbols "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE"))


(POST-PROCESS-LOGICAL-HOST-TRANSLATIONS)

(PUSH :COM.INFORMATIMAGO.PJB *FEATURES*)

(let ((path
       (merge-pathnames
        (make-pathname
         :directory '(:relative "DRIBBLES")
         :name (FLET ((implementation-id ()
                        (flet ((first-word (text)
                                 (let ((pos (position (character " ") text)))
                                   (remove (character ".")
                                           (if pos (subseq text 0 pos) text)))))
                          (format
                           nil
                           "~A-~A-~A"
                           (cond 
                            ((string-equal
                              "International Allegro CL Enterprise Edition"
                              (lisp-implementation-type))
                             "ACL")
                            (t (first-word (lisp-implementation-type))))
                           (first-word (lisp-implementation-version))
                           (first-word (machine-type))))))
                 (multiple-value-bind (se mi ho da mo ye)
                     (decode-universal-time (get-universal-time))
                   (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D-~A"
                           ye mo da ho mi se (implementation-id))))
         :type "DRIBBLE"
         :version nil
         :defaults (user-homedir-pathname))
         (user-homedir-pathname) nil)))
  (ensure-directories-exist path)
  (dribble path))

(defvar *inp* (make-synonym-stream '*standard-input*))
(defvar *out* (make-synonym-stream '*standard-output*))
(export '(*inp* *out*))





;; (in-package :asdf)
;; (defparameter *output-file-debug* nil)
;; (defmethod output-files :before ((operation compile-op) (component source-file)) 
;;   (let ((source (component-pathname component)))
;;     (print (setf *output-file-debug*
;;                  (list 'output-files-for-system-and-operation 
;;                        (component-system component) operation component source)))))
;; (in-package :cl-user)

;;;; THE END ;;;;
