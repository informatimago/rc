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

;;;----------------------------------------------------------------------
;;;
;;; Clean the packages imported into COMMON-LISP-USER:
;;;

(MAPC (LAMBDA (package) (UNUSE-PACKAGE package "COMMON-LISP-USER"))
      (set-difference
       (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))
       (delete nil (list ;; A list of all the "CL" packages possible:
                    (FIND-PACKAGE "COMMON-LISP")
                    (FIND-PACKAGE "IMAGE-BASED-COMMON-LISP")))))

;;;----------------------------------------------------------------------
;;;
;;; COM.INFORMATIMAGO.PJB
;;;

(DEFPACKAGE "COM.INFORMATIMAGO.PJB"
  (:NICKNAMES "PJB")
  (:USE "COMMON-LISP"))
(IN-PACKAGE "COM.INFORMATIMAGO.PJB")

(setf *print-circle* t
      *print-length* nil
      *print-level*  nil
      *print-lines*  nil)

(declaim (OPTIMIZE (SAFETY 3) (SPACE 0) (SPEED 0) (DEBUG 3)))


;;;----------------------------------------------------------------------
;;;
;;; Logical hosts -- the Common-Lisp way to PATH 
;;;

(progn 
  (defvar *directories*  '())
  (defun get-directory (key &optional (subpath ""))
    "
Caches the ~/directories.txt file that contains a map of
directory keys to pathnames, into *DIRECTORIES*.

Then builds and returns a pathname made by merging the directory
selected by KEY, and the given SUBPATH.
"
    (unless *directories*
      (with-open-file (dirs (merge-pathnames
                             (make-pathname :name "DIRECTORIES" :type "TXT"
                                            :version nil :case :common)
                             (user-homedir-pathname)
                             nil))
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
  (DEFVAR *LOGICAL-HOSTS* '()
    "A list of logical hosts defined by DEFINE-LOGICAL-PATHNAME-TRANSLATIONS (or DEF-LP-TRANS).
COMMON-LISP doesn't provide any introspection function for logical hosts."))

(defun list-logical-hosts ()
  "Return a list of logical hosts defined by DEFINE-LOGICAL-PATHNAME-TRANSLATIONS (or DEF-LP-TRANS)."
  (copy-list *logical-hosts*))

(EXPORT '(*directories* list-logical-hosts))


(defun make-translations (host logical-dir physical-dir &optional file-type)
  "
Returns logical pathname translations for the given HOST, mapping the
logical directory LOGICAL-DIR and all the files with the given
FILE-TYPE, in its subdirectories, to the physical directory
PHYSICAL-DIR.

If no FILE-TYPE is given, or if it's NIL, then a wildcard is used for
the file type, and the logical pathnames are translated with and
without this wildcard, in an orden that's implementation dependant.
The inclusion  of a version wildcard is also implementation dependant.
"
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
   #+(or allegro ccl)
   (if file-type
       `(((:name :wild :type ,file-type :version nil) ,(format nil "*.~(~A~)" file-type)))
       '(((:name :wild :type nil   :version nil) "*")
         ((:name :wild :type :wild :version nil) "*.*")))
   #-(OR clisp sbcl allegro ccl) 
   (if file-type 
       `(((:name :wild :type ,file-type :version nil)   ,(format nil "*.~(~A~)" file-type))
         ((:name :wild :type ,file-type :version :wild) ,(format nil "*.~(~A~)" file-type)))
       '(((:name :wild :type nil   :version nil)   "*")
         ((:name :wild :type :wild :version nil)   "*.*")
         ((:name :wild :type :wild :version :wild) "*.*")))))

 
(DEFUN DEFINE-LOGICAL-PATHNAME-TRANSLATIONS (HOST PATH &OPTIONAL (SUBPATH ""))
  "
Defines a new logical pathname (or overrides an existing one)
for the given HOST, unless the logical HOST already exists.

PATH and SUBPATH must be physical pathnames and subpaths.  The logical
pathname translation maps logical pathnames directly from the root of
the HOST, to the given path and subpath concatenated.

The HOST is added to the list of logical hosts defined.
"
  (PUSHNEW HOST *LOGICAL-HOSTS* :TEST (FUNCTION STRING-EQUAL))
  ;; If the HOST is already defined we don't change it (eg. HOME):
  (UNLESS (HANDLER-CASE (LOGICAL-PATHNAME-TRANSLATIONS HOST) (ERROR () NIL))
    (and (ignore-errors (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST) NIL) t)
         (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
               (make-translations
                host '() (format nil "~A~:[~;~:*~A~]"
                                 path (if (string= "" subpath) nil subpath)))))))

;;;------------------------------------------------------------------------

(DEFPARAMETER *asdf-install-location*  (get-directory :asdf-install))
(DEFPARAMETER *SHARE-LISP*             (get-directory :share-lisp))
(DEFPARAMETER *PJB-COMM*               (get-directory :lisp-sources))
(DEFPARAMETER *PJB-LISP*               (get-directory :pjb-lisp))

(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "SHARE-LISP" *SHARE-LISP*)
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "PJB-COMM"   *PJB-COMM*)
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "PJB-LISP"   *PJB-LISP*)
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "CMU-AI"     *SHARE-LISP* "ai/")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "CL-PDF"     *SHARE-LISP* "cl-pdf/")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "UFFI"       *SHARE-LISP* "uffi/")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES"   *SHARE-LISP* "packages/")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "CLOCC"      *SHARE-LISP* "packages/net/sourceforge/clocc/clocc/")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "CCLAN"      *SHARE-LISP* "packages/net/sourceforge/cclan/")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "DEFSYSTEM"  *SHARE-LISP*
  ;; We must go thru a translation for defsystem-3.x isn't a valid logical name!
  "packages/net/sourceforge/clocc/clocc/src/defsystem-3.x/")


(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "HOME"     (USER-HOMEDIR-PATHNAME)   "")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "LOADERS"  *PJB-COMM*        "cl-loaders/")
;;(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "NORVIG"   *PJB-LISP*        "norvig/")
(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "NORVIG"   #P"/home/pjb/src/lisp/ai/"    "norvig-paip-pjb/")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun POST-PROCESS-LOGICAL-HOST-TRANSLATIONS ()
    (WHEN (FBOUNDP 'COMMON-LISP-USER::POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS)
      (MAP NIL
           (FUNCTION COMMON-LISP-USER::POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS)
           *LOGICAL-HOSTS*)))) 

(POST-PROCESS-LOGICAL-HOST-TRANSLATIONS)


;;;------------------------------------------------------------------------
;;;
;;; ADSF
;;;


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
      (progn (LOAD file)
             (return :success))
    #-clisp
    (FILE-ERROR (err)
      (format *error-output* "Got error ~A ; trying again.~%" err)
      (format *error-output* "Translations are: ~S~%"
              (logical-pathname-translations "PACKAGES"))
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
                 :defaults (user-homedir-pathname))
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


;; (asdf:SYSTEM-SOURCE-DIRECTORY  :cl-org-mode)  --> directory where the asd file lies.
;; (asdf:SYSTEM-SOURCE-file  :cl-org-mode)       --> asd file.
;; (asdf:module-components  (asdf:find-system :cl-org-mode)) --> modules
;;
;; (mapcar (alexandria:curry 'asdf:component-depends-on 'asdf:load-op)
;;         (asdf:module-components  (asdf:find-system :cl-org-mode)))

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASDF-BINARY-LOCATIONS
;;;

(defun hostname ()
  (let ((outpath (format nil "/tmp/hostname-~8,'0X.txt" (random #x100000000))))
    (unwind-protect
         (progn
           (asdf:run-shell-command "( hostname --fqdn 2>/dev/null || hostname --long 2>/dev/null || hostname ) > ~A"
                                   outpath)
           (with-open-file (hostname outpath)
             (read-line hostname)))
      (delete-file outpath))))


;; asdf-binary-locations is already loaded in sbcl.
;; asdf-binary-locations is mutually exclusive with asdf2.
;; (or sbcl asdf2 clc-os-debian)

(let ((sym (find-symbol "ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY" "ASDF")))
  (when (and sym (fboundp sym))
    (push :HAS-ASDF-ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY *features*)))

#+HAS-ASDF-ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY
(asdf:enable-asdf-binary-locations-compatibility
 :centralize-lisp-binaries     t
 :default-toplevel-directory   (merge-pathnames (format nil ".cache/common-lisp/~A/" (hostname))
                                              (user-homedir-pathname) nil)
 :include-per-user-information nil
 ;; :map-all-source-files ???
 :source-to-target-mappings    nil)

#-HAS-ASDF-ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY
(progn
  ;; (push-asdf-repository #P"LIBCL:ASDF-BINARY-LOCATIONS;")
  (handler-case
      (progn (asdf-load :asdf-binary-locations)
             (pushnew :asdf-binary-locations *features*))
    (error (err)
      (warn "ASDF-BINARY-LOCATIONS is not available."))))

#+(and (not HAS-ASDF-ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY)
       asdf-binary-locations)
(setf asdf:*centralize-lisp-binaries*     t
      asdf:*include-per-user-information* nil
      asdf:*default-toplevel-directory*
      (merge-pathnames (format nil ".cache/common-lisp/~A/" (hostname))
                       (user-homedir-pathname) nil)
      asdf:*source-to-target-mappings* '())

;; (setf asdf:*source-to-target-mappings*
;;       #- (and)
;;       '((#P"/usr/lib/sbcl/"  #P "/home/pjb/.fasls/lib/sbcl/")
;;         (#P"/usr/lib64/sbcl/"  #P "/home/pjb/.fasls/lib64/sbcl/"))
;;       #-(and)
;;       '((#P"/usr/share/common-lisp/sources/" nil)
;;         #+sbcl (#P"/usr/lib/sbcl/"   NIL)
;;         #+sbcl (#P"/usr/lib64/sbcl/" NIL)))


;; (in-package :asdf)
;; (defparameter *output-file-debug* nil)
;; (defmethod output-files :before ((operation compile-op) (component source-file)) 
;;   (let ((source (component-pathname component)))
;;     (print (setf *output-file-debug*
;;                  (list 'output-files-for-system-and-operation 
;;                        (component-system component) operation component source)))))
;; (in-package :cl-user)


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


;;;----------------------------------------------------------------------
;;;
;;; QuickLisp
;;;

(let ((quicklisp (merge-pathnames (make-pathname :directory '(:relative "QUICKLISP")
                                                 :name "SETUP"
                                                 :type "LISP"
                                                 :case :common
                                                 :defaults (user-homedir-pathname))
                                  (user-homedir-pathname)
                                  nil)))
  (when (probe-file quicklisp)
    (load quicklisp)))


;;;----------------------------------------------------------------------
;;;
;;; Alexandria
;;;

(let* ((alexandria (merge-pathnames (make-pathname :directory '(:relative "LISP" "LIBCL" ".ASDF")
                                                   :case :common
                                                   :defaults (user-homedir-pathname))
                                    (user-homedir-pathname)
                                    nil))
       (asd-file   (merge-pathnames (make-pathname :name  "ALEXANDRIA"
                                                   :type "ASD"
                                                   :case :common
                                                   :defaults alexandria)
                                    alexandria
                                    nil)))
  (when (probe-file asd-file)
    (pushnew alexandria asdf:*central-registry* :test #'equal)))

;; (load #P"/home/pjb/lisp/libcl/compile-libcl.lisp")


;;----------------------------------------------------------------------
;; (WHEN *LOAD-VERBOSE*
;;   (FORMAT T "~& (LOAD \"LOADER:CCLAN.LISP\") ~%")
;;   (FORMAT T "~& (LOAD \"DEFSYSTEM:DEFSYSTEM.LISP\") ~%"))


;;;
;;; This is not necessary anymore, because we use the standard ASDF mechanism to load :com.informatimago.common-lisp.
;;;
;;
;; (UNLESS
;;     (BLOCK :load-package.lisp
;;       (macrolet ((MP (PATHNAME &OPTIONAL
;;                          (DIRECTORY NIL DIRECTORY-P)
;;                          (NAME      NIL NAME-P)
;;                          (TYPE      NIL TYPE-P)
;;                          (VERSION   NIL VERSION-P))
;;              `(MERGE-PATHNAMES
;;                (MAKE-PATHNAME,@(WHEN DIRECTORY-P `(:DIRECTORY '(:RELATIVE ,@DIRECTORY)))
;;                              ,@(WHEN NAME-P      `(:NAME      ,NAME))
;;                              ,@(WHEN TYPE-P      `(:TYPE      ,TYPE))
;;                              ,@(WHEN VERSION-P   `(:VERSION   ,VERSION))
;;                              :DEFAULTS ,PATHNAME)
;;                ,PATHNAME
;;                nil)))
;;         (DOLIST (FILE
;;                   (LIST
;                     #P"PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;CESARUM;PACKAGE"
;;                    #P"PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;CESARUM;PACKAGE.LISP"
;;                    (mp *PJB-COMM* ("common-lisp" "cesarum") "package")
;;                    (mp *PJB-COMM* ("common-lisp" "cesarum") "package" "lisp")
;;                    (merge-pathnames #P"COMMON-LISP;CESARUM;PACKAGE"      *PJB-COMM* nil)
;;                    (merge-pathnames #P"COMMON-LISP;CESARUM;PACKAGE.LISP" *PJB-COMM* nil)
;;                    (merge-pathnames #P"common-lisp/cesarum/package"      *PJB-COMM* nil)
;;                    (merge-pathnames #P"common-lisp/cesarum/package.lisp" *PJB-COMM* nil)))
;;           (HANDLER-CASE (PROGN (LOAD FILE) (RETURN-FROM :DONE T)) (ERROR ()))))
;;       NIL)
;;   (ERROR "Cannot find COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"))
;;
;;;
;;; We don't use PACKAGE-SYSTEM-DEFINITION anymore.
;;;
;; (push 'COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:PACKAGE-SYSTEM-DEFINITION
;;       ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*)


;; (IMPORT '(COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:DEFINE-PACKAGE
;;           COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:LIST-ALL-SYMBOLS
;;           COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:LIST-EXTERNAL-SYMBOLS))


;;;----------------------------------------------------------------------
;;;
;;; com.informatimago libraries
;;;

(setf asdf:*central-registry*
      (append (remove-duplicates
               (mapcar (lambda (path)
                         (make-pathname :name nil :type nil :version nil :defaults path))
                       (directory #P"PACKAGES:COM;INFORMATIMAGO;**;*.ASD"))
               :test (function equalp))
              asdf:*central-registry*))


               (asdf-load  :com.informatimago.common-lisp)
#-(or ccl ecl) (asdf-load  :com.informatimago.clext)
               (asdf-load  :com.informatimago.clmisc)
#+sbcl         (asdf-load  :com.informatimago.sbcl)
#+clisp        (asdf-load  :com.informatimago.clisp)
#+clisp        (asdf-load  :com.informatimago.susv3)



(USE-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")
(EXPORT     (COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:LIST-EXTERNAL-SYMBOLS
             "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"))

(PUSH :COM.INFORMATIMAGO.PJB *FEATURES*)


;;;----------------------------------------------------------------------

(defmacro defautoload (name arguments loader)
  "Defines a function that will laud the LOADER file, before calling itself again."
  `(defun ,name ,arguments
     (load ,loader)
     (eval '(,name ,@arguments))))
(export '(defautoload))


(defautoload scheme () "LOADERS:PSEUDO")
(export '(scheme))

;;;----------------------------------------------------------------------


(defun start-dribble ()
  "We dribble to a timestamped file in a specific #P\"HOME:DRIBBLE;\" directory."
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
    (dribble path)))

;; (start-dribble)

;;;----------------------------------------------------------------------

(defvar *inp* (make-synonym-stream '*standard-input*)  "Synonym to *standard-input*")
(defvar *out* (make-synonym-stream '*standard-output*) "Synonym to *standard-output*")
(export '(*inp* *out*))

;;;----------------------------------------------------------------------


;;;; THE END ;;;;
