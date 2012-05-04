;;;; -*- mode:lisp; coding:utf-8; -*-
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

(in-package "COMMON-LISP-USER")

;;;----------------------------------------------------------------------
;;;
;;; Clean the packages imported into COMMON-LISP-USER:
;;;

(mapc (lambda (package) (unuse-package package "COMMON-LISP-USER"))
      (set-difference
       (copy-seq (package-use-list "COMMON-LISP-USER"))
       (delete nil (list ;; A list of all the "CL" packages possible:
                    (find-package "COMMON-LISP")
                    (find-package "IMAGE-BASED-COMMON-LISP")))))


(setf *print-circle* t
      *print-length* nil
      *print-level*  nil
      *print-lines*  nil)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))



;;;----------------------------------------------------------------------
;;;
;;; COM.INFORMATIMAGO.PJB
;;;

(defpackage "COM.INFORMATIMAGO.PJB"
  (:nicknames "PJB")
  (:use "COMMON-LISP")
  (:export "HOSTNAME"
           "LIST-DIRECTORIES"   "GET-DIRECTORY"
           "LIST-LOGICAL-HOSTS" "DEFINE-LOGICAL-HOST-TRANSLATIONS"
           "ASDF-LOAD" "ASDF-LOAD-SOURCE" "ASDF-INSTALL" "ASDF-DELETE-SYSTEM"
           "FIND-ASDF-SUBDIRECTORIES"  "UPDATE-ASDF-REGISTRY"
           "START-DRIBBLE"
           "DEFAUTOLOAD"
           "*INP*" "*OUT*"
           "SCHEME"
           "QUICK-UPDATE" "QUICK-CLEAN"  "QUICK-INSTALL-ALL" "QUICK-UNINSTALL"
           "QUICK-APROPOS" "QUICK-LIST-SYSTEMS" "QUICK-WHERE" "QUICK-WHERE-IS"
           "QUICK-INSTALLED-SYSTEMS" "QUICK-LIST-PROJECTS"
           "QUICK-DELETE" "QUICK-RELOAD" "QUICK-LOCAL-PROJECTS")
  (:documentation "

This package contains REPL utilities, defined in ~/rc/common.lisp,
which is loaded from the various rc files of the various CL
implementations.

It also re-exports the exported symbols of
COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <a href=\"http://www.gnu.org/licenses/\">http://www.gnu.org/licenses/</a>.

"))
(in-package "COM.INFORMATIMAGO.PJB")


(defun user-pathname ()
  "On MS-Windows, it's not the USER-HOMEDIR-PATHNAME."
  #+windows-target (let ((home (ccl::getenv "HOME")))
                     (if home
                         (pathname (format nil "~A\\" home))
                         #P"C:\\cygwin\\home\\pjb\\"))
  #-windows-target (USER-HOMEDIR-PATHNAME))


(defun make-pathname* (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
                       (name nil namep) (type nil typep) (version nil versionp)
                       (defaults nil defaultsp) (case :local casep))
  (declare (ignorable casep))
  #+ (or abcl ccl allegro)
  (labels ((localize (object)
             (typecase object
               (list   (mapcar (function localize) object))
               (string (string-downcase object))
               (t      object)))
           (parameter (indicator key value)
             (when indicator
               (list key (if (eql case :common)
                             (localize value)
                             value)))))
    (apply (function make-pathname)
           (append (parameter hostp      :host      host)
                   (parameter devicep    :device    device)
                   (parameter directoryp :directory directory)
                   (parameter namep      :name      name)
                   (parameter typep      :type      type)
                   (parameter versionp   :version   version)
                   (parameter defaultsp  :defaults  defaults)
                   (list :case :local))))
  #-(or abcl ccl allegro)
  (apply (function make-pathname)
         (append
          (when hostp      (list :host      host))
          (when devicep    (list :device    device))
          (when directoryp (list :directory directory))
          (when namep      (list :name      name))
          (when typep      (list :type      type))
          (when versionp   (list :version   version))
          (when defaultsp  (list :defaults  defaults))
          (when casep      (list :case      case)))))


;;;----------------------------------------------------------------------
;;;
;;; ASDF-BINARY-LOCATIONS
;;;


;; Some implementations don't have ASDF loaded at this point.  For
;; them, we create a temporary ASDF package exporting the symbols we
;; need to write to the ~/quicklisp/adsf-config/init.lisp file.
;;
;; Some other implementations have ASDF loaded, but don't define these
;; symbols.  For them, we add these symbols temporarily, so that we
;; can write them to the  ~/quicklisp/adsf-config/init.lisp file.

(defvar *asdf* (find-package "ASDF"))
(defvar *asdf-added-symbols* '())
(defvar *asdf-symbol-names* '("RUN-SHELL-COMMAND"
                              "OOS" "LOAD-OP"
                              "ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY"
                              "*CENTRAL-REGISTRY*"
                              "*CENTRALIZE-LISP-BINARIES*"
                              "*INCLUDE-PER-USER-INFORMATION*" 
                              "*DEFAULT-TOPLEVEL-DIRECTORY*"
                              "*SOURCE-TO-TARGET-MAPPINGS*"))
(if *asdf*
    (dolist (name *asdf-symbol-names*)
      (unless (find-symbol name *asdf*)
        (let ((sym  (intern  name *asdf*)))
          (export sym *asdf*)
          (push sym *asdf-added-symbols*))))
    (unless (find-package "ASDF")
     (defpackage "ASDF"
       (:use "CL")
       (:export . #.*asdf-symbol-names*))))

;; We compute the hostname.
#-(and)
(let ((ql-asdf-init-file (merge-pathnames
                          (make-pathname* :directory '(:relative "QUICKLISP" "ASDF-CONFIG")
                                          :name "INIT" :type "LISP" :version :newest :case :common 
                                          :defaults (user-pathname))
                          (user-pathname)
                          nil)))
  ;; (print ql-asdf-init-file) (terpri) (finish-output)
  (when (or (not (ignore-errors (probe-file ql-asdf-init-file)))
            (<= (file-write-date ql-asdf-init-file)
                (file-write-date *load-pathname*)))
    (ensure-directories-exist ql-asdf-init-file)
    (with-open-file (src ql-asdf-init-file
                         :direction :output
                         :if-exists :supersede 
                         :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-readably* t))
          (dolist (form '(
                          (unless (find-package "COM.INFORMATIMAGO.PJB")
                            (defpackage "COM.INFORMATIMAGO.PJB"
                              (:use "COMMON-LISP")))
                          (in-package "COM.INFORMATIMAGO.PJB")
                          
                          (defparameter *init-verbose* nil)
                          
                          
                          (defun hostname ()
                            #-windows-target
                            (let ((outpath (make-pathname :name (format nil "hostname-~8,'0X" (random #x100000000))
                                                          :type "txt"
                                                          :case :local
                                                          :defaults (user-pathname))))
                              (unwind-protect
                                   (let ((asdf::*verbose-out* t))
                                     (asdf:run-shell-command
                                      "( hostname --fqdn 2>/dev/null || hostname --long 2>/dev/null || hostname ) > ~A"
                                      (namestring outpath))
                                     (with-open-file (hostname outpath)
                                       (read-line hostname)))
                                (delete-file outpath)))
                            #+windows-target (machine-instance))




                          ;; asdf-binary-locations is already loaded in sbcl.
                          ;; asdf-binary-locations is mutually exclusive with asdf2.
                          ;; (or sbcl asdf2 clc-os-debian)

                          #2=(let ((sym (find-symbol "ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY" "ASDF")))
                               (if (and sym (fboundp sym))
                                   (progn
                                     (when *init-verbose*
                                       (format t "~&Pushing :has-asdf-enable-asdf-binary-locations-compatibility to *features*~%"))
                                     (pushnew :has-asdf-enable-asdf-binary-locations-compatibility *features*))
                                   (when *init-verbose*
                                     (format t "~&There's no asdf:enable-asdf-binary-locations-compatibility ~%"))))

                          (sharp - :has-asdf-enable-asdf-binary-locations-compatibility)
                          (progn
                            (handler-case (progn (when *init-verbose*
                                                   (format t "~&Asdf loading asdf-binary-locations ~%"))
                                                 (asdf:oos 'asdf:load-op :asdf-binary-locations)
                                                 (when *init-verbose*
                                                   (format t "~&Got it, pushing :asdf-binary-locations to *features*~%"))
                                                 (pushnew :asdf-binary-locations *features*))
                              (error ()
                                (when *init-verbose*
                                  (format t "~&Pushing /data/lisp/asdf-install/site/asdf-binary-locations/ to asdf:*central-registry* ~%"))
                                (push #P"/data/lisp/asdf-install/site/asdf-binary-locations/"
                                      asdf:*central-registry*)
                                (progn (when *init-verbose*
                                         (format t "~&Asdf loading asdf-binary-locations ~%"))
                                       (asdf:oos 'asdf:load-op :asdf-binary-locations)
                                       (when *init-verbose*
                                         (format t "~&Got it, pushing :asdf-binary-locations to *features*~%"))
                                       (pushnew :asdf-binary-locations *features*)))))
                          
                          #2#

                          (sharp + :has-asdf-enable-asdf-binary-locations-compatibility)
                          (progn
                            (when *init-verbose*
                              (format t "~&Using asdf:enable-asdf-binary-locations-compatibility ~%"))
                            (asdf:enable-asdf-binary-locations-compatibility
                             :centralize-lisp-binaries     t
                             :default-toplevel-directory   (truename
                                                            (merge-pathnames
                                                             (format nil ".cache/common-lisp/~A/" (hostname))
                                                             (user-pathname) nil))
                             :include-per-user-information nil
                             ;; :map-all-source-files ???
                             :source-to-target-mappings    nil))
                          
                          (sharp + (:and (:not :has-asdf-enable-asdf-binary-locations-compatibility)
                                    :asdf-binary-locations))
                          (progn
                            (when *init-verbose*
                              (format t "~&Using asdf-binary-locations ~%"))
                            (setf asdf:*centralize-lisp-binaries*     t
                                  asdf:*include-per-user-information* nil
                                  asdf:*default-toplevel-directory*
                                  (truename (merge-pathnames
                                             (format nil ".cache/common-lisp/~A/" (hostname))
                                             (user-pathname) nil))
                                  asdf:*source-to-target-mappings* '()))

                          (sharp - (:or :has-asdf-enable-asdf-binary-locations-compatibility :asdf-binary-locations))
                          (error "ASDF-BINARY-LOCATIONS is not available.")))
            
            (if (and (listp form) (eql 'sharp (first form)))
                (format src "~2%#~A~S " (second form) (third form))
                (pprint form src))))))))


(if *asdf*
    (dolist (sym *asdf-added-symbols*)
      (unintern sym *asdf*))
    (delete-package "ASDF"))


;;;----------------------------------------------------------------------
;;;
;;; QuickLisp
;;;

(let ((quicklisp (merge-pathnames
                  (make-pathname* :directory '(:relative "QUICKLISP")
                                  :name "SETUP"
                                  :type "LISP"
                                  :version :newest
                                  :case :common
                                  :defaults (user-pathname))
                  (user-pathname)
                  nil)))
  (if (probe-file quicklisp)
      (load quicklisp)
      (error "Please install quicklisp.  I expect it in ~S" quicklisp)))


;; (format t "~2%asdf:*central-registry* = ~S~2%" asdf:*central-registry*)


(defun print-systems (systems pattern)
  (if pattern
      (let ((spattern (string pattern)))
        (dolist (system systems)
          (when (search spattern (slot-value system 'ql-dist:name)
                        :test (function char-equal))
            (print system))))
      (dolist (system systems)
        (print system)))
   (values))


(defun quick-installed-systems (&optional pattern)
  "Print the system installed by quicklisp."
  (print-systems (ql-dist:installed-releases (ql-dist:dist "quicklisp"))
                 pattern))

(defun quick-list-systems (&optional pattern)
  "List the quicklisp systems.  If the string designator PATTERN is
given, then only the systems containing it in their name are listed."
  (print-systems (ql-dist:provided-systems t)
                 pattern))

(defun quick-list-projects (&optional pattern)
  "List the quicklisp projects (releases).  If the string designator
PATTERN is given, then only the projects containing it in their name
are listed."
  (print-systems (ql-dist:provided-releases t)
                 pattern))


(defun quick-apropos (pattern)
  "Search the quicklisp system matching the pattern and print them."
  ;; For now, we just list the systems:
  (print-systems (ql-dist:provided-systems t) pattern))


(defun quick-update ()
  "Updates the quicklisp client, and all the system distributions."
  (ql:update-client)
  (ql:update-all-dists)) 

(defun quick-clean ()
  "Clean the quicklisp system distributions."
  #+#.(cl:if (cl:find-symbol "CLEAN" "QL-DIST") '(:and) '(:or))
  (map nil 'ql-dist:clean (ql-dist:enabled-dists))
  #-#.(cl:if (cl:find-symbol "CLEAN" "QL-DIST") '(:and) '(:or))
  (error "QL-DIST:CLEAN is not available."))

(defun quick-install-all (&key verbose)
  "Installs all the quicklisp systems, skipping over the errors."
  (map nil (lambda (system)
             (handler-case
                 (progn
                   (when verbose
                     (format *trace-output* "~&~A~%" system))
                   (ql-dist:ensure-installed system))
               (error (err)
                 (format *trace-output* "~&~A ~A~%" system err))))
       (ql-dist:provided-systems t)))

(defun quick-uninstall (&rest systems)
  "Uninstall the given systems releases from the quicklisp installation."
  (map 'list (lambda (system)
               (ql-dist:uninstall (ql-dist:release (string-downcase system))))
       systems))


(defun quick-where-is (&rest systems)
  "Says where the given systems are."
  #+#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  (map 'list (lambda (system) (ql:where-is-system (string-downcase system)))
       systems)
  #-#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  (error "QUICKLISP-CLIENT:WHERE-IS-SYSTEM is not available."))

(defun quick-where (&rest systems)
  "Says where the given systems are."
  (apply (function quick-where-is) systems))


(defun quick-delete (&rest systems)
  "Delete the ASDF systems so they'll be reloaded."
  (map 'list (lambda (system) (asdf-delete-system system)) systems))

(defun quick-reload (&rest systems)
  "Delete and reload the ASDF systems."
  (map 'list (lambda (system)
               ;; (asdf-delete-system system)
               (format *trace-output* "~&See also M-x slime-load-system RET~%")
               (force-output  *trace-output*)
               (asdf:load-system system)
               (ql:quickload system))
       systems))

(defun quick-local-projects ()
  "Rebuilds the local projects system index."
  (ql:register-local-projects))



;;;----------------------------------------------------------------------
;;;
;;; Directories.
;;;

(defvar *directories*  '())

(defun list-directories ()
  "Returns the list of named directories."
  (copy-seq *directories*))

(defun get-directory (key &optional (subpath ""))
  "
Caches the ~/directories.txt file that contains a map of
directory keys to pathnames, into *DIRECTORIES*.

Then builds and returns a pathname made by merging the directory
selected by KEY, and the given SUBPATH.
"
  (unless *directories*
    (with-open-file (dirs (merge-pathnames
                           (make-pathname* :name "DIRECTORIES" :type "TXT"
                                          :version nil :case :common)
                           (user-pathname)
                           nil))
      (loop
         :for k = (read dirs nil dirs)
         :until (eq k dirs)
         :do (push (string-trim " " (read-line dirs)) *directories*)
         :do (push (intern (substitute #\- #\_ (string k))
                           "KEYWORD") *directories*))))
  (unless (getf *directories* key)
    (error "~S: No directory keyed ~S" 'get-directory key))
  (merge-pathnames subpath (getf *directories* key) nil))


;;;----------------------------------------------------------------------
;;;
;;; Logical hosts -- the Common-Lisp way to PATH 
;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *logical-hosts* '()
    "A list of logical hosts defined by DEFINE-LOGICAL-PATHNAME-TRANSLATIONS.
COMMON-LISP doesn't provide any introspection function for logical hosts."))

(defun list-logical-hosts ()
  "Return a list of logical hosts defined by DEFINE-LOGICAL-PATHNAME-TRANSLATIONS."
  (copy-list *logical-hosts*))

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
       (list (apply (function make-pathname*)
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
   #-(or clisp sbcl allegro ccl) 
   (if file-type 
       `(((:name :wild :type ,file-type :version nil)   ,(format nil "*.~(~A~)" file-type))
         ((:name :wild :type ,file-type :version :wild) ,(format nil "*.~(~A~)" file-type)))
       '(((:name :wild :type nil   :version nil)   "*")
         ((:name :wild :type :wild :version nil)   "*.*")
         ((:name :wild :type :wild :version :wild) "*.*")))))

 
(defun define-logical-pathname-translations (host path &optional (subpath ""))
  "
Defines a new logical pathname (or overrides an existing one)
for the given HOST, unless the logical HOST already exists.

PATH and SUBPATH must be physical pathnames and subpaths.  The logical
pathname translation maps logical pathnames directly from the root of
the HOST, to the given path and subpath concatenated.

The HOST is added to the list of logical hosts defined.
"
  (pushnew host *logical-hosts* :test (function string-equal))
  ;; If the HOST is already defined we don't change it (eg. HOME):
  (unless (handler-case (logical-pathname-translations host) (error () nil))
    (and (ignore-errors (setf (logical-pathname-translations host) nil) t)
         (setf (logical-pathname-translations host)
               (make-translations
                host '() (format nil "~A~:[~;~:*~A~]"
                                 path (if (string= "" subpath) nil subpath)))))))

;;;------------------------------------------------------------------------

(defparameter *asdf-install-location*  (get-directory :asdf-install))
(defparameter *share-lisp*             (get-directory :share-lisp))
(defparameter *pjb-comm*               (get-directory :lisp-sources))
(defparameter *pjb-lisp*               (get-directory :pjb-lisp))

(define-logical-pathname-translations "SHARE-LISP" *share-lisp*)
(define-logical-pathname-translations "PJB-COMM"   *pjb-comm*)
(define-logical-pathname-translations "PJB-LISP"   *pjb-lisp*)
(define-logical-pathname-translations "CMU-AI"     *share-lisp* "ai/")
(define-logical-pathname-translations "CL-PDF"     *share-lisp* "cl-pdf/")
(define-logical-pathname-translations "UFFI"       *share-lisp* "uffi/")
(define-logical-pathname-translations "PACKAGES"   *share-lisp* "packages/")
(define-logical-pathname-translations "CLOCC"      *share-lisp* "packages/net/sourceforge/clocc/clocc/")
(define-logical-pathname-translations "CCLAN"      *share-lisp* "packages/net/sourceforge/cclan/")
(define-logical-pathname-translations "DEFSYSTEM"  *share-lisp*
  ;; We must go thru a translation for defsystem-3.x isn't a valid logical name!
  "packages/net/sourceforge/clocc/clocc/src/defsystem-3.x/")


(define-logical-pathname-translations "HOME"     (user-pathname)   "")
(define-logical-pathname-translations "LOADERS"  *pjb-comm*        "cl-loaders/")
;;(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "NORVIG"   *PJB-LISP*        "norvig/")
(define-logical-pathname-translations "NORVIG"   #p"/home/pjb/src/lisp/ai/"    "norvig-paip-pjb/")


;;;------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun post-process-logical-host-translations ()
    (when (fboundp 'common-lisp-user::post-process-logical-pathname-translations)
      (map nil
           (function common-lisp-user::post-process-logical-pathname-translations)
           *logical-hosts*)))) 

(post-process-logical-host-translations)




;;;------------------------------------------------------------------------
;;;
;;; ADSF
;;;


(defun asdf-load (&rest systems)
  "Load the ASDF systems.  See also (QL:QUICKLOAD system) to install them."
  (dolist (system systems systems)
    #+quicklisp (ql:quickload system)
    #-quicklisp (asdf:operate 'asdf:load-op system)))


(defun asdf-load-source (&rest systems)
  "Load the sources of the ASDF systems.  See also (QL:QUICKLOAD system) to install them."
  (dolist (system systems systems)
    (asdf:operate 'asdf:load-source-op system)))

(defun asdf-install (&rest systems)
  "Download and install a system.  Now, uses quicklisp."
  (dolist (system systems systems)
    #+quicklisp (ql:quickload system)
    #-quicklisp (error "Please install and use quicklisp!")))

(defun asdf-delete-system (system)
  "Clear the system from ASDF, to force reloading them on next ASDF-LOAD."
  ;;(remhash (string-downcase system) asdf::*defined-systems*)
  (asdf:clear-system system)
  (values))


;;;------------------------------------------------------------------------
;;;


(defparameter *asdf-interval-between-rescan* (* 7 24 60 60)
  "Force rescan at leastr once this amount of seconds.")

(defparameter *asdf-registry-file*
  (merge-pathnames (user-pathname)
                   (make-pathname* :name "ASDF-CENTRAL-REGISTRY" :type "DATA" :version :newest :case :common
                                   :defaults (user-pathname))
                   nil)
  "Cache file.")

(defparameter *original-asdf-registry* asdf:*central-registry*)

(defun find-asdf-subdirectories (&optional (directories (list #p"PACKAGES:" *asdf-install-location*)))
  "Return a list of all the subdirectories of DIRECTORIES that contain .asd files.
It is sorted in ascending namestring length."
  (format *trace-output* "~&;; Scanning ASDF packages...~%")
  (prog1
      (sort 
       (delete-duplicates 
        (mapcar
         (lambda (p) (make-pathname* :name nil :type nil :version nil :defaults p))
         (mapcan (lambda (dir)
                   (directory (merge-pathnames
                               (make-pathname* :directory (if (pathname-directory dir)
                                                             '(:relative :wild-inferiors)
                                                             '(:absolute :wild-inferiors))
                                              :name :wild
                                              :type "ASD"
                                              :version :newest
                                              :case :common
                                              :defaults dir)
                               dir nil)))
                 directories))
        :test (function equal))
       (lambda (a b) (if (= (length a) (length b))
                    (string< a b)
                    (< (length a) (length b))))
       :key (function namestring))
    (format *trace-output* "~&;; Done.~%")))


(defun update-asdf-registry (&key (force-scan nil) (directories nil directoriesp))
  "Update asdf:*central-registry* with the subdirectories of DIRECTORIES containing ASD files,
either scanned, or from the cache."
  (length
   (setf asdf:*central-registry*
         (nconc (if (and (not force-scan)
                         (probe-file *asdf-registry-file*)
                         (let ((fdate (file-write-date *asdf-registry-file*)))
                           (and fdate (< (get-universal-time)
                                         (+ fdate *asdf-interval-between-rescan*)))))
                    ;; Get it from the cache.
                    (with-open-file (in *asdf-registry-file*)
                      (format *trace-output* "~&;; Reading ASDF packages from ~A...~%"
                              *asdf-registry-file*)
                      (let ((*read-eval* nil))
                        (read in nil nil)))
                    ;; Scan it anew.
                    (let ((scan (apply (function find-asdf-subdirectories)
                                       (when directoriesp
                                         (list directories)))))
                      (unless force-scan ; we save only when not :force-scan t
                        (format *trace-output* "~&;; Writing ASDF packages to ~A...~%"
                                *asdf-registry-file*)
                        (with-open-file (out *asdf-registry-file*
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede)
                          (print scan out)))
                      scan))
                *original-asdf-registry*))))


;;;----------------------------------------------------------------------

(fmakunbound 'hostname)
(defun hostname ()
  "RETURN: The FQDN of the local host."
  (let ((outpath (format nil "/tmp/hostname-~8,'0X.txt" (random #x100000000))))
    (unwind-protect
         (progn
           (asdf:run-shell-command "( hostname --fqdn 2>/dev/null || hostname --long 2>/dev/null || hostname ) > ~A"
                                   outpath)
           (with-open-file (hostname outpath)
             (read-line hostname)))
      (delete-file outpath))))



;; (run-program "example" '() :input :stream)

;; nil                                    (open "/dev/null")
;; :stream                                (make-stream)
;; "/some/file" #P "/some/file"           (open "/some/file")
;; stream                                 stream

;; (with-open-file (input "/etc/passwd")
;;   (with-open-file (output "/tmp/test.txt" :direction :output
;;                           :if-does-not-exist :create
;;                           :if-exists :supersede)
;;     (multiple-value-bind (inp out err pid) (run-program #("/bin/cat" "cat")
;;                                                         :input input
;;                                                         :output output
;;                                                         :error-output :stream
;;                                                         :wait nil)
;;       (loop
;;         :for line = (read-line err nil nil)
;;         :while line :do (write-line line))
;;       (excl.osi:waitpid pid))))

;; sleep
;; ls|
;; |sort|



;;;----------------------------------------------------------------------


(defmacro defautoload (name arguments loader)
  "Defines a function that will load the LOADER file, before calling itself again."
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,arguments
       "Autoload function."
       (load ,loader)
       (,name ,@arguments))))

(defautoload scheme () "LOADERS:PSEUDO")

;;;----------------------------------------------------------------------


(defun start-dribble ()
  "We dribble to a timestamped file in a specific #P\"HOME:DRIBBLE;\" directory."
  (let ((path
         (merge-pathnames
          (make-pathname*
           :directory '(:relative "DRIBBLES")
           :name (flet ((implementation-id ()
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
           :defaults (user-pathname))
          (user-pathname) nil)))
    (ensure-directories-exist path)
    (dribble path)))

;; (start-dribble)

;;;----------------------------------------------------------------------

(defvar *inp* (make-synonym-stream '*standard-input*)  "Synonym to *standard-input*")
(defvar *out* (make-synonym-stream '*standard-output*) "Synonym to *standard-output*")

;;;----------------------------------------------------------------------
;;;
;;; Alexandria
;;;

(ql:quickload :alexandria :verbose nil)

;; (handler-case
;;     (ql:quickload :alexandria :verbose t)
;;   (error (err)
;;     (princ err *trace-output*) (terpri *trace-output*) (finish-output *trace-output*)
;;     (print (compute-restarts))
;;     (invoke-restart (find-restart 'skip))))


;;;----------------------------------------------------------------------
;;;
;;; com.informatimago libraries
;;;


;; (update-asdf-registry)

;; ;; Should be included by (UPDATE-ASDF-REGISTRY)
;; (setf asdf:*central-registry*
;;       (append (remove-duplicates
;;                (mapcar (lambda (path)
;;                          (make-pathname* :name nil :type nil :version nil :defaults path))
;;                        #+ccl(directory #P"PACKAGES:com;informatimago;**;*.asd")
;;                        #-ccl(directory #P"PACKAGES:COM;INFORMATIMAGO;**;*.ASD"))
;;                :test (function equalp))
;;               asdf:*central-registry*))


;;; This doesn't work:
;; ;; You can use :tree instead of :directory to find all directories with
;; ;; system files present.
;; ;; See section 7.5 and 7.9 of http://l1sp.org/asdf/manual
;; #+(or allegro ccl) (asdf:initialize-source-registry 
;;                     '(:source-registry (:tree #P"PACKAGES:COM;INFORMATIMAGO;")
;;                       :inherit-configuration))
;; #-(or allegro ccl) (asdf:initialize-source-registry 
;;                     '(:source-registry (:tree #P"PACKAGES:COM;INFORMATIMAGO;")
;;                       :inherit-configuration))

#-abcl (asdf-load  :com.informatimago.common-lisp)
#-abcl (asdf-load  :com.informatimago.clmisc)

#-(or abcl ccl cmu ecl sbcl)
(asdf-load  :com.informatimago.clext)

#-(and)
(asdf-load  :com.informatimago.clisp)

#-(and)
(asdf-load  :com.informatimago.susv3)


;;;----------------------------------------------------------------------
#-abcl
(use-package "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")
#-abcl
(export      (com.informatimago.common-lisp.cesarum.package:list-external-symbols
              "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"))


(push :com.informatimago.pjb *features*)

;; (format t "~2%asdf:*central-registry* = ~S~2%" asdf:*central-registry*)
;;;; THE END ;;;;
