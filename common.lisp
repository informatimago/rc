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
;;;;    Copyright Pascal Bourguignon 2003 - 2015
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
      *print-lines*  nil
      *print-right-margin* 110)

#+swank
(setq *debugger-hook* (let ((old-debugger-hook *debugger-hook*))
                        (defun trace-error-debugger-hook (condition me-or-my-encapsulation)
                          (format *trace-output* "~A~%" condition)
                          (funcall old-debugger-hook condition me-or-my-encapsulation))))



(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0)))



;;;----------------------------------------------------------------------
;;;
;;; COM.INFORMATIMAGO.PJB
;;;

(defpackage "COM.INFORMATIMAGO.PJB"
  (:nicknames "PJB")
  (:use "COMMON-LISP")
  (:shadow "USER-HOMEDIR-PATHNAME" "MAKE-PATHNAME" "TRANSLATE-LOGICAL-PATHNAME")
  (:export "LIST-DIRECTORIES"   "GET-DIRECTORY"
           "LIST-LOGICAL-HOSTS" "DEFINE-LOGICAL-HOST-TRANSLATIONS"
           "START-DRIBBLE"
           "DEFAUTOLOAD"
           "HOSTNAME"
           "SCHEME"
           "*INP*" "*OUT*"
           "SELF"
           "IT" "THIS" "THAT"
           "THEM" "THESE" "THOSE"
           "IS" "WAS" "WERE")
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


;;;----------------------------------------------------------------------
;;;
;;; COM.INFORMATIMAGO.TOOLS.PATHNAME
;;;


(defun user-homedir-pathname ()
  "On MS-Windows, it's not the USER-HOMEDIR-PATHNAME."
  #+windows-target (let ((home (ccl::getenv "HOME")))
                     (if home
                         (pathname (format nil "~A\\" home))
                         #P"C:\\cygwin\\home\\pjb\\"))
  #-windows-target (cl:user-homedir-pathname))


(defun make-pathname (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
                       (name nil namep) (type nil typep) (version nil versionp)
                       (defaults nil defaultsp) (case :local casep))
  (declare (ignorable casep))
  #+(or abcl ccl allegro)
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
    (apply (function cl:make-pathname)
           (append (parameter hostp      :host      host)
                   (parameter devicep    :device    device)
                   (parameter directoryp :directory directory)
                   (parameter namep      :name      name)
                   (parameter typep      :type      type)
                   (parameter versionp   :version   version)
                   (parameter defaultsp  :defaults  defaults)
                   (list :case :local))))
  #-(or abcl ccl allegro)
  (apply (function cl:make-pathname)
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
;;; ASDF-CONFIGURATION
;;;

(let ((asdf-conf-path (merge-pathnames
                       (make-pathname :directory '(:relative ".config" "common-lisp")
                                      :name "asdf-output-translations"
                                      :type "conf"
                                      :case :local
                                      :defaults (user-homedir-pathname))
                       (user-homedir-pathname) nil))
      (common-path *load-truename*))
  (when (or (not (ignore-errors (probe-file asdf-conf-path)))
            (null (file-write-date asdf-conf-path))
            (and common-path
                 (or (null (file-write-date common-path))
                     (< (file-write-date asdf-conf-path)
                        (file-write-date common-path)))))
    (ensure-directories-exist asdf-conf-path)
    (with-open-file (asdfconf asdf-conf-path
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists nil
                              :external-format :default)
      (write-string ";; -*- mode:lisp -*-" asdfconf)
      (print '(:output-translations
               #-clisp :ignore-invalid-entries
               (t (:home ".cache" "common-lisp" :hostname :implementation))
               (t (:home ".cache" "common-lisp" :implementation))
               :inherit-configuration)
             asdfconf)
      (terpri asdfconf))))


;;;----------------------------------------------------------------------
;;;
;;; QuickLisp
;;;

(let ((quicklisp (merge-pathnames
                  (make-pathname :directory '(:relative "QUICKLISP")
                                  :name "SETUP"
                                  :type "LISP"
                                  :version :newest
                                  :case :common
                                  :defaults (user-homedir-pathname))
                  (user-homedir-pathname)
                  nil)))
  (if (probe-file quicklisp)
      (load quicklisp)
      (error "Please install quicklisp.  I expect it in ~S" quicklisp)))


(push #P"~/src/public/lisp/" ql:*local-project-directories*)
(ql:quickload "com.informatimago.common-lisp")
(ql:quickload "com.informatimago.common-lisp.lisp.stepper")
(ql:quickload "com.informatimago.clmisc")

;; (ql:quickload "com.informatimago.tools")
(ql:quickload '("com.informatimago.tools.pathname"
                "com.informatimago.tools.manifest"
                "com.informatimago.tools.symbol"
                "com.informatimago.tools.source"
                "com.informatimago.tools.summary"
                "com.informatimago.tools.thread"
                "com.informatimago.tools.quicklisp"
                "com.informatimago.tools.make-depends"
                "com.informatimago.tools.script"
                "com.informatimago.tools.check-asdf"))

#-(or ccl cmu ecl sbcl)
(ql:quickload "com.informatimago.clext")

#+clisp
(ql:quickload "com.informatimago.clisp")

#+disabled-temporarily-for-bitrot
(ql:quickload "com.informatimago.susv3")


(ql:quickload "alexandria" :verbose nil)


;; (eval-when (:compile-toplevel :load-toplevel :execute))
(mapcar (function unintern) '(make-pathname
                              translate-logical-pathname
                              user-homedir-pathname))

(shadowing-import '(com.informatimago.tools.pathname:make-pathname
                    com.informatimago.tools.pathname:translate-logical-pathname
                    com.informatimago.tools.pathname:user-homedir-pathname))

(dolist (pname '("COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
                 "COM.INFORMATIMAGO.TOOLS.QUICKLISP"
                 "COM.INFORMATIMAGO.TOOLS.ASDF"
                 "COM.INFORMATIMAGO.TOOLS.THREAD"))
  (use-package pname)
  (export (com.informatimago.common-lisp.cesarum.package:list-external-symbols pname)))
                 

;;;----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((fundefine (fname)
           (when (fboundp fname)
             (fmakunbound fname))))
    (fundefine 'get-directory)
    (fundefine 'make-translations)))

;;;----------------------------------------------------------------------
;;;
;;; Directories.
;;;

(defvar *default-directories*
  '(:asdf-install  "/usr/local/share/lisp/asdf-install/"
    :share-lisp    "/usr/local/share/lisp/"
    :lisp-sources  "/home/pjb/src/public/lisp/"
    :pjb-lisp      "/home/pjb/src/lisp/"))

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
                           (make-pathname :name "DIRECTORIES" :type "TXT"
                                          :version nil :case :common)
                           (user-homedir-pathname)
                           nil)
                          :if-does-not-exist nil)
      (if dirs
          (handler-case
           (loop
             :for k = (read dirs nil dirs)
             :until (eq k dirs)
             :do (push (string-trim " " (read-line dirs)) *directories*)
             :do (push (intern (substitute #\- #\_ (string k)) "KEYWORD") *directories*))
            (error (err)
              (warn "Error while reading directories.txt file: ~A" err)))
       (progn
         (warn "No directories.txt file.")
         (setf *directories* *default-directories*)))))
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
       (list (apply (function make-pathname)
                    :host host
                    :directory `(:absolute ,@logical-dir :wild-inferiors)
                    :case :common
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

#-cmu
(define-logical-pathname-translations "HOME"     (user-homedir-pathname)   "")
(define-logical-pathname-translations "LOADERS"  *pjb-comm*        "cl-loaders/")
;;(DEFINE-LOGICAL-PATHNAME-TRANSLATIONS "NORVIG"   *PJB-LISP*        "norvig/")
(define-logical-pathname-translations "NORVIG"   #p"/home/pjb/src/lisp/ai/"    "norvig-paip-pjb/")


;;;------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun post-process-logical-host-translations ()
    (when (fboundp 'common-lisp-user::post-process-logical-pathname-translations)
      (map nil
           'common-lisp-user::post-process-logical-pathname-translations
           *logical-hosts*)))) 

(post-process-logical-host-translations)




;;;----------------------------------------------------------------------

(defmacro defautoload (name arguments loader)
  "Defines a function that will load the LOADER file, before calling itself again."
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,arguments
       "Autoload function."
       (load ,loader)
       (,name ,@arguments))))

#-ccl
(defautoload scheme () (translate-logical-pathname #P"LOADERS:PSEUDO"))

;;;----------------------------------------------------------------------

(defun start-dribble ()
  "We dribble to a timestamped file in a specific #P\"HOME:DRIBBLE;\" directory."
  (let ((path
         (merge-pathnames
          (make-pathname
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
           :defaults (user-homedir-pathname))
          (user-homedir-pathname) nil)))
    (ensure-directories-exist path)
    (dribble path)))

;; (start-dribble)



;;;----------------------------------------------------------------------

(fmakunbound 'hostname)
(defun hostname ()
  "RETURN: The FQDN of the local host."
  (handler-case
      (let ((outpath (format nil "/tmp/hostname-~8,'0X.txt" (random #x100000000))))
        (unwind-protect
             (progn
               (asdf:run-shell-command "( hostname --fqdn 2>/dev/null || hostname --long 2>/dev/null || hostname ) > ~A"
                                       outpath)
               (with-open-file (hostname outpath)
                 (read-line hostname)))
          (delete-file outpath)))
    (error ()
      (warn "Cannot find hostname.")
      "localhost")))

;;;----------------------------------------------------------------------
;; TODO: Make them nice DTRT, instead of Q&D shell and shell-command-to-string.

(defun shell (control-string &rest arguments)
  #-ccl (error "~S is not implemented yet on ~A" 'shell (lisp-implementation-type))
  #+ccl
  (let ((process
         (ccl:run-program "/bin/bash"
                          (list "-c" (format nil "~?" control-string arguments))
                          :output :stream
                          :error :stream)))
    (com.informatimago.common-lisp.cesarum.stream:copy-stream
     (ccl:external-process-output-stream process)
     *standard-output*)
    (com.informatimago.common-lisp.cesarum.stream:copy-stream
     (ccl:external-process-error-stream process)
     *error-output*)))

(defun shell-command-to-string (control-string &rest arguments)
  #-ccl (error "~S is not implemented yet on ~A" 'shell-command-to-string (lisp-implementation-type))
  #+ccl
  (let ((process
         (ccl:run-program "/bin/bash"
                          (list "-c" (format nil "~?" control-string arguments))
                          :output :stream
                          :error :stream)))
    (values (with-output-to-string (out)
              (com.informatimago.common-lisp.cesarum.stream:copy-stream
               (ccl:external-process-output-stream process) out))
            (with-output-to-string (err)
              (com.informatimago.common-lisp.cesarum.stream:copy-stream
               (ccl:external-process-error-stream process) err)))))

(export '(shell shell-command-to-string)  :com.informatimago.pjb)


;;;----------------------------------------------------------------------

(define-symbol-macro self -)

(define-symbol-macro it   *)
(define-symbol-macro this **)
(define-symbol-macro that ***)

(define-symbol-macro them  /)
(define-symbol-macro these //)
(define-symbol-macro those ///)

(define-symbol-macro is   +)
(define-symbol-macro was  ++)
(define-symbol-macro were +++)

(defvar *inp* (make-synonym-stream '*standard-input*)  "Synonym to *standard-input*")
(defvar *out* (make-synonym-stream '*standard-output*) "Synonym to *standard-output*")


;;;----------------------------------------------------------------------
(push :com.informatimago.pjb *features*)
;;;----------------------------------------------------------------------
(in-package "CL-USER")
(use-package "COM.INFORMATIMAGO.PJB" "CL-USER")

;; (ql:quickload :com.informatimago.common-lisp.lisp.ibcl)
;; (in-package :ibcl-user)
;; (use-package :com.informatimago.pjb)

;; (format t "~2%asdf:*central-registry* = ~S~2%" asdf:*central-registry*)

(when (probe-file "/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site.py")
  (defparameter cl-user::*clpython-module-search-paths*
    '("/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/")))


;; Not really: call-in should be in some package.
#-mocl (declaim (declaration call-in))


;;;; THE END ;;;;
