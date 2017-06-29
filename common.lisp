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


(defpackage "COM.INFORMATIMAGO.PJB.UTILITY"
  (:use "COMMON-LISP")
  (:shadow "USER-HOMEDIR-PATHNAME" "MAKE-PATHNAME" "TRANSLATE-LOGICAL-PATHNAME")
  (:export "USER-HOMEDIR-PATHNAME" "MAKE-PATHNAME" "TRANSLATE-LOGICAL-PATHNAME"
           "LOAD-ASDF3" "ASDF-CONFIGURATION" "LOAD-QUICKLISP" "INFORMATIMAGO-PACKAGES"
           "*ORIGINAL-READTABLE*")
  (:documentation "

Utilities for the com.informatimago.pjb package.

License:

    AGPL3

    Copyright Pascal J. Bourguignon 2003 - 2015

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

(defpackage "COM.INFORMATIMAGO.PJB"
  (:nicknames "PJB")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PJB.UTILITY")
  (:shadow "ED")                       ; from com.informatimago.common-lisp.cesarum.ecma048
  (:shadow "APROPOS" "APROPOS-LIST")   ; from com.informatimago.tools.symbol
  (:shadowing-import-from "COM.INFORMATIMAGO.PJB.UTILITY"
                          "USER-HOMEDIR-PATHNAME" "MAKE-PATHNAME"
                          "TRANSLATE-LOGICAL-PATHNAME")
  (:export "LIST-DIRECTORIES"   "GET-DIRECTORY"
           "LIST-LOGICAL-HOSTS" "DEFINE-LOGICAL-HOST-TRANSLATIONS"
           "START-DRIBBLE"
           "DEFINE-AUTOLOAD"
           "HOSTNAME"
           "SCHEME"
           "*INP*" "*OUT*"
           "SELF"
           "IT" "THIS" "THAT"
           "THEM" "THESE" "THOSE"
           "IS" "WAS" "WERE"
           "DEFINE-COMMAND" "HELP"
           "QUIT" "EXIT")
  (:documentation "

This package contains REPL utilities, defined in ~/rc/common.lisp,
which is loaded from the various rc files of the various CL
implementations.

It also re-exports the exported symbols of
COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE.


License:

    AGPL3

    Copyright Pascal J. Bourguignon 2003 - 2015

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



;;;----------------------------------------------------------------------
;;;

(in-package "COM.INFORMATIMAGO.PJB.UTILITY")

(defvar *original-readtable* (copy-readtable *readtable*))

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

(defun load-asdf3 ()
  (unless (member :asdf3 *features*)
    (load (merge-pathnames #P"src/public/lisp/tools/asdf.lisp"
                           (user-homedir-pathname)))))

(defun asdf-configuration ()
  "Creates the ~/.config/common-lisp/asdf-output-translations.conf file."
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
       (print '(:OUTPUT-TRANSLATIONS
                :ignore-invalid-entries
                ;; :ignore-inherited-configuration
                ;; TODO: Replace :hostname which doesn't seem to be supported anymore by a function.
                (T (:HOME ".cache" "common-lisp" :HOSTNAME :IMPLEMENTATION))
                (T (:HOME ".cache" "common-lisp" :IMPLEMENTATION))
                :INHERIT-CONFIGURATION)
              asdfconf)
       (terpri asdfconf)))))


;;;----------------------------------------------------------------------
;;;
;;; QuickLisp
;;;

(defun load-quicklisp ()
  "Loads quicklisp."
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
        (error "Please install quicklisp.  I expect it in ~S" quicklisp))))


;;;----------------------------------------------------------------------
;;;

(defun informatimago-packages ()
  "Returns a list of interesting informatimago packages."
  (remove-if
   (lambda (package)
     (flet ((prefixp (string prefix)
              (and (<= (length prefix) (length string))
                   (string= prefix string :end2 (length prefix)))))
       (let ((name (package-name package)))
         (or (not (or (prefixp name "COM.INFORMATIMAGO.COMMON-LISP.")
                      (member name '("COM.INFORMATIMAGO.TOOLS.QUICKLISP"
                                     "COM.INFORMATIMAGO.TOOLS.ASDF"
                                     "COM.INFORMATIMAGO.TOOLS.THREAD"
                                     "COM.INFORMATIMAGO.TOOLS.SYMBOL")
                              :test (function string=))))
             (member name '("COM.INFORMATIMAGO.COMMON-LISP.PARSER."
                            "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR."
                            "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER."
                            "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER"
                            "COM.INFORMATIMAGO.COMMON-LISP.HEAP"
                            "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-")
                     :test (function prefixp))
             (member name '("COM.INFORMATIMAGO.COMMON-LISP.CESARUM.JULIAN-CALENDAR"
                            "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GREGORIAN-CALENDAR"
                            "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE"
                            "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET"
                            "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET"
                            "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET"
                            "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BRELATION"
                            "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
                            "COM.INFORMATIMAGO.COMMON-LISP.DATA.CONSTANT"
                            "COM.INFORMATIMAGO.COMMON-LISP.UNIX.OPTION"
                            "COM.INFORMATIMAGO.COMMON-LISP.ED.ED" )
                     :test (function string=))))))
   (list-all-packages)))

;;;
;;;----------------------------------------------------------------------

(in-package "COM.INFORMATIMAGO.PJB")

(load-asdf3)
(asdf-configuration)
(load-quicklisp)
(push #P"~/src/public/lisp/" ql:*local-project-directories*)
(ql:quickload "com.informatimago.common-lisp"              :verbose nil)
(ql:quickload "com.informatimago.common-lisp.lisp.stepper" :verbose nil)
(ql:quickload "com.informatimago.clmisc"                   :verbose nil)
(ql:quickload "com.informatimago.common-lisp.lisp-sexp"    :verbose nil)

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
                "com.informatimago.tools.check-asdf")
              :verbose nil)

#-(or ccl cmu ecl sbcl)
(ql:quickload "com.informatimago.clext")

#+clisp
(ql:quickload "com.informatimago.clisp")

#+disabled-temporarily-for-bitrot
(ql:quickload "com.informatimago.susv3")


(ql:quickload "alexandria" :verbose nil)


(defun informatimago-import-export ()
  (shadowing-import '(com.informatimago.tools.pathname:make-pathname
                      com.informatimago.tools.pathname:translate-logical-pathname
                      com.informatimago.tools.pathname:user-homedir-pathname))
  (dolist (package (informatimago-packages))
    (format *trace-output* "~&;; Using package ~A~%" (package-name package))
    (handler-case (use-package package)
      (error (err)
        (princ err) (terpri))))
  (shadowing-import '(com.informatimago.common-lisp.cesarum.ecma048:ed))
  (shadowing-import '(com.informatimago.tools.symbol:apropos
                      com.informatimago.tools.symbol:apropos-list))
  (dolist (package (informatimago-packages))
    (export (com.informatimago.common-lisp.cesarum.package:list-external-symbols package))))

(informatimago-import-export)

(initialize)



;;;----------------------------------------------------------------------
;;;

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
;;; Logical hosts -- the Common-Lisp way to paths.
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


(define-logical-pathname-translations "LOGHOSTS" (user-homedir-pathname) "LOGHOSTS/")

;;;------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun post-process-logical-host-translations ()
    (when (fboundp 'common-lisp-user::post-process-logical-pathname-translations)
      (map nil
           'common-lisp-user::post-process-logical-pathname-translations
           *logical-hosts*))))

(post-process-logical-host-translations)




;;;----------------------------------------------------------------------

(defmacro define-autoload (name arguments loader)
  "Defines a function that will load the LOADER file, before calling itself again."
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,arguments
       "Autoload function."
       (load ,loader)
       (,name ,@arguments))))

#-ccl
(define-autoload scheme () (translate-logical-pathname #P"LOADERS:PSEUDO"))

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

(defun executable-reader (stream ch subch)
  (declare (ignorable ch subch))
  #+clisp (sys::unix-executable-reader stream ch subch)
  #-clisp (progn
            (read-line stream)
            (values)))

(set-dispatch-macro-character #\# #\! (function executable-reader))

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


;; ;; Patch swank:
;; (defvar *swank-patched-p* nil)
;; (defun check-and-patch-swank ()
;;   (when (and (member :swank *features*)
;;              (find-package "SWANK")
;;              (not (eq :external  (find-symbol "*ARGUMENT-STREAM*" "SWANK")))
;;              (not *swank-patched-p*))
;;     (let ((as (intern "*ARGUMENT-STREAM*" "SWANK"))
;;           (er (intern "EVAL-REGION"       "SWANK")))
;;       (proclaim `(special ,as))
;;       (eval `(setf ,as nil))
;;       (eval `(defun ,er (string)
;;                "Evaluate STRING.
;; Return the results of the last form as a list and as secondary value the
;; last form."
;;                (with-input-from-string (,as string)
;;                  (let (- values)
;;                    (loop
;;                      (let ((form (read ,as nil ,as)))
;;                        (when (eq form ,as)
;;                          (finish-output)
;;                          (return (values values -)))
;;                        (setq - form)
;;                        (setq values (multiple-value-list (eval form)))
;;                        (finish-output)))))))
;;       (setf *swank-patched-p* t))))
;;


(defun command-read-arguments ()
  "Read the arguments following the expression that has just been read and that is being evaluated."
  (flet ((read-arguments (stream)
           (loop :for argument := (read stream nil stream)
                 :until (eq argument stream)
                 :collect argument)))
    (if (and (member :swank *features*)
             (find-package "SWANK")
             (eq :external (nth-value 1 (find-symbol "*ARGUMENT-STREAM*" "SWANK")))
             (streamp (symbol-value (intern "*ARGUMENT-STREAM*" "SWANK"))))
        (read-arguments (symbol-value (intern "*ARGUMENT-STREAM*" "SWANK")))
        (with-input-from-string (stream  (if (listen *standard-input*)
                                             (read-line *standard-input*)
                                             ""))
          (read-arguments stream)))))

(defun call-command (command)
  "
DO:     Call the command with the arguments read from the following line.
RETURN: No value.
"
  (apply command (command-read-arguments))
  (values))

(defvar *commands* '()
  "List of commands, with their lambda-list and docstring.")

(defmacro command-error-handling (&body body)
  `(handler-case
       (progn ,@body)
     (error (err)
       (format *error-output* "~&~A~%" err))))

(defmacro define-command (name (&rest lambda-list) &body docstring-declarations-and-body)
  "
DO:         Define a REPL command that can be invoked by name, without
            surrounding it with parentheses.

NOTE:       Commands are defined as functions, associated with a
            symbol-macro expanding to a call-command call.
            Call-command will parse the rest of the line  as arguments
            for the command.  Errors are handled and printed.  No
            value is returned.  Commands must print their results.

EXAMPLE:
            (define-command cmdt (&rest arguments)
              (prin1 (append arguments arguments))
              (terpri))

            cl-user> cmdt 1 2 3
            (1 2 3 1 2 3)
            ; No value
            cl-user>
"
  (multiple-value-bind (docstrings declarations body)
      (parse-body :lambda docstring-declarations-and-body)
    `(progn
       (setf *commands* (cons (list ',name ',lambda-list ',(car docstrings))
                              (delete ',name *commands* :key (function first))))
       (defun ,name ,lambda-list
         ,@docstrings ,@declarations
         (command-error-handling ,@body))
       (define-symbol-macro ,name (call-command ',name)))))

(defun fmt-lambda-list (stream lambda-list at colon  &rest parameters)
  "Formats a lambda list using [] for optional and parameters."
  (declare (ignore at colon parameters))
  (let ((lh (make-help (parse-lambda-list lambda-list :ordinary))))
    (write-string
     (mapconcat (lambda (p)
                  (case (car p)
                    (:key (let ((*print-circle* nil))  (format nil "[:~A ~:*~A]" (cdr p))))
                    (otherwise (cdr p))))
                (if (and (find :key  lh :key (function car))
                         (find :rest lh :key (function car)))
                    (delete :rest lh :key (function car))
                    lh)
                " ")
     stream)))

(define-command help (&optional (command nil commandp))
  "With a command argument, prints the docstring of the specified command.
without, lists all the commands with their docstrings."
  (flet ((format-help (help)
           (format t "~%~S~@[ ~/com.informatimago.pjb:fmt-lambda-list/~]~%~@[~A~%~]"
                   (first help) (second help)
                   (mapconcat (lambda (line) (format nil "  ~A" line))
                              (split-string (third help) #(#\newline))
                              (coerce #(#\newline) 'string)))))
    (if commandp
        (let ((help (find command *commands* :key (function first) :test (function string-equal))))
          (if help
              (format-help help)
              (format t "~%No help for ~S~%" command)))
        (dolist (help (setf *commands* (sort *commands* (function string<) :key (function first))))
          (format-help help))))
  (terpri))

;;;----------------------------------------------------------------------
(push :com.informatimago.pjb *features*)
;;;----------------------------------------------------------------------

(defun clean-up-package (package)
  (mapc (lambda (used-package) (unuse-package used-package package))
        (package-use-list package))
  (do-symbols (s package)
    (unintern s package))
  (use-package "COMMON-LISP" package)
  package)

(clean-up-package "CL-USER")


;;;----------------------------------------------------------------------
(in-package "CL-USER")
;;;----------------------------------------------------------------------

(let ((v (find-symbol "VERSION"))
      (p (package-name *package*)))
  (when v
    (let ((*package* (load-time-value (find-package "KEYWORD"))))
      (format t "~&~S is present in ~A where does it come from?~%" v p))
    (unintern v)))

(shadow '("ED" "APROPOS" "APROPOS-LIST"))
(use-package "COM.INFORMATIMAGO.PJB" "CL-USER")
(shadowing-import '(com.informatimago.common-lisp.ed.ed:ed
                    com.informatimago.tools.symbol:apropos
                    com.informatimago.tools.symbol:apropos-list))

(defun print-variables ()
  "Prints the *print-…* and *read-…* variables."
  (com.informatimago.common-lisp.interactive.interactive:show
    *print-array*
    *print-base*
    *print-case*
    *print-circle*
    *print-escape*
    *print-gensym*
    *print-length*
    *print-level*
    *print-lines*
    *print-miser-width*
    *print-pprint-dispatch*
    *print-pretty*
    *print-radix*
    *print-readably*
    *print-right-margin*
    *read-base*
    *read-default-float-format*
    *read-eval*
    *read-suppress*
    (readtable-case *readtable*)))


;; (ql:quickload :com.informatimago.common-lisp.lisp.ibcl)
;; (in-package :ibcl-user)
;; (use-package :com.informatimago.pjb)

;; (format t "~2%asdf:*central-registry* = ~S~2%" asdf:*central-registry*)


(shadow '(grep make mv cp less more cat ls mkdir popd pushd pwd cd browse
          date))

(defmacro forward-command-macro (name)
  `(define-command ,name (&rest arguments)
     ,(format nil "Invokes unix ~(~A~)." name)
     (eval (cons ',(intern (symbol-name name) "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER")
                 arguments))))

(defmacro forward-command-function (name (&rest lambda-list))
  (let ((ll (parse-lambda-list lambda-list :ordinary)))
    `(define-command ,name ,lambda-list
       ,(format nil "Invokes the ~(~A~) command." name)
       (apply (function ,(intern (symbol-name name) "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER"))
              ,@(make-argument-list ll)))))


(forward-command-macro cp)
(forward-command-macro mv)
(forward-command-macro make)
(forward-command-macro grep)

(forward-command-function less   (&rest paths))
(forward-command-function more   (&rest paths))
(forward-command-function cat    (&rest paths))
(forward-command-function ls     (&rest args))
(forward-command-function cd     (&optional path))

(define-command pwd ()
  "Prints the current working directory."
  (format t "~&~A~%" (com.informatimago.common-lisp.interactive.browser:pwd)))

(forward-command-function popd   ())
(forward-command-function pushd  (&optional path))
(forward-command-function mkdir  (dir &rest other-dirs))
(forward-command-function browse ())


(define-command date ()
  "Prints the date-time."
  (com.informatimago.common-lisp.interactive.interactive:date))

(define-command cdui ()
  "Change current working directory to MCLGUI sources."
  (cd #P"~/works/patchwork/src/mclgui/")
  (prin1 (com.informatimago.common-lisp.interactive.browser:pwd))
  (terpri))

(define-command cdpa ()
  "Change current working directory to Patchwork sources."
  (cd #P"~/works/patchwork/src/patchwork/")
  (prin1 (com.informatimago.common-lisp.interactive.browser:pwd))
  (terpri))

(define-command ll   ()
  "Load loader.lisp"
  (load "loader.lisp"))

(define-command qu   ()
  "Quit!"
  (prin1 "Good Bye!")
  (terpri)
  (quit))

(when (probe-file "/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site.py")
  (defparameter cl-user::*clpython-module-search-paths*
    '("/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/")))


;; Not really: call-in should be in some package.
#-mocl (declaim (declaration call-in))

#-(and) (
;;; Add local include dirs for grovellers.
      (unless (find-package "IOLIB-GROVEL")
        (defpackage "IOLIB-GROVEL"
          (:use "COMMON-LISP")
          (:intern "*CC-FLAGS*")))
      (unless (find-package "CFFI-GROVEL")
        (defpackage "CFFI-GROVEL"
          (:use "COMMON-LISP")
          (:intern "*CC-FLAGS*")))
      (defvar iolib-grovel::*cc-flags* '())
      (defvar  cffi-grovel::*cc-flags* '())
      (dolist (include-dir '("/opt/local/include/" "/usr/local/include/"))
        (dolist (var '(iolib-grovel::*cc-flags* cffi-grovel::*cc-flags*))
          (unless (member include-dir (symbol-value var) :test (function string=))
            (set var (list* "-I" include-dir (symbol-value var)))))))

;;;; THE END ;;;;
