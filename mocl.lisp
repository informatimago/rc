(shadow '(list-all-packages user-homedir-pathname))

(defun user-homedir-pathname ()
  (pathname "/home/pjb/"))


(defun list-all-packages ()
  (let ((packages '()))
    (do-symbols (s)
      (let ((package (symbol-package s)))
        (when package
          (pushnew package packages))))
    packages))


(defmacro handling-errors (&body body)
  "
DO:       Execute the BODY with a handler for CONDITION and
          SIMPLE-CONDITION reporting the conditions.
"
  `(handler-case (progn ,@body)
     (simple-condition  (err) 
       (format *error-output* "~&~A:~%~?~&"
               (class-name (class-of err))
               (simple-condition-format-control   err)
               (simple-condition-format-arguments err))
       #-mocl (finish-output *error-output*))
     (condition (err) 
       (format *error-output* "~&~A:~%~A~%" (class-name (class-of err)) err)
       #-mocl (finish-output *error-output*))))


(defvar +    nil)
(defvar ++   nil)
(defvar +++  nil)
(defvar -    nil)
(defvar *    nil)
(defvar **   nil)
(defvar ***  nil)
(defvar /    nil)
(defvar //   nil)
(defvar ///  nil)

(defun repl ()
  "
DO:        Implements a minimalist CL REPL.
"
  (catch 'repl
    (do ((+eof+ (gensym))
         (hist 1 (1+ hist)))
        (nil)
      (format t "~%~A[~D]> "  #+mocl *package* #-mocl (package-name *package*) hist)
      #-mocl (finish-output)
      (handling-errors
       (setf - (read *standard-input* nil +eof+))
       (when (or (eq - +eof+)
                 (and (listp -)
                      (null (rest -))
                      (member (first -) '(quit  exit continue)
                              :test (function string-equal))))
         (return-from repl))
       (let ((results (multiple-value-list (eval -))))
         (setf +++ ++   ++ +   + -
               /// //   // /   / results
               *** **   ** *   * (first /)))
       (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
       #-mocl (finish-output)))))
 
