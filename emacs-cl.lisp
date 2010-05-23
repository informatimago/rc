(in-package "COMMON-LISP")

(defun inspect (object)
  (typecase object
    (vector
     (format t "~&~S~%" (type-of object))
     (format t "dimension: ~S~%" (array-dimensions object))
     (if (ARRAY-HAS-FILL-POINTER-P object)
	 (format t "fill-pointer: ~A~%"
		 (fill-pointer object))
       (format t "no fill pointer~%"))

     (format t "~:[not displaced~;displaced: ~A~]~%"
     (loop
     )
  (values))