(eval-when (:compile-toplevel :load-toplevel :execute)
 (unless (find-package "SWANK")
   (ql:quickload :swank)))

(defun start-swank-server (&key (interface
                                 #+ccl (ccl::primary-ip-interface-address)
                                 #-ccl "0.0.0.0")
                             (port (+ 4006 (random 94))))
  (swank:create-server :interface interface :port port)
  (format t "~&Swank server started on interface ~S port ~D~%" interface port)
  (values))


;; clisp -x '(load #P"~/rc/swank-server.lisp")'
;; (load #P"~/rc/swank-server.lisp")
;; (start-swank-server)
