(unless (find-package "SWANK")
  (ql:quickload :swank))

(let ((interface #+ccl (ccl::primary-ip-interface-address)
                 #-ccl (hostname) #|"0.0.0.0"|#)
      (port 4005 #|(+ 4005 (random 95))|#))
  (swank:create-server :interface interface :port port)
  (format t "~&Swank server started on port ~D~%" port))

;; see also: https://codeshare.io/anXKbY

;; clisp -x '(load #P"~/rc/swank-server.lisp")'
