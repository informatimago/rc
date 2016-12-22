#||

(apropos "BIND" "HEMLOCK")

(documentation 'hi:bind-key 'function)

(hi:bind-key



||#
(hi:map-bindings (lambda (&rest params) (print params)) :global)
(hi:get-command #k"C-x C-f")
(hi:get-command #k"C-x C-e")

(defmacro report-errors (expression)
  `(handler-case ,expression
      (error (err) (values nil (princ-to-string err) err))))

(print-threads)dn
(report-errors (hi::crunch-key #k"C-x C-f"))

(apropos "DIRECTORY")
#-(and)
"

"
