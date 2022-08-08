
(when nil
 (dolist (multi '(("`" (("a" "à") ("e" "è") ("i" "ì") ("o" "ò") ("u" "ù")
                        ("A" "À") ("E" "È") ("I" "Ì") ("O" "Ò") ("U" "Ù")))
                  ("'" (("a" "á") ("e" "é") ("i" "í") ("o" "ó") ("u" "ú") ("y" "ý")
                        ("A" "Á") ("E" "É") ("I" "Í") ("O" "Ó") ("U" "Ú") ("Y" "Ý")))
                  ("^" (("a" "â") ("e" "ê") ("i" "î") ("o" "ô") ("u" "û")
                        ("A" "Â") ("E" "Ê") ("I" "Î") ("O" "Ô") ("U" "Û")))
                  ("~" (("A" "Ã") ("N" "Ñ") ("O" "Õ")
                        ("a" "ã") ("n" "ñ") ("o" "õ")))
                  ("\"" (("a" "ä") ("e" "ë") ("i" "ï") ("o" "ö") ("u" "ü") ("y" "ÿ")
                         ("A" "Ä") ("E" "Ë") ("I" "Ï") ("O" "Ö") ("U" "Ü")))
                  ("s" (("s" "ß")))
                  ("t" (("h" "þ") ("H" "þ")))
                  ("T" (("h" "Þ") ("H" "Þ")))
                  ("d" (("h" "ð") ("H" "ð")))
                  ("D" (("h" "Ð") ("H" "Ð")))
                  ("A" (("E" "Æ") ("e" "Æ") ("o" "Å")("O" "Å")))
                  ("a" (("E" "æ") ("e" "æ") ("o" "å")("O" "å")))
                  ("/" ((":" "÷") ("o" "ø") ("O" "Ø")))
                  ("," (("C" "Ç") ("c" "ç")))))
   (let* ((first (first multi))
          (name  (intern (format "hyper-%s-map"
                                 (cond
                                   ((string= "`" first) "grave")
                                   ((string= "'" first) "acute")
                                   ((string= "^" first) "circumflex")
                                   ((string= "~" first) "tilde")
                                   ((string= "\"" first) "umlaut")
                                   ((string= "/" first) "slash")
                                   ((string= "," first) "comma")
                                   (t first)))))
          (table (define-prefix-command name)))
     (message "%S" `(global-set-key (kbd ,(format "H-%s" first)) ,name))
     (global-set-key (kbd (format "H-%s" first)) name)
     (dolist (entry (second multi))
       (let ((second (first entry))
             (result (second entry)))
         (message "%S" `(define-key ,name (kbd ,(format "%s" first second))
                          (lambda (n)
                            (interactive "p")
                            (dotimes (i n)
                              (insert ,result)))))
         (define-key name (kbd (format "%s" first second))
           (lambda (n)
             (interactive "p")
             (dotimes (i n)
               (insert result)))))))))