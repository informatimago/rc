(require 'cobol-mode)
(setf auto-mode-alist
      (append auto-mode-alist '(("\\.cob$" . cobol-mode))))


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
