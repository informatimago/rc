(require 'cobol-mode)
(setf auto-mode-alist
      (append auto-mode-alist '(("\\.cob$" . cobol-mode))))
(add-hook 'cobol-mode-hook
          (lambda () (interactive) (setf identi)))

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
