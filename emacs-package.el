(require 'package)
;; Any add to list for package-archives (to add marmalade or melpa) goes here
(setq package-archives (append package-archives
                               '(("marmalade" . "http://marmalade-repo.org/packages/")
                                 ("melpa" . "http://melpa.milkbox.net/packages/"))))
(package-initialize)

(defun package-update-load-path ()
  "Update the load path for newly installed packages."
  (interactive)
  (let ((package-dir (expand-file-name package-user-dir)))
    (mapc (lambda (pkg)
            (let ((stem (symbol-name (car pkg)))
		  (version "")
		  (first t)
		  path)
	      (mapc (lambda (num)
		      (if first
			  (setq first nil)
			  (setq version (format "%s." version)))
		      (setq version (format "%s%s" version num)))
		    (aref (cdr pkg) 0))
              (setq path (format "%s/%s-%s" package-dir stem version))
              (add-to-list 'load-path path)))
          package-alist)))
