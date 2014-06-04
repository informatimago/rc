;;;----------------------------------------------------------------------------
(.EMACS "emacs-hyperspec.el")
;; common-lisp-hyperspec-symbols


(defun probe-url (url)
  (cond
    ((string= "file://" (subseq url 0 (min (length url) 7)))
     (file-readable-p (subseq url 7)))
    ((file-readable-p "/tmp/no-internet")
     nil)
    (t
     (zerop (parse-integer
             (shell-command-to-string
              (format "wget -O /dev/null %S >/dev/null 2>&1 ; echo -n $?"
                      url)))))))


(defun thing-at-point-no-properties (thing)
  "Return the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING."
  (if (get thing 'thing-at-point)
      (funcall (get thing 'thing-at-point))
      (let ((bounds (bounds-of-thing-at-point thing)))
        (if bounds
            (buffer-substring-no-properties (car bounds) (cdr bounds))))))



(when (require 'hyperspec nil t)

  (load "extra/hyperspec" *pjb-load-noerror* *pjb-load-silent*)

  (defparameter *lw-clhs* "www.lispworks.com/documentation/HyperSpec/")
  (defparameter *hyperspec-path*  (or (get-directory :hyperspec)
				      (concat "/usr/local/html/local/lisp/" *lw-clhs*)))
  (setf common-lisp-hyperspec-root
        (dolist (url (list
                      (concat "file://" *hyperspec-path*)
                      "file:///usr/share/doc/hyperspec/HyperSpec/")
                     (concat "http://" *lw-clhs*))
          (when (probe-url url)
            (return url))))

  (defparameter common-lisp-hyperspec-browser (function ignore))
  (defparameter common-lisp-hyperspec-frame   (selected-frame))

  ;; (setf common-lisp-hyperspec-browser 'w3m-browse-url 
  ;; (push '("."  .  w3m-browse-url) browse-url-browser-function)






  (when (or t  (boundp 'common-lisp-hyperspec-symbols))

    (defun common-lisp-hyperspec-complete (string predicate allp)
      (if allp
	  (let ((result '()))
	    (mapatoms
	     (lambda (symbol)
	       (let ((name (symbol-name symbol)))
		 (when (or (and (<= (length string) (length name))
				(string-equal* string name :end2 (length string)))
			   (search (concat "-" string) name :test (function equalp)))
		   (push name result))))
	     common-lisp-hyperspec-symbols)
	    result)
	  (try-completion string common-lisp-hyperspec-symbols predicate)))

    
    (defun clhs-entry (symbol-designator)
      (let ((symbol (intern-soft (downcase (etypecase symbol-designator
					     (symbol (symbol-name symbol-designator))
					     (string  symbol-designator)))
				 common-lisp-hyperspec-symbols)))
	(if (and symbol (boundp symbol))
	    (symbol-value symbol)
	    nil)))

    
    (defun common-lisp-hyperspec (symbol-name)
      "View the documentation on SYMBOL-NAME from the Common Lisp HyperSpec.
If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions.
The Common Lisp HyperSpec is the full ANSI Standard Common Lisp, provided
by Kent Pitman and Xanalys Inc.  By default, the Xanalys Web site is
visited to retrieve the information.  Xanalys Inc. allows you to transfer
the entire Common Lisp HyperSpec to your own site under certain conditions.
Visit http://www.xanalys.com/software_tools/reference/HyperSpec/ for more
information.  If you copy the HyperSpec to another location, customize the
variable `common-lisp-hyperspec-root' to point to that location."
      (interactive
       (list (let ((completion-ignore-case t)
		   (symbol-at-point (thing-at-point-no-properties 'symbol)))
	       (completing-read
		"Look up symbol in Common Lisp HyperSpec: "
		(function common-lisp-hyperspec-complete) #'boundp
		t symbol-at-point
		'common-lisp-hyperspec-history))))
      (maplist
       (lambda (entry)
	 (case system-type
	   ((darwin)
        (browse-url (concat common-lisp-hyperspec-root "Body/" (car entry)))
	    ;; (case window-system
	    ;;   ((x)
	    ;;    (browse-url (concat common-lisp-hyperspec-root
        ;;                        "Body/" (car entry))))
	    ;;   ((mac ns nil)
	    ;;    (let ((browse-url-browser-function (cons '("." . browse-url-generic) browse-url-browser-function))
        ;;          (browse-url-generic-program "/usr/bin/open"))
        ;;      (browse-url (concat common-lisp-hyperspec-root "Body/" (car entry)))))
	    ;;   (otherwise
	    ;;    (error "Unknown window-system")))
        )
	   ((gnu/linux)
	    (browse-url (concat common-lisp-hyperspec-root "Body/" (car entry))))
	   (otherwise
	    (error "Unknown system-type.")))
	 (if (cdr entry)
	     (sleep-for 1.5)))
       (delete-duplicates
	(or (clhs-entry symbol-name)
	    (error "The symbol `%s' is not defined in Common Lisp"
		   symbol-name))
	:test (function equal))))
    

    (defun gcl-hyperspec (symbol-name)
      (interactive
       (list (let ((completion-ignore-case t)
		   (symbol-at-point (thing-at-point-no-properties 'symbol)))
	       (completing-read
		"Look up symbol in Common Lisp HyperSpec: "
		common-lisp-hyperspec-symbols #'boundp
		t symbol-at-point
		'common-lisp-hyperspec-history))))
      (maplist
       (lambda (entry)
	 (info (format "(gcl)%s" (car entry)))
	 (if (cdr entry)
	     (sleep-for 1.5)))
       (delete-duplicates
	(let ((symbol (intern-soft (downcase symbol-name)
				   common-lisp-hyperspec-symbols)))
	  (if (and symbol (boundp symbol))
	      (list symbol)
	      (error "The symbol `%s' is not defined in Common Lisp"
		     symbol-name)))
	:test (function equal))))


    (defalias 'clhs               'common-lisp-hyperspec)
    (defalias 'hyperspec-lookup   'common-lisp-hyperspec) ; 'gcl-hyperspec)
    (global-set-key (kbd "C-h y") 'hyperspec-lookup)

    ) ;;(boundp 'common-lisp-hyperspec-symbols)


  (defun random-hyperspec ()
    (interactive)
    (let* ((random-hyperspec-symbol
	    (let ((syms '()))
	      (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
	      (nth (random (length syms)) syms)))
	   (random-page (let ((pages (symbol-value random-hyperspec-symbol)))
			  (nth (random (length pages)) pages))))
      (browse-url (concat common-lisp-hyperspec-root "Body/" random-page)))))




;;   (defun send-url-to-safari (url)
;;     "Sends URL to Safari, using Apple's Open Scripting Architecture."
;;     (with-temp-buffer
;;       (insert "tell application \"Safari\"\n")
;;       (insert "  activate\n")
;;       (insert "  make new document at the beginning of documents\n")
;;       (insert (format "  set the URL of the front document to \"%s\"\n" url))
;;       (insert "end tell\n")
;;       (call-process-region (point-min) (point-max) "/usr/bin/osascript")))

;;; (setq common-lisp-hyperspec-root
;;;       "file://Users/ayank/Documents/text/computer/lisp/HyperSpec/")
;;; (setq common-lisp-hyperspec-symbol-table
;;;       "file://Users/ayank/Documents/text/computer/lisp/HyperSpec/Data/Map_Sym.txt")

;;; (load-library
;;;  "file://Users/ayank/Documents/text/computer/lisp/ilisp/extra/hyperspec")

;;; (global-set-key [(shift f1)]
;;;                 '(lambda ()
;;;                    (interactive)
;;;                    (common-lisp-hyperspec
;;;                     (thing-at-point 'symbol))))


;;; or:
;;;       (push browse-url-browser-function
;;; 	    '("."  . (lambda (url &optional new-win)
;;; 	       (do-applescript (concat "open location \""
;;; 				       url "\"")))))



