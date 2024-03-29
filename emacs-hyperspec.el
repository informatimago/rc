;;; emacs-hyperspec -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "emacs-hyperspec.el")
(require 'cl)
(require 'browse-url)			;you need the Emacs 20 version
(require 'thingatpt)
(require 'clhs      nil t)
(require 'hyperspec nil t)

(defun pjb-sync-get-resource-at-url (url)
  (let ((done nil))
    (url-http (url-generic-parse-url url)
              (lambda (&rest args)
                (message "args = %S" args)
                (message "current buffer = %S" (current-buffer))
                (setf done (buffer-substring (point-min) (point-max))))
              nil)
    (while (not done)
           (sleep-for 0.1))
    done))

(defun probe-url (url)
  (cond
    ((string= "file://" (subseq url 0 (min (length url) 7)))
     (file-readable-p (subseq url 7)))
    ((file-readable-p "/tmp/no-internet")
     nil)
    (t
     (handler-case (progn (pjb-sync-get-resource-at-url url)
                          t)
       (error () nil)))))


(defun pjb-get-resource-at-url (url)
  "Fetches a resource at URL, and returns it."
  (cond
    ((string= "file://" (subseq url 0 (min (length url) 7)))
     (with-file ((subseq url 7) :save nil :kill t :literal t)
       (buffer-substring-no-properties (point-min) (point-max))))
    (t
     (pjb-sync-get-resource-at-url url))))


(defparameter *clhs-lispworks* "www.lispworks.com/documentation/HyperSpec/")
(defparameter *clhs-map-sym*   "Data/Map_Sym.txt")
(defparameter common-lisp-hyperspec-root
  (dolist (url (list (concat "file://" (get-directory :hyperspec))
                     "file:///opt/local/share/doc/lisp/lisp-hyperspec-7.0/HyperSpec/"
                     "file:///usr/share/doc/hyperspec/HyperSpec/"
                     "file:///usr/share/doc/hyperspec/"
                     "file:///usr/local/share/doc/cl/HyperSpec/"
                     (concat "file:///usr/local/html/local/lisp/" *clhs-lispworks*)
                     "file:///data/lisp/hyperspec-7.0/HyperSpec/"
                     "file:///opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/"
                     (concat "file://" (expand-file-name "~/quicklisp/dists/quicklisp/software/clhs-0.6.1/HyperSpec-7-0/HyperSpec/"))
                     ;; (unless (member* (hostname) '("hubble.informatimago.com" "proteus")
                     ;;                  :test (function string=))
                     ;;   "http://kuiper.lan.informatimago.com/local/lisp/www.lispworks.com/documentation/HyperSpec/")
                     (concat "http://" *clhs-lispworks*)
                     "http://www.harlequin.com/education/books/HyperSpec/")
               nil)
    (message "url = %S" url)
    (when (and url (probe-url (concat url *clhs-map-sym*)))
      (return url)))
  "The root of the Common Lisp HyperSpec URL.
If you copy the HyperSpec to your local system, set this variable to
something like \"file:/usr/local/doc/HyperSpec/\".")

(defparameter *hyperspec-path*
  (cond ((prefixp "file://" common-lisp-hyperspec-root)
         (subseq common-lisp-hyperspec-root (length "file://")))
        ((boundp '*hyperspec-path*)
         *hyperspec-path*)))


(defparameter common-lisp-hyperspec-symbols
  (if common-lisp-hyperspec-root
      (let ((symbols (make-vector 67 0)))
        (loop
          for (name page)
          on (split-string (pjb-get-resource-at-url (concat common-lisp-hyperspec-root *clhs-map-sym*)) "\n")
          by (function cddr)
          while page
          do (let ((symbol (intern (string-downcase name) symbols))
                   (page (if (prefixp "../" page)
                             (subseq page 3 )
                             page)))
               ;; (message "%S %S" symbol page)
               (setf (get symbol 'common-lisp-hyperspec-page) page)))
        symbols)
      (make-vector 67 0)))


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




(defun common-lisp-hyperspec-complete (string predicate allp)
  (if allp
      (let ((result '()))
        (mapatoms (lambda (symbol)
                    (let ((name (symbol-name symbol)))
                      (when (or (and (<= (length string) (length name))
                                     (cl:string-equal string name :end2 (length string)))
                                (search (concat "-" string) name :test (function equalp)))
                        (push name result))))
                  common-lisp-hyperspec-symbols)
        result)
      (try-completion string common-lisp-hyperspec-symbols predicate)))


(defun clhs-page (symbol-designator)
  (let ((symbol (intern-soft (downcase (etypecase symbol-designator
                                         (symbol (symbol-name symbol-designator))
                                         (string  symbol-designator)))
                             common-lisp-hyperspec-symbols)))
    (and symbol (get symbol 'common-lisp-hyperspec-page))))


(defparameter common-lisp-hyperspec-browser 'eww)
(defvar common-lisp-hyperspec-history nil
  "History of symbols looked up in the Common Lisp HyperSpec.")


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
           (completing-read "Look up symbol in Common Lisp HyperSpec: "
                            (function common-lisp-hyperspec-complete)
                            (lambda (symbol)
                              (get symbol 'common-lisp-hyperspec-page))
                            t symbol-at-point
                            'common-lisp-hyperspec-history))))
  (let ((url  (concat common-lisp-hyperspec-root (clhs-page symbol-name)))
        (browse-url-browser-function common-lisp-hyperspec-browser))
    (message "%s" url)
    (browse-url url)))


(defalias 'clhs               'common-lisp-hyperspec)
(defalias 'hyperspec-lookup   'common-lisp-hyperspec)
(global-set-key (kbd "C-h y") 'common-lisp-hyperspec)


(defun random-hyperspec ()
  (interactive)
  (common-lisp-hyperspec (let ((syms '()))
                           (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
                           (nth (random (length syms)) syms))))


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
