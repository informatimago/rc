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

(defvar *clhs-lispworks* "www.lispworks.com/documentation/HyperSpec/")
(defvar *clhs-map-sym*   "Data/Map_Sym.txt")
(defvar common-lisp-hyperspec-root nil
  "The root of the Common Lisp HyperSpec URL.
This file resolves the local CLHS lazily, when `clhs' is first used, so
Emacs startup never probes the network.")
(defvar *hyperspec-path* nil)
(defvar common-lisp-hyperspec-symbols nil)
(defvar pjb-clhs-auto-install nil
  "When non-nil, offer to install CLHS through Quicklisp on first lookup.")
(defvar pjb-clhs-lisp-command "sbcl"
  "Common Lisp command used by `pjb-clhs-install-quicklisp'.")
(defvar pjb-clhs-quicklisp-directory (home "quicklisp/")
  "Quicklisp directory used to discover the local CLHS installation.")

(defun pjb-clhs-user-error (format-string &rest args)
  (if (fboundp 'user-error)
      (apply 'user-error format-string args)
      (apply 'error format-string args)))

(defun pjb-clhs-file-url-p (url)
  (and (stringp url)
       (string= "file://" (subseq url 0 (min (length url) 7)))))

(defun pjb-clhs-file-url-path (url)
  (when (pjb-clhs-file-url-p url)
    (subseq url (length "file://"))))

(defun pjb-clhs-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun pjb-clhs-installed-system-directory ()
  (let ((location-file (expand-file-name "dists/quicklisp/installed/systems/clhs.txt"
                                         pjb-clhs-quicklisp-directory)))
    (when (file-readable-p location-file)
      (with-temp-buffer
        (insert-file-contents location-file)
        (let ((relative (buffer-substring-no-properties (point-min) (point-max))))
          (setf relative (replace-regexp-in-string "[ \t\n\r]+\\'" "" relative))
          (file-name-directory
           (expand-file-name relative pjb-clhs-quicklisp-directory)))))))

(defun pjb-clhs-local-candidates ()
  (let ((configured (pjb-clhs-file-url-path common-lisp-hyperspec-root))
        (quicklisp-glob
         (reverse
          (sort
           (file-expand-wildcards
            (expand-file-name "dists/quicklisp/software/clhs-*/HyperSpec-7-0/HyperSpec/"
                              pjb-clhs-quicklisp-directory))
           'string<))))
    (append
     (list configured
           (ignore-errors (get-directory :hyperspec))
           (let ((system-directory (pjb-clhs-installed-system-directory)))
             (when system-directory
               (expand-file-name "HyperSpec-7-0/HyperSpec/" system-directory)))
           (expand-file-name "HyperSpec/" pjb-clhs-quicklisp-directory)
           "/opt/local/share/doc/lisp/lisp-hyperspec-7.0/HyperSpec/"
           "/usr/share/doc/hyperspec/HyperSpec/"
           "/usr/share/doc/hyperspec/"
           "/usr/local/share/doc/cl/HyperSpec/"
           (concat "/usr/local/html/local/lisp/" *clhs-lispworks*)
           "/data/lisp/hyperspec-7.0/HyperSpec/"
           "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/")
     quicklisp-glob)))

(defun pjb-clhs-valid-root-p (directory)
  (and directory
       (let ((directory (file-name-as-directory (expand-file-name directory))))
         (file-readable-p (expand-file-name *clhs-map-sym* directory)))))

(defun pjb-clhs-find-root ()
  (loop
    for directory in (pjb-clhs-local-candidates)
    for expanded = (and directory
                        (file-name-as-directory (expand-file-name directory)))
    when (pjb-clhs-valid-root-p expanded)
    return expanded))

(defun pjb-clhs-read-map-sym (map-file)
  (let ((symbols (make-vector 67 0)))
    (with-temp-buffer
      (insert-file-contents map-file)
      (loop
        for (name page)
        on (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")
        by (function cddr)
        while page
        do (let ((symbol (intern (downcase name) symbols))
                 (page (if (pjb-clhs-prefix-p "../" page)
                           (subseq page 3)
                           page)))
             (setf (get symbol 'common-lisp-hyperspec-page) page))))
    symbols))

(defun pjb-clhs-install-quicklisp ()
  "Install the Quicklisp CLHS wrapper asynchronously.
This is explicit because it may perform network access."
  (interactive)
  (let ((command
         (format "%s --eval %S --eval %S --eval %S"
                 pjb-clhs-lisp-command
                 "(load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))"
                 "(ql:quickload :clhs)"
                 "(quit)")))
    (message "Installing CLHS with: %s" command)
    (async-shell-command command "*clhs-install*")))

(defun pjb-clhs-ensure-loaded ()
  (unless common-lisp-hyperspec-symbols
    (let ((root (pjb-clhs-find-root)))
      (unless root
        (when (and pjb-clhs-auto-install
                   (y-or-n-p "Local CLHS not found. Install it with Quicklisp now? "))
          (pjb-clhs-install-quicklisp)
          (pjb-clhs-user-error "CLHS installation started; retry lookup when it finishes"))
        (pjb-clhs-user-error
         "Local CLHS not found; run M-x pjb-clhs-install-quicklisp or configure :hyperspec"))
      (setf *hyperspec-path* root
            common-lisp-hyperspec-root (concat "file://" root)
            common-lisp-hyperspec-symbols
            (pjb-clhs-read-map-sym (expand-file-name *clhs-map-sym* root))))))


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
  (pjb-clhs-ensure-loaded)
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
  (pjb-clhs-ensure-loaded)
  (let ((symbol (intern-soft (downcase (etypecase symbol-designator
                                         (symbol (symbol-name symbol-designator))
                                         (string  symbol-designator)))
                             common-lisp-hyperspec-symbols)))
    (and symbol (get symbol 'common-lisp-hyperspec-page))))


(defvar common-lisp-hyperspec-browser 'eww)
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
                              (pjb-clhs-ensure-loaded)
                              (get symbol 'common-lisp-hyperspec-page))
                            t symbol-at-point
                            'common-lisp-hyperspec-history))))
  (let ((page (clhs-page symbol-name)))
    (unless page
      (pjb-clhs-user-error "The symbol `%s' is not defined in Common Lisp" symbol-name))
    (let ((url  (concat common-lisp-hyperspec-root page))
          (browse-url-browser-function common-lisp-hyperspec-browser))
      (message "%s" url)
      (browse-url url))))


(defalias 'clhs               'common-lisp-hyperspec)
(defalias 'hyperspec-lookup   'common-lisp-hyperspec)
(global-set-key (kbd "C-h y") 'common-lisp-hyperspec)


(defun random-hyperspec ()
  (interactive)
  (pjb-clhs-ensure-loaded)
  (common-lisp-hyperspec (let ((syms '()))
                           (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
                           (nth (random (length syms)) syms))))


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
