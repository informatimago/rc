;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
(require 'org-element)


(defvar *file-id-scramble* nil)
(setf *file-id-scramble* nil)

(defun file-id (path root)
  "Compute a unique file ID from the `path', relative to `root',
without the extension."
  (let ((path (expand-file-name path))
        (root (expand-file-name root)))
    (unless (and (< (length root) (length path))
                 (string= root (subseq path 0 (length root))))
      (error "Path %S should be under the root %S" path root))
    (let ((base (file-name-sans-extension
                 (subseq path (+ (length root)
                                 (if (= ?/ (aref path (length root)))
                                     1
                                     0))))))
      (let ((result (mapconcat (function identity) (split-string base "/") "--")))
        (if *file-id-scramble*
            (sha1 result) ; or a substring?
            result)))))

;; (file-id "emacs-acquire.el" "~/")
;; "rc--emacs-acquire"
;; "4f1d9803ba58ea1aeb4d06217a5e54d93d465d11"



(defun prepare-links-for-single-file () " This command must be called
  from the root .org; it will process (recursively) only the
  #+INCLUDEd files.

Convert :CUSTOM_ID prefixing them with the file identifier,
and Convert file links:

- file links to an anchor are converted by prefixing the anchor with
  the linked file identifier,

- file links to a file are converted by using the anchor of the first
  header in the linked file.

"
  (interactive)
  (let* ((root-path (buffer-file-name))
         (root-dir  (directory-namestring root-path))
         (root-id   (file-id root-path root-dir)))
    (goto-char (point-min))

    (org-element-map (org-element-parse-buffer)
                     '(link headline keyword)

                     ;; org-element-all-elements
                     ;; (babel-call center-block clock comment comment-block diary-sexp drawer dynamic-block example-block export-block fixed-width footnote-definition headline horizontal-rule inlinetask item keyword latex-environment node-property paragraph plain-list planning property-drawer quote-block section special-block src-block table table-row verse-block)
                     ;; org-element-all-objects
                     ;; (bold code entity export-snippet footnote-reference inline-babel-call inline-src-block italic line-break latex-fragment link macro radio-target statistics-cookie strike-through subscript superscript table-cell target timestamp underline verbatim)

                     (lambda (datum)
                       (message "element type = %s" (org-element-type datum))
                       ;; (message "datum = %S" datum)
                       (case (org-element-type datum)
                         ((headline)
                          (message "%s %S"
                                   (org-element-property :level datum)
                                   (substring-no-properties (first (org-element-property :title datum)) 0)))
                         ((keyword)
                          (message "%S %S"
                                   (org-element-property :key datum)
                                   (org-element-property :value datum))
                          (when (string-equal include (org-element-property :key datum))
                            (destructuring-bind (file &rest keys)
                                (car (read-from-string (concat "(" (org-element-property :value datum) ")")))
                              (message "%S -> %S" file keys))
                            ))
                         ((link)
                          (message "%S" datum)))))
    ))

(defun org-lint-duplicate-name (ast)
  (org-lint--collect-duplicates
   ast
   org-element-all-elements
   (lambda (datum) (org-element-property :name datum))
   (lambda (datum name)
     (goto-char (org-element-property :begin datum))
     (re-search-forward
      (format "^[ \t]*#\\+[A-Za-z]+: +%s *$" (regexp-quote name)))
     (match-beginning 0))
   (lambda (key) (format "Duplicate NAME \"%s\"" key))))

(defun pjb-wrap-latex-block (start end)
  (interactive "r")
  (goto-char end)
  (end-of-line)
  (let ((end (point)))
    (beginning-of-line)
    (when (< (point) end)
      (end-of-line)
      (insert "\n")))
  (insert "#+LATEX: }\n")
  (goto-char start)
  (beginning-of-line)
  (insert "#+LATEX: {\\small\n"))

