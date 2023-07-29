;;; emacs-patches -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(require 'faces)

(defun defined-colors (&optional frame)
  "Return a list of colors supported for a particular frame.
The argument FRAME specifies which frame to try.
The value may be different for frames on different display types.
If FRAME doesn't support colors, the value is nil.
If FRAME is nil, that stands for the selected frame."
  (remove* nil
           (if (memq (framep (or frame (selected-frame))) '(x w32 ns))
               (xw-defined-colors frame)
               (mapcar 'car (tty-color-alist frame)))
           :key (function color-values)))


(require 'compile)

(defun compilation-parse-errors (start end &rest rules)
  "Parse errors between START and END.
The errors recognized are the ones specified in RULES which default
to `compilation-error-regexp-alist' if RULES is nil."
  (let ((case-fold-search compilation-error-case-fold-search)
        (omake-included (memq 'omake compilation-error-regexp-alist)))
    (dolist (rule-item (or rules compilation-error-regexp-alist))
      (let* ((item (if (symbolp rule-item)
                       (cdr (assq rule-item compilation-error-regexp-alist-alist))
                       rule-item))
             (pat (car item))
             (file (nth 1 item))
             (line (nth 2 item))
             (col (nth 3 item))
             (type (nth 4 item))
             (rule (and (symbolp rule-item) rule-item))
             end-line end-col fmt
             props)

        ;; omake reports some error indented, so skip the indentation.
        ;; another solution is to modify (some?) regexps in
        ;; `compilation-error-regexp-alist'.
        ;; note that omake usage is not limited to ocaml and C (for stubs).
        ;; FIXME-omake: Doing it here seems wrong, at least it should depend on
        ;; whether or not omake's own error messages are recognized.
        (cond
          ((not omake-included) nil)
          ((not pat) nil)
          ((string-match "\\`\\([^^]\\|\\^\\( \\*\\|\\[\\)\\)" pat)
           nil) ;; Not anchored or anchored but already allows empty spaces.
          (t (setq pat (concat "^\\(?:      \\)?" (substring pat 1)))))

        (if (and (consp file) (not (functionp file)))
            (setq fmt (cdr file)
                  file (car file)))
        (if (and (consp line) (not (functionp line)))
            (setq end-line (cdr line)
                  line (car line)))
        (if (and (consp col) (not (functionp col)))
            (setq end-col (cdr col)
                  col (car col)))

        (unless (or (null (nth 5 item)) (integerp (nth 5 item)))
          (error "HYPERLINK should be an integer: %s" (nth 5 item)))

        (goto-char start)
        (while (and pat (re-search-forward pat end t))
          (when (setq props (compilation-error-properties
                             file line end-line col end-col
                             (or type 2) fmt rule))

            (when file
              (let ((this-type (if (consp type)
                                   (compilation-type type)
                                   (or type 2))))
                (compilation--note-type this-type)

                (compilation--put-prop
                 file 'font-lock-face
                 (symbol-value (aref [compilation-info-face
                                      compilation-warning-face
                                      compilation-error-face]
                                     this-type)))))

            (compilation--put-prop
             line 'font-lock-face compilation-line-face)
            (compilation--put-prop
             end-line 'font-lock-face compilation-line-face)

            (compilation--put-prop
             col 'font-lock-face compilation-column-face)
            (compilation--put-prop
             end-col 'font-lock-face compilation-column-face)

            ;; Obey HIGHLIGHT.
            (dolist (extra-item (nthcdr 6 item))
              (let ((mn (pop extra-item)))
                (when (match-beginning mn)
                  (let ((face (eval (car extra-item))))
                    (cond
                      ((null face))
                      ((or (symbolp face) (stringp face))
                       (put-text-property
                        (match-beginning mn) (match-end mn)
                        'font-lock-face face))
		              ((and (listp face)
			                (eq (car face) 'face)
			                (or (symbolp (cadr face))
			                    (stringp (cadr face))))
                       (compilation--put-prop mn 'font-lock-face (cadr face))
                       (add-text-properties
                        (match-beginning mn) (match-end mn)
                        (nthcdr 2 face)))
                      (t
                       (error "Don't know how to handle face %S"
                              face)))))))
            (let ((mn (or (nth 5 item) 0)))
              (when compilation-debug
                (font-lock-append-text-property
                 (match-beginning 0) (match-end 0)
                 'compilation-debug (vector 'std item props)))
              (add-text-properties
               (match-beginning mn) (match-end mn)
               (cddr props))
              (font-lock-append-text-property
               (match-beginning mn) (match-end mn)
               'font-lock-face (cadr props)))))))))



;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
