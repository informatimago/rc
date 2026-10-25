;;; emacs-patches -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(load (rc "emacs-patches-compilation.el"))
;; (load (rc "emacs-patches-ox.el"))

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

;; auto-complete pre-30 obarray compatibility shim — convert obarrays
;; to their symbol list before AC tries to sequence-iterate them.
(with-eval-after-load 'auto-complete
  (defun my/ac-symbols-source (&rest _)
    (let ((acc '()))
      (mapatoms (lambda (s) (push (symbol-name s) acc)) obarray)
      acc))
  (when (boundp 'ac-source-symbols)
    (setq ac-source-symbols
          '((candidates . my/ac-symbols-source)
            (cache)))))



(require 'shell)
(load (rc "emacs-windows-paths.el")) ; posix-to-windows-path

(defun shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to
`shell-dirstack-query' (default \"dirs\"), reads the next
line output and parses it to form the new directory stack."
  (interactive)
  (let* ((dls (car
               (string-lines
                (string-chop-newline
                 (shell-eval-command (concat shell-dirstack-query "\n"))))))
         (dlsl nil)
         (pos 0)
         (ds nil))
    (setq dls (string-trim-right dls "[ ]+"))
    ;; Split the dirlist into whitespace and non-whitespace chunks.
    ;; dlsl will be a reversed list of tokens.
    (while (string-match "\\(\\S-+\\|\\s-+\\)" dls pos)
           (push (match-string 1 dls) dlsl)
           (setq pos (match-end 1)))

    (message "dlsl=%S"dlsl) 
    ;; ;; Prepend trailing entries until they form an existing directory,
    ;; ;; whitespace and all.  Discard the next whitespace and repeat.
    ;; (while dlsl
    ;;        (let ((newelt "")
    ;;              tem1 tem2)
    ;;          (while (and dlsl newelt)
    ;;                 ;; We need tem1 because we don't want to prepend
    ;;                 ;; `comint-file-name-prefix' repeatedly into newelt via tem2.
    ;;                 (setq tem1 (pop dlsl)
    ;;                       tem2 (concat comint-file-name-prefix tem1 newelt))
    ;;                 (cond ((file-directory-p tem2)
    ;;                        (push tem2 ds)
    ;;                        (when (string= " " (car dlsl))
    ;;                          (pop dlsl))
    ;;                        (setq newelt nil))
    ;;                       (t
    ;;                        (setq newelt (concat tem1 newelt)))))))
    (setf ds dlsl)
    (message "ds=%S" ds)
    (with-demoted-errors "Couldn't cd: %s"
      (shell-cd (file-name-as-directory (case window-system
                                          ((w32)     (posix-to-windows-path (car ds)))
                                          (otherwise (car ds)))))
      (setq shell-dirstack (cdr ds)
            shell-last-dir (car shell-dirstack))
      (shell-dirstack-message))))

;; For your typing convenience:
(defalias 'dirs 'shell-resync-dirs)

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:

