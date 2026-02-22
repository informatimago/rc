;;; emacs-package -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "~/rc/emacs-package.el")

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")

(.EMACS "Initializing emacs packages.")
(require 'package)
(ignore-errors (package-initialize))

(defun package-install-from-archive (pkg-desc)
  "Download and install a package defined by PKG-DESC."
  ;; This won't happen, unless the archive is doing something wrong.
  (when (eq (package-desc-kind pkg-desc) 'dir)
    (error "Can't install directory package from archive"))
  (message "pkg-desc = %S" pkg-desc)
  (message "package-desc-archive --> = %S %S"
           (package-desc-archive pkg-desc)
           (assoc (package-desc-archive pkg-desc) package-archives))
  (let* ((location (package-archive-base pkg-desc))
         (file (concat (package-desc-full-name pkg-desc)
                       (package-desc-suffix pkg-desc))))
  (message "location = %S" location)
    (package--with-response-buffer location :file file
      (if (or (not (package-check-signature))
              (member (package-desc-archive pkg-desc)
                      package-unsigned-archives))
          ;; If we don't care about the signature, unpack and we're
          ;; done.
          (let ((save-silently t))
            (package-unpack pkg-desc))
        ;; If we care, check it and *then* write the file.
        (let ((content (buffer-string)))
          (package--check-signature
           location file content nil
           ;; This function will be called after signature checking.
           (lambda (&optional good-sigs)
             ;; Signature checked, unpack now.
             (with-temp-buffer ;FIXME: Just use the previous current-buffer.
               (set-buffer-multibyte nil)
               (cl-assert (not (multibyte-string-p content)))
               (insert content)
               (let ((save-silently t))
                 (package-unpack pkg-desc)))
             ;; Here the package has been installed successfully, mark it as
             ;; signed if appropriate.
             (when good-sigs
               ;; Write out good signatures into NAME-VERSION.signed file.
               (write-region (mapconcat #'epg-signature-to-string good-sigs "\n")
                             nil
                             (expand-file-name
                              (concat (package-desc-full-name pkg-desc) ".signed")
                              package-user-dir)
                             nil 'silent)
               ;; Update the old pkg-desc which will be shown on the description buffer.
               (setf (package-desc-signed pkg-desc) t)
               ;; Update the new (activated) pkg-desc as well.
               (when-let* ((pkg-descs (cdr (assq (package-desc-name pkg-desc)
                                                 package-alist))))
                 (setf (package-desc-signed (car pkg-descs)) t))))))))))

(add-to-list 'package-archives '("nongnu"        . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("gnu"           . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable"  . "https://stable.melpa.org/packages/"))

;; (progn (setf package-archive-contents nil)
;;        (package-refresh-contents))

(unless package-archive-contents
  (setf package-archive-contents nil)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


(when (fboundp 'run-lac-functions)
  (run-lac-functions))


(defvar *packages* '())
(setq *packages* (append '(

			               paredit

			               compat
			               json popup

			               company auto-complete inf-ruby enh-ruby-mode
			               smartparens highlight-indentation textmate
			               flycheck

			               stack
			               cobol-mode

			               )

			             (when (< 26 emacs-major-version)
			               '(;; emms

			                 lsp-mode
			                 ;; lsp-haskell
			                 lsp-ui
			                 ))

			             (when nil
			               '(
			                 ;; intero
			                 ;; ormolu
			                 ;; retrie
			                 ;; shm
			                 ;; ghc
			                 ;; ghc-imported-from
			                 ;; ghci-completion
			                 ;; ac-haskell-process
			                 ;; dante
			                 ;; flycheck-ghcmod
			                 ;; flycheck-haskell
			                 ;; flycheck-hdevtools
			                 ;; flycheck-liquidhs
			                 ;; haskell-emacs
			                 ;; haskell-emacs-base
			                 ;; haskell-emacs-text
			                 ;; haskell-mode
			                 ;; haskell-snippets
			                 ;; haskell-tab-indent
			                 ;; hi2
			                 ;; hindent
			                 ;; hyai
                                        ; robe not on emacs-24.4 ?
			                 ))))

(defun pjb-install-packages ()
  (interactive)
  (mapc (lambda (package)
          (ignore-errors (package-install package)))
        *packages*))

(pjb-install-packages)


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
;; (package-install 'google-translate)
