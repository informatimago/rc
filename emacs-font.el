;;; emacs-font -- Pascal J. Bourguignon's emacs startup files.
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary:
;;; Code:

(.EMACS "~/rc/emacs-font.el %s" "Pascal Bourguignon's emacs font stuff.")

;;;----------------------------------------------------------------------------
;; See also:
;; (info "(emacs)Defining Fontsets")

(require 'font nil t)

(when (< emacs-major-version 22)

  (defun font-spatial-to-canonical (spec &optional device)
    "Convert SPEC (in inches, millimeters, points, or picas) into points"
    ;; 1 in = 6 pa = 25.4 mm = 72 pt
    (cond
      ((numberp spec)
       spec)
      ((null spec)
       nil)
      (t
       (let ((num nil)
             (type nil)
             ;; If for any reason we get null for any of this, default
             ;; to 1024x768 resolution on a 17" screen
             (pix-width (float (or (device-pixel-width device) 1024)))
             (mm-width (float (or (device-mm-width device) 293)))
             (retval nil))
         (cond
           ((string-match "^ *\\([-+*/]\\) *" spec) ; math!  whee!
            (let ((math-func (intern (match-string 1 spec)))
                  (other (font-spatial-to-canonical
                          (substring spec (match-end 0) nil)))
                  (default (font-spatial-to-canonical
                            (font-default-size-for-device device))))
              (if (and default (fboundp math-func))
                  (setq type "px"
                        spec (int-to-string (funcall math-func default other)))
                  (setq type "px"
                        spec (int-to-string other)))))
           ((string-match "[^0-9.]+$" spec)
            (setq type (substring spec (match-beginning 0))
                  spec (substring spec 0 (match-beginning 0))))
           (t
            (setq type "px"
                  spec spec)))
         (setq num (string-to-number spec))
         (cond
           ((member type '("pixel" "px" "pix"))
            (setq retval (* num (/ pix-width mm-width) (/ 25.4 72.0))))
           ((member type '("point" "pt"))
            (setq retval num))
           ((member type '("pica" "pa"))
            (setq retval (* num 12.0)))
           ((member type '("inch" "in"))
            (setq retval (* num 72.0)))
           ((string= type "mm")
            (setq retval (* num (/ 72.0 25.4))))
           ((string= type "cm")
            (setq retval (* num 10 (/ 72.0 25.4))))
           (t
            (setq retval num)))
         retval))))


  (when  (boundp 'x-font-alist)
    ;; Correct the font menu.
    (setf x-font-alist
          (let ((monop (find "monospaced fonts" (rest x-font-alist)
                             :test (function string=)
                             :key (function first))))
            (cons (first x-font-alist)
                  (loop for (a b) on (rest x-font-alist)
                     unless (equalp a b)
                     collect (cond
                               (monop a)
                               ((string= (first a) "proportional fonts")
                                '("monospaced fonts"   nil))
                               ((string= (first a) "non-proportional fonts")
                                '("proportional fonts" nil))
                               (t a)))))))

  );; when emacs-major-version < 23


(defvar *pjb-font-list* '("fixed"))
(defvar *pjb-current-font-index* 0)

(defun sign (number)
  (cond ((< number 0) -1)
        ((> number 0) +1)
        (t             0)))

;; (set-face-attribute 'default nil :height 150)
;; (set-face-attribute 'default nil :height 200)


(defun set-current-font ()
  (let ((font (case window-system
                ((ns)
                 (font-spec :family (elt *pjb-font-list* *pjb-current-font-index*)
                            :size 17))
                (otherwise
                 (elt *pjb-font-list* *pjb-current-font-index*)))))
    (set-frame-font font)
    (message "set frame font to %S"
             (elt *pjb-font-list* *pjb-current-font-index*))))


(defun* forward-font (&optional (increment 1))
  (interactive "p")
  (typecase increment
    (integer
     (let ((increment (if (zerop increment) 1 increment)))
       (setf *pjb-current-font-index* (mod (+ *pjb-current-font-index* increment)
                                           (length *pjb-font-list*)))))
    (string
     (let ((new-index (or (position increment *pjb-font-list*
                                    :test (function string=))
                          0)))
       (setf increment (- new-index *pjb-current-font-index*)
             *pjb-current-font-index* new-index))))
  (loop
     for try below (length *pjb-font-list*)
     do (ignore-errors (return (set-current-font)))
     do (message "Failed to set frame font %S" (elt *pjb-font-list* *pjb-current-font-index*))
     do (setf *pjb-current-font-index* (mod (+ *pjb-current-font-index* (sign increment))
                                            (length *pjb-font-list*))))
  (when (eq window-system 'ns)
    (mac-adjust-full-screen)))


(defun* backward-font (&optional (increment 1))
  (interactive "p")
  (forward-font (- increment)))

(global-set-key (kbd "C-c C-f +") 'forward-font)
(global-set-key (kbd "C-c C-f -") 'backward-font)

(defun after-make-frame/set-current-font (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (set-current-font)))

(pushnew 'after-make-frame/set-current-font after-make-frame-functions)


(defun remove-unknown-fonts (font-list)
  (let ((current-font (pjb-frame-font (selected-frame))))
    (unwind-protect
         (remove-if-not (lambda (font)
                          (ignore-errors (progn (set-frame-font font) t)))
                        font-list)
      (set-frame-font current-font))))


;; ------------------------------------------------------------------------
(defparameter *pjb-font-list*
  (cond
    ((string= (hostname) "L0253344") '("terminus-12" "terminus-14"  "terminus-18" "terminus-24") )
    ((string= (hostname)"frprld7818008")
                  '("fixed"
                   "terminus-12"
                   "terminus-14"
                   "terminus-18"
                   "terminus-32"
                   ))
    (t (remove-unknown-fonts
        (case window-system
          ((ns)
           '("Andale Mono"
             "Courier"
             "Courier New"
             "Menlo"
             "Monoid"
             "Monaco"
             "PCmyungjo"
             "PT Mono"))

          ((kuiper-x11-despina)

           '("Courier Rgular"
             "Courier Bold Italic"
             "Courier Italic"
             "Courier Bold"
             "DejaVu Sans Mono Bold Oblique"
             "DejaVu Sans Mono Oblique"
             "DejaVu Sans Mono Bold"
             "DejaVu Sans Mono Book"
             "Latin Modern Mono Bold"
             "Latin Modern Mono Caps Bold"
             "Latin Modern Mono Cond Bold"
             "Latin Modern Mono Cond Italic"
             "Latin Modern Mono Prop Bold"
             "Latin Modern Mono Prop Bold Italic"
             "Latin Modern Mono Light Bold"
             "Latin Modern Mono Light Bold Italic"
             "Latin Modern Mono Slanted Bold"
             "Latin Modern Mono Slanted Bold Italic"
             "Liberation Mono Regular"
             "Liberation Mono Bold Italic"
             "Liberation Mono Bold"
             "Liberation Mono Italic"
             "Monospace"
             "Monospace Bold"
             "Monospace Bold Italic"
             "Nimbus Mono L Bold"
             "Nimbus Mono L Bold Oblique"
             "Nimbus Mono L Regular"
             "Nimbus Mono L Regular Oblique"
             "Noto Mono Bold"
             "Noto Mono Bold Italic"
             "TeX Gyre Cursor Bold"
             "TeX Gyre Cursor Bold Italic")
           
           ;; $ xlsfonts|grep -e -m-|sed -e 's/-[rio]-.*//'|sort -u
           ;; -adobe-courier-bold
           ;; -adobe-courier-medium
           ;; -apple-.sf ns mono-light
           ;; -b&h-lucidatypewriter-bold
           ;; -b&h-lucidatypewriter-medium
           ;; -b&h-luxi mono-bold
           ;; -b&h-luxi mono-medium
           ;; -bitstream-bitstream vera sans mono-bold
           ;; -bitstream-bitstream vera sans mono-medium
           ;; -bitstream-courier 10 pitch-bold
           ;; -bitstream-courier 10 pitch-medium
           ;; -bitstream-menlo-medium
           ;; -ibm-courier-bold
           ;; -ibm-courier-medium
           
           )
          (otherwise
           '(
             "fixed"
             "terminus-14"
             "terminus-18"
             "terminus-32"

             "-unknown-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
             "-unknown-DejaVu Sans Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"
             "-unknown-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"
             "-unknown-DejaVu Sans Mono-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1"
             "-unknown-DejaVu Sans Mono-normal-normal-normal-*-25-*-*-*-m-0-iso10646-1"



             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-17-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-21-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-25-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-29-*-*-*-m-0-*-*"
             "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-33-*-*-*-m-0-*-*"


             "-b&h-lucidatypewriter-medium-r-normal-sans-8-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-10-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-11-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-12-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-bold-r-normal-sans-12-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-14-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-bold-r-normal-sans-14-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-15-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-17-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-18-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-19-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-21-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-23-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-25-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-27-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-29-*-*-*-m-*-*-*"
             "-b&h-lucidatypewriter-medium-r-normal-sans-32-*-*-*-m-*-*-*"

             "-bitstream-courier 10 pitch-medium-r-normal--*-*-*-*-m-*-*-*"
             "-bitstream-courier 10 pitch-medium-r-normal--11-130-*-*-m-*-*-*"
             "-bitstream-courier 10 pitch-medium-r-normal--12-130-*-*-m-*-*-*"
             "-bitstream-courier 10 pitch-medium-r-normal--13-130-*-*-m-*-*-*"
             "-bitstream-courier 10 pitch-medium-r-normal--14-130-*-*-m-*-*-*"
             "-bitstream-courier 10 pitch-medium-r-normal--15-150-*-*-m-*-*-*"
             "-bitstream-courier 10 pitch-medium-r-normal--17-170-*-*-m-*-*-*"
             "-bitstream-courier 10 pitch-medium-r-normal--19-170-*-*-m-*-*-*"

             "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-9-*-*-*-m-0-*-*"
             "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-11-*-*-*-m-0-*-*"
             "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-13-*-*-*-m-0-*-*"
             "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-15-*-*-*-m-0-*-*"
             "-unknown-Droid Sans Mono Dotted-normal-normal-normal-*-17-*-*-*-m-0-*-*"



             "-bitstream-terminal-medium-r-normal--18-140-100-100-c-110-iso8859-1"
             "-sony-fixed-medium-r-normal--16-120-100-100-c-80-iso8859-1"

             "-LFP-Bright-normal-normal-normal-*-9-*-*-*-c-60-*-*"
             "-LFP-Smooth-normal-normal-normal-*-9-*-*-*-c-60-*-*"
             "-LFP-LucidaTerminal-normal-normal-normal-*-9-*-*-*-c-90-*-*"

             "-LFP-Computer-normal-normal-normal-*-11-*-*-*-c-90-*-*"
             "-LFP-Computer Alt-normal-normal-normal-*-9-*-*-*-c-90-iso10646-1"


             "-adobe-courier-medium-r-normal--*-*-*-*-m-*-*-*"

             "-b&h-luxi mono-medium-r-normal--*-*-*-*-m-*-*-*"
             "-b&h-Luxi Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"
             "-b&h-Luxi Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"
             "-b&h-Luxi Mono-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1"
             "-b&h-Luxi Mono-normal-normal-normal-*-23-*-*-*-m-0-iso10646-1"

             "-ibm-courier-medium-r-normal--*-*-*-*-m-*-*-*"
             "-monotype-courier new-medium-r-normal--*-*-*-*-m-*-*-*"
             "-urw-courier-medium-r-normal--*-*-*-*-m-*-*-*"
             "-urw-nimbus mono l-medium-r-normal--*-*-*-*-m-*-*-*"

             "-Schumacher-Clean-normal-normal-normal-*-12-*-*-*-c-60-*-*"

             "-urw-Nimbus Mono L-normal-normal-normal-*-15-*-*-*-m-0-fontset-auto25"
             "-KC-Fixed-normal-normal-normal-*-15-*-*-*-c-80-fontset-auto1"
             "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"


             "-unknown-ArnoldBoecklin-extra-bold-normal-normal-*-16-*-*-*-*-0-*-*"
             "-unknown-Becker-normal-normal-normal-*-16-*-*-*-*-0-*-*"
             "-unknown-Caligula-normal-normal-normal-*-19-*-*-*-*-0-*-*"


             "-unknown-Bandal-normal-normal-normal-*-16-*-*-*-*-0-*-*"
             "-unknown-Penguin Attack-normal-normal-normal-*-19-*-*-*-*-0-*-*"
             "-artwiz-glisp-medium-r-normal--11-110-75-75-p-90-*-*"
             "-artwiz-glisp-medium-r-normal--13-130-75-75-p-90-*-*"

             "-adobe-courier-medium-r-normal--*-*-*-*-m-*-*-*"
             "-ibm-courier-medium-r-normal--*-*-*-*-m-*-*-*"
             "-monotype-courier new-medium-r-normal--*-*-*-*-m-*-*-*"
             "-urw-courier-medium-r-normal--*-*-*-*-m-*-*-*"
             "-urw-nimbus mono l-medium-r-normal--*-*-*-*-m-*-*-*"

             "-urw-Nimbus Mono L-normal-normal-normal-*-15-*-*-*-m-0-fontset-auto25"
             "-KC-Fixed-normal-normal-normal-*-15-*-*-*-c-80-fontset-auto1"

             "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
             ))

          ;; (otherwise
          ;;  '("-adobe-courier-medium-o-normal-*-*-*-*-*-m-*-*-*"
          ;;    "-adobe-courier-medium-r-normal-*-*-*-*-*-m-*-*-*"
          ;;    "-b&h-lucidatypewriter-medium-r-normal-sans-*-*-*-*-m-*-*-*"
          ;;    "-bitstream-courier  pitch-medium-i-normal-*-*-*-*-*-m-*-*-*"
          ;;    "-bitstream-courier  pitch-medium-r-normal-*-*-*-*-*-m-*-*-*"
          ;;
          ;;    "fixed"
          ;;    "lucidasans-*"
          ;;    "lucidasanstypewriter-*"
          ;;    ))

          )))))


;; ------------------------------------------------------------------------

(when nil

  (pjb-frame-font (selected-frame))
  "-bitstream-bitstream vera sans mono-medium-r-normal--21-152-100-100-m-130-iso8859-1"
  "-bitstream-bitstream vera sans mono-medium-r-normal--19-137-100-100-m-110-iso8859-1"
  "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1"

  (set-frame-font  )
  )


;; ------------------------------------------------------------------------


;; (let ((*pjb-font-list* (split-string (shell-command-to-string "xlsfonts -fn  -*-*-medium-r-normal-*-19-137-*-*-m-*-iso10646-*") "\n" t)))
;;   (forward-font))


;; *** Which font backends to use can be specified by the X resource
;; "FontBackend".  For instance, to use both X core fonts and Xft fonts:
;;
;; Emacs.FontBackend: x,xft
;;
;; If this resource is not set, Emacs tries to use all font backends
;; available on your graphic device.
;;
;; *** New frame parameter `font-backend' specifies a list of
;; font-backends supported by the frame's graphic device.  On X, they are
;; currently `x' and `xft'.


;; (when (eq window-system 'x)
;;   (set-frame-font
;;    (if (fboundp 'font-exists-p)
;;      (cond
;;       ((font-exists-p  "7x13") "7x13")
;;       ((font-exists-p (make-font-pattern :foundry "lispm" :family "fixed"))
;;        (create-fontset-from-fontset-spec
;;         "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-fontset-lispm,
;; ascii:,
;; latin-iso8859-1:-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*,
;; latin-iso8859-15:-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*")
;;        ;; once the fontset has been defined, it can be invoked :
;;        "fontset-lispm")
;;       ((font-exists-p  "lucidasanstypewriter-12") "lucidasanstypewriter-12")
;;       (t *default-font*))
;;      *default-font*))
;;   (when (fboundp 'single-frame) (single-frame)))


;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
