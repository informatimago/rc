;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;
;;;; Pascal J. Bourguignon's emacs android startup file.
;;;;----------------------------------------------------------------------------

(.EMACS "~/rc/emacs-palette.el %s" "Pascal Bourguignon's palette stuff.")

(defvar *palettes* '())
(defvar *current-palette* nil)


(defstruct palette
  name foreground background cursor region mouse)


(defmacro defpalette (name foreground background cursor region mouse)
  `(progn
     (defparameter ,name (make-palette :name ',name
                                       :foreground ,foreground
                                       :background ,background
                                       :cursor ,cursor
                                       :region ,region
                                       :mouse ,mouse))
     (pushnew ',name *palettes*)
     (when (eq ',name *current-palette*)
       (set-palette ',name))
     ',name))


(defun set-palette (palette)
  (interactive
   (list (completing-read
          "Palette: "
          (mapcar (lambda (pal) (cons (symbol-name pal) pal)) *palettes*)
          nil  t  nil nil *current-palette*)))
  (typecase palette
    (string (set-palette (intern palette)))
    (symbol (if (boundp palette)
              (let ((palval (symbol-value palette)))
                (if (and (palette-p palval) (eq palette (palette-name palval)))
                  (set-palette palval)
                  (error "%S is not a palette name." palette)))
              (error "%S is not a palette name." palette)))
    (palette
     (setf *current-palette* (palette-name palette))
     (when (fboundp 'set-default-frame-parameter)
       (set-default-frame-parameter 'foreground-color (palette-foreground palette))
       (set-default-frame-parameter 'background-color (palette-background palette))
       (set-default-frame-parameter 'cursor-color     (palette-cursor palette))
       (set-default-frame-parameter 'mouse-color      (palette-mouse palette)))
     (set-face-background 'region (palette-region palette))
     
     (set-face-background 'mode-line (palette-foreground palette))
     (set-face-foreground 'mode-line (palette-background palette))
     (let ((box  (list :line-width -1
                       :color (palette-foreground palette)
                       :style 'released-button)))
       (set-face-attribute  'mode-line (selected-frame) :box box)
       (set-face-attribute  'mode-line t                :box box))
     (set-face-background 'mode-line-inactive (palette-background palette))
     (set-face-foreground 'mode-line-inactive (palette-foreground palette))
     (let ((box (list :line-width -1
                      :color (palette-foreground palette)
                      :style 'released-button)))
       (set-face-attribute  'mode-line-inactive (selected-frame) :box box)
       (set-face-attribute  'mode-line-inactive t                :box box))
     
     (when (getenv "EMACS_WM")
       (set-face-background 'border (palette-background palette)))
     (set-foreground-color (palette-foreground palette))
     (set-background-color (palette-background palette))
     (set-face-background 'fringe (palette-background palette))
     (set-cursor-color     (palette-cursor palette))
     (when (fboundp 'set-mouse-color)
       (set-mouse-color     (palette-mouse palette))))
    (otherwise (error "%S is not a palette" palette))))


(defparameter *turquoise*      "#1abc9c")
(defparameter *green-sea*      "#16a085")

(defparameter *emerland*       "#2ecc71")
(defparameter *nephritis*      "#27ae60")

(defparameter *peter-river*    "#3498db")
(defparameter *belize-hole*    "#2980b9")

(defparameter *amethyst*       "#9b59b6")
(defparameter *wisteria*       "#8e44ad")

(defparameter *wet-asphalt*    "#34495e")
(defparameter *midnight-blue*  "#2c3e50")

(defparameter *sun-flower*     "#f1c40f")
(defparameter *orange*         "#f39c12")

(defparameter *carrot*         "#e67e22")
(defparameter *pumpkin*        "#d35400")

(defparameter *alizarin*       "#e74c3c")
(defparameter *pomegranate*    "#c0392b")

(defparameter *clouds*         "#ecf0f1")
(defparameter *silver*         "#bdc3c7")

(defparameter *concrete*       "#95a5a6")
(defparameter *asbestos*       "#7f8c8d")

;; (apply 'format "#%02x%02x%02x" (mapcar (lambda (x) (* 0.199219 x)) '( 42 203 243)))

;;          name              foreground     background      cursor   region           mouse
(defpalette pal-tg            "Black"        *turquoise*     "Red"     *green-sea*     "#444444")
(defpalette pal-en            "Black"        *emerland*      "Red"     *nephritis*     "#444444")
(defpalette pal-pb            "Black"        *peter-river*   "Red"     *belize-hole*   "#444444")
(defpalette pal-aw            "Black"        *amethyst*      "Red"     *wisteria*      "#444444")
(defpalette pal-wm            "Black"        *wet-asphalt*   "Red"     *midnight-blue* "#444444")
(defpalette pal-so            "Black"        *sun-flower*    "Red"     *orange*        "#444444")
(defpalette pal-cp            "Black"        *carrot*        "Red"     *pumpkin*       "#444444")
(defpalette pal-ap            "Black"        *alizarin*      "Red"     *pomegranate*   "#444444")
(defpalette pal-cs            "Black"        *clouds*        "Red"     *silver*        "#444444")
(defpalette pal-ca            "Black"        *concrete*      "Red"     *asbestos*      "#444444")

(defpalette pal-default       "White"        "Black"         "Red"     "blue3"         "#444444")
(defpalette pal-white         "#000000"      "#ffffff"       "#555555" "#aaaaaa"       "#444444")
(defpalette pal-whiteish      "gray20"       "gray90"       "gray30" "gray70"       "#444444")
(defpalette pal-ltgray        "#000000"      "#aaaaaa"       "#ffffff" "#555555"       "#444444")
(defpalette pal-dkgray        "#ffffff"      "#555555"       "#000000" "#aaaaaa"       "#444444")
(defpalette pal-black         "#ffffff"      "#000000"       "#aaaaaa" "#555555"       "#444444")
(defpalette pal-lukhas        "#fff8dc"      "#537182"       "Red"     "#ddd"          "#444444")
(defpalette pal-thalassa      "MidnightBlue" "#e0f8ff"       "Pink4"   "orchid1"       "#444444")
(defpalette pal-larissa       "DarkOrchid4"  "#f8e8ff"       "Pink4"   "orchid1"       "#444444")
(defpalette pal-lassell       "green yellow" "#08350F"       "yellow"  "#0f0835"       "#444444")
(defpalette pal-triton        "#929982"      "#2d4e4e"       "cyan"    "#336666"       "#444444")
(defpalette pal-naiad         "MidnightBlue" "DarkSeaGreen1" "Pink3"   "orchid1"       "#444444")
(defpalette pal-galatea       "#3080ff"      "#030828"       "Pink4"   "orchid1"       "#444444")
(defpalette pal-galatea-light "#60c0ff"      "#030828"       "Pink4"   "orchid1"       "#444444")
(defpalette pal-green         "green"        "black"         "yellow"  "grey50"        "magenta")
(defpalette pal-dark          "White"        "#055045"       "yellow"  "grey40"        "#444444")
(defpalette pal-dark-cyan     "#11eef2"      "black"         "yellow"  "grey80"        "#444444")
(defpalette pal-dark-blue     "#1199f2"      "black"         "yellow"  "grey80"        "#444444")
(defpalette pal-dark-amber    "#e0d010"      "black"         "cyan"    "grey40"        "#444444")
(defpalette pal-dark-galatea  "#60f0c0"      "#0c2040"       "green"   "gray60"        "#444444")
(defpalette pal-irc           "MidnightBlue" "light yellow"  "blue"    "light green"   "#444444")
(defpalette pal-stripe        "#a7feff"      "#0a171b"       "Cyan"    "#082830"       "#446688")
(defpalette pal-stripe1       "#a7feff"      "#0a171b"       "Cyan"    "#105060"       "#446688")
(defpalette pal-anevia        "white"        "#081040"       "green"   "cadetblue4"    "yellow")
(defpalette pal-blueprint     "white"        "#392b8d"       "yellow"  "cadetblue4"    "yellow")
(defpalette pal-blueprint2    "white"        "#06104d"       "yellow"  "cadetblue4"    "yellow")
(defpalette pal-blueprint3    "white"        "#080635"       "yellow"  "cadetblue4"    "yellow")
(defpalette pal-blueprint4    "white"        "#010635"       "yellow"  "cadetblue4"    "yellow")

(set-palette  pal-default)


;; ----------------------------------------
(.EMACS "set-default-frame-alist")
(defun set-default-frame-alist (&optional font)
  "Sets default-frame-alist depending on the current environment (host, display, etc)."
  (interactive)
  (let* (
         ;; ---------------------
         (display  (let* ((display (getenv "DISPLAY"))
                          (colon   (and display (string-match ":" display))))
                     (if (or (not display) (zerop colon))
                       system-name
                       (substring display 0 colon))))
         ;; --- default values ---
         (font                 (or font (frame-font)))
         (width                (frame-width))
         (height               (frame-height))
         (top                  1)
         (left                 1)
         (cursor-type            'box)
         (horizontal-scroll-bars 'nil)
         (vertical-scroll-bars   'nil) ; or left or right
         (palette              pal-default)
         (hname                (subseq *hostname* 0 (position (character ".") *hostname*)))
         ;; (name (format "emacs: %s@%s" (user-real-login-name) host-name))
         (name "EMACS")
         ;; ---------------------
         (fringe-background nil))

    (setf default-cursor-type cursor-type)
    (string-case hname

                 (("thalassa" "despina" "kuiper")
                  (forward-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*")
                  (setq palette            pal-thalassa
                        width              81
                        height             70))

                 (("triton" "proteus")
                  (forward-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*")
                  (setq palette            pal-galatea
                        width              86
                        height             52))

                 (("galatea")
                  (forward-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*-*")
                  (setq palette            pal-blueprint3
                        width              81
                        height             54
                        font   (let ((fixed (make-font-pattern :foundry "Misc"
                                                               :family "Fixed"
                                                               :weight "Medium"
                                                               :slant "R"
                                                               :width "SemiCondensed"
                                                               :style ""
                                                               :pixel-size "13"
                                                               :point-size "120"
                                                               :resolution-x "75"
                                                               :resolution-y "75"
                                                               :spacing "C"
                                                               :average-width "60"
                                                               :registry "ISO8859"
                                                               :encoding "1")))
                                 (if (and (eq window-system 'x)
                                          (font-exists-p fixed))
                                   fixed
                                   font))))
                 
                 (("larissa") 
                  (setq palette            pal-larissa
                        Width              81
                        height             70))

                 (("naiad")
                  (setq palette            pal-naiad
                        width              81
                        height             54))

                 (("lassell")
                  (setq palette            pal-lassell
                        width              81
                        height             54))

                 (("mini")
                  (setq palette            pal-white
                        width              86
                        height             52))

                 (("mdi-development-1" "mdi-development-2")
                  (setf fringe-background "yellow"))

                 (("simias")
                  (setq palette            pal-anevia)))

    (if (getenv "EMACS_WM")
      (progn
        (setq
         width    140
         height   58
         top      2
         left     2
         font     (make-font-pattern :foundry "Adobe"
                                     :family "Courier"
                                     :weight "Medium"
                                     :slant "R"
                                     :width "Normal"
                                     :style ""
                                     :pixel-size "12"
                                     :point-size "120"
                                     :resolution-x "75"
                                     :resolution-y "75"
                                     :spacing "M"
                                     :average-width "70"
                                     :registry "ISO8859"
                                     :encoding "1"))
        (set-face-background 'border (palette-background palette))
        (shell-command (format "xsetroot -solid %s" (palette-background palette))))
      (setq initial-frame-alist  `((left  . -64))))

    (when (getenv "EMACS_OLD")
      (setq palette            pal-green)
      (setq font               (make-font-pattern :foundry "Adobe"
                                                  :family "Courier"
                                                  :weight "Bold"
                                                  :slant "R"
                                                  :width "Normal"
                                                  :style ""
                                                  :pixel-size "12"
                                                  :point-size "120"
                                                  :resolution-x "75"
                                                  :resolution-y "75"
                                                  :spacing "M"
                                                  :average-width "70"
                                                  :registry "ISO8859")
            background-color "black"
            foreground-color "green"
            region-color     "navyblue"
            cursor-color     "yellow"))

    (when (getenv "EMACS_BG")
      (setq palette (copy-palette palette))
      (setf (palette-background palette) (getenv "EMACS_BG")))

    (when (zerop (user-uid))
      (setq palette (copy-palette palette))
      (setf (palette-foreground palette) "Red"))

    (when (fboundp 'max-frame-line-number)
      (setf height (- (max-frame-line-number (car (frame-list))) 2)))

    (setq default-frame-alist
          `(
            (tool-bar-lines       . 0)
            (menu-bar-lines       . 0) ;; window-system 'mac
            (font                 . ,font)
            ,@(unless (getenv "RATPOISON")
                `((width                . ,width)
                  (height               . ,height)
                  (top                  . ,top)
                  (left                 . ,left)))
            (cursor-type          . ,cursor-type)
            (cursor-color         . ,(palette-cursor palette))
            (mouse-color          . ,(palette-mouse palette))
            (foreground-color     . ,(palette-foreground palette))
            (background-color     . ,(palette-background palette))
            (vertical-scroll-bars . ,vertical-scroll-bars)
            (name                 . ,name)))

    (when (and (string= "21.3.1" emacs-version)
               (not (getenv "EMACS_WM"))
               (not (getenv "RATPOISON")))
      (set-frame-position (car (frame-list)) -64 top)
      (set-frame-size     (car (frame-list)) width height)
      (setq frame-initial-frame nil))

    (set-face-background 'region (palette-region palette))
    (set-palette palette)
    (unless (fboundp 'mdi)
      (when (facep 'fringe)
        (if fringe-background
          (set-face-background 'fringe fringe-background)
          (set-face-background 'fringe (palette-background palette)))))
    (set-frame-name name)
    (when (zerop (user-uid))
      (set-foreground-color "Red"))))


;; (set-default-frame-alist *default-font*)
;; (.EMACS "set-default-frame-alist done")

;;;; THE END ;;;;
