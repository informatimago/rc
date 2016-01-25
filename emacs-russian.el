;;;; -*- mode:emacs-lisp;coding:utf-8 -*-

;; This doesn't work exactly like I want.

;;; http://stackoverflow.com/questions/10639429/emacs-linux-and-international-keyboard-layouts

;; USAGE:
;; Put in your .emacs:
;; 
;; (translate-keystrokes-ru->en)
;; (add-hook 'text-mode-hook
;;           (lambda () (literal-insert-mode 1)))
;; 
;; Only buffers with literal-insert-mode active will be sensitive to the
;; environment language. Prefixed keybindings will still be usable.

(defun translate-keystrokes-ru->en ()
  "Make emacs output english characters, regardless whether
the OS keyboard is english or russian"
  (flet ((make-key-stroke (prefix char)
           (eval `(kbd ,(if (and (string-match "^C-" prefix)
                                 (string-match "[A-Z]" (string char)))
                            (concat "S-" prefix (string (downcase char)))
                            (concat prefix (string char)))))))
    (let ((case-fold-search nil)
          (keys-pairs (mapcar* 'cons
                               "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
                               "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
          (prefixes '("C-" "M-" "s-" "H-" "C-M-" "C-S-" "C-s-" "C-H-"
                      "M-S-" "M-s-" "M-H-" "S-s-" "S-H-" "s-H-"
                      "C-M-S-" "C-M-s-" "C-M-H-" "C-S-s-" "C-S-H-"
                      "C-s-H-" "M-S-s-" "M-S-H-" "M-s-H-" "S-s-H-")))
      (combinations)
      (mapc (lambda (prefix)
              (mapc (lambda (pair)
                      (define-key key-translation-map
                          (make-key-stroke prefix (car pair))
                        (make-key-stroke prefix (cdr pair))))
                    keys-pairs))
            prefixes))))

(defun literal-insert ()
  (interactive)
  (insert-char last-input-event 1))

(define-minor-mode literal-insert-mode
    "Make emacs output characters corresponging to the OS keyboard,
 ignoring the key-translation-map"
  :keymap (let ((new-map (make-sparse-keymap))
                (english-chars "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
            (mapc (lambda (char)
                    (define-key new-map (string char)
                      'literal-insert))
                  english-chars)
            new-map))

(define-minor-mode literal-insert-mode
    "Make emacs output characters corresponging to the OS keyboard,
 ignoring the key-translation-map"
  :keymap (let ((new-map (make-sparse-keymap))
                (english-chars "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
            (mapc (lambda (char)
                    (define-key new-map (string char)
                      'literal-insert))
                  english-chars)
            new-map))


;; (translate-keystrokes-ru->en)
;; (literal-insert-mode 1)

;;(add-hook 'text-mode-hook (lambda () (literal-insert-mode 1)))

