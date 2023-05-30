;;; emacs-sensors.el -- Adds the CPU temperature to the mode-line
;;; -*- mode:emacs-lisp;lexical-binding:t;coding:utf-8 -*-
;;; Commentary: Written with the help of copilot.
;;; Code:


;; sensors|sed -n -e 's/.*: *\([-+][0-9]\+\.[0-9]\+.C\).*/\1/p'|sort -rn|head -1
;; let's translate that to elisp

(defun instantaneous-cpu-temperature ()
  (let ((output (shell-command-to-string "sensors")))
    (let ((lines (split-string output "\n" t))
          (max 0.0))
      (dolist (line lines)
        (when (string-match ".*: *\\([-+][0-9]+\\.[0-9]+.C\\).*" line)
          (let ((temp (string-to-number (match-string 1 line))))
            (when (> temp max)
              (setq max temp)))))
      max)))

;; let's cache the temperature for 10 seconds

(defvar *temperature-cache* (cons 0.0 0))

(defun current-cpu-temperature ()
  (let ((now (float-time)))
    (if (> (- now (cdr *temperature-cache*)) 10)
        (let ((temp (instantaneous-cpu-temperature)))
          (setcdr *temperature-cache* now)
          (setcar *temperature-cache* temp)))
    (car *temperature-cache*)))

(defun install-current-temperature-to-mode-line ()
  (interactive)
  (setq-default mode-line-format
                (append mode-line-format
                        '((:eval (let ((temperature  (current-cpu-temperature)))
                                   (cond
                                     ((< temperature 60.0)
                                      (format " %s°C"))
                                     ((< temperature 70.0)
                                      ;; display it in yellow
                                      (propertize (format " %s°C" temperature)
                                                  'face '(:foreground "yellow")))
                                     (t
                                      ;; display it in red
                                      (propertize (format " %s°C" temperature)
                                                  'face '(:foreground "red"))))))))))


(install-current-temperature-to-mode-line)
