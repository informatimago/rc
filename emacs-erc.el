(require 'erc)
(require 'erc-join)
(require 'cl)
(require 'pjb-cl)
(require 'tls)

(defparameter *pjb-autojoin-channels-alist*
  '(("libera.chat"

     "#lisp"
     "#clschool"
     "#abcl" "#ccl" "#ecl" "#sicl" "#sbcl"
     "#clim" "#slime"  "#nyxt"
     "#cl-naive"
     "#clergo"
     
     "#emacs" "#org-mode" "#erc"

     "#hn" "#space" "#tesla" "#teslamotors" 
     )))

(setf erc-autojoin-channels-alist *pjb-autojoin-channels-alist*)

(defun pjb-join-channels ()
  (interactive)
  ;; (when (buffer-named "irc.freenode.org:6667")
  ;;   (with-current-buffer "irc.freenode.org:6667"
  ;;     (mapcar 'erc-join-channel
  ;;             (cdr (assoc "freenode.org" *pjb-autojoin-channels-alist*)))))
  (let ((irc (or (buffer-named "irc.libera.chat:6697")
                 (buffer-named "libera"))))
   (when irc
       (with-current-buffer irc
         (mapcar 'erc-join-channel
                 (cdr (assoc "libera.chat" *pjb-autojoin-channels-alist*)))))))


(defvar *erc-cmd-BR-line* (make-string 72 ?=))

(defun erc-cmd-BR (&optional filler)
  "Inserts a line."
  (let ((inhibit-read-only t)
        (line (cond
                ((characterp filler) (make-string 72 filler))
                ((stringp filler)    (with-output-to-string
                                         (loop repeat (ceiling 72 (length filler))
                                               do (write-string filler))))
                (t *erc-cmd-BR-line*))))
    (erc-display-line (erc-make-notice line) 'active))
  t)

(put 'erc-cmd-BR 'process-not-needed t)

(defun pjb-erc-clear-all-buffers  ()
  (interactive)
  (dolist (buf (erc-buffer-list))
    (with-current-buffer buf
      (erc-cmd-CLEAR)
      (bury-buffer))))


(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697
           :nick "tuck" :full-name "T.A.L."))

(defun start-irc-ogamita ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697
           :nick "ogamita" :full-name "Pascal J. Bourguignon"))

(defun br-all ()
  (interactive)
  (dolist (buffer (erc-buffer-list))
    (with-current-buffer buffer
      (erc-set-active-buffer buffer)
      (erc-cmd-BR))))

;; Local Variables:
;; coding: utf-8
;; eval: (flycheck-mode -1)
;; End:
