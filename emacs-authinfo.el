;;;; emacs-authinfo.el -- generic secret lookup for ~/.apikeys and ~/.authinfo  -*- lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               emacs-authinfo.el
;;;;LANGUAGE:           emacs lisp
;;;;DESCRIPTION
;;;;
;;;;    Generic secret lookup for ~/.apikeys and ~/.authinfo, the emacs
;;;;    counterpart of bash/lib/authinfo.bash.
;;;;
;;;;    Both files are token streams of "key value" pairs.  A new *record*
;;;;    begins at a "lead" keyword:
;;;;
;;;;      ~/.authinfo : lead keyword "machine" (plus bare keyword "default")
;;;;                    netrc-style, one record per line:
;;;;                        machine HOST login LOGIN password SECRET port PORT
;;;;
;;;;      ~/.apikeys  : lead keyword "name", one-line OR multi-line records:
;;;;                        name binance label prod apikey K secret S password P
;;;;                    -- or --
;;;;                        name binance
;;;;                          label prod
;;;;                          apikey K
;;;;                          ...
;;;;
;;;;    Lines whose first non-blank character is `#' are comments.  Blank
;;;;    lines are ignored.
;;;;
;;;;    A record is selected by giving key/value pairs for any NON-SECRET
;;;;    field; the secret fields (apikey, secret, password) may never be
;;;;    used as a selection key.  When the selection is too lax and several
;;;;    records match, the FIRST one is returned and a warning is emitted.
;;;;
;;;;    Public API:
;;;;
;;;;      (get-apikey WHAT &rest PLIST)   WHAT in {apikey,secret,password}
;;;;        (get-apikey 'apikey :name "binance" :label "test") --> "some-api-key"
;;;;        (get-apikey 'secret :name "binance" :label "test") --> "some-secret"
;;;;
;;;;      (get-authinfo-password &rest PLIST)
;;;;        (get-authinfo-password :machine "fabrik.sncf.fr" :port "gitlab")
;;;;        (get-authinfo-password :machine "news.individual.net" :login "bourguignon")
;;;;
;;;;    Returns the secret string, or nil when no record matches.
;;;;**************************************************************************

(require 'cl-lib)

(defun authinfo--key->string (key)
  "Normalize KEY (keyword, symbol or string) to a plain field-name string."
  (cond ((keywordp key) (substring (symbol-name key) 1))
        ((symbolp  key) (symbol-name key))
        ((stringp  key) key)
        (t              (format "%s" key))))

(defun authinfo--plist->alist (plist)
  "Turn PLIST (:key val ...) into an alist of (FIELD-STRING . VALUE-STRING)."
  (let (alist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (push (cons (authinfo--key->string key) (format "%s" val)) alist)))
    (nreverse alist)))

(defun authinfo--tokens (file)
  "Return the flat list of tokens of FILE, skipping comment lines.
Both one-line and multi-line records collapse to the same token stream."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (let (tokens)
      (dolist (line (split-string (buffer-string) "\n"))
        (unless (string-match-p "\\`[ \t]*#" line)
          (setq tokens (nconc tokens (split-string line "[ \t\r]+" t)))))
      tokens)))

(defun authinfo--records (file leadkeys barekeys)
  "Parse FILE into a list of records, each an alist of (FIELD . VALUE) strings.
LEADKEYS start a record and consume a value; BAREKEYS start a record alone."
  (let ((tokens (authinfo--tokens file))
        records rec have)
    (while tokens
      (let ((tok (pop tokens)))
        (cond
         ((member tok barekeys)
          (when have (push (nreverse rec) records))
          (setq rec nil have t))
         ((member tok leadkeys)
          (when have (push (nreverse rec) records))
          (setq rec nil have t)
          (when tokens (push (cons tok (pop tokens)) rec)))
         (have
          (when tokens (push (cons tok (pop tokens)) rec))))))
    (when have (push (nreverse rec) records))
    (nreverse records)))

(defun authinfo--describe (selalist)
  "Human readable description of selection SELALIST, for warnings."
  (mapconcat (lambda (kv) (format "%s=%s" (car kv) (cdr kv))) selalist " "))

(defun authinfo--select (records selalist resultkey)
  "Return RESULTKEY of the first record in RECORDS matching SELALIST, or nil.
Warn when more than one record matches the (too lax) selection."
  (let ((matches
         (cl-remove-if-not
          (lambda (rec)
            (and (assoc resultkey rec)
                 (cl-every (lambda (kv)
                             (let ((cell (assoc (car kv) rec)))
                               (and cell (string= (cdr cell) (cdr kv)))))
                           selalist)))
          records)))
    (when (cdr matches)
      (message "authinfo: ambiguous query {%s} matched %d records; using the first"
               (authinfo--describe selalist) (length matches)))
    (when matches
      (cdr (assoc resultkey (car matches))))))

(defconst authinfo--apikeys-secrets '("apikey" "secret" "password")
  "Field names that hold secrets in ~/.apikeys and cannot be selection keys.")

;;;###autoload
(defun get-apikey (what &rest plist)
  "Return secret field WHAT of the record in ~/.apikeys selected by PLIST.
WHAT is one of the symbols `apikey', `secret' or `password'.  PLIST gives
non-secret selection keys, e.g. :name and :label.

  (get-apikey \\='apikey :name \"binance\" :label \"test\") --> \"some-api-key\"
  (get-apikey \\='secret :name \"binance\" :label \"test\") --> \"some-secret\""
  (let ((result (authinfo--key->string what))
        (selalist (authinfo--plist->alist plist)))
    (unless (member result authinfo--apikeys-secrets)
      (error "get-apikey: WHAT must be one of apikey/secret/password, got %S" what))
    (dolist (kv selalist)
      (when (member (car kv) authinfo--apikeys-secrets)
        (error "get-apikey: %s is a secret field and cannot be a selection key"
               (car kv))))
    (authinfo--select (authinfo--records (home ".apikeys") '("name") nil)
                      selalist result)))

;;;###autoload
(defun get-authinfo-password (&rest plist)
  "Return the password of the record in ~/.authinfo selected by PLIST.
PLIST gives non-secret selection keys, e.g. :machine, :port, :login.

  (get-authinfo-password :machine \"fabrik.sncf.fr\" :port \"gitlab\")
  (get-authinfo-password :machine \"news.individual.net\" :login \"bourguignon\")"
  (let ((selalist (authinfo--plist->alist plist)))
    (dolist (kv selalist)
      (when (string= (car kv) "password")
        (error "get-authinfo-password: password is a secret field and cannot be a selection key")))
    (authinfo--select (authinfo--records (home ".authinfo") '("machine") '("default"))
                      selalist "password")))

(provide 'emacs-authinfo)
;;;; THE END ;;;;
