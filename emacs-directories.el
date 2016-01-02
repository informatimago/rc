;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               emacs-directories.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Query the directory.txt database.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-20 <PJB> Extracted from emacs-common.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

;;;----------------------------------------------------------------------------
;;; File and directory stuff
;;;----------------------------------------------------------------------------

(defvar *directories* '() "A cache for the ~/directories.txt dictionary.")
;; (setf  *directories* '())


(defun load-directories (&optional directories-file)
  "Loads ~/directories.txt (or the given DIRECTORIES-FILE),
and stores it in `*directories*'.
"
  (let ((directories-file (or directories-file "~/directories.txt")))
    (setf *directories*
          (progn
            (find-file directories-file)
            (prog1
                (loop
                   for (k v)
                   on (split-string (buffer-substring-no-properties
                                     (point-min) (point-max)))
                   by (function cddr)
                   nconc (list (intern (format ":%s" (substitute ?- ?_ (downcase k))))
                               v))
              (kill-buffer (current-buffer)))))))


(defun get-directory (key &optional subpath)
  "
RETURN: The directory in ~/directories.txt for the key, concatenated with the subpath.
NOTE:   ~/directories.txt is cached in *directories*.
"
  (unless *directories*
    (load-directories))
  (when  (getf *directories* key)
    (let ((dir (getf *directories* key)))
      (if (or (null subpath) (string= "" subpath))
          dir
          (flet ((lastchar (str) (and (< 0 (length str)) (aref str (1- (length str)))))
                 (firstchar (str) (and (< 0 (length str)) (aref str 0)))
                 (xor (a b) (or (and a (not b)) (and (not a) b))))
            (if (xor (eql ?/ (lastchar dir)) (eql ?/ (firstchar subpath)))
                (concat dir subpath)
                (concat dir "/" subpath)))))))


;;;; THE END ;;;;
