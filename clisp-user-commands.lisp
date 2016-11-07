;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               clisp-user-commands.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    Utilities to help creating clisp REPL user commands.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-10-18 <PJB> Extracted from clisprc.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;  
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
(defpackage "COM.INFORMATIMAGO.CLISP.USER-COMMANDS"
  (:use "COMMON-LISP" "SPLIT-SEQUENCE")
  (:export "DEFINE-USER-COMMAND" "DELETE-USER-COMMAND"))
(in-package "COM.INFORMATIMAGO.CLISP.USER-COMMANDS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *user-command-groups* (make-hash-table :test (function equal)))
  (defun ensure-list (item) (if (listp item) item (list item)))
  (defstruct command names lambda-list docstring body))

;;(setf custom:*user-commands* nil)


(defun read-arguments (stream &key (prompt "> ") (prompt-function nil))
  "
DO:     Reads from the stream a line and parses the arguments
        separated by spaces.  Argument characters may be quoted
        with double-quotes, single-quotes or backspaces, and may
        stand on several lines.
RETURN: A list of string, the parsed arguments.
"
  (loop
     :with prompt-function = (or prompt-function
                                 (lambda (state)
                                   (case state
                                     ((:escape-out)
                                      (format stream "~&\\~A" prompt))
                                     ((:escape-in-double :string-double)
                                      (format stream "~&\"~A" prompt))
                                     ((:escape-in-single :string-single)
                                      (format stream "~&'~A"  prompt))
                                     (otherwise
                                      (format stream "~&~A"   prompt)))))
     :with line = "\\"
     :with state = :initial
     :with arguments = '()
     :with buffer = nil
     :with i = 0
     ;; :do (print `(:line ,line :i ,i :state ,state
     ;;                                :arguments ,arguments
     ;;                                :buffer ,buffer))
     :do (if (< i (length line))
             (let ((ch (prog1 (aref line i) (incf i))))
               (case state
                 (:out
                  (case ch
                    ((#\")     (setf state :string-double))
                    ((#\')     (setf state :string-single))
                    ((#\\)     (setf state :escape-out))
                    ((#\space) (when buffer
                                 (push (coerce (nreverse buffer) 'string)
                                       arguments)
                                 (setf buffer nil)))
                    (otherwise (if buffer
                                   (push ch buffer)
                                   (setf buffer (list ch))))))
                 (:string-double
                  (case ch
                    ((#\")     (setf state :out))
                    ((#\\)     (setf state :escape-in-double))
                    (otherwise (if buffer
                                   (push ch buffer)
                                   (setf buffer (list ch))))))
                 (:string-single
                  (case ch
                    ((#\')     (setf state :out))
                    ((#\\)     (setf state :escape-in-single))
                    (otherwise (if buffer
                                   (push ch buffer)
                                   (setf buffer (list ch))))))
                 (:escape-out
                  (if buffer
                      (push ch buffer)
                      (setf buffer (list ch)))
                  (setf state :out))
                 (:escape-in-double
                  (if buffer
                      (push ch buffer)
                      (setf buffer (list ch)))
                  (setf state :string-double))
                 (:escape-in-single
                  (if buffer
                      (push ch buffer)
                      (setf buffer (list ch)))
                  (setf state :string-single))))
             (progn
               (case state
                 (:out
                  (when buffer
                    (push (coerce (nreverse buffer) 'string)
                          arguments)
                    (setf buffer nil))
                  (return (nreverse arguments)))
                 ((:initial :escape-out)
                  (funcall prompt-function state)
                  (setf state :out))
                 ((:escape-in-double :escape-in-single)
                  (funcall prompt-function state)
                  (if buffer
                      (push #\newline buffer)
                      (setf buffer (list #\newline))))
                 (otherwise
                  (funcall prompt-function state)))
               (finish-output stream)
               (setf line (read-line stream)
                     i 0)))))


(defun parse-arguments (line)
  (with-input-from-string (inp line)
    (with-input-from-string (nl (string #\Newline))
      (read-arguments (make-two-way-stream (make-concatenated-stream inp nl *query-io*) *query-io*)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-user-commands ()
   "DO:   sets CUSTOM:*USER-COMMANDS* to the commands generated from *USER-COMMAND-GROUPS*."
   (setf custom:*user-commands*
         (let ((commands '()))
           (maphash
            (lambda (category command-list)
              (dolist (command  command-list)
                (push (coerce
                       (let ((vfun (gensym))
                             (varg (gensym)))
                         `(lambda ()
                            (flet ((,vfun (,varg)
                                     (handler-case
                                         (flet ((,(first (command-names command)) ,(command-lambda-list command)
                                                  ,@(command-body command)))
                                           (apply (function ,(first (command-names command)))
                                                  (parse-arguments ,varg)))
                                       (error (err) (format t "~&~A~%" err)))))
                              (list
                               ,(format nil "~%~:@(~{~S ~}~) ~A" (command-names command) (command-docstring command))
                               ,@(mapcar (lambda (name)
                                           `(cons ,(format nil "~(~S~)" name)
                                                  (function ,vfun)))
                                         (command-names command))))))
                       'function) commands))
              (push (coerce
                     `(lambda () (list ,(format nil "~2%~A:" category)))
                     'function) commands))
            *user-command-groups*)
           commands))
   (values)))


(defmacro define-user-command (names category command-lambda-list docstring &body body)
  "Defines a user command
NAMES:   a designator for a list of symbols by which the command can be invoked."
  (let ((names (ensure-list names)))
    `(let ((entry (find ',names  (gethash ',category *user-command-groups* '())
                        :test (function equal)
                        :key (function command-names))))
       (if entry
           (setf (command-lambda-list entry) ',command-lambda-list
                 (command-docstring   entry) ',docstring
                 (command-body        entry) ',body)
           (push (make-command :names ',names
                               :lambda-list ',command-lambda-list
                               :docstring ',docstring
                               :body ',body)
                 (gethash ',category *user-command-groups* '())))
       (generate-user-commands)
       ',(first names))))


(defun delete-user-command (name)
  "Delete the user command with the given NAME.
If the command was defined with several names, it's removed under ALL its names.
If the last command in a category is removed, then the category is removed."
  (maphash (lambda (category commands)
             (setf commands (delete name commands
                                    :test (function member)
                                    :key (function command-names)))
             (if commands
                 (setf (gethash category *user-command-groups*) commands)
                 (remhash category *user-command-groups*)))
            *user-command-groups*)
  (generate-user-commands))


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define-user-command (date) "interactive commands" () 
  "prints the date."
  (com.informatimago.common-lisp.interactive.interactive:date))

(define-user-command (uptime) "interactive commands" ()
  "prints the uptime."
  (com.informatimago.common-lisp.interactive.interactive:uptime))

(define-user-command (pwd) "interactive commands" ()
  "print the working directory."
  (format t "~&~a~%" (com.informatimago.common-lisp.interactive.interactive:pwd)))

(define-user-command (cd) "interactive commands" (directory)
  "change working directory."
  (com.informatimago.common-lisp.interactive.interactive:cd directory))

(define-user-command (ls) "interactive commands" (&rest options)
  "list files."
  (apply (function com.informatimago.common-lisp.interactive.interactive:ls) options))

(define-user-command (cat less more) "interactive commands" (&rest files)
  "catenate files."
  (apply (function com.informatimago.common-lisp.interactive.interactive:cat) files))






(define-user-command (panic :pa) "user-defined commands" ()
  "hit the panic button!"
   (format t "don't panic, ~d~%" (random 42)))




(defparameter *insults*
  '(("accapareur" (nm))
    ("aérolithe" (nm))
    ("amiral de bateau­lavoir" (gnm))
    ("amphitryon" (nm))
    ("anacoluthe" (nf))
    ("analphabète" (n a))
    ("anthracite" (nm))
    ("anthropophage" (nm a))
    ("anthropopithèque" (nm))
    ("apache" (nm))
    ("apprenti dictateur à la noix de coco" (gnm))
    ("arlequin" (nm))
    ("astronaute d'eau douce" (gn))
    ("athlète complet" (n))
    ("autocrate" (nm))
    ("autodidacte" (n a))
    ("azteque" (nm))
    ("babouin" (nm))
    ("bachi­bouzouk" (nm))
    ("bande de" (|...|))
    ("bandit" (nm))
    ("bayadère" (nf))
    ("bibendum" (nm))
    ("boit­sans­soif" (ni))
    ("brontosaure" (nm))
    ("bougre de" (|...|))
    ("brute" (nf))
    ("bulldozer à réaction" (gnm))
    ("vieux" (a))
    ("cachalot" (nm))
    ("canaille" (nf))
    ("canaque" (nm a))
    ("cannibale" (nm))
    ("carnaval" (nm))
    ("catachrèse" (nf))
    ("cataplasme" (nm))
    ("cercopithèque" (nm))
    ("chauffard" (nm))
    ("chenapan" (nm))
    ("choléra" (nm))
    ("chouette mal empaillée" (gnf))
    ("cloporte" (nm))
    ("clysopompe" (nm))
    ("coléoptère" (nm))
    ("coloquinte" (nf))
    ("coquin" (n a))
    ("cornemuse" (nf))
    ("cornichon" (nm))
    ("corsaire" (nm))
    ("coupe­jarret" (nm))
    ("cow­boy de la route" (gnm))
    ("crétin des alpes" (gnm))
    ("Cro­magnon" (np))
    ("cyanure" (nm))
    ("cyclone" (nm))
    ("cyclotron" (nm))
    ("Cyrano à quatre pattes" (gnm))
    ("diablesse" (nf))
    ("diplodocus" (nm))
    ("doryphore" (nm))
    ("dynamiteur" (nm))
    ("ecornifleur" (nm))
    ("ecraseur" (nm))
    ("ectoplasme" (nm))
    ("egoïste" (nm))
    ("emplatre" (nm))
    ("empoisonneur" (nm a))
    ("enragé" (nm a))
    ("épouvantail" (nm))
    ("équilibriste" (nm))
    ("esclavagiste" (nm))
    ("escogriffe" (nm))
    ("espèce de" (|...|))
    ("extrait de" (|...|))
    ("faux jeton" (nm))
    ("flibustier" (nm))
    ("forban" (nm))
    ("frères de la côte" (gnm))
    ("froussard" (nm a))
    ("galopin" (nm))
    ("gangster" (nm))
    ("garde­côte à la mie de pain" (gnm))
    ("gargarisme" (nm))
    ("garnement" (nm))
    ("gibier de potence" (nm))
    ("goujat" (nm))
    ("gredin" (nm))
    ("grenouille" (nf))
    ("gros plein de soupe" (gnm))
    ("gyroscope" (nm))
    ("hérétique" (n a))
    ("hors­la­loi" (nm))
    ("huluberlu" (nm))
    ("hydrocarbure" (nm))
    ("iconoclaste" (nm a))
    ("incas de carnaval" (gnmp))
    ("individou de général" (gnm))
    ("invertébré" (nm))
    ("ivrogne" (n))
    ("jet d'eau ambulant" (gnm))
    ("jocrisse" (nm))
    ("judas" (nm))
    ("jus de réglisse" (gnm))
    ("kroumir" (nm))
    ("ku klux klan" (gnm))
    ("lâche" (nm))
    ("lépidoptère" (nm))
    ("logarithme" (nm))
    ("loup­garou à la graisse de renoncule" (gnm))
    ("macaque" (nm))
    ("macrocéphale" (nm))
    ("malappris" (n a))
    ("malotru" (n))
    ("mamelouk" (nm))
    ("marchand de guano" (gnm))
    ("marchand de tapis" (gnm))
    ("marin d'eau douce" (gnm))
    ("marmotte" (nf))
    ("mégalomane" (nm a))
    ("mégacycle" (nm))
    ("mercanti" (nm))
    ("mercenaire" (nm a))
    ("mérinos" (nm))
    ("mille sabords" (gnmp))
    ("misérable" (a))
    ("mitrailleur à bavette" (gnm))
    ("moratorium" (nm))
    ("moricaud" (nm a))
    ("mouchard" (nm))
    ("moujik" (nm))
    ("moule à gaufres" (gnm))
    ("moussaillon" (nm))
    ("mrkrpxzkrmtfrz" (nm))
    ("mufle" (nm))
    ("Mussolini de carnaval" (nm))
    ("naufrageur" (nm))
    ("négrier" (nm))
    ("noix de coco" (gnm))
    ("nyctalope" (n a))
    ("olibrius" (nm))
    ("ophicléïde" (nm))
    ("ornithorynque" (nm))
    ("oryctérope" (nm))
    ("ostrogoth" (n a))
    ("ours mal lèché" (gnm))
    ("pacte à quatre" (gnm))
    ("paltoquet" (nm))
    ("pantoufle" (nf))
    ("Papous" (nm))
    ("paranoïaque" (nm a))
    ("parasite" (nm a))
    ("Patagon" (nm))
    ("patapouf" (nm))
    ("patate" (nf))
    ("péronnelle" (nf))
    ("perruche bavarde" (gnf))
    ("phénomène" (nm))
    ("phlébotome" (nm))
    ("phylactère" (nm))
    ("phylloxéra" (nm))
    ("pignouf" (nm))
    ("pirate" (nm))
    ("Polichinelle" (nm))
    ("polygraphe" (nm))
    ("porc­épic mal embouché" (gnm))
    ("potentat emplumé" (gnm))
    ("poussière" (nf))
    ("profiteur" (nm))
    ("projectile guidé" (gnm))
    ("protozoaire" (nm))
    ("pyromane" (nm))
    ("pyrophore" (nm))
    ("rapace" (nm))
    ("rat" (nm))
    ("Ravachol" (nm))
    ("renégat" (nm))
    ("rhizopode" (nm))
    ("Rocambole" (nm))
    ("sacripant" (nm))
    ("sajou" (nm))
    ("saltimbanque" (nm))
    ("sapajou" (nm))
    ("satané bazar de fourbi de truc" (gnm))
    ("satrape" (nm))
    ("sauvage" (n a))
    ("scélérat" (nm))
    ("schizophrène" (n a))
    ("scolopendre" (nf))
    ("scorpion" (nm))
    ("serpent" (nm))
    ("simili martien à la graisse de cabestan" (gnm))
    ("sinapisme" (nm))
    ("soulographe" (nm))
    ("squatter" (nm))
    ("tchouk­tchouk­nougat" (nm))
    ("technocrate" (nm))
    ("tête de lard" (gnf))
    ("tête de mule" (gnf))
    ("tigresse" (nf))
    ("tonnerre de Brest" (gnm))
    ("topinanbour" (nm))
    ("tortionnaire" (nm))
    ("traficant de chair humaine" (gnm))
    ("traine­potence" (nm))
    ("traitre" (nm a))
    ("troglodyte" (nm))
    ("trompe­la­mort" (nm))
    ("vampire" (nm))
    ("vandale" (nm a))
    ("va­nu­pieds" (nm))
    ("vaurien" (nm))
    ("végétarien" (nm))
    ("Vercingétorix de carnaval" (nm))
    ("ver de terre" (gnm))
    ("vermicelle" (nm))
    ("vermine" (nm))
    ("vipère" (nf))
    ("vivisectionniste" (nm))
    ("voleur" (nm))
    ("wisigoth" (n a))
    ("zapotèque" (nm))
    ("zèbre" (nm))
    ("zigomar" (nm))
    ("zouave" (nm))
    ("Zoulou" (nm))))


(defparameter nm (remove-if-not (lambda (x) (intersection '(n np nm gnm) (second x))) *insults*))
(defparameter nf (remove-if-not (lambda (x) (intersection '(nf np) (second x))) *insults*))
(defparameter ad (remove-if-not (lambda (x) (member 'a (second x))) *insults*))
(defparameter *insulte-random-state* (make-random-state t))

(defun insulte ()
  (let* ((*random-state*  *insulte-random-state*)
         (ga (format nil " ~A" (first (elt ad (random (length ad))))))
         (gn (let ((n (random (+ (length nf) (length nm)))))
               (if (>= n (length nm))
                 (prog1
                     (first (elt nf (- n (length nm))))
                   (cond
                    ((= 0 (length ga)))
                    ((string= "e" ga :start2 (1- (length ga))))
                    ((string= "eux" ga :start2 (- (length ga) 3))
                     (setf ga (concatenate 'string
                                (subseq ga 0 (- (length ga) 2)) "ille")))
                    ((string= "eur" ga :start2 (- (length ga) 3))
                     (setf ga (concatenate 'string
                                (subseq ga 0 (1- (length ga))) "se")))
                    (t 
                     (setf ga (concatenate 'string ga "e")))))
                 (first (elt nm n)))))
         (conj (if (position (aref gn 0) "aeiouyh") "d'" "de "))
         (ins  (case (random 4)
                 ((0)       (concatenate 'string "espèce " conj gn ga))
                 ((1)       (concatenate 'string "bande "  conj gn ga))
                 (otherwise (concatenate 'string gn ga)))))
    (concatenate 'string
      (string-capitalize (subseq ins 0 1))
      (subseq ins 1)
      " !")))

(define-user-command (swear :sw) "user-defined commands" ()
  "curse"
  (format t "~^~A~%" (insulte)))


;;;; THE END ;;;;
