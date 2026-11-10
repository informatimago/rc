(.EMACS "Google Maps")
(let ((gomapel  (get-directory :share-lisp  "packages/org/naquadah/google-maps/google-maps.el")))
  (when (and gomapel (file-exists-p gomapel))
    (push (get-directory :share-lisp  "packages/org/naquadah/google-maps/") load-path)
    (require 'google-maps)))
;; (google-maps-static-show :center "Valencia"
;;                          :maptype 'hybrid)
;; (google-maps-static-show
;;  :center "Cimetière du Montparnasse"
;;  :maptype 'hybrid
;;  ;; :zoom 5
;;  :markers '((("Place Saint-Michel, Paris") . (:label ?M :color "blue"))
;;             (("Jardin du Luxembourg, Paris" "Parc Montsouris, Paris") . (:label ?P :color "green")))
;;  :visible '("44 rue de l'Ouest, Paris" "Montrouge")
;;  :paths '((("Tour Eiffel, Paris" "Arc de triomphe, Paris" "Panthéon, Paris")
;;            . (:weight 3 :color "black" :fillcolor "yellow"))))


(let ((gowel (get-directory :share-lisp  "packages/org/naquadah/google-weather-el/google-weather.el")))
  (when (and gowel (file-exists-p gowel))
    (push (get-directory :share-lisp  "packages/org/naquadah/google-weather-el/") load-path)
    (require 'google-weather)
    (require 'org-google-weather)))

