(defun analyser-performance (arme score)
  (format t "--- Analyse du Round ---~%")

  ;; 1. Utilisation de CASE (pour des valeurs fixes)
  (case arme
    (:epee   (format t "Style : Combat rapproché.~%"))
    (:arc    (format t "Style : Combat à distance.~%"))
    (:magie  (format t "Style : Sortilèges de zone.~%"))
    (otherwise (format t "Style : Inconnu.~%")))

  ;; 2. Utilisation de COND (pour des conditions logiques/intervalles)
  (cond
    ((>= score 90) (format t "Résultat : Rang S (Légendaire !)~%"))
    ((>= score 70) (format t "Résultat : Rang A (Excellent)~%"))
    ((>= score 50) (format t "Résultat : Rang B (Pas mal)~%"))
    (t             (format t "Résultat : Rang C (Peut mieux faire)~%"))))

;; --- Tests du code ---
(analyser-performance :epee 95)
(format t "~%")
(analyser-performance :arc 40)