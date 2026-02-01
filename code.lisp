;; 1. Récursivité Simple (Test de la pile de retour et de R0)
(defun FACTO (N)
  (if (<= N 1)
      1
      (* N (FACTO (- N 1)))))

;; 2. Récursivité Croisée (Test intensif de PC, FP et des sauts)
;; Détermine si un nombre est pair en sautant entre deux fonctions
(defun MON-EST-PAIR (N)
  (if (= N 0)
      1
      (MON-EST-IMPAIR (- N 1))))

(defun MON-EST-IMPAIR (N)
  (if (= N 0)
      0
      (MON-EST-PAIR (- N 1))))

;; 3. Test de Portée et Arithmétique (Test des offsets Arguments vs Locaux)
;; Cette fonction manipule des arguments profonds dans la pile et des variables LET
(defun CALCUL-COMBINE (X Y Z)
  (let ((BASE (+ X Y)))                       ;; Local 1 : BASE
    (let ((EST-PAIR (MON-EST-PAIR BASE)))     ;; Local 2 : Appel de fonction
      (let ((MODIF (* Z 2)))                  ;; Local 3 : MODIF
        (if (= EST-PAIR 1)
            (+ BASE MODIF)                    ;; Branche Vrai
            (- (* BASE 2) MODIF))))))         ;; Branche Faux

;; 4. Fonction Maîtresse (Stress Test Final)
;; Combine tout : conditions imbriquées, calculs et appels de fonctions
(defun STRESS-TOTAL (VALEUR)
  (let ((LIMITE 100))
    (let ((TEMP (CALCUL-COMBINE VALEUR 5 10))) ;; Appel avec mélange constantes/args
      (if (> TEMP LIMITE)
          (let ((REDUCTION (/ TEMP 2)))
            (+ REDUCTION (FACTO 3)))           
          (let ((AUGMENTATION (+ TEMP 50)))
            (- AUGMENTATION (FACTO 4)))))))    

;; --- POINT D'ENTRÉE DU TEST ---
;; Simulation de l'exécution pour (STRESS-TOTAL 10) :
;; 1. CALCUL-COMBINE(10, 5, 10) -> BASE = 15
;; 2. MON-EST-PAIR(15) -> 0 (Impair)
;; 3. MODIF = 10 * 2 = 20
;; 4. Branche Impair : (15 * 2) - 20 = 10 -> TEMP = 10
;; 5. STRESS-TOTAL : TEMP (10) < LIMITE (100) -> Branche ELSE
;; 6. AUGMENTATION = 10 + 50 = 60
;; 7. FACTO(4) = 24
;; 8. RÉSULTAT FINAL : 60 - 24 = 36
(STRESS-TOTAL 10)