(defun get-prop (mv prop)
  (get mv prop)
)

(defun set-prop (mv prop val)
  (setf (get mv prop) val)
)
; TODO : gestion des étiquettes
(defun get-mem (vm cible) ;; 1. Parenthèses autour des arguments
  (cond 
    ((integerp cible) 
      (aref (get-prop vm :mem) cible)) 
    (t 
     (get-prop vm cible))
  )
)

(defun set-mem (vm cible valeur) ; variable_cible = valeur
  (cond 
    ((integerp cible) 
     (setf (aref (get-prop vm :mem) cible) valeur)) 
    (t 
     (set-prop vm cible valeur))))

(defun vm_jmp_cond_helper (vm check_flt check_feq check_fgt label)
  (let ((flt (get-mem vm :FLT))
        (feq (get-mem vm :FEQ))
        (fgt (get-mem vm :FGT)))
    (cond 
      ((or 
         (and (= check_flt 1) (= flt 1))
         (and (= check_feq 1) (= feq 1))
         (and (= check_fgt 1) (= fgt 1))
       ) 
        (vm_exec_inst_JMP vm label))
    ))
)

; Prend une valeur ou un registre, retourne la valeur (:CONST 4 -> 4, :REF R4 -> valeur stockée dans le registre R4)
(defun read_value (vm val)
  (cond
    ((integerp val) (get-mem vm val)) ; Adresse directe
    ((symbolp val) (get-prop vm val)) ; Registre
    ((listp val)
     (case (first val)
       (:CONST (second val))
       ;; Pour :REF et +, on délègue à resolve_addr pour trouver l'adresse, puis on lit
       (t (get-mem vm (resolve_addr vm val)))))
    (t (error "Val invalide : ~S" val))))

(defun afficher_registres (vm)
  (format t "~%=== État de la VM : ~S ===~%" vm)
  
  ;; Registres Généraux
  (format t " [Registres] R0: ~4D | R1: ~4D | R2: ~4D | R3: ~4D~%"
          (get-prop vm :R0)
          (get-prop vm :R1)
          (get-prop vm :R2)
          (get-prop vm :R3))
  
  ;; Pile et Pointeurs
  (format t " [Piles/PC]  PC: ~4D | SP: ~4D | BP: ~4D | FP: ~4D | LC: ~4D~%"
          (get-prop vm :PC)
          (get-prop vm :SP)
          (get-prop vm :BP)
          (get-prop vm :FP)
          (get-prop vm :LC))
  
  ;; Comparaisons (Flags)
  (format t " [Drapeaux]  FLT:   ~A | FEQ:   ~A | FGT: ~A~%"
          (get-prop vm :FLT)
          (get-prop vm :FEQ)
          (get-prop vm :FGT))
          
  (format t "=============================~%")
  (values)) ;; Retourne rien de spécifique pour ne pas polluer l'affichage