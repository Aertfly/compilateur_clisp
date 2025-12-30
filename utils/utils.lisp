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

(defun vm_jmp_cond_helper (vm val_flt val_feq val_fgt label)
  (let ((flt (get-mem vm :FLT))
        (feq (get-mem vm :FEQ))
        (fgt (get-mem vm :FGT)))
    (cond 
      ((or (= flt val_flt) (= feq val_feq) (= fgt val_fgt)) 
        (vm_exec_inst_JMP vm label))
    ))
)

; Prend une valeur ou un registre, retourne la valeur (:CONST 4 -> 4, :REF R4 -> valeur stockée dans le registre R4)
(defun read_value (vm val)
  (cond
    ((integerp val)
      ;; 1. Si c'est un entier -> Adresse mémoire
     (get-mem vm val))

    ((symbolp val) (get-prop vm val))

    ((listp val)
      ;; 2. Si c'est une liste -> On regarde le premier élément
     (case (first val)
       (:CONST (second val))             ; Valeur brute
       (:REF   (get-mem vm (second val))) ; Soit un registre soit une étiquette
       (t (error "Type d'opérande inconnu : ~S" val))))

    (t (error "Argument invalide (ni entier ni liste) : ~S" val))))

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