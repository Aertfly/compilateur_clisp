(require "utils/utils.lisp")

;Structure Mémoire : avec le code=Array et la pile=Array et stackpointer basepointer ? 
(defun make_vm (name size)
    (set-prop name :name name)

    ; Registre
    (set-prop name :R0 0)
    (set-prop name :R1 0)
    (set-prop name :R2 0)
    (set-prop name :R3 0)
    
    ; Pile
    (set-prop name :BP 100)
    (set-prop name :SP 100)
    (set-prop name :FP 0)
    
    ; Comparaisons flag
    (set-prop name :FLT 0); booléen plus petit que 
    (set-prop name :FEQ 0); booléen égal
    (set-prop name :FGT 0); booléan plus grand que

    (set-prop name :PC 0); Program counter, compteur ordinal
    (set-prop name :LC 0); Load counter, ????

    ;etiq Table de hashage pour les étiquettes
    ;etiqNr Table non résolue des étiquettes

    (set-prop name :mem (make-array size :initial-element 0)); Mémoire avec code + pile

    'ok ;Retourne la string "ok"
)

; 
(defun vm_exec (vm) )
; 
(defun vm_load_exec (vm) ) ; charge et exécute du code
; 

;;;;;

(defun vm_get_registre (vm) )
(defun vm_set_registre (vm) )

; Manipulation mémoire/registres
(defun vm_exec_inst_LOAD (vm src dest) 
  ;(set-mem vm dest (read_value vm src))
  (let (addr (
    (cond
      ((integerp src)
        (src)
      )
      ((listp src)
        (case (first src)
          (:CONST (error "Pas de constante acceptée par load"))             ; erreur
          (:REF   (get-mem vm (second src))) ; est un registre on recup sa valeur, on ira chercher à cette adresse
          (+    
            (let ((val-registre (get-mem vm (second src))) 
                      (offset (third src)))                     
                  (+ val-registre offset)))                 
          ) 
          (t (error "Type d'opérande inconnu : ~S" src))
        ))
        (t (error "Argument invalide (ni entier ni liste) : ~S")
      )
    )
  ))
  (set-mem vm dest (read_value vm sr)) 
)

(defun vm_exec_inst_STORE (src dest) )

(defun vm_exec_inst_MOVE (vm src dest) 
  (set-mem vm dest (read_value vm sr))
)

(defun vm_exec_inst_ADD (vm src dest) ; DEST += SRC
  (set-mem vm dest (+ (read_value vm src) (read_value vm dest)))
)

(defun vm_exec_inst_SUB (vm src dest) ; DEST -= SRC
  (set-mem vm dest (+ (read_value vm src) (read_value vm dest)))
)
(defun vm_exec_inst_MUL (vm src dest) ; DEST *= SRC
  (set-mem vm dest (* (read_value vm src) (read_value vm dest)))
)

(defun vm_exec_inst_DIV (vm src dest) ; DEST /= SRC
  (set-mem vm dest (/ (read_value vm dest) (read_value vm src)))
)

(defun vm_exec_inst_INCR (vm dest) ; DEST += 1
  (set-mem vm dest (+ (read_value vm dest) 1))
)

(defun vm_exec_inst_DECR (vm dest) ; DEST -= 1
  (set-mem vm dest (- (read_value vm dest) 1))
)

; Pile
(defun vm_exec_inst_PUSH (vm src) ;push src sur la pile et incrémente SP
  (vm_exec_inst_incr(vm :REF SP)) ; incrémente SP
  (vm_exec_inst_move(vm src :REF SP))
); On incrémente puis on met la valeur

(defun vm_exec_inst_POP (vm dest) ;pop est met dans dest la valeur au sommet de la pile et décrémente SP
  (vm_exec_inst_move(vm :REF SP dest))
  (vm_exec_inst_decr(vm :REF SP)) ; décrémente SP
); On enlève la valeur puis on décrémente

; Labels/sauts 
(defun vm_exec_inst_LABEL (vm label) )
(defun vm_exec_inst_JMP   (vm label) )

; Appels et retours
(defun vm_exec_inst_JSR (vm label) ) ; met l'adresse de retour dans la pile et fait un jump
(defun vm_exec_inst_RTN (vm) )

; Comparaisons
(defun vm_exec_inst_CMP (vm src1 src2)
; alloue respectivement les registres FLT, FEQ, FGT à 1 pour celui qui est vrai, 0 pour les autres. Par ex si src1 > src2, on a 001
  (let 
    ((val1 (get-mem vm src1)) 
    (val2 (get-mem vm src2)))

    (set-mem vm :FLT 0)
    (set-mem vm :FEQ 0)
    (set-mem vm :FGT 0)

    (cond
      ((< val1 val2) (set-mem vm :FLT 1))
      ((= val1 val2) (set-mem vm :FEQ 1))
      ((> val1 val2) (set-mem vm :FGT 1)))
  )
)

(defun vm_exec_inst_JGT (vm label)
  (vm_jmp_cond_helper vm 0 0 1 label)
)

(defun vm_exec_inst_JGE (vm label) 
  (vm_jmp_cond_helper vm 0 1 1 label)
)

(defun vm_exec_inst_JLT (vm label) 
  (vm_jmp_cond_helper vm 1 0 0 label)
)

(defun vm_exec_inst_JLE (vm label) 
  (vm_jmp_cond_helper vm 1 1 0 label)
)

(defun vm_exec_inst_JEQ (vm label) 
  (vm_jmp_cond_helper vm 0 1 0 label)
)

(defun vm_exec_inst_JNE (vm label) 
  (vm_jmp_cond_helper vm 1 0 1 label)
)

; Tests logiques
(defun vm_exec_inst_TEST  (vm src) )
(defun vm_exec_inst_JTRUE (vm label) )
(defun vm_exec_inst_JNIL  (vm label) )

; Contrôle neutre
(defun vm_exec_inst_NOP (vm) )
(defun vm_exec_inst_HALT (vm) )
