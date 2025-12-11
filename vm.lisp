(defun get-prop (mv prop)
  (get mv prop)
)

(defun set-prop (mv prop val)
  (setf (get mv prop) val)
)

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
)

; 
(defun vm_exec (vm) )
; 
(defun vm_load_exec (vm) )
; 
(defun load (vm) )

;;;;;

(defun vm_get_registre (vm) )
(defun vm_set_registre (vm) )

; Manipulation mémoire/registres
(defun vm_exec_inst_LOAD (src dest) )
(defun vm_exec_inst_STORE (src dest) )

(defun vm_exec_inst_MOVE (vm src dest) 
   (set-prop dest (get-prop(src)))
)

(defun vm_exec_inst_ADD (vm src dest) )
(defun vm_exec_inst_SUB (vm src dest) )
(defun vm_exec_inst_MUL (vm src dest) )
(defun vm_exec_inst_DIV (vm src dest) )
(defun vm_exec_inst_INCR (vm dest) )
(defun vm_exec_inst_DECR (vm dest) )

; Pile
(defun vm_exec_inst_PUSH (vm src) )
(defun vm_exec_inst_POP (vm dest) )

; Labels/sauts 
(defun vm_exec_inst_LABEL (vm label) )
(defun vm_exec_inst_JMP   (vm label) )

; Appels et retours
(defun vm_exec_inst_JSR (vm label) )
(defun vm_exec_inst_RTN (vm) )

; Comparaisons
(defun vm_exec_inst_CMP (vm src1 src2) )
; alloue respectivement les registres FLT, FEQ, FGT à 1 pour celui qui est vrai, 0 pour les autres. Par ex si src1 > src2, on a 001
(
    (set-prop (vm FLT 0))
    (set-prop (vm FLT 0))
    (set-prop (vm FLT 0))

    if (src1 < src2) ( set-prop (vm FLT 1) ) 
    if (src1 = src2) ( set-prop (vm FEQ 1) ) 
    if (src1 > src2) ( set-prop (vm FGT 1) ) 
    
)
(defun vm_exec_inst_JGT (vm label) )
(defun vm_exec_inst_JGE (vm label) )

(defun vm_exec_inst_JLT (vm label) )
(defun vm_exec_inst_JLE (vm label) )

(defun vm_exec_inst_JEQ (vm label) )
(defun vm_exec_inst_JNE (vm label) )

; Tests logiques
(defun vm_exec_inst_TEST  (vm src) )
(defun vm_exec_inst_JTRUE (vm label) )
(defun vm_exec_inst_JNIL  (vm label) )

; Contrôle neutre
(defun vm_exec_inst_NOP (vm) )
(defun vm_exec_inst_HALT (vm) )




