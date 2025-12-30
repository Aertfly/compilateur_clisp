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

(defun vm_exec (vm) ; exécution de la VM : boucle principale
  (loop
    (let* ((pc (get-prop vm :PC))
          (inst (get-mem vm pc))) ; On récupère la valeur du compteur ordinal, puis on stocke dans inst l'instruction à l'emplacement mémoire correspondant
      (format t "Instruction en ~D : ~S~%" pc inst) ; Affichage de l'instruction

      ; Arrêt de l'exécution
      (if 
        (or 
          (null inst) ; cas où pas d'instruction
          (eq (first inst) 'HALT)) ; cas où instruction HALT
        (progn 
          (format t "Fin execution~%")
          (return 'DONE)))

      (set-prop vm :PC (+ pc 1)) ; Incrémentation du compteur ordinal

      (let ((op (first inst)) ; op contient l'opérateur
            (args (rest inst))) ; args contient les arguments
        (case op ; ordre des instructions correspondant à celui de LEC.pdf
          (LOAD (vm_exec_inst_LOAD vm (first args) (second args)))
          (STORE (vm_exec_inst_STORE vm (first args) (second args)))
          ;
          (MOVE (vm_exec_inst_MOVE vm (first args) (second args)))
          ;
          (ADD (vm_exec_inst_ADD vm (first args) (second args)))
          (SUB (vm_exec_inst_SUB vm (first args) (second args)))
          (MUL (vm_exec_inst_MUL vm (first args) (second args)))
          (DIV (vm_exec_inst_DIV vm (first args) (second args)))
          (INCR (vm_exec_inst_INCR vm (first args)))
          (DECR (vm_exec_inst_DECR vm (first args)))
          ;
          (PUSH (vm_exec_inst_PUSH vm (first args)))
          (POP (vm_exec_inst_POP vm (first args)))
          ;
          (LABEL (vm_exec_inst_LABEL vm (first args)))
          (JMP (vm_exec_inst_JMP vm (first args)))
          ;
          (JSR (vm_exec_inst_JSR vm (first args)))
          (RTN (vm_exec_inst_RTN vm))
          ;
          (CMP (vm_exec_inst_CMP vm (first args) (second args)))
          (JGT (vm_exec_inst_JGT vm (first args)))
          (JGE (vm_exec_inst_JGE vm (first args)))
          (JLT (vm_exec_inst_JLT vm (first args)))
          (JLE (vm_exec_inst_JLE vm (first args)))
          (JEQ (vm_exec_inst_JEQ vm (first args)))
          (JNE (vm_exec_inst_JNE vm (first args)))
          ;
          (TEST (vm_exec_inst_TEST vm (first args)))
          (JTRUE (vm_exec_inst_JTRUE vm (first args)))
          (JNIL (vm_exec_inst_JNIL vm (first args)))
          ;
          ; NOP : rien à faire
          ; HALT : déjà traité plus haut
        )      
      )
    )
  )
)

; 
(defun vm_load_exec (vm) ) ; TODO charge et exécute du code
; 

;;;;;

(defun vm_get_registre (vm) ) ; TODO
(defun vm_set_registre (vm) ) ; TODO


(defun resolve_addr (vm  src)
  (cond
    ((integerp src) src
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

; Manipulation mémoire/registres
; charge une addr dans la pile dans un registre
(defun vm_exec_inst_LOAD (vm src dest) 
  (set-prop vm dest (read_value vm (resolve_addr vm src))) 
)

; charge une valeur dans un registre dans une addr sur la pile
(defun vm_exec_inst_STORE (src dest) 
  (set-mem vm (resolve_addr vm dest) (read_value vm src))
)

(defun vm_exec_inst_MOVE (vm src dest) 
  (set-mem vm dest (read_value vm src))
)

(defun vm_exec_inst_ADD (vm src dest) ; DEST += SRC
  (set-mem vm dest (+ (read_value vm src) (read_value vm dest)))
)

(defun vm_exec_inst_SUB (vm src dest) ; DEST -= SRC
  (set-mem vm dest (- (read_value vm src) (read_value vm dest)))
)
(defun vm_exec_inst_MUL (vm src dest) ; DEST *= SRC
  (set-mem vm dest (* (read_value vm src) (read_value vm dest)))
)

(defun vm_exec_inst_DIV (vm src dest) ; DEST /= SRC
  (set-mem vm dest (/ (read_value vm dest) (read_value vm src)))
)

(defun vm_exec_inst_INCR (vm dest)
  (set-mem 
    vm 
    (if (listp dest) (second dest) dest) ; Adresse (:R0 si on passe (:REF :R0), ou un entier passé directement)
    (+ (read_value vm dest) 1))) ; Valeur incrémentée

(defun vm_exec_inst_DECR (vm dest) ; DEST -= 1
  (set-mem 
    vm
    (if (listp dest) (second dest) dest)
    (- (read_value vm dest) 1)))

; Pile
(defun vm_exec_inst_PUSH (vm src) ;push src sur la pile et incrémente SP
  (vm_exec_inst_INCR(vm :REF SP)) ; incrémente SP
  (vm_exec_inst_MOVE(vm src :REF SP))
); On incrémente puis on met la valeur

(defun vm_exec_inst_POP (vm dest) ; pop, met dans dest la valeur au sommet de la pile et décrémente SP
  (vm_exec_inst_MOVE(vm :REF SP dest))
  (vm_exec_inst_DECR(vm :REF SP)) ; décrémente SP
); On enlève la valeur puis on décrémente

; Labels/sauts 
(defun vm_exec_inst_LABEL (vm label) ) ; TODO déclare une étiquette
(defun vm_exec_inst_JMP   (vm label) ) ; TODO saute à une étiquette

; Appels et retours
(defun vm_exec_inst_JSR (vm label)) ; TODO met l'adresse de retour dans la pile et fait un jump
(defun vm_exec_inst_RTN (vm) ) ; TODO retourne à l'adresse de retour

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
(defun vm_exec_inst_TEST  (vm src) ) ; TODO
(defun vm_exec_inst_JTRUE (vm label) ) ; TODO
(defun vm_exec_inst_JNIL  (vm label) ) ; TODO

; Contrôle neutre
(defun vm_exec_inst_NOP (vm) ) ; TODO ? Rien
(defun vm_exec_inst_HALT (vm) ) ; TODO Arrêt de la VM

; Fonction de test
(defun test ()
  (make_vm 'test 1000)
  (afficher_registres 'test)
  (vm_exec_inst_MOVE )
  (afficher_registres 'test)
)

(defun test-exec ()
  (make_vm 'test 100)
  ;; On injecte le programme manuellement en mémoire :
  (set-mem 'test 0 '(MOVE (:CONST 10) :R0))
  (set-mem 'test 1 '(MOVE (:CONST 2) :R1))
  (set-mem 'test 2 '(MUL :R0 :R1))
  (set-mem 'test 3 '(HALT))

  (vm_exec 'test) ; Doit tourner la boucle
  (afficher_registres 'test)) ; R0 devrait valoir 11

(test-exec)