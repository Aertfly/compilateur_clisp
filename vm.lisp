(require "utils/utils.lisp")

;Structure Mémoire : avec le code=Array et la pile=Array et stackpointer basepointer ? 
(defun vm_make (name size)
    (set-prop name :name name)

    ; Registre
    (set-prop name :R0 0)
    (set-prop name :R1 0)
    (set-prop name :R2 0)
    (set-prop name :R3 0)
    
    ; Pile
    (set-prop name :BP 500)
    (set-prop name :SP 500)
    (set-prop name :FP 0)
    
    ; Comparaisons flag
    (set-prop name :FLT 0); booléen plus petit que 
    (set-prop name :FEQ 0); booléen égal
    (set-prop name :FGT 0); booléan plus grand que

    (set-prop name :PC 0); Program counter, compteur ordinal
    (set-prop name :LC 0); Load counter, ????

    ;etiq Table de hashage pour les étiquettes
    (set-prop name :labels (make-hash-table))
    ;etiqNr Table non résolue des étiquettes

    (set-prop name :mem (make-array size :initial-element 0)); Mémoire avec code + pile

    'ok ;Retourne la string "ok"
)

(defun vm_run (vm) ; exécution de la VM : boucle principale
  (loop
    (let* ((pc (get-prop vm :PC))
          (inst (get-mem vm pc))) ; On récupère la valeur du compteur ordinal, puis on stocke dans inst l'instruction à l'emplacement mémoire correspondant
      (format t "Instruction en ~D : ~S~%" pc inst) ; Affichage de l'instruction

      (when (not (listp inst))
        (format t "ERREUR FATALE : PC (~D) pointe sur une donnée brute : ~S~%" pc inst)
        (format t "Vérifiez qu'un HALT est bien présent avant cette adresse.~%")
        (return 'CRASH))

      (if (or (null inst) (eq (first inst) 'HALT))
          (progn (format t "Fin execution (HALT)~%") (return 'DONE)))

      ; Arrêt de l'exécution
      (if 
        (or 
          (null inst) ; cas où pas d'instruction
          (not (listp inst)) ; protection contre les données brutes
          (eq (first inst) 'HALT)) ; cas où instruction HALT
        (progn 
          (unless (or (null inst) (eq (first inst) 'HALT))
            (format t "Erreur : Instruction invalide en ~D : ~S~%" pc inst))
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
          ; LABEL : pas une instruction
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
          ;
          ; Gestion des listes 
          (CONS (let ((val2 (read_value vm (second args)))  ; Le CDR est souvent sur la pile ou R1
                      (val1 (read_value vm (first args))))  ; Le CAR
                  (set-prop vm :R0 (cons val1 val2))))

          (CAR (let ((liste (read_value vm (first args))))
                 (if (consp liste)
                     (set-prop vm :R0 (car liste))
                     (set-prop vm :R0 nil))))

          (CDR (let ((liste (read_value vm (first args))))
                 (if (consp liste)
                     (set-prop vm :R0 (cdr liste))
                     (set-prop vm :R0 nil))))
        )      
      )
    )
  )
)

; 
(defun vm_load (code-list vm)
  (let ((addr 0))
    (dolist (inst code-list)
      (if (eq (first inst) 'LABEL) ; On est dans le cas LABEL avec (second inst) qui est le nom du label
        (setf (gethash (second inst) (get-prop vm :labels)) addr)
        (progn ; Pour exécuter plusieurs instructions
          (set-mem vm addr inst)
          (incf addr)
        )
      )
    )
  )
)


(defun resolve_addr (vm src)
  (cond
    ((integerp src) src)
    ((listp src)
     (case (first src)
       (:REF  (read_value vm (second src))) ; On lit la VALEUR du registre
       (+     (let ((val-registre (read_value vm (second src))) ; On prend la valeur numérique (ex: 100)
                    (offset (third src)))
                (+ val-registre offset))) ; Résultat : 98
       (t (error "Type d'opérande inconnu : ~S" src))))
    (t (error "Argument invalide : ~S" src))))

; Manipulation mémoire/registres
; charge une addr dans la pile dans un registre
(defun vm_exec_inst_LOAD (vm src dest) 
  (let ((val (get-mem vm (resolve_addr vm src)))) ; On lit dans la mémoire à l'adresse calculée
    (if (symbolp dest)
        (set-prop vm dest val)   ; Si dest est :R0, on écrit dans le registre
        (set-mem vm dest val)))) ; Sinon on écrit en mémoire

; charge une valeur dans un registre dans une addr sur la pile
(defun vm_exec_inst_STORE (vm src dest) 
  (set-mem vm (resolve_addr vm dest) (read_value vm src))
)

(defun vm_exec_inst_MOVE (vm src dest) 
  (let ((val (read_value vm src))
        (target (if (listp dest) (resolve_addr vm dest) dest)))
    (if (symbolp target)
        (set-prop vm target val)
        (set-mem vm target val))))

(defun vm_exec_inst_ADD (vm src dest) ; DEST += SRC
  (write_value vm dest (+ (read_value vm src) (read_value vm dest))))

(defun vm_exec_inst_SUB (vm src dest) ; DEST -= SRC
  (write_value vm dest (- (read_value vm dest) (read_value vm src))))

(defun vm_exec_inst_MUL (vm src dest) ; DEST *= SRC
  (write_value vm dest (* (read_value vm src) (read_value vm dest))))

(defun vm_exec_inst_DIV (vm src dest) ; DEST /= SRC
  (write_value vm dest (/ (read_value vm dest) (read_value vm src))))

(defun vm_exec_inst_INCR (vm dest)
  (let ((target (if (listp dest) (resolve_addr vm dest) dest)))
    (set-mem vm target (+ (read_value vm dest) 1))))

(defun vm_exec_inst_DECR (vm dest)
  (let ((target (if (listp dest) (resolve_addr vm dest) dest)))
    (set-mem vm target (- (read_value vm dest) 1))))

; Pile
(defun vm_exec_inst_PUSH (vm src) ;push src sur la pile et incrémente SP
  (vm_exec_inst_INCR vm :SP) ; incrémente SP
  (vm_exec_inst_MOVE vm src '(:REF :SP))
); On incrémente puis on met la valeur

(defun vm_exec_inst_POP (vm dest) ; pop, met dans dest la valeur au sommet de la pile et décrémente SP
  (vm_exec_inst_MOVE vm '(:REF :SP) dest)
  (vm_exec_inst_DECR vm :SP) ; décrémente SP
); On enlève la valeur puis on décrémente

; Labels/sauts 
(defun vm_exec_inst_JMP (vm label)
  ;; On récupère l'adresse associée au label dans la table
  (let ((target-addr (gethash label (get-prop vm :labels))))
    (if target-addr
        (set-prop vm :PC target-addr) ; On met à jour le PC
        (error "Erreur : Label inconnu ~S" label))))

; Appels et retours
(defun vm_exec_inst_JSR (vm cible)
  ;; Étape 1 : Déterminer la valeur cible brute
  (let ((valeur-brute 
         (cond
           ;; Cas 1 : C'est un registre (ex: :R0), on lit son contenu
           ((keywordp cible) (read_value vm cible))
           
           ;; Cas 2 : C'est un label (ex: TEST-LAMBDA), on cherche son adresse
           ((symbolp cible) (gethash cible (get-prop vm :labels)))
           
           ;; Cas 3 : Autre (adresse directe), on lit la valeur
           (t (read_value vm cible)))))

    ;; Étape 2 : Résolution finale (gestion des sauts indirects via labels)
    ;; Si valeur-brute est un symbole (ex: on a lu #:LAMBDA... depuis :R0),
    ;; il faut maintenant trouver l'adresse de ce symbole.
    (let ((target-addr (if (symbolp valeur-brute)
                           (gethash valeur-brute (get-prop vm :labels))
                           valeur-brute))) ;; Sinon c'est déjà une adresse (entier)
      
      (if (and target-addr (integerp target-addr))
          (progn
            (vm_exec_inst_PUSH vm (list :CONST (get-prop vm :PC)))
            (vm_exec_inst_PUSH vm (list :CONST (get-prop vm :FP)))
            (set-prop vm :FP (get-prop vm :SP))
            (set-prop vm :PC target-addr))
          (error "JSR : Cible invalide ~S (Valeur résolue: ~S)" cible target-addr)))))

(defun vm_exec_inst_RTN (vm) 
  ;; 1. On vide la pile locale (SP revient à FP)
  (set-prop vm :SP (get-prop vm :FP))
  
  ;; 2. On restaure l'ancien FP
  (vm_exec_inst_POP vm :FP)
  
  ;; 3. On restaure le PC (adresse de retour)
  (vm_exec_inst_POP vm :PC)
)

; Comparaisons
(defun vm_exec_inst_CMP (vm src1 src2)
  (let ((val1 (read_value vm src1))
        (val2 (read_value vm src2)))
    ;; Réinitialisation des drapeaux
    (set-mem vm :FLT 0)
    (set-mem vm :FEQ 0)
    (set-mem vm :FGT 0)
    (cond
      ;; Cas numérique : on peut utiliser < et >
      ((and (numberp val1) (numberp val2))
       (cond
         ((< val1 val2) (set-mem vm :FLT 1))
         ((= val1 val2) (set-mem vm :FEQ 1))
         ((> val1 val2) (set-mem vm :FGT 1))))
      ;; Cas non-numérique (Listes, NIL, Symboles) : Seule l'égalité fait sens
      (t
       (if (equal val1 val2)
           (set-mem vm :FEQ 1))))))

(defun vm_exec_inst_JGT (vm label) (vm_jmp_cond_helper vm 0 0 1 label))

(defun vm_exec_inst_JGE (vm label) (vm_jmp_cond_helper vm 0 1 1 label))

(defun vm_exec_inst_JLT (vm label) (vm_jmp_cond_helper vm 1 0 0 label))

(defun vm_exec_inst_JLE (vm label) (vm_jmp_cond_helper vm 1 1 0 label))

(defun vm_exec_inst_JEQ (vm label) (vm_jmp_cond_helper vm 0 1 0 label))

(defun vm_exec_inst_JNE (vm label) (vm_jmp_cond_helper vm 1 0 1 label))

; Tests logiques
(defun vm_exec_inst_TEST (vm src)
  (let ((val (read_value vm src)))
    (set-mem vm :FEQ 0)
    (set-mem vm :FGT 0)
    ;; Si la valeur est NIL ou 0, c'est FAUX -> on active FEQ (pour JNIL)
    (if (or (null val) (and (numberp val) (= val 0)))
        (set-mem vm :FEQ 1)  ;; Signal pour JNIL
        (set-mem vm :FGT 1)))) ;; Signal pour JTRUE / IF

(defun vm_exec_inst_JTRUE (vm label)
  ;; Saute si le résultat n'était pas NIL (donc FGT=1 selon la logique ci-dessus)
  (vm_jmp_cond_helper vm 0 0 1 label))

(defun vm_exec_inst_JNIL (vm label)
  ;; Saute si le résultat était NIL (donc FEQ=1)
  (vm_jmp_cond_helper vm 0 1 0 label))
