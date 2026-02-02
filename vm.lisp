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
      ; (format t "Instruction en ~D : ~S~%" pc inst) ; Affichage de l'instruction

      (when (not (listp inst))
        (format t "ERREUR FATALE : PC (~D) pointe sur une donnée brute : ~S~%" pc inst)
        (format t "Vérifiez qu'un HALT est bien présent avant cette adresse.~%")
        (return 'CRASH))

      ;(if (or (null inst) (eq (first inst) 'HALT))
      ;    (progn (format t "Fin execution (HALT)~%") (return 'DONE)))

      ; Arrêt de l'exécution
      (if 
        (or 
          (null inst) ; cas où pas d'instruction
          (not (listp inst)) ; protection contre les données brutes
          (eq (first inst) 'HALT)) ; cas où instruction HALT
        (progn 
          (unless (or (null inst) (eq (first inst) 'HALT))
            (format t "Erreur : Instruction invalide en ~D : ~S~%" pc inst))
          ; (format t "Fin execution~%")
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
          ;
          (PRIN (vm_exec_inst_PRIN vm (first args)))
        )      
      )
    )
  )
)

(defun resolve-argument (arg label-map)
  "Si l'argument est un label connu, retourne son adresse. 
   Gère aussi le cas (:CONST LABEL)."
  (cond
    ;; Cas 1 : L'argument est directement le label (ex: pour JMP LABEL)
    ((and (symbolp arg) (gethash arg label-map))
     (gethash arg label-map))
     
    ;; Cas 2 : L'argument est (:CONST LABEL) (ex: pour MOVE (:CONST LABEL) :R0)
    ((and (listp arg) 
          (eq (first arg) :CONST)
          (symbolp (second arg)) 
          (gethash (second arg) label-map))
     (list :CONST (gethash (second arg) label-map)))
     
    ;; Cas 3 : Rien à changer
    (t arg)))

(defun vm_load (code-list vm)
  (let ((addr 0)
        (label-map (make-hash-table))
        (resolved-code '()))
    
    ;; --- PASSE 1 : Repérage des Labels ---
    ;; On compte les instructions "réelles" pour savoir où tombent les labels.
    (dolist (inst code-list)
      (if (eq (first inst) 'LABEL)
          ;; Si c'est un label, on stocke sa position future (addr actuelle)
          (setf (gethash (second inst) label-map) addr)
          ;; Si c'est une vraie instruction, on incrémente le compteur d'adresse
          (incf addr)))

    ;; --- PASSE 2 : Résolution et Nettoyage ---
    (dolist (inst code-list)
      ;; On ignore les lignes LABEL, elles ne doivent pas être en mémoire
      (unless (eq (first inst) 'LABEL)
        (let ((op (first inst))
              (args (rest inst))
              (new-args nil))
          
          ;; Pour chaque argument de l'instruction, on regarde si c'est un label à remplacer
          (setf new-args (mapcar (lambda (arg) (resolve-argument arg label-map)) args))
          
          ;; On reconstruit l'instruction avec les adresses (ex: (JMP 12) au lieu de (JMP :FIN))
          (push (cons op new-args) resolved-code))))
    
    ;; On remet le code dans le bon ordre (car push inverse la liste)
    (setf resolved-code (nreverse resolved-code))

    ;; --- CHARGEMENT EN MÉMOIRE ---
    (setf addr 0)
    (dolist (inst resolved-code)
      (set-mem vm addr inst)
      (incf addr))
    
    ;; On stocke la table des labels dans la VM juste pour le debug (optionnel)
    (set-prop vm :labels label-map)
    ;(format t "~%[Loader] Code chargé et résolu (~D instructions).~%" addr)))
    ))

(defun resolve_addr (vm src)
  (cond
    ((integerp src) src)
    ((listp src)
     (case (first src)
       (:REF  (read_value vm (second src)))
       (+     (let ((val (read_value vm (second src))) (off (third src))) (+ val off)))
       (:CONST (second src)) ;; Au cas où
       
       ;; --- GESTION DES VARIABLES LEXICALES ---
       (:VAR  (let ((depth (second src))
                    (index (third src))
                    (current-fp (get-prop vm :FP)))
                
                ;; Boucle pour remonter les liens statiques
                (dotimes (i depth)
                  ;; On lit le Static Link stocké à l'adresse (FP - 2)
                  ;; Attention : get-mem prend une adresse absolue.
                  (setf current-fp (get-mem vm (- current-fp 2))))
                
                ;; Une fois au bon étage, on applique l'offset
                (+ current-fp index)))
       
       (t (error "Type inconnu : ~S" src))))
    (t (error "Arg invalide : ~S" src))))

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
(defun vm_exec_inst_JMP (vm target)
  (if (integerp target)
      (set-prop vm :PC target)
      (error "Erreur JMP : L'adresse ~S n'est pas un entier (Problème de résolution ?)" target)))

; Appels et retours
(defun vm_exec_inst_JSR (vm cible)
  (let ((adresse-saut 
          (cond 
            ((integerp cible) cible)
            ((keywordp cible) (read_value vm cible))
            (t (error "JSR : Cible invalide ~S" cible)))))
    
    ;; 1. On empile le STATIC LINK (Le FP courant devient le parent du futur cadre)
    (vm_exec_inst_PUSH vm (list :CONST (get-prop vm :FP))) 
    
    ;; 2. On empile le PC (Adresse de retour)
    (vm_exec_inst_PUSH vm (list :CONST (get-prop vm :PC)))
    
    ;; 3. On empile le DYNAMIC LINK (L'ancien FP)
    (vm_exec_inst_PUSH vm (list :CONST (get-prop vm :FP)))
    
    ;; 4. Mise à jour des registres
    (set-prop vm :FP (get-prop vm :SP))
    (set-prop vm :PC adresse-saut)))

(defun vm_exec_inst_RTN (vm) 
  ;; 1. On ramène SP au niveau de FP (vide les locales)
  (set-prop vm :SP (get-prop vm :FP))
  
  ;; 2. On restaure le FP (Lien Dynamique)
  (vm_exec_inst_POP vm :FP)
  
  ;; 3. On restaure le PC (Adresse de retour)
  (vm_exec_inst_POP vm :PC)
  
  ;; 4. On vire le Static Link (on le met dans une poubelle, ex: R2, car on n'en a plus besoin)
  (vm_exec_inst_POP vm :R2))

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

(defun vm_exec_inst_PRIN (vm src)
  (let ((val (read_value vm src)))
    (format t "~S~%" val) ;; Affiche la valeur suivie d'un saut de ligne
    (set-prop vm :R0 val))) ;; PRIN retourne la valeur affichée (comme print en Lisp)

(defun executer-cible ()
  (vm_make 'vm 5000)
  
  (let ((code-charge '()))
    ;; 1. Lecture du fichier cible.asm
    (with-open-file (stream "cible.asm" :direction :input)
      (do ((ins (read stream nil 'eof) (read stream nil 'eof)))
          ((eq ins 'eof))
        (push ins code-charge)))
    
    ;; 2. On inverse la liste car 'push' l'a construite à l'envers
    (let ((code-final (nreverse code-charge)))
      ;; 3. Chargement et exécution
      (vm_load code-final 'vm)
      (vm_run 'vm)
      
      ;; 4. Affichage du résultat
      (print (get-prop 'vm :R0)))))
