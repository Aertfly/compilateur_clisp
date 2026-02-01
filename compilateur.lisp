(defun compilation-op (exp &optional env)
  (let ((op (car exp))
        (arg1 (cadr exp))
        (arg2 (caddr exp))
        (reste (cdddr exp)))
    (if (null reste)
        ; cas 2 arguments
        (append 
          (compilation arg1 env)
          '((PUSH :R0))      ; On sauve le résultat de arg1 sur la pile
          (compilation arg2 env) ; On calcule arg2, le résultat finit dans :R0
          '((MOVE :R0 :R1))  ; On déplace arg2 dans :R1
          '((POP :R0))       ; On récupère arg1 dans :R0
          (case op
            (+ '((ADD :R1 :R0)))
            (- '((SUB :R1 :R0)))
            (* '((MUL :R1 :R0)))
            (/ '((DIV :R1 :R0)))))
        ; cas >2 arguments
        (compilation `(,op (,op ,arg1 ,arg2) ,@reste) env))))

(defun compilation-fun (exp &optional env)
  (let ((nom-ou-exp (car exp))
        (args (cdr exp))
        (code '()))
    ;; 1. Empiler les arguments
    (dolist (arg args)
      (setf code (append code (compilation arg env)))
      (setf code (append code '((PUSH :R0)))))
    ;; 2. Appel
    (if (symbolp nom-ou-exp)
        (setf code (append code `((JSR ,nom-ou-exp))))
        (setf code (append code 
                           (compilation nom-ou-exp env) ;; Calcul de l'adresse -> R0
                           `((JSR :R0)))))              ;; Appel indirect
    ;; 3. Nettoyage
    (let ((nb-args (length args)))
      (when (> nb-args 0)
        (setf code (append code `((SUB (:CONST ,nb-args) :SP))))))
    code))

(defun compilation-defun (exp)
  (let ((nom (second exp))
        (args (third exp))
        (corps (cdddr exp))
        (code '()))
    (setf code (append code `((LABEL ,nom))))
    ;; MODIF ICI : env est maintenant une liste de cadres ((nil . args))
    (let ((env (list (cons nil args)))) 
      (dolist (e corps)
        (setf code (append code (compilation e env)))))
    (append code '((RTN)))))

(defun compilation-comp (exp &optional env)
  (let* ((op (car exp))
         (args (cdr exp))
         (label-false (gensym "FALSE"))
         (label-end (gensym "END"))
         (code '()))
    
    (setf code (append code (compilation (car args) env)))
    
    (dolist (next-arg (cdr args))
      (setf code (append code 
        '((PUSH :R0))
        (compilation next-arg env)
        '((POP :R1))
        '((CMP :R1 :R0))
        (case op
          (<  `((JGE ,label-false)))
          (>  `((JLE ,label-false)))
          (=  `((JNE ,label-false)))
          (<= `((JGT ,label-false)))
          (>= `((JLT ,label-false)))
        ))))

    (setf code (append code
      '((MOVE (:CONST 1) :R0))
      `((JMP ,label-end))
      `((LABEL ,label-false))
      '((MOVE (:CONST 0) :R0))
      `((LABEL ,label-end))))
    code))

(defun compilation-if (exp env)
  (let ((condition (second exp))
        (alors (third exp))
        (sinon (fourth exp))
        (label-sinon (gensym "ELSE"))
        (label-fin (gensym "ENDIF")))
    (append
     (compilation condition env) 
     '((TEST :R0))               ;; Positionne les flags selon R0
     `((JEQ ,label-sinon))       ;; Si FEQ=1 (donc R0 était NIL/0), on saute au SINON
     (compilation alors env)     
     `((JMP ,label-fin))
     `((LABEL ,label-sinon))
     (if sinon 
         (compilation sinon env)
         '((MOVE (:CONST NIL) :R0)))
     `((LABEL ,label-fin)))))

(defun compilation-let (exp env)
  (let* ((bindings (second exp))
         (corps (cddr exp))
         ;; NOUVELLE GESTION DE L'ENVIRONNEMENT
         ;; On récupère le cadre courant (le premier de la liste env)
         (frame (if env (car env) (cons nil nil))) 
         (current-locals (car frame))  ; Les locales actuelles
         (current-args (cdr frame))    ; Les arguments actuels
         (rest-env (cdr env))          ; Les cadres parents
         
         (new-locals current-locals)   ; On va ajouter les variables ici
         (code '()))
    
    ;; 1. On compile les valeurs et on les empile (avec l'ancien environnement)
    (dolist (binding bindings)
      (setf code (append code (compilation (second binding) env)))
      (setf code (append code '((PUSH :R0)))))
    
    ;; 2. On déclare les variables dans l'environnement du compilateur
    (dolist (binding bindings)
      (push (first binding) new-locals))
      
    ;; 3. On crée le nouvel environnement pour le corps
    ;; On remplace le premier cadre par le nouveau (avec new-locals)
    (let ((new-env (cons (cons new-locals current-args) rest-env)))
      (dolist (e corps)
        (setf code (append code (compilation e new-env)))))
    
    ;; 4. Nettoyage de la pile à la fin du let
    (let ((nb-vars (length bindings)))
      (when (> nb-vars 0)
        (setf code (append code `((SUB (:CONST ,nb-vars) :SP))))))
    code))

(defun compilation-progn (exp env)
  (let ((expressions (cdr exp)) ; On retire le symbole 'progn
        (code '()))
    (if (null expressions)
        '((MOVE (:CONST NIL) :R0)) ; Un progn vide retourne NIL
        (progn
          (dolist (e expressions)
            (setf code (append code (compilation e env))))
          code))))

(defun compilation-while (exp env)
  (let ((condition (second exp))
        (corps (cddr exp))
        (label-test (gensym "WTEST"))
        (label-end (gensym "WEND")))
    (append
     `((LABEL ,label-test))
     (compilation condition env)     ; On évalue la condition
     `((TEST :R0))                   ; On vérifie si c'est vrai (non-NIL/non-0)
     `((JNIL ,label-end))            ; Si faux, on saute à la fin
     ;; On compile le corps du while (comme un progn)
     (let ((code-corps '()))
       (dolist (e corps)
         (setf code-corps (append code-corps (compilation e env))))
       code-corps)
     `((JMP ,label-test))            ; On boucle
     `((LABEL ,label-end))
     '((MOVE (:CONST NIL) :R0)))))   ; Un while retourne NIL par défaut

(defun compilation-setq (exp env)
  (let ((var (second exp))
        (val-exp (third exp)))
    (append
      (compilation val-exp env) ; Le résultat est dans R0
      (let ((result (trouver-variable var env 0)))
        (if result
            (let ((depth (car result))
                  (index (cdr result)))
              ;; On génère un STORE vers l'adresse résolue (:VAR depth index)
              ;; La VM utilisera resolve_addr pour trouver la bonne case mémoire
              `((STORE :R0 (:VAR ,depth ,index))))
            (error "Variable inconnue pour SETQ : ~S" var))))))

(defun compilation-cons (exp env)
  (append 
    (compilation (caddr exp) env) ; Compile le 2ème arg (CDR)
    '((PUSH :R0))                 ; Sauve sur la pile
    (compilation (cadr exp) env)  ; Compile le 1er arg (CAR)
    '((POP :R1))                  ; Récupère le CDR dans R1
    '((CONS :R0 :R1))))           ; R0 = (cons R0 R1)

(defun compilation-car (exp env)
  (append 
    (compilation (cadr exp) env)
    '((CAR :R0))))

(defun compilation-cdr (exp env)
  (append 
    (compilation (cadr exp) env)
    '((CDR :R0))))

(defun compilation-quote (exp env)
  ;; L'argument de quote est (cadr exp)
  ;; On génère un MOVE direct de la constante vers R0
  `((MOVE (:CONST ,(cadr exp)) :R0)))

(defvar *macros-table* (make-hash-table))

(defun get-defmacro (nom)
  (gethash nom *macros-table*))

(defun set-defmacro (nom fonction)
  (setf (gethash nom *macros-table*) fonction))

(defun compilation-defmacro (exp)
  (let ((nom (second exp))
        (args (third exp))
        (corps (cdddr exp)))
    ;; On crée une fonction Lisp réelle qui prend les arguments de la macro
    ;; et retourne la forme expansée.
    (set-defmacro nom 
                  (eval `(lambda ,args (progn ,@corps))))
    ;; Une définition de macro ne produit pas de code exécutable par la VM
    nil))

(defun expansion-macro (exp)
  (let ((macro-fun (get-defmacro (car exp))))
    (if macro-fun
        ;; On applique la fonction de la macro aux arguments (non évalués !)
        (apply macro-fun (cdr exp))
        exp)))

(defun transformer-backquote (exp)
  (cond
    ;; Si c'est un atome, on le quote pour qu'il reste tel quel
    ((atom exp) (list 'quote exp))
    
    ;; Détection de la virgule (unquote) : CLISP utilise SYSTEM::UNQUOTE
    ((eq (car exp) 'system::unquote)
     (second exp))
    
    ;; Détection de la virgule-arobase (splice) : SYSTEM::SPLICE
    ;; On utilise APPEND pour fusionner la liste résultante
    ((and (consp (car exp)) (eq (caar exp) 'system::splice))
     (list 'append (second (car exp)) (transformer-backquote (cdr exp))))
    
    ;; Cas récursif standard
    (t (list 'cons 
             (transformer-backquote (car exp)) 
             (transformer-backquote (cdr exp))))))

(defun compilation-lambda (exp env)
  (let* ((args-lambda (second exp))
         (corps (cddr exp))
         (label-fonc (gensym "LAMBDA"))
         (label-skip (gensym "SKIP"))
         ;; MODIF ICI : On ajoute le nouveau cadre en tête de l'env existant
         (env-lambda (cons (cons nil args-lambda) env))) 
    (append
     `((JMP ,label-skip))
     `((LABEL ,label-fonc))
     (let ((code-corps '()))
       (dolist (e corps)
         (setf code-corps (append code-corps (compilation e env-lambda))))
       (append code-corps '((RTN))))
     `((LABEL ,label-skip))
     `((MOVE (:CONST ,label-fonc) :R0)))))

(defun trouver-variable (var env depth)
  (if (null env)
      nil
      (let* ((frame (car env))
             (locals (car frame))
             (args (cdr frame))
             (pos-local (position var locals))
             (pos-arg (position var args)))
        (cond
          (pos-local 
           (cons depth (- (length locals) pos-local)))
          (pos-arg
           ;; MODIF ICI : On change le 2 en 3 car la pile a grossi (FP, PC, SL)
           (cons depth (- 0 (+ 3 (- (length args) 1 pos-arg)))))
          (t 
           (trouver-variable var (cdr env) (+ 1 depth)))))))

(defun compilation (exp &optional env)
  ;; Note : On a supprimé le let ((locals ...)) qui était ici car il est obsolète
  (cond
    ;; 1. CONSTANTES
    ((null exp) `((MOVE (:CONST NIL) :R0))) 
    ((eq exp t) `((MOVE (:CONST 1) :R0)))   
    ((numberp exp) `((MOVE (:CONST ,exp) :R0)))
    
    ;; 2. SYMBOLES (VARIABLES) -- C'est ici que ça changeait
    ((symbolp exp) 
     (let ((result (trouver-variable exp env 0)))
       (if result
           (let ((depth (car result))
                 (index (cdr result)))
             ;; On génère l'accès avec la profondeur (:VAR depth index)
             `((MOVE (:VAR ,depth ,index) :R0)))
           (error "Variable inconnue : ~S" exp))))

    ;; 3. FORMES IMBRIQUÉES
    ((atom exp) `((MOVE (:CONST ,exp) :R0)))    
    
    ((eq (car exp) 'defun) (compilation-defun exp))
    ((eq (car exp) 'lambda) (compilation-lambda exp env)) 
    
    ((eq (car exp) 'if)     (compilation-if exp env))
    ((eq (car exp) 'let)    (compilation-let exp env))
    ((eq (car exp) 'progn)  (compilation-progn exp env))
    ((eq (car exp) 'while)  (compilation-while exp env))
    ((eq (car exp) 'setq)   (compilation-setq exp env))

    ;; Macros
    ((eq (car exp) 'defmacro) (compilation-defmacro exp))
    ((and (symbolp (car exp)) (get-defmacro (car exp)))
     (compilation (expansion-macro exp) env))
    ((and (consp exp) (eq (car exp) 'system::backquote))
     (compilation (transformer-backquote (second exp)) env))

    ;; Primitives
    ((eq (car exp) 'cons)  (compilation-cons exp env))
    ((eq (car exp) 'car)   (compilation-car exp env))
    ((eq (car exp) 'cdr)   (compilation-cdr exp env))
    ((eq (car exp) 'quote) (compilation-quote exp env))
    ;; Ajout pour le debug
    ((eq (car exp) 'print) 
     (append 
      (compilation (second exp) env) ;; On calcule ce qu'on veut afficher (résultat dans R0)
      '((PRIN :R0))))                ;; On appelle l'instruction PRIN sur R0
    
    ;; Opérations
    ((member (car exp) '(+ - * /)) (compilation-op exp env))
    ((member (car exp) '(< > = <= >=)) (compilation-comp exp env))
    
    ;; Appel de fonction utilisateur
    (t (compilation-fun exp env))))

(defun compiler-expression (exp)
  (append (compilation exp) '((HALT))))

(defun compiler-fichier (filename)
  (let ((code-functions '())
        (expressions-main '()))
    (with-open-file (stream filename)
      (do ((exp (read stream nil 'eof) (read stream nil 'eof)))
          ((eq exp 'eof))
        (if (and (listp exp) (eq (car exp) 'defun))
            (setf code-functions (append code-functions (compilation-defun exp)))
            (setf expressions-main (append expressions-main (compilation exp))))))
    
    (append '((JMP :START)) 
            code-functions      
            '((LABEL :START)) 
            expressions-main   
            '((HALT)))))       