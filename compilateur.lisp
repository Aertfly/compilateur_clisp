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
    (let ((env (cons nil args))) 
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
         (current-locals (if (consp env) (car env) nil)) 
         (current-args (if (consp env) (cdr env) env)) ; Fallback si env malformé
         (new-locals (copy-list current-locals)) 
         (code '()))
    
    (dolist (binding bindings)
      (setf code (append code (compilation (second binding) env)))
      (setf code (append code '((PUSH :R0)))))
    
    (dolist (binding bindings)
      (push (first binding) new-locals))
    
    (let ((new-env (cons new-locals current-args)))
      (dolist (e corps)
        (setf code (append code (compilation e new-env)))))
    
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
  (let* ((var (second exp))
         (val-exp (third exp))
         (locals (if (consp env) (car env) nil))
         (args (if (consp env) (cdr env) env))
         (pos-local (position var locals))
         (pos-arg (position var args)))
    (append
     (compilation val-exp env) ; Le résultat de la nouvelle valeur est dans R0
     (cond
       (pos-local
        `((STORE :R0 (+ :FP ,(- (length locals) pos-local)))))
       (pos-arg
        `((STORE :R0 (+ :FP ,(- 0 (+ 2 (- (length args) 1 pos-arg)))))))
       (t (error "Variable inconnue pour SETQ : ~S" var))))))

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
         ;; On imite strictement la structure de compilation-defun
         ;; env-lambda doit être une liste où le premier élément est NIL 
         ;; et le reste de la liste sont les arguments.
         (env-lambda (cons nil args-lambda))) 
    (append
     `((JMP ,label-skip))
     `((LABEL ,label-fonc))
     (let ((code-corps '()))
       (dolist (e corps)
         (setf code-corps (append code-corps (compilation e env-lambda))))
       (append code-corps '((RTN))))
     `((LABEL ,label-skip))
     `((MOVE (:CONST ,label-fonc) :R0)))))

(defun compilation (exp &optional env)
  (let ((locals (if (consp env) (car env) nil))
        (args (if (consp env) (cdr env) env)))
    (cond
      ;; 1. TRAITEMENT DES CONSTANTES
      ((null exp) `((MOVE (:CONST NIL) :R0))) 
      ((eq exp t) `((MOVE (:CONST 1) :R0)))    
      ((numberp exp) `((MOVE (:CONST ,exp) :R0)))
      
      ;; 2. TRAITEMENT DES SYMBOLES
      ((symbolp exp) 
       (let ((locals (car env))
             (args (cdr env)))
         (let ((pos-local (and locals (position exp locals))))
           (if pos-local
               `((LOAD (+ :FP ,(- (length locals) pos-local)) :R0))
               (let ((pos-arg (and args (position exp args))))
                 (if pos-arg
                     `((LOAD (+ :FP ,(- 0 (+ 2 (- (length args) 1 pos-arg)))) :R0))
                     (error "Variable inconnue : ~S (env était ~S, locals=~S, args=~S)" 
                            exp env locals args)))))))

      ;; 3. TRAITEMENT DES FORMES IMBRIQUÉES
      ((atom exp) `((MOVE ,exp :R0)))
      
      ((eq (car exp) 'defun) (compilation-defun exp))
      ;; AJOUT DE LA LIGNE MANQUANTE ICI :
      ((eq (car exp) 'lambda) (compilation-lambda exp env)) 
      
      ((eq (car exp) 'if)    (compilation-if exp env))
      ((eq (car exp) 'let)   (compilation-let exp env))
      ((eq (car exp) 'progn) (compilation-progn exp env))
      ((eq (car exp) 'while) (compilation-while exp env))
      ((eq (car exp) 'setq)  (compilation-setq exp env))

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
      
      ;; Opérations
      ((member (car exp) '(+ - * /)) (compilation-op exp env))
      ((member (car exp) '(< > = <= >=)) (compilation-comp exp env))
      
      ;; Appel de fonction utilisateur (par défaut)
      (t (compilation-fun exp env)))))

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