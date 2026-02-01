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
  (let ((nom-fonc (car exp))
        (args (cdr exp))
        (code '()))

    (dolist (arg args)
      (setf code (append code (compilation arg env))) 
      (setf code (append code '((PUSH :R0)))))

    (setf code (append code `((JSR ,nom-fonc)))) 

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
        (sinon (fourth exp)) ; Sera NIL si absent
        (label-sinon (gensym "ELSE"))
        (label-fin (gensym "ENDIF")))
    (append
     (compilation condition env) 
     `((TEST :R0))               
     `((JNIL ,label-sinon))      
     (compilation alors env)     
     `((JMP ,label-fin))
     `((LABEL ,label-sinon))
     (if sinon 
         (compilation sinon env) ; On compile le sinon s'il existe
         '((MOVE (:CONST NIL) :R0))) ; Sinon on retourne NIL par défaut
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

(defun compilation (exp &optional env)
  (let ((locals (if (consp env) (car env) nil))
        (args (if (consp env) (cdr env) env)))
    (cond
      ((numberp exp) `((MOVE (:CONST ,exp) :R0)))
      
      ((symbolp exp) 
       ;; CAS 1 : C'est une variable locale (LET)
       (let ((pos-local (position exp locals)))
         (if pos-local
             ;; Formule locale : FP + (taille - pos)
             `((LOAD (+ :FP ,(- (length locals) pos-local)) :R0))
             
             ;; CAS 2 : C'est un argument de fonction
             (let ((pos-arg (position exp args)))
               (if pos-arg
                   ;; Formule argument : FP - (2 + (nb_args - 1 - pos))
                   ;; On recule de 2 (PC+FP) puis on remonte dans les args
                   `((LOAD (+ :FP ,(- 0 (+ 2 (- (length args) 1 pos-arg)))) :R0))
                   
                   (error "Variable inconnue : ~S" exp))))))

      ((atom exp) `((MOVE ,exp :R0)))
      ((eq (car exp) 'defun) (compilation-defun exp))
      ((eq (car exp) 'if) (compilation-if exp env))
      ((eq (car exp) 'let) (compilation-let exp env))
      ((member (car exp) '(+ - * /)) (compilation-op exp env))
      ((member (car exp) '(< > = <= >=)) (compilation-comp exp env))
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