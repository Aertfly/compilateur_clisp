(defun compilation-op (exp)
  (let ((op (car exp))
        (arg1 (cadr exp))
        (arg2 (caddr exp))
        (reste (cdddr exp)))
    (if (null reste)
        ; cas 2 arguments
        (append 
          (compilation arg1)
          '((PUSH :R0))      ; On sauve le résultat de arg1 sur la pile
          (compilation arg2) ; On calcule arg2, le résultat finit dans :R0
          '((MOVE :R0 :R1))  ; On déplace arg2 dans :R1
          '((POP :R0))       ; On récupère arg1 dans :R0
          (case op
            (+ '((ADD :R1 :R0)))
            (- '((SUB :R1 :R0)))
            (* '((MUL :R1 :R0)))
            (/ '((DIV :R1 :R0)))))
        ; cas >2 arguments
        (compilation `(,op (,op ,arg1 ,arg2) ,@reste)))))

(defun compilation-fun (exp)
  (let ((nom-fonc (car exp))
        (args (cdr exp))
        (code '()))
    
    (dolist (arg args)
      (setf code (append code (compilation arg))) ; résultats dans r0
      (setf code (append code '((PUSH :R0)))))

    (setf code (append code `((JSR ,nom-fonc)))) ; tous les arguments empilés, on fait l'appel

    (let ((nb-args (length args)))
      (when (> nb-args 0)
        (setf code (append code `((SUB (:CONST ,nb-args) :SP))))))

    code))


(defun compilation-comp (exp)
  (let* ((op (car exp))
         (args (cdr exp))
         (label-false (gensym "FALSE"))
         (label-end (gensym "END"))
         (code '()))
    
    (setf code (append code (compilation (car args))))
    
    (dolist (next-arg (cdr args))
      (setf code (append code 
        '((PUSH :R0))
        (compilation next-arg)
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
      '((MOVE (:CONST T) :R0))
      `((JMP ,label-end))
      `((LABEL ,label-false))
      '((MOVE (:CONST NIL) :R0))
      `((LABEL ,label-end))))
    code))

(defun compilation (exp)
  (cond
    ((numberp exp) `((MOVE (:CONST ,exp) :R0)))
    ((atom exp) `((MOVE ,exp :R0)))
    ((member (car exp) '(+ - * /)) (compilation-op exp)) ; Opération arithmétiques
    ((member (car exp) '(< > = <= >=)) (compilation-comp exp)) ; Comparaison
    (t (error "Opérateur inconnu"))))

(defun compiler-expression (exp)
  (append (compilation exp) '((HALT))))