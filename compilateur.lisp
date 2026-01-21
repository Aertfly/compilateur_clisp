(defun compilation-op (exp)
  (let ((op (car exp))
        (arg1 (cadr exp))
        (arg2 (caddr exp))
        (reste (cdddr exp)))
    (if (null reste)
        ; cas 2 arguments
        (append 
          (compilation arg1)
          '((PUSH :R0))
          (compilation arg2)
          '((POP :R1))
          (case op
            (+ '((ADD :R1 :R0)))
            (- '((SUB :R0 :R1) (MOVE :R1 :R0)))
            (* '((MUL :R1 :R0)))
            (/ '((DIV :R0 :R1) (MOVE :R1 :R0)))))
        ; cas >2 arguments
        (compilation `(,op (,op ,arg1 ,arg2) ,@reste)))))

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