(defun compilation-op (exp)
  (let ((op (car exp))
        (arg1 (cadr exp))
        (arg2 (caddr exp))
        (reste (cdddr exp)))
    (if (null reste)
        ; cas 2 arguments
        (append (compilation arg1)
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

(defun compilation (exp)
  (cond
    ((numberp exp) `((MOVE (:CONST ,exp) :R0))) ; Si c'est un nombre, on utilise :CONST
    ((atom exp) `((MOVE ,exp :R0))) ; Si c'est un symbole, on le met tel quel (registre)
    ((member (car exp) '(+ - * /)) (compilation-op exp))
    ; Pour continuer : passer le reste (donc tout sauf op) à des fonctions dédiées
    (t (error "Opérateur inconnu"))))

(compilation '(+ 1 2 3 4))