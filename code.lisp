(defun TEST-LEXICAL ()
  (let ((x 10))       ; x est déclaré dans l'environnement parent
    ((lambda (y) 
       (+ x y))       ; La lambda accède à x (profondeur 1) et y (profondeur 0)
     5)))

(TEST-LEXICAL)