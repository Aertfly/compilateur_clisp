(require "vm.lisp")
(require "compilateur.lisp")

(defmacro chronometre (expression)
  (let ((debut (gensym)))
    `(let ((,debut (get-internal-real-time))
           (resultat ,expression))
       (format t "~&[Chrono] ~5F s | ~A~%" 
               (/ (- (get-internal-real-time) ,debut) (float internal-time-units-per-second))
               ',expression)
       resultat)))

(defun test ()
  (compiler-fichier)
  (chronometre (executer-cible)) ; Exécution dans la VM
  (chronometre (eval (with-open-file (in "code.lisp") (read in))))) ; Exécution native

(test)
