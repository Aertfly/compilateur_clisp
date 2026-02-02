(require "vm.lisp")
(require "compilateur.lisp")
(require "utils/chronometre.lisp")

(defun test ()
  (when (probe-file "cible.asm") ; Suppression du fichier compilé
    (delete-file "cible.asm"))

  (chronometre (compiler-fichier)) ; Compilation
  (chronometre (executer-cible)) ; Exécution dans la VM
  (chronometre (eval (with-open-file (in "code.lisp") (read in))))) ; Exécution native

(test)
