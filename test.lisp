(require "vm.lisp")
(load "compilateur.lisp")

(defun executer-source (nom-fichier)
  (format t "~%--- Compilation de ~A ---~%" nom-fichier)
  (vm_make 'vm 5000)
  
  (let ((code (compiler-fichier nom-fichier)))
    (vm_load code 'vm)
    
    (format t "Code généré : ~S~%~%" code)
    
    (vm_run 'vm)
    
    (format t "~%--- RÉSULTAT FINAL ---~%")
    (format t "R0 : ~S~%" (get-prop 'vm :R0))
    (format t "SP : ~D~%" (get-prop 'vm :SP))))

(executer-source "code.lisp")