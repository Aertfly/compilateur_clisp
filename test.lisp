(require "vm.lisp")
(load "compilateur.lisp")

(defun executer-source (nom-fichier)
  (vm_make 'vm 5000)
  
  (let ((code (compiler-fichier nom-fichier)))
    (vm_load code 'vm)
    
    (vm_run 'vm)
    
    (print (get-prop 'vm :R0))))
    
(executer-source "code.lisp")