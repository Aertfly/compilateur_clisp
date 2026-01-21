(require "vm.lisp")
(load "compilateur.lisp")

(defun test-compilation-run ()
  (vm_make 'vm 500)
  (vm_load (
    compiler-expression '
      (> 5 4 5 2 1)
    ) 'vm)
  (vm_run 'vm)    
  (format t "~%~S~%" (get-prop 'vm :R0)))

(test-compilation-run)