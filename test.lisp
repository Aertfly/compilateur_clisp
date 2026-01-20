(require "vm.lisp")
(load "compilateur.lisp") ; On charge ton compilateur

(defun test-compilation-run ()
  (vm_make 'ultimate-vm 500)
  
  ;; L'expression Lisp que l'on veut tester
  (let* ((expression '(+ 100 (- 75 25))) ; Résultat attendu : 150
         ;; On appelle ta fonction compilation pour générer le code assembleur
         (code (append (compilation expression) '((HALT)))))

    (format t "~%--- TEST : COMPILATION ET EXÉCUTION ---~%")
    (format t "Expression : ~A~%" expression)
    (format t "Code généré : ~A~%~%" code)

    ;; On charge le code généré dynamiquement dans la VM
    (vm_load code 'ultimate-vm)
    (vm_run 'ultimate-vm)
    
    (afficher_registres 'ultimate-vm)
    
    (let ((res (get-prop 'ultimate-vm :R0)))
      (if (= res 150)
          (format t "~%[SUCCÈS] R0 vaut bien 150. La compilation et l'exécution ont réussi.~%")
          (format t "~%[ÉCHEC] R0 vaut ~D au lieu de 150.~%" res)))
  )
)

;; Lancer le test
(test-compilation-run)