(require "vm.lisp")

(defun test-coverage-ultimate ()
  (vm_make 'ultimate-vm 500)
  
  (let ((code 
    '(
      ;; --- INIT ---
      (NOP)                     ; 1. Test NOP
      (JMP :START)              ; 2. Test JMP inconditionnel
      (HALT)                    ; Doit être sauté

      (LABEL :START)
      (MOVE (:CONST 100) :R0)   ; R0 = 100 (Valeur initiale)
      (MOVE (:CONST 0) :R1)     ; R1 = 0 (Sera utilisé pour vérifier NIL/0)
      
      ;; --- APPEL DE FONCTION ---
      (PUSH (:CONST 999))       ; 3. Test PUSH (On met une valeur "bruit" sur la pile)
      (JSR :CALCUL_BOUCLE)      ; 4. Test JSR
      (POP :R2)                 ; 5. Test POP (On récupère 999 dans R2 pour nettoyer)
      (HALT)                    ; Fin normale

      ;; --- CORPS DE LA FONCTION ---
      (LABEL :CALCUL_BOUCLE)
      
      ;; 6. Test adressage relatif (+ FP offset) et STORE
      ;; On stocke le compteur de boucle (5) à l'adresse FP+1
      (STORE (:CONST 5) (+ :FP 1)) 

      (LABEL :LOOP_START)
      ;; 7. Test LOAD relatif
      (LOAD (+ :FP 1) :R3)      ; Charge le compteur dans R3
      
      ;; 8. Test CMP et JEQ (Sortie de boucle)
      (CMP :R3 (:CONST 0))
      (JEQ :LOOP_END)

      ;; --- LOGIQUE CONDITIONNELLE COMPLEXE (Switch Case) ---
      
      ;; Cas > 3 (Test JGT)
      (CMP :R3 (:CONST 3))
      (JGT :CAS_GRAND)
      
      ;; Cas == 3 (Test JLE pour "pas grand" puis JEQ)
      (JEQ :CAS_EGAL)
      
      ;; Cas < 3 (Il reste 2 et 1)
      ;; Test JGE (Si >= 2)
      (CMP :R3 (:CONST 2))
      (JGE :CAS_MOYEN)
      
      ;; Cas < 2 (Donc 1) - Test JLT implicite par élimination, testons JLT explicitement
      (CMP :R3 (:CONST 2))
      (JLT :CAS_PETIT)
      
      ;; --- BRANCHES ---
      
      (LABEL :CAS_GRAND)        ; Pour 5 et 4
      (ADD (:CONST 10) :R0)     ; 9. Test ADD
      (JMP :FIN_TOUR)

      (LABEL :CAS_EGAL)         ; Pour 3
      (SUB (:CONST 20) :R0)     ; 10. Test SUB (Attention : R0 = R0 - 20)
      (JMP :FIN_TOUR)

      (LABEL :CAS_MOYEN)        ; Pour 2
      (DIV (:CONST 2) :R0)      ; 11. Test DIV
      (JMP :FIN_TOUR)

      (LABEL :CAS_PETIT)        ; Pour 1
      (MUL (:CONST 3) :R0)      ; 12. Test MUL
      (JMP :FIN_TOUR)

      ;; --- FIN DE TOUR ET TESTS LOGIQUES ---
      (LABEL :FIN_TOUR)

      ;; 13. Test TEST, JNIL, JTRUE
      ;; On vérifie que 0 est bien considéré comme NIL (Faux)
      (MOVE (:CONST 0) :R1)
      (TEST :R1)                ; Doit mettre FEQ=1 (si fix "0 is false" appliqué)
      (JNIL :CHECK_TRUE)
      (MOVE (:CONST 9999) :R0)  ; Erreur fatale si JNIL échoue
      (HALT)

      (LABEL :CHECK_TRUE)
      ;; On vérifie qu'un nombre non nul est VRAI
      (TEST :R3)                ; R3 contient le compteur (1 à 5), donc VRAI
      (JTRUE :DECREMENTATION)
      (MOVE (:CONST 8888) :R0)  ; Erreur fatale si JTRUE échoue
      (HALT)

      (LABEL :DECREMENTATION)
      ;; 14. Test DECR sur mémoire directe
      ;; On décrémente la variable locale sur la pile
      (DECR (+ :FP 1))
      
      ;; 15. Test INCR (juste pour couvrir le code, on incrémente R1 qui vaut 0 -> 1)
      (INCR :R1) 
      
      (JMP :LOOP_START)

      (LABEL :LOOP_END)
      ;; 16. Test RTN
      (RTN)
    )))

    (format t "~%--- DÉBUT DU TEST ULTIME (State Machine) ---~%")
    (vm_load code 'ultimate-vm)
    (vm_run 'ultimate-vm)
    (afficher_registres 'ultimate-vm)
    
    (let ((res (get-prop 'ultimate-vm :R0)))
      (if (= res 150)
          (format t "~%[SUCCÈS] R0 vaut bien 150. Tous les opérateurs et sauts fonctionnent.~%")
          (format t "~%[ÉCHEC] R0 vaut ~D au lieu de 150.~%" res)))
  )
)

;; Lancer le nouveau test
(test-coverage-ultimate)