;; Macro while pour compatibilité avec le code compilable
(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))

(defun get-prop (mv prop)
  (get mv prop))

(defun set-prop (mv prop val)
  (setf (get mv prop) val))

(defun get-mem (vm cible)
  (if (integerp cible)
      (aref (get-prop vm :mem) cible)
      (get-prop vm cible)))

(defun set-mem (vm cible valeur)
  (if (integerp cible)
      (setf (aref (get-prop vm :mem) cible) valeur)
      (set-prop vm cible valeur)))

(defun vm_jmp_cond_helper (vm check_flt check_feq check_fgt label)
  (let ((flt (get-mem vm :FLT))
        (feq (get-mem vm :FEQ))
        (fgt (get-mem vm :FGT)))
    (when (or (and (= check_flt 1) (= flt 1))
              (and (= check_feq 1) (= feq 1))
              (and (= check_fgt 1) (= fgt 1)))
      (vm_exec_inst_JMP vm label))))

(defun read_value (vm val)
  (cond
    ((integerp val) (get-mem vm val))
    ((symbolp val) (get-prop vm val))
    ((listp val)
     (if (eq (first val) :CONST)
         (second val)
         (get-mem vm (resolve_addr vm val))))
    (t (error "Val invalide : ~S" val))))

(defun write_value (vm dest val)
  (let ((target (if (listp dest) (resolve_addr vm dest) dest)))
    (if (symbolp target)
        (set-prop vm target val)
        (set-mem vm target val))))

(defun afficher_registres (vm)
  (format t "~%=== État de la VM : ~S ===~%" vm)
  (format t " [Registres] R0: ~4D | R1: ~4D | R2: ~4D | R3: ~4D~%"
          (get-prop vm :R0) (get-prop vm :R1)
          (get-prop vm :R2) (get-prop vm :R3))
  (format t " [Piles/PC]  PC: ~4D | SP: ~4D | BP: ~4D | FP: ~4D | LC: ~4D~%"
          (get-prop vm :PC) (get-prop vm :SP)
          (get-prop vm :BP) (get-prop vm :FP)
          (get-prop vm :LC))
  (format t " [Drapeaux]  FLT:   ~A | FEQ:   ~A | FGT: ~A~%"
          (get-prop vm :FLT) (get-prop vm :FEQ) (get-prop vm :FGT))
  (format t "=============================~%")
  (values))