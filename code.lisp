(defun chargeur (code-list vm)
  (let ((addr 0) (label-map nil) (lst nil) (inst nil) (resolved nil))
    ;; Première passe : construire la table des labels
    (setq lst code-list)
    (while lst
      (setq inst (car lst))
      (if (eq (car inst) 'LABEL)
          (setq label-map (cons (cons (car (cdr inst)) addr) label-map))
          (setq addr (+ addr 1)))
      (setq lst (cdr lst)))
    ;; Deuxième passe : résoudre les labels et charger en mémoire
    (setq addr 0)
    (setq lst code-list)
    (while lst
      (setq inst (car lst))
      (if (eq (car inst) 'LABEL)
          nil
          (progn
            (setq resolved (cons (car inst) (resolve-inst-args (cdr inst) label-map)))
            (set-mem vm addr resolved)
            (setq addr (+ addr 1))))
      (setq lst (cdr lst)))
    (set-prop vm :labels label-map)))
