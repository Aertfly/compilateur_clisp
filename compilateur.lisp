(defun compiler (proc e1 ... en env)
    (empiler e1 env)
    (PUSH R0)
    (compiler e2 env)
    (PUSH R0)
    ...
    (compiler en env)
    (PUSH R0)
)