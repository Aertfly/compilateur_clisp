; par convention, résultat des fonctions stocké dans R0

(PUSH <n>)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB @u? R2)
(DECR R2)
(PUSH R2)
(PUSH R1)
(JUMP )
