
; 
(defun vm_exec (vm) )

; 
(defun vm_load_exec (vm) )
; 
(defun load (vm) )

;;;;;

(defun vm_get_registre (vm) )
(defun vm_set_registre (vm) )

; Manipulation mémoire/registres
(defun vm_exec_inst_LOAD (src dest) )
(defun vm_exec_inst_STORE (src dest) )

(defun vm_exec_inst_MOVE (src dest) )

(defun vm_exec_inst_ADD (src dest) )
(defun vm_exec_inst_SUB (src dest) )
(defun vm_exec_inst_MUL (src dest) )
(defun vm_exec_inst_DIV (src dest) )
(defun vm_exec_inst_INCR (dest) )
(defun vm_exec_inst_DECR (dest) )

; Pile
(defun vm_exec_inst_PUSH (src) )
(defun vm_exec_inst_POP (dest) )

; Labels/sauts 
(defun vm_exec_inst_LABEL (label) )
(defun vm_exec_inst_JMP   (label) )

; Appels et retours
(defun vm_exec_inst_JSR (label) )
(defun vm_exec_inst_RTN () )

; Comparaisons
(defun vm_exec_inst_CMP (src1 src2) )

(defun vm_exec_inst_JGT (label) )
(defun vm_exec_inst_JGE (label) )

(defun vm_exec_inst_JLT (label) )
(defun vm_exec_inst_JLE (label) )

(defun vm_exec_inst_JEQ (label) )
(defun vm_exec_inst_JNE (label) )

; Tests logiques
(defun vm_exec_inst_TEST  (src) )
(defun vm_exec_inst_JTRUE (label) )
(defun vm_exec_inst_JNIL  (label) )

; Contrôle neutre
(defun vm_exec_inst_NOP () )
(defun vm_exec_inst_HALT () )




