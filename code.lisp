(defun fibo (n)
  (if (< n 2)
      n
      (+ (fibo (- n 1)) 
         (fibo (- n 2)))))

(defun factorielle (n)
  (if (<= n 1)
      1
      (* n (factorielle (- n 1)))))

(print (fibo 10))
(print (factorielle 50))
