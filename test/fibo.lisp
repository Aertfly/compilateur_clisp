(defun fibo (n)
  (if (< n 2)
      n
      (+ (fibo (- n 1)) 
         (fibo (- n 2)))))

(defun factorielle (n)
  (if (<= n 1)
      1
      (* n (factorielle (- n 1)))))

(defun test-append ()
  (append '(1 2 3) '(4 5 6)))

(print "TEST FIBO")
(print (fibo 10))

;(print "TEST APPEND")
;(print (test-append))

;(print "TEST FACTORIELLE")
;(factorielle 50)
