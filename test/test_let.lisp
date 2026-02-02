;; ============================================
;; Tests de compilation LET
;; ============================================

(print "Test 1 : Let simple -> 42")
(defun test-let-simple ()
  (let ((x 42))
    x))
(print (test-let-simple))

;; --------------------------------------------

(print "Test 2 : Let calcul -> 15")
(defun test-let-calcul ()
  (let ((x 10))
    (+ x 5)))
(print (test-let-calcul))

;; --------------------------------------------

(print "Test 3 : Let multi -> 10")
(defun test-let-multi ()
  (let ((a 3)
        (b 7))
    (+ a b)))
(print (test-let-multi))

;; --------------------------------------------

(print "Test 4 : Let imbrique -> 30")
(defun test-let-imbrique ()
  (let ((x 10))
    (let ((y 20))
      (+ x y))))
(print (test-let-imbrique))

;; --------------------------------------------

(print "Test 5 : Let shadow -> 5")
(defun test-let-shadow ()
  (let ((x 100))
    (let ((x 5))
      x)))
(print (test-let-shadow))

;; --------------------------------------------

(print "Test 6 : Let param -> 30")
(defun test-let-param (n)
  (let ((x (+ n 10)))
    (* x 2)))
(print (test-let-param 5))

;; --------------------------------------------

(print "Test 7 : Let progn -> 555 puis 105")
(defun test-let-progn ()
  (let ((x 5))
    (print 555)
    (+ x 100)))
(print (test-let-progn))

;; --------------------------------------------

(print "Test 8 : Let profond -> 6")
(defun test-let-profond ()
  (let ((a 1))
    (let ((b 2))
      (let ((c 3))
        (+ a (+ b c))))))
(print (test-let-profond))

;; --------------------------------------------

(print "Test 9a : Let if vrai -> 12")
(defun test-let-if (n)
  (let ((x (* n 2)))
    (if (> x 10)
        x
        0)))
(print (test-let-if 6))

(print "Test 9b : Let if faux -> 0")
(print (test-let-if 3))

;; --------------------------------------------

(print "Test 10 : Let setq -> 15")
(defun test-let-setq ()
  (let ((x 10))
    (setq x (+ x 5))
    x))
(print (test-let-setq))

(print "FIN TESTS")
