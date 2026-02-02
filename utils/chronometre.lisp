(defmacro chronometre (expression)
  (let ((debut (gensym)))
    `(let ((,debut (get-internal-real-time))
           (resultat ,expression))
       (format t "~&~5F s | ~A~%" 
               (/ (- (get-internal-real-time) ,debut) (float internal-time-units-per-second))
               ',expression)
       resultat)))
