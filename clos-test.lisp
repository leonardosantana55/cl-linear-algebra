(defmethod magnitude ((v vector))
  (let ((sum 0))
    (dotimes (i (array-total-size v) (sqrt sum))
      (setf sum
            (+ sum
               (expt (aref v i) 2))))))



(let ((v (vector 1 2 3 3)))
  (print (magnitude v)))



(class-of '#(0 0))


;; � poss�vel adicionar m�todos � objetos nativos do CL, como por exemplo o vector
;; Um m�todo se difere de uma defini��o de fun��o comum porque aceita especifica��o de
;; tipo de objeto na lista de par�metros(ou lambda list)
