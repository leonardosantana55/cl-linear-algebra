(defmethod magnitude ((v vector))
  (let ((sum 0))
    (dotimes (i (array-total-size v) (sqrt sum))
      (setf sum
            (+ sum
               (expt (aref v i) 2))))))



(let ((v (vector 1 2 3 3)))
  (print (magnitude v)))



(class-of '#(0 0))


;; É possível adicionar métodos à objetos nativos do CL, como por exemplo o vector
;; Um método se difere de uma definição de função comum porque aceita especificação de
;; tipo de objeto na lista de parâmetros(ou lambda list)
