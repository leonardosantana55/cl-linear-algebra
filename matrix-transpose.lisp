(defparameter *matrix* (make-array '(2 2) :initial-contents '((2 5) (1 3))))
(defparameter *vector* (vector 1 2))

(defun matrix-transpose (input)
  (let* ((m (array-dimension input 0))
         (n (array-dimension input 1))
         (output (make-array (list n m) :initial-element nil)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref output i j)
              (aref input j i))))
    output))

(defparameter *m-t* (matrix-transpose *matrix*))


(+ (* (aref *vector* 0) (aref *m-t* 0 0))
   (* (aref *vector* 1) (aref *m-t* 1 0)))

(+ (* (aref *vector* 0) (aref *m-t* 0 1))
   (* (aref *vector* 1) (aref *m-t* 1 1)))

(defun matrix-multiplication (matrix vector)
  (let ((m-t (matrix-transpose matrix))
        (m (array-dimension matrix 0))
        (n (array-dimension matrix 1))
        (output (make-array (array-total-size vector) :initial-element 0)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref output i)
              (+ (* (aref vector j) (aref m-t j i))
                 (aref output i)))))
    output))

(matrix-multiplication *matrix* *vector*)


(defun matrix-multiplication (matrix vector)
  (let* ((m (array-dimension matrix 0))
        (n (array-dimension matrix 1))
        (v (array-total-size vector))
        (output (make-array v)))
    (if (= m v)
        (dotimes (i v)
          (dotimes (j n)
            (setf (aref output n)
                  (+ (aref output n)
                     (* (aref matrix i j)
                        (aref vector i)))))))))


(matrix-multiplication *matrix* *vector*)

 
;;eu vou multiplcar vector vezes 


(aref *vector* 0)


(make-array (list 2 3))
