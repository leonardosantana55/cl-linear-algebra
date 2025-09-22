(defun matrix-transpose (input)
  (let* ((m (array-dimension input 0))
         (n (array-dimension input 1))
         (output (make-array (list n m) :initial-element 0)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref output i j)
              (aref input j i))
        (break)))
    output))

(defparameter *matrix* (make-array '(3 3) :initial-contents '((1 2 3) (1 2 3) (1 2 3))))

(matrix-transpose *matrix*)

