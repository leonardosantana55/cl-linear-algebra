(defparameter *matrix* (make-array '(2 2) :initial-contents '((2 5) (1 3))))
(defparameter *vector* (vector 1 2))

(defun vector-sum (v1 v2)
  (dotimes (i (array-total-size v1) v1)
    (setf (aref v1 i)
          (+ (aref v1 i)
             (aref v2 i)))))
(vector-sum *vector* *vector*)

(defun vector-magnitude (vector)
  (let ((sum 0))
    (dotimes (i (array-total-size vector) (sqrt sum))
      (setf sum
            (+ sum
               (expt (aref vector i) 2))))))

(defun vector-angle (adjacent hipotenuse &key (unit 'radians))
  "returns in radians the angle betwheen two vectors"
  (let ((result
        (acos (/ (vector-magnitude adjacent)
                 (vector-magnitude hipotenuse)))))
    (case unit
      ((radians) result)
      ((degrees) (* result (/ 180 pi)))
      (otherwise "wrong unit of measure"))))
(vector-angle (vector 2 0) (vector 2 2) :unit 'degrees)

(defun vector-scalar-multiplication (vector scalar)
  (let ((output (make-array (array-total-size vector) :initial-element 0)))
    (dotimes (i (array-total-size vector))
      (setf (aref output i)
            (* scalar (aref vector i))))
    output))
(vector-scalar-multiplication *vector* 2)

(defun vector-unit (vector)
  (vector-scalar-multiplication
   vector
   (/ 1 (vector-magnitude vector))))
(vector-magnitude (vector-unit (vector 2 3)))


(defun dot-product (v1 v2)
  (let ((lst nil))
    (dotimes (i (array-total-size v1) (apply #'+ lst))
      (setf lst (cons (* (aref v1 i)
                         (aref v2 i))
                      lst)))))
(dot-product (vector 0 2) (vector 2 0))

(defun matrix-transpose (input)
  (let* ((m (array-dimension input 0))
         (n (array-dimension input 1))
         (output (make-array (list n m) :initial-element nil)))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref output i j)
              (aref input j i))))
    output))
(defparameter *m-t* (matrix-transpose *matrix*))

(defun matrix-vector-multiplication (matrix vector &key (round nil))
  (let ((m-t (matrix-transpose matrix))
        (m (array-dimension matrix 0))
        (n (array-dimension matrix 1))
        (output (make-array (array-total-size vector) :initial-element 0)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref output i)
              (+ (* (aref vector j) (aref m-t j i))
                 (aref output i)))))
    (if (eql round nil)
        output
        (dotimes (i (array-total-size output) output)
          (setf (aref output i)
                (round (aref output i)))))))
(matrix-vector-multiplication *matrix* *vector*)


(defun (vector)
  (dotimes (i (array-total-size v) v)
    (setf (aref v i) (round (aref v i)))))

(dot-product *vector* *vector*)
(* (vector-magnitude *vector*)
   (vector-magnitude *vector*)
   (cos (vector-angle *vector* *vector*)))

(defun matrix-rotation (angle)
  (make-array '(2 2) :initial-contents `((,(cos angle) ,(asin angle))
                                         (,(sin angle) ,(cos angle)))))
(matrix-rotation 1)


;; TESTs

(dotimes (i 4 nil)
  (print (matrix-)))

(setf *vector* (matrix-vector-multiplication (matrix-rotation 1) *vector* :round t))

;; (defparameter *p-matrix* (make-array '(16 16) :initial-element "  ."))
;; (defparameter *p-vector* (vector 8 8))
;; (defparameter *r-vector* (vector 2 2))
;; (defparameter *r-matrix* (make-array '(2 2) :initial-contents '((-1 0) (0 -1))))

;; (defun print-matrix (matrix)
;;   (dotimes (i (array-dimension matrix 0))
;;     (dotimes (j (array-dimension matrix 1))
;;       (format t "~a" (aref matrix i j)))
;;     (format t "~%"))
;;   (format t "~%~a~%" *p-vector*))


;; (dotimes (i 5 nil)
;;   (print *r-vector*)
;;   (setf *r-vector* (matrix-vector-multiplication *r-matrix* *r-vector*)))

;; (dotimes (i 10 nil)
;;   (print-matrix *p-matrix*)
;;   (setf (aref *p-matrix* (aref *p-vector* 0) (aref *p-vector* 1)) "00.")
;;   (print-matrix *p-matrix*)
;;   (setf (aref *p-matrix* (aref *p-vector* 0) (aref *p-vector* 1)) "  .")
;;   ;; soma o vector raio ao vector p
;;   (setf *p-vector* (vector-sum *p-vector* *r-vector*))
;;   (setf *r-vector* (matrix-vector-multiplication *r-matrix* *r-vector*))
;;   ;; transforma o vetor raio
;;   (sleep 1))



;; TODO criar uma classe que constroi um vetor e que possui vários métodos
;; já estabelicidos aqui neste arquivo.
;; A idéia que eu tive foi, por exemplo, um vetor instanciado possui um
;; metodo que calcula o seu produto interno em comparação a um outro vetor
;; informado como parametro
