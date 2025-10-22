(defparameter *matrix* (make-array '(2 2) :adjustable t :initial-contents '((2 5) (1 3))))
(defparameter *vector* (vector 1 2))

(defmethod sum ((v1 vector) (v2 vector) &key (out 'natural))
  (dotimes (i (array-total-size v1) v1)
    (case out
      ((real) (setf (aref v1 i)
                    (+ (aref v1 i)
                      (aref v2 i))))
       ((natural) (setf (aref v1 i)
                       (if (< (+ (aref v1 i)
                                 (aref v2 i)) 0)
                           0
                           (+ (aref v1 i)
                              (aref v2 i))))))))

(sum *vector* *vector* :out 'real)


(defmethod magnitude ((vector vector))
  (let ((sum 0))
    (dotimes (i (array-total-size vector) (sqrt sum))
      (setf sum
            (+ sum
               (expt (aref vector i) 2))))))

(defmethod angle ((adjacent vector) (hipotenuse vector) &key (unit 'radians))
  "returns in radians the angle betwheen two vectors"
  (let ((result
        (acos (/ (magnitude adjacent)
                 (magnitude hipotenuse)))))
    (case unit
      ((radians) result)
      ((degrees) (* result (/ 180 pi)))
      (otherwise "wrong unit of measure"))))

(defmethod scalar-multiplication ((vector vector) scalar)
  (let ((output (make-array (array-total-size vector) :initial-element 0)))
    (dotimes (i (array-total-size vector))
      (setf (aref output i)
            (* scalar (aref vector i))))
    output))

(defmethod unit ((vector vector))
  (scalar-multiplication
   vector
   (/ 1 (magnitude vector))))

(defmethod dot-product ((v1 vector) (v2 vector))
  (let ((lst nil))
    (dotimes (i (array-total-size v1) (apply #'+ lst))
      (setf lst (cons (* (aref v1 i)
                         (aref v2 i))
                      lst)))))

(defmethod matrix-multiplication ((vector vector) matrix &key (round nil))
  (let ((m-t (m-transpose matrix))
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

;; MATRIX METHODS
(defmethod m-transpose ((input array))
  (let* ((m (array-dimension input 0))
         (n (array-dimension input 1))
         (output (make-array (list n m) :initial-element 0)))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref output i j)
              (aref input j i))))
    output))

(defmethod m-sum ((matrix array) &rest matrices)
  (let* ((m (array-dimension matrix 0))
         (n (array-dimension matrix 1))
         (output (make-array (list m n) :initial-element 0)))
    (push matrix matrices)
    (dotimes (item (length matrices))
      (dotimes (i n)
        (dotimes (j m)
          (setf (aref output i j)
                (+ (aref (nth item matrices) i j)
                   (aref output i j))))))
    output))

(defmethod m-scalar-multiplication ((matrix array) scalar)
  (let* ((m (array-dimension matrix 0))
         (n (array-dimension matrix 1))
         (output (make-array (list m n) :initial-element 0)))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref output i j)
              (* (aref matrix i j)
                 scalar))))
    output))

(defmethod m-get-vector ((matrix array) index &key (axis 'row))
  (case axis
    ((row) (let* ((size (array-dimension matrix 0))
                 (output (make-array size :initial-element 0)))
             (dotimes (i size output)
               (setf (aref output i)
                     (aref matrix index i)))))
    ((col) (let* ((size (array-dimension matrix 1))
                 (output (make-array size :initial-element 0)))
             (dotimes (i size output)
               (setf (aref output i)
                     (aref matrix i index)))))
    ((otherwise "wrong axis argument"))))

(m-get-vector *matrix* 1 :axis 'col)
;;TODO tem que testar isso aqui'



;; SPECIAL MATRICES FACILITIES
(defun rotation-matrix (angle)
  (make-array '(2 2) :initial-contents `((,(cos angle) ,(* -1 (sin angle)))
                                         (,(sin angle) ,(cos angle)))))
(rotation-matrix pi)


;; TESTs
(let ((test (make-array '(2 2) :initial-element nil :adjustable t)))
  (setf (aref test 0 0) 0)
  test)

(m-transpose (m-transpose #2a((16 6 0) (6 10 -3) (0 -3 22))))


(array-row-major-index *matrix* 0 1)

(defparameter *test* (make-array '(3 3) :initial-contents `(,(vector 1 2 3)
                                                            ,(vector 1 2 3)
                                                            ,(vector 1 2 3))))


(if (array-in-bounds-p *matrix* 0 5)
    (print t))








;; cartesian plane matrix

(defun print-matrix (matrix)
  (dotimes (i (array-dimension matrix 0))
    (dotimes (j (array-dimension matrix 1))
      (format t "~a" (aref matrix i j)))
    (format t "~%"))
  (format t "~%~a~%" *p-vector*))

(defun main ()
  (defparameter *p-matrix* (make-array '(24 24) :initial-element "  ."))
  (defparameter *p-vector* (vector 10 2)) ;; origin
  (defparameter *r-vector* (vector 4 4)) ;; destination vector o radius of the circle
  (defparameter *r-matrix* (rotation-matrix (/ pi 5))) ;; rotation matrix

    
  (let ((counter 0)
        (limit 7))
    (dotimes (i 50 nil)
      (print-matrix *p-matrix*)
      (setf (aref *p-matrix* (aref *p-vector* 0) (aref *p-vector* 1)) "000")
      (print-matrix *p-matrix*)
      ;; (setf (aref *p-matrix* (aref *p-vector* 0) (aref *p-vector* 1)) "  .") 
      ;; soma o vector raio ao vector p
      (setf *p-vector* (sum *p-vector* *r-vector*))
      (setf *r-vector* (matrix-multiplication *r-vector* *r-matrix* :round t))
      (if (>= counter limit)
          (progn
            (setf *r-vector*
                  (sum *r-vector*
                       (scalar-multiplication (vector 1 1) -1)))
            (setf limit
                  (round (* limit 0.8)))
            (setf counter 0)))    
      ;; transforma o vetor raio
      (sleep 0.1)
      (incf counter))))

(main)



;; TODO criar uma classe que constroi um vetor e que possui vários métodos
;; já estabelicidos aqui neste arquivo.
;; A idéia que eu tive foi, por exemplo, um vetor instanciado possui um
;; metodo que calcula o seu produto interno em comparação a um outro vetor
;; informado como parametro
