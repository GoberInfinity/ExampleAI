;;Insertion Sort
(defparameter *worst-case* '(5 4 3 2 1))
(defparameter *best-case* '(1 2 3 4 5))
;;[Global] Allow to see the steps followed by the algorithm
(defparameter *n* 0)
(defparameter *j* 0)
(defparameter *i* 1)

(defun insertion-sort (scenario)
  (setq *n* 0)
  (setq *j* 0)
  (setq *i* 1)
  (let* ((newList (copy-list scenario)))
    (loop for j from 1 to (1- (length scenario)) do
         (let*((key (nth j newList)))
           (setq *i* (1- j))
           (loop while (and (>= *i* 0)(> (nth *i* newList) key)) do
                (setf (nth (1+ *i*) newList) (nth *i* newList))
                (setq *i* (1- *i*)))
           (setf (nth (1+ *i*) newList) key)
           (print newList)
           ))
    newList)
  )

(insertion-sort *worst-case*)

(defun merge-sort (A p q r)
  (let* ((n1 (1+ (- q p)))
         (n2 (- r q))
         (ii '1)
         (jj '1)
         (LA (make-array (1+ n1)))
         (RA (make-array (1+ n2))))

    (loop for i from 1 to n1 do
         (setf (aref LA (1- i)) (aref A (1- (+ p i)))))

    (loop for j from 1 to n2 do
         (setf (aref RA (1- j)) (aref A (+ q j))))

    (setf (aref LA n1) MOST-POSITIVE-FIXNUM)
    (setf (aref RA n2) MOST-POSITIVE-FIXNUM)

    (loop for k from p to r do
         (progn
           (if (<= (aref LA ii)(aref RA jj))
               (print "YOlaso")
               (print "asfds")))
         )



    ))

(merge-sort #(9 8 7 6 5 4 3 2 1) '0 '4 '8)


