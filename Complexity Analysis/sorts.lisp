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
    (loop for x from 1 to (1- (length scenario)) do
         (let*((key (nth *i* newList))
               (temporalValuei '()))
           (setq *j* (1- *i*))
           (loop while (and (>= *j* 0)(> (nth *j* newList) key)) do

                (setf temporalValuei (nth (1+ *j*) newList))
                (setf (nth (1+ *j*) newList) (nth *j* newList))
                (setf (nth *j* newList) temporalValuei)

                (setq *j* (1- *j*))
                (print newList)
                )
           (setq *i* (+ 1 *i*))
           ))
    newList)
  )

(trace Insertion-sort)
(insertion-sort '(5 4 3 2 1))

