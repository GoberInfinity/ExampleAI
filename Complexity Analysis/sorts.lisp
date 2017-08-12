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

