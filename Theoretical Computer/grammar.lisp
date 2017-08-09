(defparameter *vector-conocimiento* (make-array 17
                                                :adjustable T
                                                :element-type 'list))
(defparameter *terminals* nil)

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun adj* ()
  (if (= (random 2) 0) nil (append (adj) (adj*))))

(defun pp* ()
  (if (random-elt '(t nil)) (append (pp) (pp*)) nil))

(defun verb-phrase ()
  (append (verb) (noun-phrase)))

(defun noun-phrase ()
  (append (article) (adj*) (noun) (pp*)))

(defun pp ()
  (append (prep) (noun-phrase)))

(defun adj ()
  (one-of (second (nth 1 *terminals*))))

(defun prep ()
  (one-of (second (nth 2 *terminals*))))

(defun article ()
  (one-of (second (nth 3 *terminals*))))

(defun noun ()
  (one-of (second (nth 4 *terminals*))))

(defun verb ()
  (one-of (second (nth 5 *terminals*))))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choice)
  "Choose an element for a list at random."
  (elt choice (random (length choice))))

(defun read-user (filename)
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
      (adjust-array *vector-conocimiento* numrows)
      (dotimes (row numrows *vector-conocimiento*)
        (setf (aref *vector-conocimiento* row) (read stream nil nil))))
    (setf *terminals* (second (coerce *vector-conocimiento* 'list)))
    (print (second (nth 1 *terminals*)))
    (print (second (nth 2 *terminals*)))
    (print (second (nth 3 *terminals*)))
    (print (second (nth 4 *terminals*)))
    (print (second (nth 5 *terminals*)))
    ))

(trace verb noun article prep adj pp noun-phrase verb-phrase pp* adj* sentence)

(defun main ()
  (read-user "c:/Users/Gober/Desktop/lol.txt")
  (sentence)
  )

(main)

