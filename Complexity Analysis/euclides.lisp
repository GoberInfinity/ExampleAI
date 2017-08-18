(defun euclides (m n)
  (loop while (not (zerop n)) do
     (let ((r (mod m n)))
       (setf m n)
       (setf n r)))
  m)

(euclides 270 192)



