;Reyes Fragoso Roberto

;Problema 1
(defun ultimoElemento (l)
  (car (last l)))

(defun palindromo (l)
  (if (equal nil l)
      t
      (let ((inicio (first l))
	    (final (ultimoElemento l)))
	(if (equal inicio final)
	    (palindromo (cdr (butlast l)))
	    nil))))

(palindromo '(o o s o o)) ; => T

;Problema 
(defun listRotate (l &key (r nil) (lf nil))
  (if r (print 'r) (print 'lf)))

(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))

(listRotate '(a b c d e f g h) :r 3)

(subseq "Hola prro" 0 1)


