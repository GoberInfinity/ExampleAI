;Reyes Fragoso Roberto


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
