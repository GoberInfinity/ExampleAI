;Reyes Fragoso Roberto

;Ejercicio 16
(defun Encontrar (elem lista)
  (cond ((null lista) nil)
	((eql elem (car lista)) lista)
	(t (append(Encontrar elem (cdr lista) 0)))))

(Encontrar 'a '(b c d a  c b)) ;=> (A C B)

;Ejercicio 17
(defun Cambia (lista e1 e2)
  (cond ((null lista) nil)
	((eql (car lista) e1) (cons e2 (Cambia (cdr lista) e1 e2)))
	(t (cons (car lista) (Cambia (cdr lista) e1 e2)))))

(trace Cambia)
(Cambia '(a a c d) 'a 'b)

;Ejercicio 19
(defun Mapeo (function l1 l2)
  (if l1
      (cons (funcall function (car l1) (car l2))
	    (Mapeo function (cdr l1) (cdr l2)))
      nil))

(Mapeo #'+ '(1 2 3 4) '(1 1 1 1)) ;(2 3 4 5)

;Ejercicio 20
(defun Aplana (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (Aplana (cdr l))))
        (t (append (Aplana (car l)) (Aplana (cdr l))))))

(Aplana '((a b) c d ((e f g)) h)) ;=> (A B C D E F G H)

;Ejercicio 21 
(defun Elimina (lista elemento)
  (cond ((null lista) nil)
	 ((not (numberp (car lista)))  (Elimina (cdr lista) elemento))
	 ((<= (car lista) elemento) (Elimina (cdr lista) elemento))
	 (t (cons (car lista) (Elimina (cdr lista) elemento)))))

(trace Elimina)
(Elimina '(1 2 a b 5) 2);=> (5)

;Ejercicio 22
(defun PegaYCambia (lista1 lista2 a b)
  (Cambia (PegaYCambiaAux lista1 lista2) a b)
  )

(defun PegaYCambiaAux (lista1 lista2)
  (cond ((null lista1) lista2)
	(t (cons (car lista1) (PegaYCambiaAux (cdr lista1) lista2)))))

(trace Pegaycambiaaux)
(trace Cambia)
(PegaYCambia '(a a c d) '(a a a a) 'a 'x) ;=>(X X C D X X X X)

(defun Qsort (lista)
  
