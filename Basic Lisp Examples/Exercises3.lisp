;Reyes Fragoso Roberto

;Ejercicio 1
(defun Eleminpos (e l pos &optional cont)
  (cond ((null l) nil)
	((and (eql e (car l)) (eql pos cont)) T)
	(t (Eleminpos e (cdr l) pos (+ cont 1)))))

(ElemInPos 'a '(a b c d) 0 0);=> T

;Ejercicio 2
(defun Inicioen (l e)
  (cond ((null l) (cons e nil))
	((eq e (car l)) (cons e l))
	(t (cons (car l) (Inicioen (cdr l) e)))))

(Inicioen '(b c d) 'a)
	
;Ejercicio 3
(defun Terminaen (l e)
  (cond ((null l) (cons e nil))
	((eq e (car l))  (cons e nil))
	(t (cons (car l) (Terminaen (cdr l) e)))))

(Terminaen '(a b c) 'a);=> (A)

;Ejercicio 4
(defun Primerimpar (l &optional cont)
  (cond ((null l) nil)
	((oddp (car l)) (list (car l) cont))
	(t (Primerimpar (cdr l) (+ cont 1)))))

(Primerimpar '(2 5 2 5 7) 0) ;=> (5 1)

;Ejercicio 5 
(defun UltimoAux (l)
  (cond ((null l) nil)
	((>= (car l) 0) (car l))
	 (t (UltimoAux (cdr l)))))

(defun Contar (e l &optional cont)
  (cond ((null l) (list cont e))
	((eql e (car l)) (Contar e (cdr l) (+ cont 1)))
	 (t (Contar e (cdr l) (+ cont 0)))))

(defun Ultimo (l)
   (Contar (UltimoAux (reverse l)) l 0))

(Ultimo '(2 3 2 3 -2)) ;=>(2 3)

;Ejercicio 6 
(defun Conteo (l &optional n s)
  (cond ((null l) (list n s))
	((numberp (car l)) (Conteo (cdr l) (+ n 1) (+ s 0)))
	((listp (car l)) (Conteo (cdr l) (+ n 0) (+ s 1)))
	(t (Conteo (cdr l) (+ n 0) (+ s 0)))))

(Conteo '((1 2) 1 2 3 (1 1 1)) 0 0) ;=> (3 2)

;Ejercicio 7
(defun Aplana (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (Aplana (cdr l))))
        (t (append (Aplana (car l)) (Aplana (cdr l))))))

(Aplana '((a b) c d ((e f g)) h)) ;=> (A B C D E F G H)

;Ejercicio 8
(defun Diagonal (l &optional c)
  (cond ((null l) nil)
        ((listp (car l)) (cons (nth c (car l)) (Diagonal (cdr l) (+ c 1))))
        (t (Diagonal (car l) (+ c 0)))))

(Diagonal '((a b) (c d)) 0) ;=> (A D)

;Ejercicio 9
(defun tipo (l)
  (cond ((null l) nil)
	((null (car l)) (cons 'N (Tipo (cdr l))))
        ((listp (car l)) (cons 'L (Tipo (cdr l))))
        (t (cons 'A (Tipo (cdr l))))))

(Tipo '(1 ( ) a  (1 2 a) b c)) ;=> (A N A L A A)

;Ejercicio 10 
(defun SumaNumerica (l)
  (cond ((null l) 0)
	((not(numberp (car l))) (SumaNumerica (cdr l)))
        (t (+ (car l) (SumaNumerica (cdr l))))))

(Sumanumerica '(1 a 2 c 4 d));=> 7

;Ejercicio 11
(defun FiltraVocales (l)
  (cond ((null l) nil)
        ((atom (car l)) (if (or (equal 'a (car l))
       				(equal 'e (car l))
       				(equal 'i (car l))
       				(equal 'o (car l))
       				(equal 'u (car l)))
       			    (FiltraVocales (cdr l))
       			    (cons (car l) (Filtravocales (cdr l)))))
        (t (append (FiltraVocales (car l)) (FiltraVocales (cdr l))))))

(FiltraVocales '((A E I) 1 2 a u u)) ;=> (1 2)

;Ejercicio 12
(defun FiltraMultiplos (l n)
  (cond ((null l) nil)
	((eql n (car l)) (FiltraMultiplos (cdr l) n))
        (t (cons (car l) (FiltraMultiplos (cdr l) n)))))

(Filtramultiplos '(1 2 3 3 4 6) 3) ;=> (1 2 4 6)

;Ejercicio 13
(defun Celdas (l &optional n)
  (cond ((null l) n)
       	((atom (car l)) (Celdas(cdr l) (+ n 1)))
       	(t (+ (Celdas (car l) (+ n 0)) (Celdas (cdr l) (+ n 1))))))

(Celdas '(((1)) 2 3 4 5) 0) ;=> 7

;Ejercicio 14
(defun Implica (&rest a)
  (cond ((null a) t)
       	(t (AND (car a) (Implica (cdr a))))))

(Implica T T nil t) ;=> Nil

;Ejercicio 15
(defun matrix-multiply (m1 m2)
 (mapcar
  (lambda (row)
   (apply #'mapcar
    (lambda (&rest column)
      (apply #'+ (mapcar #'* row column))) m2)) m1))

(matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4))) ;=> ((-7 -6 11) (-17 -20 25))

;Ejercicio 16
(defun Encontrar (&rest argumentos)
  (cond ((null lista) nil)
	((eql elem (car lista)) lista)
	(t (append(Encontrar elem (cdr lista))))))

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

;Ejercicio 23
(defun qsort (l)
   (cond
   ((null l) nil)
   (t (append
      (qsort (listMenor (car l) (cdr l)))
      (cons (car l) nil)
      (qsort (listMayor (car l) (cdr l)))))))

(defun listMenor (a b)
    (cond
    (( or (null a) (null b)) nil)
    (( < a (car b)) (listMenor a (cdr b)))
    (t (cons (car b) (listMenor a (cdr b))))))

(defun listMayor (a b)
    (cond
    (( or ( null a)(null b)) nil)
    (( >= a (car b)) (listMayor a (cdr b)))
    (t (cons (car b) (listMayor a (cdr b))))))

(qsort '(6 5 4 3 1)) ;=> (1 3 4 5 6)
