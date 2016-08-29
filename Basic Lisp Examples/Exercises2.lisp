;Reyes Fragoso Roberto

;Ejercicio 1
(defun ElemInPos (elemento  posicion lista)
  (eq elemento (nth posicion lista)))

(ElemInPos 'a 0 '(a b c d)) ;=> T

;Ejercicio 2
(defun Inicio-en (argumento lista)
  (cons argumento lista))

(Inicio-en 'a '(b c d)) ;=> (A B C D)

;Ejercicio 3
(defun Termina-en (argumento lista)
  (let ((respuesta '()))
    (loop for i in (reverse lista)
       while (not (equal argumento i))
       do (setq respuesta (cons i respuesta)))
    (print respuesta)))

(Termina-en 'a '(a b c)) ;=> (B C)

;Ejercicio 4
(defun Primer-impar (lista)
  (let ((indice -1))
    (loop for i in lista
       do (setq indice(+ indice 1))
       if (oddp i)
       return (list i indice))))

(Primer-impar '(2 5 2 5 7)) ;=> (5 1)

;Ejercicio 5
(defun Ultimo-impar (lista)
  (let ((respuesta '()) (veces 0) (ocurrio 0) (rlista (reverse lista)))
    (dolist (elemento rlista respuesta) ;Primer doList para saber el primer impar
      (if (>= elemento 0)
	  (setq veces (+ veces 1)))
      (when (>= elemento 0)
	(if (eql veces 1)
	    (setq respuesta elemento))))
    (print respuesta)
    (dolist (elemento2 lista ocurrio) ;Segundo doList no. de repeticiones 
       (when (eql respuesta elemento2 )
	 (setq ocurrio (+ ocurrio 1))))))

(Ultimo-impar '(2 3 2 3 -2)) ;=> 3 2

;Ejercicio 6
(defun Conteo (lista)
   (let ((eNumerico 0) (eSublista 0))
     (dolist (elemento lista eNumerico)
       (when (atom elemento)
	 (setq eNumerico (+ eNumerico 1))))
     (setq eSublista (- (length lista) eNumerico)) 
     (list eNumerico eSublista)))

(Conteo '((1 2) 1 2 3 (1 1 1))) ;=> (3 2)

;Ejercicio 7
(defun Aplana (l)
  (let ((res nil))
    (loop for x in l
       if (listp x)
       do (loop while (not (equal x nil))
	     do (setf x (reduce #'append
				(loop for y in x
				   if (atom y)
				   do (setq res (append res (cons y nil)))
				   else
				   collect y))))
       else
       do (setq res (append res (cons x nil)))
       finally (return res))))

(Aplana '((a b) c d ((e f g)) h)) ;=> (A B C D E F G H)


;Ejercicio 8
(defun Diagonal (lista)
  (loop for i from 0 for elemento in lista
     collect ( nth i elemento)))

(Diagonal '((a b) (c d))) ;=> (A D)

;Ejercicio 9 
(defun TipoLista (lista)
  (let ((respuesta '()))
    (dolist (elemento lista (reverse respuesta))
      (cond ((null elemento) (setq respuesta (cons 'N respuesta)))
	    ((listp elemento) (setq respuesta (cons 'L respuesta)))
	    (t (setq respuesta (cons 'A respuesta))))))) 
   
(Tipolista '(1 ( ) a  (1 2 a) b c)) ;=> (A N A L A A)

;Ejercicio 10 
(defun SumaNumerica (lista)
  (loop for elemento in lista
     if(numberp elemento)
        sum elemento))

(Sumanumerica '(1 a 2 c 4 d)) ;=> 7

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

(FiltraVocales '((A E I) 1 2 a u u)) ;(1 2)

;Ejercicio 12 
(defun FiltraMultiplos (lista numero)
  (loop for elemento in lista
     if(not (eq 0 (mod elemento numero)))
        collect elemento))

(Filtramultiplos '(1 2 3 3 4 6) 3)

;Ejercicio 13
(defun Celdas (l n)
  (cond ((null l) n)
	((atom (car l)) (Celdas(cdr l) (+ n 1)))
	(t (+ (Celdas (car l) (+ n 0)) (Celdas (cdr l) (+ n 1))))))

(defun Testing (lista)
  (Celdas lista 0))

(Testing '(((1)) 2 3 4 5 )) ;=> 7

;Ejercicio 14
(defun Implica (&rest argumentos)
             (every #'identity argumentos))

(Implica T T) ;=> T

;Ejercicio 15
(defun Multiplicar (a-matrix  b-matrix)
  (let ((result (make-array
          (list (nth 0 (array-dimensions a-matrix))
                (nth 1 (array-dimensions b-matrix))))) 
	(m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions b-matrix)))
        (common (nth 0 (array-dimensions b-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j) 0.0)
        (dotimes (k common)
          (incf (aref result i j)
                (* (aref a-matrix i k) (aref b-matrix k j))))))))

(defun Mult  (&key fm sm)
  (if (not (eq (length (first fm)) (length sm)))
      (return-from mm nil))
      (let* (( a (make-array (list (length fm)
                    (length (first fm)))
			     :initial-contents fm))
	     ( b (make-array (list (length sm)
                    (length (first sm)))
			     :initial-contents sm)))
	  (Multiplicar a b)))

(Mult :fm '((1 2 3)(1 2 3)(1 2 3)) :sm '((1 2)(1 2)(1 2))) ;=> #2A((6.0 12.0) (6.0 12.0) (6.0 12.0))



