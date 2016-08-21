;Examples of loop
(defun ElemInPos (elemento  posicion lista)
  (eq elemento (nth posicion lista)))

(ElemInPos 'a 0 '(a b c d))

(defun Inicio-en (argumento lista)
  (cons argumento lista))

(Inicio-en 'a '(b c d))

(defun Termina-EN (argumento lista)
  (reverse (cons argumento (reverse lista))))

(Termina-en 'd '(a b c))

(defun Primer-impar (lista)
  (let ((respuesta '()) (indice -1))
    (dolist (elemento lista respuesta)
      (setq indice (+ indice 1))
      (when (oddp elemento)
      (setq respuesta(cons indice (cons elemento respuesta)))))))

(Primer-impar '(2 3 2 5 7))

(defun Ultimo-impar (lista)
  (let ((respuesta '()) (veces 0) (ocurrio 0) (rlista (reverse lista)))
    (dolist (elemento rlista respuesta)
      (if (>= elemento 0) (setq veces (+ veces 1)))
      (when (>= elemento 0)
	(if (eql veces 1) (setq respuesta elemento))))
    (print respuesta)
     (dolist (elemento2 lista ocurrio)
       (when (eql respuesta elemento2 )
	 (setq ocurrio (+ ocurrio 1))))))

(Ultimo-impar '(2 3 2 3 -2))

(defun Conteo (lista)
   (let ((eNumerico 0) (eSublista 0))
     (dolist (elemento lista eNumerico)
       (when (numberp elemento)
	 (setq eNumerico (+ eNumerico 1))))
     (setq eSublista (- (length lista) eNumerico))
     (list eNumerico eSublista)))

(Conteo '((1 2) 1 2 3 (1 1 1)))

(defun Aplana (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (Aplana (cdr l))))
        (t (append (Aplana (car l)) (Aplana (cdr l))))))

(Aplana '((a b) c d ((e f g)) h))

(defun Diagonal (lista)
  (loop for i from 0 for elemento in lista
     collect ( nth i elemento)))

(Diagonal '((a b) (c d) (f g)))

(defun TipoLista (lista)
  (let ((respuesta '()))
    (dolist (elemento lista (reverse respuesta))
      (cond
	((null elemento) (setq respuesta (cons 'N respuesta)))
	((listp elemento) (setq respuesta (cons 'L respuesta)))
	(t  (setq respuesta (cons 'A respuesta))))))) 
   
(Tipolista '(1 ( ) a  (1 2 a) b c))

(defun SumaNumerica (lista)
  (loop for elemento in lista
     if(numberp elemento)
       sum elemento))

(Sumanumerica '(1 a 2 c 4 d))

(defun FiltraMultiplos (lista numero)
  (loop for elemento in lista
     if(not (eq 0 (mod elemento numero))) collect elemento))

(Filtramultiplos '(1 2 3 3 4 6) 3)

(defun Implica (&rest argumentos)
             (every #'identity argumentos))

(Implica T F F F T)

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

(FiltraVocales '((A E I) 1 2 a u u))

(defun Celdas (l n)
  (cond ((null l) n)
	((atom (car l)) (Celdas(cdr l) (+ n 1)))
	(t (+ (Celdas (car l) (+ n 0)) (Celdas (cdr l) (+ n 1))))))

(defun Testing (lista)
  (Celdas lista 0))

(Testing '(((1)) 2 3 4 5 ))

(defun multiply-two-matrices
       (a-matrix
        b-matrix)
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

(multiply-two-matrices #2a((0 0 1) (0 1 0) (1 0 0))
                       #2a((10 9) (8 7) (6 5)))


(Matriz '((0 0 1)) '(0 1 0))

(defun mm (&key fm sm)
  (if (not (eq (length (first fm)) (length sm)))
      (return-from mm nil))
      (let* (( a (make-array (list (length fm)
                    (length (first fm)))
			     :initial-contents fm))
	     ( b (make-array (list (length sm)
                    (length (first sm)))
			     :initial-contents sm)))
	  (multiply-two-matrices a b)))

(mm :fm '((1 2 3)(1 2 3)(1 2 3)) :sm '((1 2)(1 2)(1 2)))
					;(defun Aplana (lista) 

   #|| (loop for i
  below 4
  unless (oddp i)
   do (print i))

(loop for i
   below 5
   if(oddp i)
   do(print i)
   else
   do
     (print "woot"))

(loop for i from 1 to 10 do (when (oddp i) (print i)))

(loop for (a nil) in '((1 2) ((9 9)) (3 4) (5 6)) if(listp a) do(print "Lista")) 
 ||#


     

					;DoList, loop, do times, do, cond 
