;Reyes Fragoso Roberto

;Problema 1
(defun Collect (p l)
  (cond ((null l) nil)
	(t (Collect (funcall fun (car l) (cadr l))))))

(Combine #'+ '(1 2 3 4)) ;=> 10

;Problema 2
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

;Problema 3
(defun 2palindrome (cadena &optional (inicio 0))
       (if (= (length cadena) 0) T
       	    (if (char= (char cadena inicio) (char (reverse cadena) inicio))
      	    	(2palindrome (subseq cadena (+ inicio 1) (- (length cadena) 1)) inicio) nil)))

(2palindrome "oossoo") ; T

;Problema 4
(defun iterativepalindrome (lista)
       (loop for i from 0
       	     for a in lista
	           for b in (reverse lista)
	           always (equal a b)
	           until (> i(/(length lista)2))))

(iterativepalindrome '(a b a)) ; T


;Problema 5
(defun listRotate (l &key (right nil) (left nil))
  (if right
      (rotate l right)
      (rotate l (* -1 left))))

(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))

(listRotate '(a b c d e f g h) :right 3) ; => (D E F G H A B C) 

;Problema 7 
(defun Combine (fun l)
  (cond ((null l) 0)
	( t (funcall fun (Combine fun (cddr l)) (car l) (cadr l)))))

(Combine #'+ '(1 2 3 4)); => 7

;Problema 8 
(defun LevelAux (l cad cont)
  (cond ((null l) cont)
	((if (listp (car l))
	     (LevelAux (car l) cad (+ 1 cont))
	     (LevelAux (cdr l) cad cont)))
	((if (equalp (string (car l)) cad)
	     (return-from LevelAux cont)))	
	(t (LevelAux (cdr l) cad cont))))

(defun Level (lista cadena)
  (LevelAux lista cadena 0))

(Level '(colonia (alemana) cuidad perrito) "alemana") ;=> 1
  
;Problema 9
(defun encode (list)
  (labels ((encode-run (element count list)
             (cond
               ((null list) (list (list count element)))
               ((eql element (first list)) (encode-run element (1+ count) (rest list)))
               (t (cons (list count element) (encode-run (first list) 1 (rest list)))))))
    (if (null list)
        '()
        (encode-run (first list) 1 (rest list)))))

(encode '(a a a a b c c a a d e e e e)) ; => ((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))

;Problema 10 
(defun StrCypherAux (cadena code ct)
  (cond ((equal (length cadena) ct) (return-from StrCypherAux cadena))
	(t (StrCypherAux (substitute (char code ct) (char cadena ct) cadena) code (+ ct 1)))))

(defun StrCypher (c cc)
  (StrCypherAux c cc 0))

(StrCypher "Clor" "abcd") ;=> "abcd"


(subseq "Hola" 1 4)
(equalp "" "")
(aref "Hola" 0)
(length "Hola")

;Problema 11
(defun mmul (A B)
  (if (not (equal (car (array-dimensions A)) (car (array-dimensions B))))
      (return-from mmul nil))
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (C (make-array `(,m ,l) :initial-element 0)))
    (loop for i from 0 to (- m 1) do
              (loop for k from 0 to (- l 1) do
                    (setf (aref C i k)
                          (loop for j from 0 to (- n 1)
                                sum (* (aref A i j)
                                       (aref B j k))))))
    C))

(mmul #2a((1 2) (3 4)) #2a((5 6 7) (8 9 10))) ;=> #2A((21 24 27) (47 54 61))

(length #2a((1 2) (3 4)))
(equal (array-dimensions #2a((1 2) (3 4))) (array-dimensions #2a((1 2) (1 2))))
(car (array-dimensions #2a((1 2) (3 4))))
(car (array-dimensions #2a((-3 -8 3) (-2 1 4))))

;Problema 13 
(defun recorta (l n)
  (cond ((equal 0 n) nil)
	(t (cons (car l) (recorta (cdr l) (- n 1))))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun filtersubset (lista posicion)
  (all-permutations ( recorta lista posicion)))

(filtersubset '(a b c d) 2) ;=>((A B) (B A))

;Problema 14
(defun combinations (count list)
  (cond
    ((zerop count) '(())) 
    ((endp list) '())   
    (t (nconc (mapcar (let ((item (first list))) (lambda (combi) (cons item combi)))
                      (combinations (1- count) (rest list)))
              (combinations count (rest list))))))

(combinations 5 '(a b c d e f)); => ((A B C D E) (A B C D F) (A B C E F) (A B D E F) (A C D E F) (B C D E F))

;Problema 15 
(defmacro If-positive (number)
  (let ((c (gensym "cond-")))
    `(let* ((,c ,number))
       (if (plusp ,c)
	   (print "NumeroPositivo")
	   (print "NumeroNegativo")))))
	   
(If-positive 10) ; => Numero Positivo 



