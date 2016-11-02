;;Reyes Fragoso Roberto

;; Definimos como parametro el vector en donde vayamos a almacenar nuestra base de conocimiento
(defparameter *vector-conocimiento* (make-array 100
                                            :adjustable T
                                            :element-type 'list))
;; Definimos un indice con el proposito de no recorrer todo el arreglo y ser mas rapidos en nuestra busqueda
(defparameter *indice* '((dios (0 24))
                         (semidios (25 45))
                         (humano (46 46))
                         (simbolo (47 58))
                         (relacion (59 100))))

(defparameter *inicio* '((T.T)))
(defconstant fail nil
  "Indicates pat-match failure")

;;[Funcion] Permite leer de archivo nuestra base de conocimiento
(defun leerConocimiento (filename)
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
          (adjust-array *vector-conocimiento* numrows)
          (read-line stream nil nil)
          (dotimes (row numrows *vector-conocimiento*)
            (setf (aref *vector-conocimiento* row) (read stream nil nil))))))

;[Principal} Permite comenzar con el sistema Experto
(defun miniSistemaExperto ()
  ;Comenzamos leyendo desde archivo nuestra base de conocimiento
  (leerConocimiento "baseDeConocimiento.txt")
  ;Hacemos un loop infinito hasta que el usuario teclee Salir
	(loop
		(print "Escriba la consulta que desea realizar")
     (let* ((entrada (read)))
       ;Para poder salir de nuestro sistema experto solo tenemos que escribir Salir
       (if (equal entrada 'Salir) (RETURN))
       (motorIntefencia entrada))))

;[Funcion] Permite hacer el motor de Inferencia
(defun motorIntefencia (entrada)
  (let ((operador (first entrada))
        (etiquetas (rest entrada)))
    (cond ((eql operador '+)
           (etiquetaExistencial etiquetas))
          ((eql operador '-)
           (print "ES -"))
          ((eql operador '*)
           (print "ES *"))
          ((eql operador '/)
           (print "ES /"))
          (T (print "Error")))))

;[Funcion] Nos permite hacer el motor de inferencia para la etiqueta existencial
(defun etiquetaExistencial (etiquetas)
  (let* ((clase (first etiquetas))
         (valorClase (rest clase))
         (indiceClase (obtenerIndiceDeClase valorClase *indice*))
         (inicioIndice (first indiceClase))
         (finalIndice (second indiceClase)))


    (format t "~& ESTO ES MI ETIQUETA ~A ~%"  valorClase)
    (format t "~& ESTO ES MI Indice Completo ~A ~%"  indiceClase)
    (format t "~& ESTO ES MI Inicio ~A ~%"  inicioIndice)
    (format t "~& ESTO ES MI Final ~A ~%"  finalIndice)
    (consultaABaseDeConocimiento inicioIndice finalIndice)
    ))


(defun consultaABaseDeConocimiento (inicioIndice finalIndice)

  (loop for i from inicioIndice to finalIndice do
                              (print (aref *vector-conocimiento* i))))

;[Funcion] Permite saber cual es el indice de donde vamos a revisar la clase
(defun obtenerIndiceDeClase (valorDeClase indice)
  (cond ((null indice)
         nil)
        ((equal (first (first indice)) valorDeClase)
         (second (first indice)))
        (T (obtenerIndiceDeClase valorDeClase (rest indice)))))

  ;  (cond ((eql clase +)
   ;        (print "FUNCIONA"))
    ;      (T (print "NO FUNCIONA")))))

	#|  
(defun patternMatch (patron expresion &optional(alist *inicio*))
		(cond ((esVariable? patron)
				(matchearVariable patron expresion alist))
			  ((esConstante? patron)
				(matchearConstante patron expresion alist))
		((and (consp pat) (consp exp))
(patternMatch (rest pat) (rest exp)
(patternMatch (first pat) (first exp) alist))
(T alist))))

(defun esVariable? (x)
  (and (symbolp x)
       (equal (char (symbol-name x) 0)
              #\?)))
			  
(defun segment-pattern-p (pat)
  "Is this a segment-matching pattern: (?*var ...)"
  (and (listp pat)
       (>= (length (symbol-name (car pat))) 2)
       (equal (char (symbol-name (car pat)) 0) #\?)
       (equal (char (symbol-name (car pat)) 1) #\*)))			  




		|#	  

(miniSistemaExperto)
(first (aref *vector-conocimiento* 0))
(aref *vector-conocimiento* 1)
(first (first *indice*))
(second (first *indice*))
(second *indice*)
;; (print *vector-conocimiento*)

;; (setq perro '(perro . terrier))
;; (first perro)
;; (rest perro)
; (setq usuario '( - (clase . persona) (edad . [>120]) ))
;(eql (first usuario) '- )
;(length (rest usuario))
