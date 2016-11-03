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
                         (relacion (59 99))))

;; Nos permite ir filtrando la respuesta para aplicarle los valores de verdad
(defparameter *respuesta* nil)
(defparameter *respuestaFinal* nil)

;;Permiten hacer de forma mas sencilla la obtencion de los valores y operadores a evaular
(defparameter *operador* nil)
(defparameter *valor* nil)

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

;TODO Crear funcion para limpiar las variables globales
;TODO Si no existe la clase muere
;[Funcion] Permite hacer el motor de Inferencia
(defun motorIntefencia (entrada)
  (let ((operador (first entrada))
        (etiquetas (rest entrada)))
    (setq *respuesta* nil)
    (setq *respuestaFinal* nil)
    (motorIntefenciaAux operador etiquetas)))

;;TODO Validar bien cuando solo el usuario ponga la clase
;;TODO Validar que si la clase no existe de nulo 
;[Funcion] Nos permite hacer el motor de inferencia para la etiqueta existencial
(defun motorIntefenciaAux (operador etiquetas)
  (let* ((clase (first etiquetas))
         (atributos (rest etiquetas))
         (valorClase (rest clase))
         (indiceClase (obtenerIndiceDeClase valorClase *indice*))
         (inicioIndice (first indiceClase))
         (finalIndice (second indiceClase)))

    (cond ((eql operador '+)
           (progn
             ;;Si el usuario no teclea ningun atributo, solo vamos a obtener los datos filtrados por clase
             (if (null atributos)
                 (consultaABaseDeConocimiento inicioIndice finalIndice nil *vector-conocimiento*)
                 ;;En caso contrario, le enviamos a nuestra consulta los atriburos que el usuario desea
                 (progn
                   (consultaABaseDeConocimiento inicioIndice finalIndice atributos *vector-conocimiento*)
                   (if (null *respuestaFinal*)
                       (print "False")
                       (progn
                         (print "True")
                         (print *respuestaFinal*)))))))
          ((eql operador '-)
           (progn
             (consultaABaseDeConocimiento inicioIndice finalIndice atributos *vector-conocimiento*)
             (if (null *respuestaFinal*)
                 (print "True")
                 (progn
                   (print "False")
                   (print *respuestaFinal*)))))
          ((eql operador '*)
           (progn
             (consultaABaseDeConocimientoUniversal inicioIndice finalIndice atributos *vector-conocimiento*)
             (if (null *respuestaFinal*)
                 (print "True")
                 (progn
                   (print "False")
                   (print *respuestaFinal*)))))
          ((eql operador '/)
           (progn
             (consultaABaseDeConocimientoUniversal inicioIndice finalIndice atributos *vector-conocimiento*)
             (if (null *respuestaFinal*)
                 (print "False")
                 (progn
                   (print "True")
                   (print *respuestaFinal*)))))
          (T (print "Error")))))


;TODO No repetir valores el desplegar al final
;[Funcion] Permite obtener de la base de conocimiento los datos correspondientes a la consulta
(defun consultaABaseDeConocimiento (inicioIndice finalIndice atributos baseDeConocomiento)
    (loop for i from inicioIndice to finalIndice do
         (if (null atributos)
             (print (aref baseDeConocomiento i))
             (progn
               (loop for j from 1 to (1- (length (aref baseDeConocomiento i))) do
                    (let ((tuplaNombre (first (nth j (aref baseDeConocomiento i))))
                          (tuplaValor (rest (nth j (aref baseDeConocomiento i)))))
                      (loop for atributo in atributos do
                           (patternMatching (rest atributo))
                           (if (equal (first atributo) tuplaNombre)
                               (if (null *operador*)
                                   (if (equal (rest atributo) tuplaValor)
                                       (push 1 *respuesta*))
                                   (progn
                                     (if (funcall *operador* tuplaValor *valor*)
                                         (push 1 *respuesta*))))))))

                   (if (= (apply #'+ *respuesta*) (length atributos))
                       (push (aref baseDeConocomiento i) *respuestaFinal*))
                   (setq *respuesta* '(0))
                   (setq *operador* nil)
                   (setq *valor* nil)))))

;;TODO Juntar el universal y el existencial
;;TODO Agregar por si no tiene clase
;;[Funcion] Permite obtener de la base de conocimiento los datos para universal y universal negado
(defun consultaABaseDeConocimientoUniversal (inicioIndice finalIndice atributos baseDeConocomiento)
    (loop for i from inicioIndice to finalIndice do
               (loop for j from 1 to (1- (length (aref baseDeConocomiento i))) do
                    (let ((tuplaNombre (first (nth j (aref baseDeConocomiento i))))
                          (tuplaValor (rest (nth j (aref baseDeConocomiento i)))))
                      (loop for atributo in atributos do
                           (patternMatching (rest atributo))
                           (if (equal (first atributo) tuplaNombre)
                               (if (null *operador*)
                                   (if (not (equal (rest atributo) tuplaValor))
                                       (setq *respuesta* t))
                                   (progn
                                     (if (not (funcall *operador* tuplaValor *valor*))
                                         (progn
                                           (setq *respuesta* t)
                                        ;(print (funcall *operador* tuplaValor *valor*))
                                           )
                                         (setq *respuesta* nil))))))))
         (if (not (null *respuesta*))
             (push (aref baseDeConocomiento i) *respuestaFinal*))
         (setq *respuesta* nil)
         (setq *operador* nil)
         (setq *valor* nil)))

;[Funcion] Permite saber cual es el indice de donde vamos a revisar la clase
(defun obtenerIndiceDeClase (valorDeClase indice)
  (cond ((null indice)
         nil)
        ((equal (first (first indice)) valorDeClase)
         (second (first indice)))
        (T (obtenerIndiceDeClase valorDeClase (rest indice)))))

(defun patternMatching (expresion)
  (let* ((stringExpresion (string expresion))
         (longitudExpresion (length stringExpresion))
         (subPrimera (subseq stringExpresion 0 3))
         (subSegunda (subseq stringExpresion 0 2)))

    (cond ((string-equal subPrimera '[>=)
           (progn
             (setq *operador* #'>=)
             (setq *valor* (parse-integer(subseq stringExpresion 3 (1- longitudExpresion))))))
          ((string-equal subPrimera '[<=)
           (progn
             (setq *operador* #'<=)
             (setq *valor* (parse-integer(subseq stringExpresion 3 (1- longitudExpresion))))))
          ((string-equal subSegunda '[>)
           (progn
             (setq *operador* #'>)
             (setq *valor* (parse-integer(subseq stringExpresion 2 (1- longitudExpresion))))))
           ((string-equal subSegunda '[<)
            (progn
              (setq *operador* #'<)
              (setq *valor* (parse-integer(subseq stringExpresion 2 (1- longitudExpresion))))))
           (T (progn (setq *operador* nil)
                     (setq *valor* nil))))))

(miniSistemaExperto)
;;(+ (clase . dios)(lugar . [>=5]))
;(length(string 'perro))
                                        ;(+ (clase . dios)(lugar . [>=25]))
;(STRING-EQUAL "[>" (subseq "[>20]" 0 2))
                                        ;(subseq "[>20]" 0 1)
;(setq f #'>)
;(setq f '>)
;(funcall f 8 10)
;(funcall #'f 8 10)
;(intern (string-upcase "value-of-a"))
