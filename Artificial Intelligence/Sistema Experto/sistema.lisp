;;Reyes Fragoso Roberto

;;Definimos como parametro el vector en donde vayamos a almacenar nuestra base de conocimiento
(defparameter *vector-conocimiento* (make-array 100
                                                :adjustable T
                                                :element-type 'list))

;;Definimos un indice con el proposito de no recorrer todo el arreglo y ser mas rapidos en nuestra busqueda
(defparameter *indice* '((dios (0 24))
                         (semidios (25 45))
                         (humano (46 46))
                         (simbolo (47 58))
                         (relacion (59 99))))

;;Nos permite ir filtrando la respuesta para aplicarle los valores de verdad
(defparameter *respuesta* nil)
(defparameter *respuestaFinal* nil)

;;Permite manejar la logica del or
(defparameter *valorVerdadOr1* nil)
(defparameter *valorVerdadOr2* nil)

;;Permiten hacer de forma mas sencilla la obtencion de los valores y operadores a evaular
(defparameter *operador* nil)
(defparameter *valor* nil)
(defparameter *contadorRepuesta* 0)

;;[Funcion] Permite leer de archivo nuestra base de conocimiento
(defun leerConocimiento (filename)
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
      (adjust-array *vector-conocimiento* numrows)
      (read-line stream nil nil)
      (dotimes (row numrows *vector-conocimiento*)
        (setf (aref *vector-conocimiento* row) (read stream nil nil))))))

;;[Principal} Permite comenzar con el sistema Experto
(defun miniSistemaExperto ()
  ;;Comenzamos leyendo desde archivo nuestra base de conocimiento
  (leerConocimiento "baseDeConocimiento.txt")
                                        ;Hacemos un loop infinito hasta que el usuario teclee Salir
	(loop
     (print "Motor Basico de Consultas a Base De Conocimiento de la Mitologia Griega")
     (print "Las consultas deben estar escritas de la siguiente manera: ")
     (print "(+ (clase . dios)(nombre . zeus)(lugar . [>=5])) ")
     (print "Escriba la consulta que desea realizar")
     (let* ((entrada (read)))
       ;;Para poder salir de nuestro sistema experto solo tenemos que escribir Salir
       (if (equal entrada 'Salir) (RETURN))
       (motorIntefencia entrada))))

;;[Funcion] Permite hacer el motor de Inferencia
(defun motorIntefencia (entrada)
  ;;Como recibimos una lista propia obtenermos como primer argumento el cuantificados y despues los atributos
  (let ((operador (first entrada))
        (etiquetas (rest entrada)))
    ;;Limpiamos nuestras variables para cada busqueda que ralice el usuario
    (setq *respuesta* nil)
    (setq *respuestaFinal* nil)
    (setq *valorVerdadOr1* nil)
    (setq *valorVerdadOr2* nil)
    (setq *contadorRepuesta* 0)

    ;;Si detectamos que nuestra segunda etiqueta es un OR, hacemos la logica del or.
    (if (equal (third entrada) 'OR)
        (progn
          (motorInferenciaAuxOr operador (first etiquetas) (first(rest (rest etiquetas)))))
        (motorIntefenciaAux operador etiquetas))))

;;  
(defun motorInferenciaAuxOr (operador clase etiquetas)
  (let* ((atributos etiquetas)
         (valorClase (rest clase))
         (indiceClase (obtenerIndiceDeClase valorClase *indice*))
         (inicioIndice (first indiceClase))
         (auxContador 0)
         (auxContador2 0)
         (finalIndice (second indiceClase)))

    (if (null indiceClase)
        (print "No existe la Clase")
        (progn
          (cond ((eql operador '*)
                 (progn
                   (consultaABaseDeConocimientoUniversal inicioIndice finalIndice (list(first atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr1* *respuestaFinal*)
                   (setq *respuestaFinal* nil)
                   (consultaABaseDeConocimientoUniversal inicioIndice finalIndice (list(second atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr2* *respuestaFinal*)

                   (if (or (null *valorVerdadOr1*) (null *valorVerdadOr2*))
                           (print "True")
                           (progn
                             (print "False")
                             (cond ((null *valorVerdadOr1*)
                                    (format t "~& ~A ~%" *valorVerdadOr2*))
                                   ((null *valorVerdadOr2*)
                                    (format t "~& ~A ~%" *valorVerdadOr1*))
                                   (T (format t "~& ~A ~%" *valorVerdadOr1*)))))))
                ((eql operador '/)
                 (progn
                   (consultaABaseDeConocimientoUniversal inicioIndice finalIndice (list(first atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr1* *respuestaFinal*)
                   (setq auxContador *contadorRepuesta*)
                   (setq *respuestaFinal* nil)
                   (setq *contadorRepuesta* 0)
                   (consultaABaseDeConocimientoUniversal inicioIndice finalIndice (list(second atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr2* *respuestaFinal*)
                   (setq auxContador2 *contadorRepuesta*)


                   (if (or (= auxContador (1+ (- finalIndice inicioIndice ))) (= auxContador2 (1+ (- finalIndice inicioIndice ))))
                       (progn
                         (print "True")
                         (cond ((null *valorVerdadOr1*)
                                (format t "~& ~A ~%" *valorVerdadOr2*))
                               ((null *valorVerdadOr2*)
                                (format t "~& ~A ~%" *valorVerdadOr1*))
                               (T (format t "~& ~A ~%" *valorVerdadOr1*))))
                       (print "False"))))


                ((eql operador '+)
                 (progn
                   (consultaABaseDeConocimiento inicioIndice finalIndice (list(first atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr1* *respuestaFinal*)
                   (setq *respuestaFinal* nil)
                   (consultaABaseDeConocimiento inicioIndice finalIndice (list(second atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr2* *respuestaFinal*)
                   (if (and (null *valorVerdadOr1*) (null *valorVerdadOr2*))
                       (print "False")
                       (progn
                         (print "True")
                         (cond ((null *valorVerdadOr1*)
                                (format t "~& ~A ~%" *valorVerdadOr2*))
                               ((null *valorVerdadOr2*)
                                (format t "~& ~A ~%" *valorVerdadOr1*))
                               (T (format t "~& ~A ~%" *valorVerdadOr1*)))))))
                ((eql operador '-)
                 (progn
                   (consultaABaseDeConocimiento inicioIndice finalIndice (list(first atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr1* *respuestaFinal*)
                   (setq *respuestaFinal* nil)
                   (consultaABaseDeConocimiento inicioIndice finalIndice (list(second atributos)) *vector-conocimiento*)
                   (setq *valorVerdadOr2* *respuestaFinal*)

                   (if (and (null *valorVerdadOr1*) (null *valorVerdadOr2*))
                       (print "True")
                       (progn
                         (print "Flase")
                         (cond ((null *valorVerdadOr1*)
                                (format t "~& ~A ~%" *valorVerdadOr2*))
                               ((null *valorVerdadOr2*)
                                (format t "~& ~A ~%" *valorVerdadOr1*))
                               (T (format t "~& ~A ~%" *valorVerdadOr1*)))))))   ))  )))

;;[Funcion] Nos permite hacer el motor de inferencia para la etiqueta existencial

(defun motorIntefenciaAux (operador etiquetas)
  ;;Usamos let* por que necesitamos crear nuetras otras variables apartir de la primera
  ;;Para las etiquetas usamos first y rest para obtener los valores de la lista propia (argumento . valor)
  (let* ((clase (first etiquetas))
         (atributos (rest etiquetas))
         (valorClase (rest clase))
         (indiceClase (obtenerIndiceDeClase valorClase *indice*))
         (inicioIndice (first indiceClase))
         (finalIndice (second indiceClase)))

    ;;Si no existe devolvemos inmediatamente que no existe esa clase
    (if (null indiceClase)
        (print "No existe la Clase")
        (progn
          ;;Cuando nuestro cuantificador sea existencial
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
                ;;Cuando nuestro cuantificador sea existencial negado
                ((eql operador '-)
                 (progn
                   (consultaABaseDeConocimiento inicioIndice finalIndice atributos *vector-conocimiento*)
                   (if (null *respuestaFinal*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *respuestaFinal*)))))
                ;;Cuando nuestro cuantificador sea universal
                ((eql operador '*)
                 (progn
                   (consultaABaseDeConocimientoUniversal inicioIndice finalIndice atributos *vector-conocimiento*)
                   (if (null *respuestaFinal*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *respuestaFinal*)))))
                ;;Cuando nuestro cuantificador sea universal negado
                ((eql operador '/)
                 (progn
                   (consultaABaseDeConocimientoUniversal inicioIndice finalIndice atributos *vector-conocimiento*)
                   (if (null *respuestaFinal*)
                       (print "False")
                       (progn
                         (if (/= *contadorRepuesta* (1+ (- finalIndice inicioIndice )))
                             (print "False")
                             (progn
                               (print "True")
                               (print *respuestaFinal*)))))))
                ;;Incluimos la opcion de error por si el usuario escribio mal alguno de los cuantificadores
                (T (print "Error")))))))


;;[Funcion] Permite obtener de la base de conocimiento los datos correspondientes a la consulta
(defun consultaABaseDeConocimiento (inicioIndice finalIndice atributos baseDeConocomiento)
  ;;Iteramos en nuestra base de conocimiento
  (loop for i from inicioIndice to finalIndice do
       ;;Cuando no se tengan atributos significa que solo el usuario desea la clase
       (if (null atributos)
           (print (aref baseDeConocomiento i))
           (progn
             ;;Iteramos para cada tupla de conocomiento
             (loop for j from 1 to (1- (length (aref baseDeConocomiento i))) do
                  ;;Usamos let para no repetir tantas veces el valor y nombre de la tupla de conocimiento
                  (let ((tuplaNombre (first (nth j (aref baseDeConocomiento i))))
                        (tuplaValor (rest (nth j (aref baseDeConocomiento i)))))
                    ;;Para poder saber si la tupla cumple con la consulta del usuario iteramos para cada atributo que el usuario desea
                    (loop for atributo in atributos do
                         ;;Cuando el usuario requera de consultas mas avanzadas realizamos un patternMatchin para obtener los valores y la funcion
                         (patternMatching (rest atributo))
                         ;;Cuando sean iguales el nombre de la etiqueta con el nombre de la tupla de conocomiento entramos al if
                         (if (equal (first atributo) tuplaNombre)
                             (if (null *operador*)
                                 ;;La logica para saber si cumple las caracteristicas es que si se cumplen todos los atributos del usuario
                                 ;; con los de la base de conocomiento entonces satisface la consulta
                                 (if (equal (rest atributo) tuplaValor)
                                     (push 1 *respuesta*))
                                 (progn
                                   ;; Cuando sea necesario evaluar una funcion, ya la obtenemos gracias a nuestro patternMatching.
                                   (if (funcall *operador* tuplaValor *valor*)
                                       (progn
                                         (push 1 *respuesta*))
                                         )))))))

             ;;Finalmente aplicamos una suma sobre lo que obtuvimos al iterar sobre nuestras tuplas, si es igual a la longitud de los atributos
             ;; la consulta se satisface
             (if (= (apply #'+ *respuesta*) (length atributos))
                 (push (aref baseDeConocomiento i) *respuestaFinal*))
             (setq *respuesta* '(0))
             (setq *operador* nil)
             (setq *valor* nil)))))



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
                           ;;Para el existencial lo que hacemos es que si todos los valores se cumplen regresamos falso
                           (if (not (equal (rest atributo) tuplaValor))
                               (progn
                                 (setq *respuesta* t)
                                 (setq *contadorRepuesta* (1+ *contadorRepuesta*))))
                           (progn
                             (if (not (funcall *operador* tuplaValor *valor*))
                                 (progn
                                   ;;Si todos los valores al aplicarle nuestra funcion son falsos si no, regresamos t.
                                   (setq *respuesta* t))
                                 (setq *respuesta* nil))))))))

       ;;Finalmente si no son falsos agregamos la respuesta a nuestra variable global
       (if (not (null *respuesta*))
           (push (aref baseDeConocomiento i) *respuestaFinal*))
       (setq *respuesta* nil)
       (setq *operador* nil)
       (setq *valor* nil)))

;;[Funcion] Permite saber cual es el indice de donde vamos a revisar la clase
(defun obtenerIndiceDeClase (valorDeClase indice)
  (cond ((null indice)
         nil)
        ((equal (first (first indice)) valorDeClase)
         (second (first indice)))
        (T (obtenerIndiceDeClase valorDeClase (rest indice)))))

;;[Funcion] Permite hacer != como una funcion
(defun notEqual (valor1 valor2)
  (not (equal valor1 valor2)))

;;[Funcion] Permite usar patternMatching para obtener los valores de las expresiones
(defun patternMatching (expresion)
  (let* ((stringExpresion (string expresion))
         (valorAuxiliar nil)
         (longitudExpresion (length stringExpresion))
         (subPrimera (subseq stringExpresion 0 3))
         (subSegunda (subseq stringExpresion 0 2)))

    (cond  ((string-equal subPrimera '[==)
            (progn
              (setq *operador* #'equal)
              (setq valorAuxiliar (read-from-string (subseq stringExpresion 3 (1- longitudExpresion))))
              (if (numberp valorAuxiliar)
                  (progn
                    (setq *operador* #'=)
                    (setq *valor* (parse-integer(subseq stringExpresion 3 (1- longitudExpresion)))))
                  (setq *valor* (intern(subseq stringExpresion 3 (1- longitudExpresion)))))))
           ((string-equal subPrimera '[!=)
            (progn
              (setq *operador* #'notEqual)
              (setq valorAuxiliar (read-from-string (subseq stringExpresion 3 (1- longitudExpresion))))
              (if (numberp valorAuxiliar)
                  (progn
                    (setq *operador* #'/=)
                    (setq *valor* (parse-integer(subseq stringExpresion 3 (1- longitudExpresion)))))
                  (setq *valor* (intern(subseq stringExpresion 3 (1- longitudExpresion)))))))
           ((string-equal subPrimera '[>=)
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

;;Permite iniciar el motor basico de consultas
(miniSistemaExperto)

;; Para numeros siempre usar []
;;  (+ (clase . dios)(lugar . [==0]))
;;  (+ (clase . dios)(habitat . [!=olimpo]))
;;  (+ (clase . dios)(nombre . [!=zeus]))
;;  (+ (clase . dios)(habitat . [!=olimpo]))
;;  ( * (clase . dios) OR ((habitat . olimpo) (habitat . inframundo)))
;;  (* (clase . dios)(habitat . tierra))
;; ( + (clase . dios) OR ((habitat . tierra) (habitat . inframundo)))
;; ( / (clase . dios)(habitat . olimpo))

