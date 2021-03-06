;Reyes Fragoso Roberto

;Cargamos la libreria para comenzar a trabajar con ella
(load "maze_lib.lisp")

;Algoritmo al menu de la pagina principal
(add-algorithm 'depth-first)
(add-algorithm 'breath-first)
(add-algorithm 'best-first)
(add-algorithm 'A*)

;Permite saber para cada problema la frontera de busqueda y memoria
(defparameter *fronteraDeBusqueda* '())
(defparameter *memoria* '())

;Permite almacenar los datos del laberinto
(defparameter *id* 0)
(defparameter *ancestro* nil)
(defparameter *solution* nil)
(defparameter *numeroDeFilas* nil)
(defparameter *numeroDeColumnas* nil)
(defparameter *sol* nil)

;Permite saber de donde vino el puente
(defparameter *puente* 0)
;Definicion de operadores
(defparameter *operadores* '((:Mover-Arriba 0)
                       (:Mover-Arriba-Derecha 1)
                       (:Mover-Derecha 2)
                       (:Mover-Abajo-Derecha 3)
                       (:Mover-Abajo 4)
                       (:Mover-Abajo-Izquierda 5)
                       (:Mover-Izquierda 6)
                       (:Mover-Arriba-Izquierda 7)))

;[Funcion] Permite resetear todo
(defun limpiarVariables ()
  (setq *fronteraDeBusqueda*  nil)
  (setq *memoria*  nil)
  (setq *id*  0)
  (setq *sol* nil)
  (setq *puente* 0)
  (setq *ancestro*  nil)
  (setq *solution*  nil))

;[Funcion] Permite crear los nodos necesarios
(defun crearNodo (estado operador importancia)
  (incf *id*)
  (list (1- *id*) importancia estado *ancestro* (second operador)))

;[Funcion] Permite saber la distancia manhattan, esta basada en la idea de la ecuacion de la distancia entre
; dos puntos, pero con una ligera modificacion, solo obtenemos el maximo de: (x2-x1),(y2-y1)
(defun Manhattan (estado)
  (max (- (max (aref estado 0) (aref *goal* 0))
          (min (aref estado 0) (aref *goal* 0)))
       (- (max (aref estado 1) (aref *goal* 1))
          (min (aref estado 1) (aref *goal* 1)))))

;[Funcion] Permite insertar a frontera de Busqueda
(defun insertarAFronteraDeBusqueda (estado operador metodoBusqueda)
  (let* ((nodo '()))
    (cond ((eql metodoBusqueda :depth-first )
           (setq nodo (crearNodo estado operador nil))
           (push nodo *fronteraDeBusqueda*))
          ((eql metodoBusqueda :breath-first )
           (setq nodo (crearNodo estado operador nil))
           (setq *fronteraDeBusqueda* (append *fronteraDeBusqueda* (list nodo))))
		  ((eql metodoBusqueda :best-first)
           (setq nodo (crearNodo estado operador (Manhattan estado)))
           (push nodo *fronteraDeBusqueda*)
           (setq *fronteraDeBusqueda* (stable-sort *fronteraDeBusqueda* '< :key #'(lambda (x) (second x)))))
		  ((eql metodoBusqueda :Astar)
           (setq nodo (crearNodo estado operador (Manhattan estado)))
           (setf (second nodo) (+ (second nodo) (Backtracking nodo 0)))
           (if (recuerdasElEstadoEnMemoria? (third nodo) *fronteraDeBusqueda*)
               (checarEstado nodo *fronteraDeBusqueda*)
               (push nodo *fronteraDeBusqueda*))
           (setq *fronteraDeBusqueda* (stable-sort *fronteraDeBusqueda* '< :key #'(lambda (x) (second x)))))
		   )))

;[Funcion] Permite hacer backtracking, se usa para el algoritmo A*
(defun Backtracking (nodo num)
  (labels ((locate-node (id lista)
             (cond ((null lista) nil)
                   ((eql id (first (first lista))) (first lista))
                   (T (locate-node id (rest lista))))))
    (let ((current (locate-node (fourth nodo) *memoria*)))
      (loop while (not (null current)) do
        (setq num (incf num))
        (setq current (locate-node (fourth current) *memoria*))))
   num))


;[Funcion] Permite obtener el ultimo elemento de la frontera de busqueda
(defun obtenerDeFronteraDeBusqueda ()
  (pop *fronteraDeBusqueda*))

;[Funcion] Permite obtener el ultimo elemento de nuestra memoria, es una funcion no destructiva
(defun NDobtenerDeMemoria ()
  (first *memoria*))

;[Validacion] Permite validar los diferentes operadores
(defun puedeOperarArriba? (casillaArriba casillaActual operadorPasado)
  (cond ((null casillaArriba) nil)
        ((or (= casillaActual 16)(= casillaActual 17))
         (if (= operadorPasado 0) T nil))
        ((= (boole boole-and casillaActual 1) 0) T)
        (T nil)))

;[Validacion]
(defun puedeOperarDerecha? (casillaDerecha casillaActual operadorPasado)
  (cond ((null casillaDerecha) nil)
        ((or (= casillaActual 16)(= casillaActual 17))
         (if (= operadorPasado 2) T nil))
        ((= (boole boole-and casillaActual 2) 0) T)
        (T nil)))

;[Validacion]
(defun puedeOperarAbajo? (casillaAbajo casillaActual operadorPasado)
  (cond ((or (= casillaActual 16)(= casillaActual 17))
         (if (= operadorPasado 4) T nil))
        ((null casillaAbajo) nil)
        ((= (boole boole-and casillaActual 4) 0) T)
        (T nil)))

;[Validacion]
(defun puedeOperarIzquierda? (casillaIzquierda casillaActual operadorPasado)
  (cond ((null casillaIzquierda) nil)
        ((or (= casillaActual 16)(= casillaActual 17))
         (if (= operadorPasado 6) T nil))
        ((= (boole boole-and casillaActual 8) 0) T)
        (T nil)))

;[Validacion]
(defun puedeOperarArribaDerecha? (casillaActual casillaArriba casillaDerecha casillaDiagonalArribaDerecha diagonalALosLados)
  (cond ((or (= casillaDiagonalArribaDerecha 16)(= casillaDiagonalArribaDerecha 17)) nil)
        ((null diagonalALosLados) nil)
        ((or (null casillaArriba) (null casillaDerecha)) nil)
        ((and (or (= (boole boole-and casillaActual 1) 0)
                  (= (boole boole-and casillaDerecha 1) 0))
              (or (= (boole boole-and casillaArriba 2) 0)
                  (= (boole boole-and casillaDerecha 1) 0))
              (or (= (boole boole-and casillaArriba 2) 0)
                  (= (boole boole-and casillaActual 2) 0))
              (or (= (boole boole-and casillaActual 1) 0)
                  (= (boole boole-and casillaActual 2) 0))) T)
        (T nil)))

;[Validacion]
(defun puedeOperarAbajoDerecha? (casillaActual casillaAbajo casillaDerecha casillaDiagonalAbajoDerecha diagonalALosLados)
  (cond ((or (= casillaDiagonalAbajoDerecha 16)(= casillaDiagonalAbajoDerecha 17)) nil)
        ((null diagonalALosLados) nil)
        ((or (null casillaDerecha) (null casillaAbajo)) nil)
        ((and (or (= (boole boole-and casillaActual 4) 0)
                  (= (boole boole-and casillaDerecha 4) 0))
              (or (= (boole boole-and casillaAbajo 2) 0)
                  (= (boole boole-and casillaDerecha 4) 0))
              (or (= (boole boole-and casillaAbajo 2) 0)
                  (= (boole boole-and casillaActual 2) 0))
              (or (= (boole boole-and casillaActual 4) 0)
                  (= (boole boole-and casillaActual 2) 0))) T)
        (T nil)))

;[Validacion]
(defun puedeOperarAbajoIzquierda? (casillaActual casillaAbajo casillaIzquierda casillaDiagonalAbajoIzquierda diagonalALosLados)
  (cond ((or (= casillaDiagonalAbajoIzquierda 16)(= casillaDiagonalAbajoIzquierda 17)) nil)
        ((null diagonalALosLados) nil)
        ((or (null casillaAbajo) (null casillaIzquierda)) nil)
        ((and (or (= (boole boole-and casillaActual 4) 0)
                 (= (boole boole-and casillaIzquierda 4) 0))
             (or (= (boole boole-and casillaAbajo 8) 0)
                 (= (boole boole-and casillaIzquierda 4) 0))
             (or (= (boole boole-and casillaAbajo 8) 0)
                 (= (boole boole-and casillaActual 8) 0))
             (or (= (boole boole-and casillaActual 4) 0)
                 (= (boole boole-and casillaActual 8) 0))) T)
        (T nil)))

;[Validacion]
(defun puedeOperarArribaIzquierda? (casillaActual casillaArriba casillaIzquierda casillaDiagonalArribaIzquierda diagonalALosLados)
  (cond ((or (= casillaDiagonalArribaIzquierda 16)(= casillaDiagonalArribaIzquierda 17)) nil)
        ((null diagonalALosLados) nil)
        ((or (null casillaArriba) (null casillaIzquierda)) nil)
        ((and (or (= (boole boole-and casillaActual 1) 0)
                 (= (boole boole-and casillaIzquierda 1) 0))
             (or (= (boole boole-and casillaArriba 8) 0)
                 (= (boole boole-and casillaIzquierda 1) 0))
             (or (= (boole boole-and casillaArriba 8) 0)
                 (= (boole boole-and casillaActual 8) 0))
             (or (= (boole boole-and casillaActual 1) 0)
                 (= (boole boole-and casillaActual 8) 0))) T)
        (T nil)))

;[Funcion] Permite validar nuestro operador
(defun operadorValido? (op estado)
  (let* ((fila (aref estado 0))
         (columna (aref estado 1))
         (casillaActual (get-cell-walls fila columna))
         (operador (second op))
         (casillaArriba nil)
         (casillaAbajo nil)
         (casillaDiagonalArribaDerecha -1)
         (casillaDiagonalAbajoDerecha -1)
         (casillaDiagonalArribaIzquierda -1)
         (casillaDiagonalAbajoIzquierda -1)
         (casillaIzquierda nil)
         (casillaDerecha nil)
         (operadorPasado -1)
         (diagonalALosLados nil))

    (if (not (= fila 0))
        (setq casillaArriba (get-cell-walls (1- fila) columna)))
    (if (not (= columna 0))
        (setq casillaIzquierda (get-cell-walls fila (1- columna))))
	  (if (not (= columna (1- *numeroDeColumnas*)))
        (setq casillaDerecha (get-cell-walls fila (1+ columna))))
    (if (not (= fila (1- *numeroDeFilas*)))
        (setq casillaAbajo (get-cell-walls (1+ fila) columna)))
    (if (not (or (null casillaArriba)(null casillaDerecha)))
        (setq casillaDiagonalArribaDerecha (get-cell-walls (1- fila) (1+ columna))))
    (if (not (or (null casillaDerecha) (null casillaAbajo)))
        (setq casillaDiagonalAbajoDerecha (get-cell-walls (1+ fila) (1+ columna))))
    (if (not (or (null casillaIzquierda) (null casillaAbajo)))
        (setq casillaDiagonalAbajoIzquierda (get-cell-walls (1+ fila) (1- columna))))
    (if (not (or (null casillaIzquierda) (null casillaArriba)))
        (setq casillaDiagonalArribaIzquierda (get-cell-walls (1- fila) (1- columna))))
    (if (not (or (null casillaArriba) (null casillaAbajo ) (null casillaIzquierda ) (null casillaDerecha )))
        (progn
          (if (and (not (or (= casillaArriba 16 ) (= casillaAbajo 16 ) (= casillaIzquierda 16 ) (= casillaDerecha 16 ) (= casillaActual 16 )))
                   (not (or (= casillaArriba 17 ) (= casillaAbajo 17 ) (= casillaIzquierda 17 ) (= casillaDerecha 17 ) (= casillaActual 17 ))))
              (setq diagonalALosLados T))))
    (if (not ( null (fifth (NDobtenerDeMemoria))))
        (setq operadorPasado (fifth(NDobtenerDeMemoria))))
    

	
    (cond ((= operador 0)
           (puedeOperarArriba? casillaArriba casillaActual operadorPasado))
          ((= operador 1)
           (puedeOperarArribaDerecha? casillaActual casillaArriba casillaDerecha casillaDiagonalArribaDerecha diagonalALosLados))
          ((= operador 2)
           (puedeOperarDerecha? casillaDerecha casillaActual operadorPasado))
          ((= operador 3)
           (puedeOperarAbajoDerecha? casillaActual casillaAbajo casillaDerecha casillaDiagonalAbajoDerecha diagonalALosLados))
          ((= operador 4)
           (puedeOperarAbajo? casillaAbajo casillaActual operadorPasado))
          ((= operador 5)
           (puedeOperarAbajoIzquierda? casillaActual casillaAbajo casillaIzquierda casillaDiagonalAbajoIzquierda diagonalALosLados))
          ((= operador 6)
           (puedeOperarIzquierda? casillaIzquierda casillaActual operadorPasado))
          ((= operador 7)
           (puedeOperarArribaIzquierda? casillaActual casillaArriba casillaIzquierda casillaDiagonalArribaIzquierda diagonalALosLados))
          (T nil))))

;[Funcion] Permite aplicar el operador al estado
(defun aplicarOperador (operador estado)
  (let* ((fila (aref estado 0))
         (columna (aref estado 1))
		 (casillaActual (get-cell-walls fila columna))
         (operador (first operador))
         (estadoFinal nil))
	(if (or (= casillaActual 16)(= casillaActual 17))
		(progn 
			(print "VALOR DEL PUENTE ES")
			(setq *puente* 1)
			(print "Se encontro en la casilla")
			(print fila)
			(print columna))
		(setq *puente* 0))	 
		
		

    (case operador
      (:Mover-Arriba (setq estadoFinal (make-array 3 :initial-contents (list (1- fila) columna *puente*))))
      (:Mover-Arriba-Derecha (setq estadoFinal (make-array 3 :initial-contents (list (1- fila) (1+ columna) *puente*))))
      (:Mover-Derecha (setq estadoFinal (make-array 3 :initial-contents (list fila (1+ columna) *puente*))))
      (:Mover-Abajo-Derecha (setq estadoFinal (make-array 3 :initial-contents (list (1+ fila) (1+ columna) *puente* ))))
      (:Mover-Abajo (setq estadoFinal (make-array 3 :initial-contents (list (1+ fila) columna *puente*))))
      (:Mover-Abajo-Izquierda (setq estadoFinal (make-array 3 :initial-contents (list (1+ fila) (1- columna) *puente*))))
      (:Mover-Izquierda (setq estadoFinal (make-array 3 :initial-contents (list fila (1- columna) *puente*))))
      (:Mover-Arriba-Izquierda (setq estadoFinal (make-array 3 :initial-contents (list (1- fila) (1- columna) *puente*))))
      (T "error"))
    estadoFinal))


;[Funcion] Permite ayudarnos en nuestro algoritmo A*
(defun checarEstado (nodo listaMemoria)
  (let ((nodoAux nil))
    (cond ((null listaMemoria) (push nodo *fronteraDeBusqueda*))
          ((and (equal (aref (third nodo) 0) (aref (third (first listaMemoria)) 0))
                (equal (aref (third nodo) 1) (aref (third (first listaMemoria)) 1)))
           (setq nodoAux (first listaMemoria))
           (if (< (second nodo) (second nodoAux))
               (progn (delete nodoAux listaMemoria)
                      (push nodo *fronteraDeBusqueda*))))
          (T (checarEstado nodo (rest listaMemoria))))))

;[Funcion] Permite expandir el estado
(defun expandir (estado)
  (let ((descendientes nil) (nuevoEstado nil))
    (dolist (operador *operadores* descendientes)
      (if (operadorValido? operador estado)
          (progn
            (setq nuevoEstado (aplicarOperador operador estado))
            (setq descendientes (cons (list nuevoEstado operador) descendientes)))))))

;[Funcion] Permite filtrar nuestra memoria
(defun filtrarMemoria (listaDeEstados lista)
  (cond ((null listaDeEstados) nil)
        ((recuerdasElEstadoEnMemoria? (first (first listaDeEstados)) lista)
         (filtrarMemoria (rest listaDeEstados) lista))
        (T (cons (first listaDeEstados) (filtrarMemoria (rest listaDeEstados) lista)))))

;[Funcion] Es un predicado, devuelve verdadero o falso si recuerda el estado en la memoria
(defun recuerdasElEstadoEnMemoria? (estado memoria)
(let* ((fila (aref estado 0))
         (columna (aref estado 1))
         (casillaActual (get-cell-walls fila columna)))
  (cond ((null memoria) nil)
        ((and (equal (aref estado 0) (aref (third (first memoria)) 0))
              (equal (aref estado 1) (aref (third (first memoria)) 1))
              (not (or (= casillaActual 16) (= casillaActual 17)))) T)
        (T (recuerdasElEstadoEnMemoria? estado (rest memoria))))))

;[Funcion] Permite extraer la solucion
(defun extract-solution (nodo)
  (labels ((locate-node (id lista)
             (cond ((null lista) nil)
                   ((eql id (first (first lista))) (first lista))
                   (T (locate-node id (rest lista))))))
    (let ((current (locate-node (first nodo) *memoria*)))
      (loop while (not (null current)) do
        (if (not (null (fifth current)))
        (push (fifth current) *sol*))
        (setq current (locate-node (fourth current) *memoria*))))
    *sol*))


(defun depth-first ()
  (limpiarVariables)
  (let ((nodo nil)
        (estado nil)
        (sucesores '())
        (inicialColumna nil)
        (inicialFila nil)
        (meta-encontrada nil)
        (metodo :depth-first))
	(setq *numeroDeFilas* (get-maze-rows))
	(setq *numeroDeColumnas* (get-maze-cols))
  (setq inicialColumna (aref *start* 0))
  (setq inicialFila (aref *start* 1))
  (insertarAFronteraDeBusqueda (make-array 3 :initial-contents (list inicialColumna inicialFila 0 )) nil metodo)
  (loop until (or meta-encontrada (null *fronteraDeBusqueda*)) do
       (setq nodo (obtenerDeFronteraDeBusqueda)
             estado (third nodo))
       (push nodo *memoria*)
       (cond ((and (equal (aref *goal* 0) (aref estado 0))
                   (equal (aref *goal* 1) (aref estado 1)))
              (setq *solution* (extract-solution nodo))
              (setq meta-encontrada T))
             (T (setq *ancestro* (first nodo)
                      sucesores (filtrarMemoria (expandir estado) *memoria*))
                (loop for elem in sucesores do
                     (insertarAFronteraDeBusqueda (first elem) (second elem) metodo)))))))

(defun breath-first ()
  (limpiarVariables)
      (let ((nodo nil)
            (estado nil)
            (sucesores '())
            (meta-encontrada nil)
            (inicialColumna nil)
            (inicialFila nil)
            (metodo :breath-first))
		(setq *numeroDeFilas* (get-maze-rows))
		(setq *numeroDeColumnas* (get-maze-cols))
    (setq inicialColumna (aref *start* 0))
    (setq inicialFila (aref *start* 1))
    (insertarAFronteraDeBusqueda (make-array 3 :initial-contents (list inicialColumna inicialFila 0 )) nil metodo)
    (loop until (or meta-encontrada
                    (null *fronteraDeBusqueda*)) do
         (setq nodo (obtenerDeFronteraDeBusqueda) estado (third nodo))
         (push nodo *memoria*)
         (cond ((and (equal (aref *goal* 0) (aref estado 0))
                     (equal (aref *goal* 1) (aref estado 1)))
                (setq *solution* (extract-solution nodo))
                (setq meta-encontrada T))
               (T (setq *ancestro* (first nodo)
                        sucesores (filtrarMemoria (expandir estado) *memoria*))
                  (loop for elem in sucesores do
                       (insertarAFronteraDeBusqueda (first elem) (second elem) metodo)))))))

(defun best-first ()
  (limpiarVariables)
      (let ((nodo nil)
            (estado nil)
            (sucesores '())
            (inicialColumna nil)
            (inicialFila nil)
            (meta-encontrada nil)
            (metodo :best-first))
        (setq *numeroDeFilas* (get-maze-rows))
        (setq *numeroDeColumnas* (get-maze-cols))
        (setq inicialColumna (aref *start* 0))
        (setq inicialFila (aref *start* 1))
        (insertarAFronteraDeBusqueda (make-array 3 :initial-contents (list inicialColumna inicialFila 0 )) nil metodo)
        (loop until (or meta-encontrada (null *fronteraDeBusqueda*)) do
             (setq nodo (obtenerDeFronteraDeBusqueda)
                   estado (third nodo))
             (push nodo *memoria*)
             (cond ((and (equal (aref *goal* 0) (aref estado 0))
                         (equal (aref *goal* 1) (aref estado 1)))
                    (setq *solution* (extract-solution nodo))
                    (setq meta-encontrada T))
                   (T (setq *ancestro* (first nodo)
                            sucesores (filtrarMemoria (filtrarMemoria (expandir estado) *fronteraDeBusqueda*) *memoria*))
                      (loop for elem in sucesores do
                           (insertarAFronteraDeBusqueda (first elem) (second elem) metodo)))))))

(defun A* ()
(limpiarVariables)
      (let ((nodo nil)
            (estado nil)
            (sucesores '())
            (inicialColumna nil)
            (inicialFila nil)
            (meta-encontrada nil)
            (metodo :Astar))
        (setq *numeroDeFilas* (get-maze-rows))
        (setq *numeroDeColumnas* (get-maze-cols))
        (setq inicialColumna (aref *start* 0))
        (setq inicialFila (aref *start* 1))
        (insertarAFronteraDeBusqueda (make-array 3 :initial-contents (list inicialColumna inicialFila 0 )) nil metodo)
        (loop until (or meta-encontrada (null *fronteraDeBusqueda*)) do
             (setq nodo (obtenerDeFronteraDeBusqueda)
                   estado (third nodo))
             (push nodo *memoria*)
             (cond ((and (equal (aref *goal* 0) (aref estado 0))
                         (equal (aref *goal* 1) (aref estado 1)))
                    (setq *solution* (extract-solution nodo))
                    (setq meta-encontrada T))
                   (T (setq *ancestro* (first nodo)
                            sucesores (filtrarMemoria (expandir estado) *memoria*))
                      (loop for elem in sucesores do
                           (insertarAFronteraDeBusqueda (first elem) (second elem) metodo)))))))

;[Inicio] Iniciamos nuestro laberinto
(start-maze)



