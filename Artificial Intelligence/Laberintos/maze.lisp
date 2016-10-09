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
  (setq *ancestro*  nil)
  (setq *solution*  nil))

;[Funcion]Permite crear los nodos necesarios
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

;[Funcion] Permite hacer el backtracking para A*
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

;[FUNCIONES DE VALIDACIONES] El bloque siguiente de funciones permite ver si es valido el operador, regresa T o nil
(defun puedeOperarArriba? (casillaArriba casillaActual)
  (cond ((null casillaArriba) nil)
        ((= (boole boole-and casillaActual 1) 0) T)
        (T nil)))

;[Validacion]		
(defun puedeOperarDerecha? (casillaDerecha casillaActual)
  (cond ((null casillaDerecha) nil)
        ((= (boole boole-and casillaActual 2) 0) T)
        (T nil)))

;[Validacion]
(defun puedeOperarAbajo? (casillaAbajo casillaActual)
  (cond ((null casillaAbajo) nil)
        ((= (boole boole-and casillaActual 4) 0) T)
        (T nil)))

;[Validacion]
(defun puedeOperarIzquierda? (casillaIzquierda casillaActual)
  (cond ((null casillaIzquierda) nil)
        ((= (boole boole-and casillaActual 8) 0) T)
        (T nil)))
		
;[Validacion]
(defun puedeOperarArribaDerecha? (casillaActual casillaArriba casillaDerecha)
  (cond ((or (null casillaArriba) (null casillaDerecha)) nil)
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
(defun puedeOperarAbajoDerecha? (casillaActual casillaAbajo casillaDerecha)
  (cond ((or (null casillaDerecha) (null casillaAbajo)) nil)
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
(defun puedeOperarAbajoIzquierda? (casillaActual casillaAbajo casillaIzquierda)
  (cond ((or (null casillaAbajo) (null casillaIzquierda)) nil)
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
(defun puedeOperarArribaIzquierda? (casillaActual casillaArriba casillaIzquierda)
  (cond ((or (null casillaArriba) (null casillaIzquierda)) nil)
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
         (casillaArriba 0)
         (casillaAbajo 0)
         (casillaIzquierda 0)
         (casillaDerecha 0))

    (if (not (= fila 0)) (setq casillaArriba (get-cell-walls (1- fila) columna))(setq casillaArriba nil))
    (if (not (= columna 0)) (setq casillaIzquierda (get-cell-walls fila (1- columna))) (setq casillaIzquierda nil))
	  (if (not (= columna (1- *numeroDeColumnas*))) (setq casillaDerecha (get-cell-walls fila (1+ columna)))(setq casillaDerecha nil))
    (if (not (= fila (1- *numeroDeFilas*))) (setq casillaAbajo (get-cell-walls (1+ fila) columna))(setq casillaAbajo nil))

    (cond ((= operador 0)
           (puedeOperarArriba? casillaArriba casillaActual))
          ((= operador 1)
           (puedeOperarArribaDerecha? casillaActual casillaArriba casillaDerecha))
          ((= operador 2)
           (puedeOperarDerecha? casillaDerecha casillaActual))
          ((= operador 3)
           (puedeOperarAbajoDerecha? casillaActual casillaAbajo casillaDerecha))
          ((= operador 4)
           (puedeOperarAbajo? casillaAbajo casillaActual))
          ((= operador 5)
           (puedeOperarAbajoIzquierda? casillaActual casillaAbajo casillaIzquierda))
          ((= operador 6)
           (puedeOperarIzquierda? casillaIzquierda casillaActual))
          ((= operador 7)
           (puedeOperarArribaIzquierda? casillaActual casillaArriba casillaIzquierda))
          (T nil))))

;[Funcion] Permite aplicar el operador al estado
(defun aplicarOperador (operador estado)
  (let* ((fila (aref estado 0))
         (columna (aref estado 1))
         (operador (first operador))
         (estadoFinal nil))

    (case operador
      (:Mover-Arriba (setq estadoFinal (make-array 2 :initial-contents (list (1- fila) columna))))
      (:Mover-Arriba-Derecha (setq estadoFinal (make-array 2 :initial-contents (list (1- fila) (1+ columna)))))
      (:Mover-Derecha (setq estadoFinal (make-array 2 :initial-contents (list fila (1+ columna)))))
      (:Mover-Abajo-Derecha (setq estadoFinal (make-array 2 :initial-contents (list (1+ fila) (1+ columna)))))
      (:Mover-Abajo (setq estadoFinal (make-array 2 :initial-contents (list (1+ fila) columna))))
      (:Mover-Abajo-Izquierda (setq estadoFinal (make-array 2 :initial-contents (list (1+ fila) (1- columna)))))
      (:Mover-Izquierda (setq estadoFinal (make-array 2 :initial-contents (list fila (1- columna)))))
      (:Mover-Arriba-Izquierda (setq estadoFinal (make-array 2 :initial-contents (list (1- fila) (1- columna)))))
      (T "error"))
    estadoFinal))

;[Funcion] Permite checar nuestro estado en A*
(defun checarEstado (nodo lista-memoria)
  (let ((nodoAux nil))
    (cond ((null lista-memoria) (push nodo *fronteraDeBusqueda*))
          ((and (equal (aref (third nodo) 0) (aref (third (first lista-memoria)) 0))
                (equal (aref (third nodo) 1) (aref (third (first lista-memoria)) 1)))
           (setq nodoAux (first lista-memoria))
           (if (< (second nodo) (second nodoAux))
               (progn (delete nodoAux lista-memoria)
                      (push nodo *fronteraDeBusqueda*))))
          (T (checarEstado nodo (rest lista-memoria))))))

;[Funcion] Permite expandir el estado
(defun expandir (estado)
  (let ((descendientes nil) (nuevoEstado nil))
    (dolist (operador *operadores* descendientes)
      (if (operadorValido? operador estado)
          (progn
            (setq nuevoEstado (aplicarOperador operador estado))
            (setq descendientes (cons (list nuevoEstado operador) descendientes)))))))

(defun filtrarMemoria (listaDeEstados lista)
  (cond ((null listaDeEstados) nil)
        ((recuerdasElEstadoEnMemoria? (first (first listaDeEstados)) lista)
         (filtrarMemoria (rest listaDeEstados) lista))
        (T (cons (first listaDeEstados) (filtrarMemoria (rest listaDeEstados) lista)))))

;[Funcion] Es un predicado pemrite saber si el estado esta en la memoria
(defun recuerdasElEstadoEnMemoria? (estado memoria)
  (cond ((null memoria) nil)
        ((and (equal (aref estado 0) (aref (third (first memoria)) 0))
              (equal (aref estado 1) (aref (third (first memoria)) 1))) T)
        (T (recuerdasElEstadoEnMemoria? estado (rest memoria)))))

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
        (meta-encontrada nil)
        (metodo :depth-first))
	(setq *numeroDeFilas* (get-maze-rows))
	(setq *numeroDeColumnas* (get-maze-cols))
    (insertarAFronteraDeBusqueda *start* nil metodo)
    (loop until (or meta-encontrada
                    (null *fronteraDeBusqueda*)) do
         (setq nodo (obtenerDeFronteraDeBusqueda)
               estado (third nodo))
         (push nodo *memoria*)
         (cond ((and (equal (aref *goal* 0)
                            (aref estado 0))
                     (equal (aref *goal* 1)
                            (aref estado 1)))
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
            (metodo :breath-first))
		(setq *numeroDeFilas* (get-maze-rows))
		(setq *numeroDeColumnas* (get-maze-cols))
        (insertarAFronteraDeBusqueda *start* nil metodo)
        (loop until (or meta-encontrada
                        (null *fronteraDeBusqueda*)) do
                          (setq nodo (obtenerDeFronteraDeBusqueda)
                                estado (third nodo))
                          (push nodo *memoria*)
                          (cond ((and (equal (aref *goal* 0)
                                             (aref estado 0))
                                      (equal (aref *goal* 1)
                                             (aref estado 1)))
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
            (meta-encontrada nil)
            (metodo :best-first))
		(setq *numeroDeFilas* (get-maze-rows))
		(setq *numeroDeColumnas* (get-maze-cols))
        (insertarAFronteraDeBusqueda *start* nil metodo)
        (loop until (or meta-encontrada
                        (null *fronteraDeBusqueda*)) do
                          (setq nodo (obtenerDeFronteraDeBusqueda)
                                estado (third nodo))
                          (push nodo *memoria*)
                          (cond ((and (equal (aref *goal* 0)
                                             (aref estado 0))
                                      (equal (aref *goal* 1)
                                             (aref estado 1)))
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
            (meta-encontrada nil)
            (metodo :Astar))
			(setq *numeroDeFilas* (get-maze-rows))
		(setq *numeroDeColumnas* (get-maze-cols))
        (insertarAFronteraDeBusqueda *start* nil metodo)
        (loop until (or meta-encontrada
                        (null *fronteraDeBusqueda*)) do
                          (setq nodo (obtenerDeFronteraDeBusqueda)
                                estado (third nodo))
                          (push nodo *memoria*)
                          (cond ((and (equal (aref *goal* 0)
                                             (aref estado 0))
                                      (equal (aref *goal* 1)
                                             (aref estado 1)))
                                 (setq *solution* (extract-solution nodo))
                                 (setq meta-encontrada T))
                                (T (setq *ancestro* (first nodo)
                                         sucesores (filtrarMemoria (expandir estado) *memoria*))
                                   (loop for elem in sucesores do
                                     (insertarAFronteraDeBusqueda (first elem) (second elem) metodo)))))))

;[Funcion] Permite iniciar nuestro laberinto
(start-maze)



