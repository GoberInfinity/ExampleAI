;Reyes Fragoso Roberto

;Cargamos la libreria para comenzar a trabajar con ella
(load "maze_lib.lisp")

;Algoritmo al menu de la pagina principal
(add-algorithm 'depth-first)

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
;(defparameter *numeroDeFilas* (get-maze-rows))
;(defparameter *numeroDeColumnas* (get-maze-cols))

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

;Permite crear los nodos necesarios
(defun crearNodo (estado operador importancia)
  (incf *id*)
  (list (1- *id*) importancia estado *ancestro* (second operador)))

;[Funcion] Permite insertar a frontera de Busqueda
(defun insertarAFronteraDeBusqueda (estado operador metodoBusqueda)
  (let* ((nodo '()))
    (cond ((eql metodoBusqueda :depth-first )
           (setq nodo (crearNodo estado operador nil))
           (push nodo *fronteraDeBusqueda*))
          ((eql metodoBusqueda :breath-first )
           (setq nodo (crearNodo estado operador nil))
           (setq *fronteraDeBusqueda* (append *fronteraDeBusqueda* (list nodo)))))))

;[Funcion] Permite obtener el ultimo elemento de la frontera de busqueda
(defun obtenerDeFronteraDeBusqueda ()
  (pop *fronteraDeBusqueda*))

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

    (if (not (= fila 0)) (setq casillaArriba (get-cell-walls (1- fila) columna)))
    (if (not (= columna 0)) (setq casillaIzquierda (get-cell-walls fila (1- columna))))
	(if (not (= columna (1- *numeroDeColumnas*))) (setq casillaDerecha (get-cell-walls fila (1+ columna))))
    (if (not (= fila (1- *numeroDeFilas*))) (setq casillaAbajo (get-cell-walls (1+ fila) columna)))
   

    (cond ((= operador 0)
           (and (not (= fila 0))
                (= (boole boole-and casillaActual 1) 0)))
          ((= operador 1)
           (and (not (= fila 0))
                (not (= columna (1- *numeroDeColumnas*)))
                (and (or (= (boole boole-and casillaActual 1) 0)
                         (= (boole boole-and casillaDerecha 1) 0))
                     (or (= (boole boole-and casillaArriba 2) 0)
                         (= (boole boole-and casillaDerecha 1) 0))
                     (or (= (boole boole-and casillaArriba 2) 0)
                         (= (boole boole-and casillaActual 2) 0))
                     (or (= (boole boole-and casillaActual 1) 0)
                         (= (boole boole-and casillaActual 2) 0)))))
          ((= operador 2) (and (not (= columna (1- *numeroDeColumnas*)))
                         (= (boole boole-and casillaActual 2) 0)))
          ((= operador 3) (and (not (= fila (1- *numeroDeFilas*)))
                         (not (= columna (1- *numeroDeColumnas*)))
                         (and (or (= (boole boole-and casillaActual 4) 0)
                                  (= (boole boole-and casillaDerecha 4) 0))
                              (or (= (boole boole-and casillaAbajo 2) 0)
                                  (= (boole boole-and casillaDerecha 4) 0))
                              (or (= (boole boole-and casillaAbajo 2) 0)
                                  (= (boole boole-and casillaActual 2) 0))
                              (or (= (boole boole-and casillaActual 4) 0)
                                  (= (boole boole-and casillaActual 2) 0)))))
          ((= operador 4) (and (not (= fila (1- *numeroDeFilas*)))
                         (= (boole boole-and casillaActual 4) 0)))
          ((= operador 5) (and (not (= fila (1- *numeroDeFilas*)))
                         (not (= columna 0))
                         (and (or (= (boole boole-and casillaActual 4) 0)
                                  (= (boole boole-and casillaIzquierda 4) 0))
                              (or (= (boole boole-and casillaAbajo 8) 0)
                                  (= (boole boole-and casillaIzquierda 4) 0))
                              (or (= (boole boole-and casillaAbajo 8) 0)
                                  (= (boole boole-and casillaActual 8) 0))
                              (or (= (boole boole-and casillaActual 4) 0)
                                  (= (boole boole-and casillaActual 8) 0)))))
          ((= operador 6) (and (not (= columna 0))
                         (= (boole boole-and casillaActual 8) 0)))
          ((= operador 7) (and (not (= fila 0))
                         (not (= columna 0))
                         (and (or (= (boole boole-and casillaActual 1) 0)
                                  (= (boole boole-and casillaIzquierda 1) 0))
                              (or (= (boole boole-and casillaArriba 8) 0)
                                  (= (boole boole-and casillaIzquierda 1) 0))
                              (or (= (boole boole-and casillaArriba 8) 0)
                                  (= (boole boole-and casillaActual 8) 0))
                              (or (= (boole boole-and casillaActual 1) 0)
                                  (= (boole boole-and casillaActual 8) 0)))))
          (T nil))))

;[Funcion] Permite aplicar el operador al estado
(defun aplicarOperador (operador estado)
  (if (operadorValido? operador estado)
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
    estadoFinal)))

;[Funcion] Permite expandir el estado
(defun expandir (estado)
  (let ((descendientes nil) (nuevoEstado nil))
    (dolist (operador *operadores* descendientes)
      (setq nuevoEstado (aplicarOperador operador estado))
      (if (not (null nuevoEstado))
          (setq descendientes (cons (list nuevoEstado operador) descendientes))))))

(defun filtrarMemoria (listaDeEstados lista)
  (cond ((null listaDeEstados) nil)
        ((recuerdasElEstado? (first (first listaDeEstados)) lista)
         (filtrarMemoria (rest listaDeEstados) lista))
        (T (cons (first listaDeEstados) (filtrarMemoria (rest listaDeEstados) lista)))))

(defun recuerdasElEstado? (estado memoria)
  (cond ((null memoria) nil)
        ((and (equal (aref estado 0) (aref (third (first memoria)) 0))
              (equal (aref estado 1) (aref (third (first memoria)) 1)))
         T)
        (T (recuerdasElEstado? estado (rest memoria)))))

(defun extract-solution (nodo)
  "Función para obtener la solución analizando los id's de cada nodo recorrido hasta el nodo meta que es el que se proporciona como atributo de la función"
  (labels ((locate-node (id lista); Hacemos una función local para localizar el nodo que le precede al nodo actual
             (cond ((null lista) nil); En caso de ser nula la lista regresamos nil
                   ((eql id (first (first lista))) (first lista)); Si encontramos el id que buscamos regresamos ese elemento
                   (T (locate-node id (rest lista)))))); En caso contrario seguimod buscando el nodo
    (let ((current (locate-node (first nodo) *memoria*))); Buscamos por primera vez el nodo ancestro y le asignamos el valor a current
      (loop while (not (null current)) do ; Mientras current no sea nil hacemos el siguiente ciclo
        (if (not (null (fifth current)))
        (push (fifth current) *sol*)); Agregamos current que es el nodo ancestro a solución
        (setq current (locate-node (fourth current) *memoria*)))); Y buscamos el nodo ancestro al previamente encontrado y se le asigna nuevamente a current
    *sol*)); Por último regresamos la solución


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


(start-maze)



