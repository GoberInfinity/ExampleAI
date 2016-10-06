;Reyes Fragoso Roberto

;Cargamos la libreria para comenzar a trabajar con ella
(load "maze_lib.lisp")

;Algoritmo al menu de la pagina principal
(add-algorithm 'depth-first)
(add-algorithm 'breadth-first)
(add-algorithm 'best-first)
(add-algorithm 'a-star)

;Permite saber para cada problema la frontera de busqueda y memoria
(defparameter *fronteraDeBusqueda* '())
(defparameter *memoria* '())

;Permite almacenar los datos del laberinto
(defparameter *id* 0)
(defparameter *ancestro* nil)
(defparameter *solucion* nil)
(defparameter *filas* (get-maze-rows))
(defparameter *columnas* (get-maze-cols))

;Definicion de operadores
(defparameter *operadores* '((:Arriba 0)
                             (:ArribaIzquierda 1)
                             (:ArribaDerecha 2)
                             (:Izquierda 3)
                             (:AbajoIzquierda 4)
                             (:Abajo 5)
                             (:AbajoDerecha 6)
                             (:Derecha 7)))

;[Funcion] Permite resetear todo
(defun limpiarVariables ()
  (setq *fronteraDeBusqueda*  nil)
  (setq *memoria*  nil)
  (setq *id*  0)
  (setq *ancestro*  nil)
  (setq *solucion*  nil))

;Permite crear los nodos necesarios
(defun crearNodo (estado operador importancia)
  (incf *id*)
  (list (1- *id*) importancia *ancestro* operador))

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


;;(get-base-data)



;;(start-maze)







