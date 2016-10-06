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
(defparameter *numeroDeFilas* (get-maze-rows))
(defparameter *numeroDeColumnas* (get-maze-cols))

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

;[Funcion] Permite validar nuestro operador
(defun operadorValido? (estado operador)
  (let* ((fila (aref estado 0))
         (columna (aref estado 1))
         (casillaActual (get-cell-walls fila columna))
         (operador (second operador))
         (casillaArriba 0)
         (casillaAbajo 0)
         (casilaIzquierda 0)
         (casillaDerecha 0))

    (if (not (= fila 0)) (setq casillaArriba (get-cell-walls (1- fil) columna)))
    (if (not (= columna 0)) (setq casillaIzquierda (get-cell-walls fila (1- col))))
    (if (not (= fila (1- *numeroDeFilas*))) (setq casillaAbajo (get-cell-walls (1+ fila) columna)))
    (if (not (= columna (1- *numeroDeColumnas*))) (setq casillaDerecha (get-cell-walls fila (1+ columna))))

    (cond ((= operador 0)
           (and (not (= fila 0))
                (= (boole boole-and 1 casillaActual) 0)))
          ((= operador 1)
           (and (not (= fila 0))
                (not (= col (1- *columnas*)))
                (and (or (= (boole boole-and casillaArriba 1) 0)
                         (= (boole boole-and casillaDerecha 1) 0))
                     (or (= (boole boole-and casillaArriba 2) 0)
                         (= (boole boole-and casillaDerecha 1) 0))
                     (or (= (boole boole-and casillaArriba 2) 0)
                         (= (boole boole-and casillaActual 2) 0))
                     (or (= (boole boole-and casiilaActual 1) 0)
                         (= (boole boole-and casillaActual 2) 0)))))
          ((= op 2) (and (not (= col (1- *columnas*)))
                         (= (boole boole-and val 2) 0)))
          ((= op 3) (and (not (= fil (1- *filas*)))
                         (not (= col (1- *columnas*)))
                         (and (or (= (boole boole-and val 4) 0)
                                  (= (boole boole-and casDer 4) 0))
                              (or (= (boole boole-and casAbajo 2) 0)
                                  (= (boole boole-and casDer 4) 0))
                              (or (= (boole boole-and casAbajo 2) 0)
                                  (= (boole boole-and val 2) 0))
                              (or (= (boole boole-and val 4) 0)
                                  (= (boole boole-and val 2) 0)))))
          ((= op 4) (and (not (= fil (1- *filas*)))
                         (= (boole boole-and val 4) 0)))
          ((= op 5) (and (not (= fil (1- *filas*)))
                         (not (= col 0))
                         (and (or (= (boole boole-and val 4) 0)
                                  (= (boole boole-and casIzq 4) 0))
                              (or (= (boole boole-and casAbajo 8) 0)
                                  (= (boole boole-and casIzq 4) 0))
                              (or (= (boole boole-and casAbajo 8) 0)
                                  (= (boole boole-and val 8) 0))
                              (or (= (boole boole-and val 4) 0)
                                  (= (boole boole-and val 8) 0)))))
          ((= op 6) (and (not (= col 0))
                         (= (boole boole-and val 8) 0)))
          ((= op 7) (and (not (= fil 0))
                         (not (= col 0))
                         (and (or (= (boole boole-and val 1) 0)
                                  (= (boole boole-and casIzq 1) 0))
                              (or (= (boole boole-and casArriba 8) 0)
                                  (= (boole boole-and casIzq 1) 0))
                              (or (= (boole boole-and casArriba 8) 0)
                                  (= (boole boole-and val 8) 0))
                              (or (= (boole boole-and val 1) 0)
                                  (= (boole boole-and val 8) 0)))))
          (T nil))))

;[Funcion] Permite aplicar el operador al estado
(defun apply-operator (operador estado)
  (if (operadorValido? operador estado)
      (let* ((fila (aref estado 0))
             (columna (aref estado 1))
             (operador (first op))
             (estadoFinal nil))
    (case operador
      (:Arriba (setq estadoFinal (make-array 2 :initial-contents (list (1- fila) columna))))
      (:ArribaDerecha (setq estadoFinal (make-array 2 :initial-contents (list (1- fila) (1+ columna)))))
      (:Derecha (setq estadoFinal (make-array 2 :initial-contents (list fila (1+ columna)))))
      (:AbajoDerecha (setq estadoFinal (make-array 2 :initial-contents (list (1+ fila) (1+ columna)))))
      (:Abajo (setq estadoFinals (make-array 2 :initial-contents (list (1+ fila) columna))))
      (:AbajoIzquierda (setq estadoFinal (make-array 2 :initial-contents (list (1+ fila) (1- columna)))))
      (:Izquierda (setq estadoFinal (make-array 2 :initial-contents (list fila (1- columna)))))
      (:ArribaIzquierda (setq estadoFinal (make-array 2 :initial-contents (list (1- fila) (1- columna)))))
      (T "error"))
    estadoFinal)))



;;(get-base-data)



;;(start-maze)







