;==================================================================
; lab2d.lisp                                                      |
;   Resuleve el problema de Laberintos 2D con los 4 algoritmos de |
;   de búsqueda (Depth-first, Breath-first, BS-first, A*)         |
; Eugenio Gabriel Andreu Lecona                                   |
;==================================================================

;===================================================================================================
; Nodo (<id> <prioridad> <estado> <id-ancestro> <operador>)                                  		   |
; Estado #(<fila> <columna>)                                                                       |
; Operador (<etiqueta> <lista-operacion>)                                                  		     |
;===================================================================================================

;Librería

(load "maze_lib.lisp")

(add-algorithm 'depth-first)
(add-algorithm 'breath-first)
(add-algorithm 'best-first)
(add-algorithm 'A*)

; Variables globales
(defparameter *open* '()) ; Frontera de busqueda
(defparameter *closed* '()) ; Memoria de intentos previos

(defparameter *oper* '((:Mover-Arriba 0)
                       (:Mover-Arriba-Derecha 1)
                       (:Mover-Derecha 2)
                       (:Mover-Abajo-Derecha 3)
                       (:Mover-Abajo 4)
                       (:Mover-Abajo-Izquierda 5)
                       (:Mover-Izquierda 6)
                       (:Mover-Arriba-Izquierda 7))) ; Operadores

(defparameter *id* 0) ; Identificador de cada nodo
(defparameter *id-ancestor* nil) ; Identificador del ancestro actual
(defparameter *sol* nil) ; Lista donde se alamacenara la solución
(defparameter *filas* (get-maze-rows))
(defparameter *columnas* (get-maze-cols))

; Funciones

;=================================================================
; Función para crear nodos                                       |
;=================================================================

(defun create-node (estado op prioridad)
  "Función que crea un nodo con la estructura (<id> <estado> <id-ancestro> <operador>) mediante un estado y un operador proporcionados"
  (incf *id*) ; Aumentamos el contador
  (list (1- *id*) prioridad estado *id-ancestor* (second op))); Creamos el nodo

;=================================================================
; Funciones de Heurística                                        |
;=================================================================

(defun Manhattan (edo)
  (max (- (max (aref edo 0) (aref *goal* 0))
          (min (aref edo 0) (aref *goal* 0)))
       (- (max (aref edo 1) (aref *goal* 1))
          (min (aref edo 1) (aref *goal* 1)))))

(defun Backtracking (nodo num)
  (labels ((locate-node (id lista); Hacemos una función local para localizar el nodo que le precede al nodo actual
             (cond ((null lista) nil); En caso de ser nula la lista regresamos nil
                   ((eql id (first (first lista))) (first lista)); Si encontramos el id que buscamos regresamos ese elemento
                   (T (locate-node id (rest lista)))))); En caso contrario seguimod buscando el nodo
    (let ((current (locate-node (fourth nodo) *closed*))); Buscamos por primera vez el nodo ancestro y le asignamos el valor a current
      (loop while (not (null current)) do ; Mientras current no sea nil hacemos el siguiente ciclo
        (setq num (incf num)); Agregamos current que es el nodo ancestro a solución
        (setq current (locate-node (fourth current) *closed*)))); Y buscamos el nodo ancestro al previamente encontrado y se le asigna nuevamente a current
   num)); Por último regresamos la solución

;=================================================================
; Funciones para Administrar la Frontera de Búsqueda             |
;=================================================================

(defun insert-to-open (estado op metodo)
  "Función que inserta un nodo a la frontera de búsqueda dependiendo el tipo de búsqueda ya sea :depth-first o :breath-first"
  (let ((nodo nil)) ; Creamos el nodo con el estado y operador que tenemos
    (cond ((eql metodo :depth-first)  ; En caso de ser búsqueda a lo profundo insertamos de esta forma en la frontera de búsqueda
           (setq nodo (create-node estado op nil))
           (push nodo *open*))
          ((eql metodo :breath-first) ; En caso de ser búsqueda a lo ancho insertamos de esta forma en la frontera de búsqueda
           (setq nodo (create-node estado op nil))
           (setq *open* (append *open* (list nodo))))
          ((eql metodo :best-first)
           (setq nodo (create-node estado op (Manhattan estado)))
           (push nodo *open*)
           (setq *open* (stable-sort *open* '< :key #'(lambda (x) (second x)))))
          ((eql metodo :Astar)
           (setq nodo (create-node estado op (Manhattan estado)))
           (setf (second nodo) (+ (second nodo) (Backtracking nodo 0)))
           (if (remember-state? (third nodo) *open*)
               (check-state nodo *open*)
               (push nodo *open*))
           (setq *open* (stable-sort *open* '< :key #'(lambda (x) (second x)))))
          (T nil))))

(defun get-from-open ()
  "Función que regresa el primer elemento en la frontera de búsqueda"
  (pop *open*));

;=================================================================================================================
; Funciones de validación (Predicados), la primera de disponibilidad de recursos y la segunda de estados válidos |
;=================================================================================================================

(defun valid-operator? (oper estado)
  "Predicado para validar que los recursos que requiere el operador proporcionado los tenga disponibles el estado al cual sera aplicado"
  (let* ((fil (aref estado 0))
         (col (aref estado 1))
         (val (get-cell-walls fil col))
         (casArriba nil)
         (casDer nil)
         (casIzq nil)
         (casAbajo nil)
         (op (second oper)))
    (if (not (= fil 0)) (setq casArriba (get-cell-walls (1- fil) col)))
    (if (not (= col 0)) (setq casIzq (get-cell-walls fil (1- col))))
    (if (not (= col (1- *columnas*))) (setq casDer (get-cell-walls fil (1+ col))))
    (if (not (= fil (1- *filas*))) (setq casAbajo (get-cell-walls (1+ fil) col)))
    (cond ((= op 0) (and (not (= fil 0))
                         (= (boole boole-and val 1) 0)))
          ((= op 1) (and (not (= fil 0))
                         (not (= col (1- *columnas*)))
                         (and (or (= (boole boole-and val 1) 0)
                                  (= (boole boole-and casDer 1) 0))
                              (or (= (boole boole-and casArriba 2) 0)
                                  (= (boole boole-and casDer 1) 0))
                              (or (= (boole boole-and casArriba 2) 0)
                                  (= (boole boole-and val 2) 0))
                              (or (= (boole boole-and val 1) 0)
                                  (= (boole boole-and val 2) 0)))))
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

;=================================================================
; Funciones para aplicar operadores y expandir estados           |
;=================================================================

(defun apply-operator (op estado)
  "Función para aplicar un operador a cierto estado y obtener el estado resultante en caso de cumplir las dos validaciones previas"
  (if (valid-operator? op estado) ; En caso de tener recursos disponibles realizamos lo siguiente
      (let* ((fil (aref estado 0))
             (col (aref estado 1))
             (operador (first op)); Obtenemos la etiqueta humana
             (estado-res nil)); Variable auxiliar para obtener el estado resultante
    (case operador; Dependiendo de la etiqueta humana del operador es lo que se va a realizar
      (:Mover-Arriba (setq estado-res (make-array 2 :initial-contents (list (1- fil) col))))
      (:Mover-Arriba-Derecha (setq estado-res (make-array 2 :initial-contents (list (1- fil) (1+ col)))))
      (:Mover-Derecha (setq estado-res (make-array 2 :initial-contents (list fil (1+ col)))))
      (:Mover-Abajo-Derecha (setq estado-res (make-array 2 :initial-contents (list (1+ fil) (1+ col)))))
      (:Mover-Abajo (setq estado-res (make-array 2 :initial-contents (list (1+ fil) col))))
      (:Mover-Abajo-Izquierda (setq estado-res (make-array 2 :initial-contents (list (1+ fil) (1- col)))))
      (:Mover-Izquierda (setq estado-res (make-array 2 :initial-contents (list fil (1- col)))))
      (:Mover-Arriba-Izquierda (setq estado-res (make-array 2 :initial-contents (list (1- fil) (1- col)))))
      (T "error"))
    estado-res))) ; En caso de que el estado sea valido se regresa el estado generado, en caso contrario nil

(defun expand (estado)
  "Función para expandir un estado dado con cada operador que genere un estado valido para ser ingresado a la frontera de búsqueda"
  (let ((descendientes nil) (nuevo-edo nil)); Generamos dos variables auxiliares descendientes y nuevo-edo
    (dolist (op *oper* descendientes); Para cada operador en la lista de operadores hacemos lo siguiente y regresamos al final la lista descendientes
      (setq nuevo-edo (apply-operator op estado)); Aplicamos el operador a dicho estado y se lo asignamos a nuevo-edo
      (if (not (null nuevo-edo)); En caso de que no sea nulo significa que es válido entonces lo agregamos a descendientes
          (setq descendientes (cons (list nuevo-edo op) descendientes))))))

;=================================================================
; Funciones para el manejo de intentos previos (*closed*)        |
;=================================================================

(defun check-state (nodo lista-memoria)
  (let ((nodoAux nil))
    (cond ((null lista-memoria) (push nodo *open*))
          ((and (equal (aref (third nodo) 0) (aref (third (first lista-memoria)) 0))
                (equal (aref (third nodo) 1) (aref (third (first lista-memoria)) 1)))
           (setq nodoAux (first lista-memoria))
           (if (< (second nodo) (second nodoAux))
               (progn (delete nodoAux lista-memoria)
                      (push nodo *open*))))
          (T (check-state nodo (rest lista-memoria))))))

(defun remember-state? (estado lista-memoria)
  "Predicado para ver si un estado se encuentra en la memoria de intentos previos"
  (cond ((null lista-memoria) nil); Si la memoria esta vacia regresamos nil
       ; En caso de que el estado coincida con el segundo elemento del primer nodo, regresamos T
        ((and (equal (aref estado 0) (aref (third (first lista-memoria)) 0))
              (equal (aref estado 1) (aref (third (first lista-memoria)) 1)))
         T)
        (T (remember-state? estado (rest lista-memoria))))); En caso de no encontrarlo seguimos analiando el resto de los nodos en memoria

(defun filter-memories (lista-estados-y-ops lista-a-consultar)
  "Función para filtrar los estados ya analizados y almacenados en memoria de intentos previos de  una lista con estados y operadores"
  (cond ((null lista-estados-y-ops) nil) ; Si la lista esta vacía, regresamos nil
        ((remember-state? (first (first lista-estados-y-ops)) lista-a-consultar)
         (filter-memories (rest lista-estados-y-ops) lista-a-consultar)); En caso de que el estado ya se encuentre en memoria revisamos el resto
        (T (cons (first lista-estados-y-ops) (filter-memories (rest lista-estados-y-ops) lista-a-consultar))))); De lo contrario regresamos los que no se han aplicado y no estan en memoria aún

;=================================================================
; Funciones para obtener y desplegar la solución en consola      |
;=================================================================

(defun extract-solution (nodo)
  "Función para obtener la solución analizando los id's de cada nodo recorrido hasta el nodo meta que es el que se proporciona como atributo de la función"
  (labels ((locate-node (id lista); Hacemos una función local para localizar el nodo que le precede al nodo actual
             (cond ((null lista) nil); En caso de ser nula la lista regresamos nil
                   ((eql id (first (first lista))) (first lista)); Si encontramos el id que buscamos regresamos ese elemento
                   (T (locate-node id (rest lista)))))); En caso contrario seguimod buscando el nodo
    (let ((current (locate-node (first nodo) *closed*))); Buscamos por primera vez el nodo ancestro y le asignamos el valor a current
      (loop while (not (null current)) do ; Mientras current no sea nil hacemos el siguiente ciclo
        (if (not (null (fifth current)))
        (push (fifth current) *sol*)); Agregamos current que es el nodo ancestro a solución
        (setq current (locate-node (fourth current) *closed*)))); Y buscamos el nodo ancestro al previamente encontrado y se le asigna nuevamente a current
    *sol*)); Por último regresamos la solución

;===================================================================================================
; Funciones generales, para reseatear variables globales y la función principal de nuestro programa|
;===================================================================================================

(defun reset-all ()
  "Función para resetear variables globales"
  (setq *open* nil
        *closed* nil
        *id* 0
        *id-ancestor* nil
        *sol* nil
        *columnas* (get-maze-cols)
        *filas* (get-maze-rows)))

(defun depth-first ()
  (progn (reset-all); Reseteamos las variables globales
      (let ((nodo nil); Creamos variables locales para facilitar la legibilidad del código
            (estado nil)
            (sucesores '())
            (meta-encontrada nil)
            (metodo :depth-first))
        (insert-to-open *start* nil metodo); Insertamos a la frontera de búsqueda el nodo del estado inicial
        (loop until (or meta-encontrada; Entramos en un ciclo mientras no se encuentre el estado final o este vacía la frontera de búsqueda, que realize lo siguiente
                        (null *open*)) do
                          (setq nodo (get-from-open); Obtenemos el primer nodo de la frontera de búsqueda
                                estado (third nodo)); Obtenemos el estado de ese nodo
                          (push nodo *closed*); Agregamos el nodo a la memoria de intentos previos
                          (cond ((and (equal (aref *goal* 0)
                                             (aref estado 0))
                                      (equal (aref *goal* 1)
                                             (aref estado 1))); si el estado actual es igual al meta
                                 (setq *solution* (extract-solution nodo)); Desplegamos la solución
                                 (setq meta-encontrada T)); Asignamos el valor T a meta-encontrada para terminar el ciclo
                                (T (setq *id-ancestor* (first nodo)
                                         sucesores (filter-memories (expand estado) *closed*)); En caso contrario exandimos el nodo con los operadores, filtramos ese resultado con la memoria de intentos previos y se van insertando a la frontera de búsqueda
                                   (loop for elem in sucesores do
                                     (insert-to-open (first elem) (second elem) metodo))))))))

(defun breath-first ()
  (progn (reset-all); Reseteamos las variables globales
      (let ((nodo nil); Creamos variables locales para facilitar la legibilidad del código
            (estado nil)
            (sucesores '())
            (meta-encontrada nil)
            (metodo :breath-first))
        (insert-to-open *start* nil metodo); Insertamos a la frontera de búsqueda el nodo del estado inicial
        (loop until (or meta-encontrada; Entramos en un ciclo mientras no se encuentre el estado final o este vacía la frontera de búsqueda, que realize lo siguiente
                        (null *open*)) do
                          (setq nodo (get-from-open); Obtenemos el primer nodo de la frontera de búsqueda
                                estado (third nodo)); Obtenemos el estado de ese nodo
                          (push nodo *closed*); Agregamos el nodo a la memoria de intentos previos
                          (cond ((and (equal (aref *goal* 0)
                                             (aref estado 0))
                                      (equal (aref *goal* 1)
                                             (aref estado 1))); si el estado actual es igual al meta
                                 (setq *solution* (extract-solution nodo)); Desplegamos la solución
                                 (setq meta-encontrada T)); Asignamos el valor T a meta-encontrada para terminar el ciclo
                                (T (setq *id-ancestor* (first nodo)
                                         sucesores (filter-memories (expand estado) *closed*)); En caso contrario exandimos el nodo con los operadores, filtramos ese resultado con la memoria de intentos previos y se van insertando a la frontera de búsqueda
                                   (loop for elem in sucesores do
                                     (insert-to-open (first elem) (second elem) metodo))))))))

(defun best-first ()
  (progn (reset-all); Reseteamos las variables globales
      (let ((nodo nil); Creamos variables locales para facilitar la legibilidad del código
            (estado nil)
            (sucesores '())
            (meta-encontrada nil)
            (metodo :best-first))
        (insert-to-open *start* nil metodo); Insertamos a la frontera de búsqueda el nodo del estado inicial
        (loop until (or meta-encontrada; Entramos en un ciclo mientras no se encuentre el estado final o este vacía la frontera de búsqueda, que realize lo siguiente
                        (null *open*)) do
                          (setq nodo (get-from-open); Obtenemos el primer nodo de la frontera de búsqueda
                                estado (third nodo)); Obtenemos el estado de ese nodo
                          (push nodo *closed*); Agregamos el nodo a la memoria de intentos previos
                          (cond ((and (equal (aref *goal* 0)
                                             (aref estado 0))
                                      (equal (aref *goal* 1)
                                             (aref estado 1))); si el estado actual es igual al meta
                                 (setq *solution* (extract-solution nodo)); Desplegamos la solución
                                 (setq meta-encontrada T)); Asignamos el valor T a meta-encontrada para terminar el ciclo
                                (T (setq *id-ancestor* (first nodo)
                                         sucesores (filter-memories (filter-memories (expand estado) *closed*) *open*)); En caso contrario exandimos el nodo con los operadores, filtramos ese resultado con la memoria de intentos previos y se van insertando a la frontera de búsqueda
                                   (loop for elem in sucesores do
                                     (insert-to-open (first elem) (second elem) metodo))))))))

(defun A* ()
  (progn (reset-all); Reseteamos las variables globales
      (let ((nodo nil); Creamos variables locales para facilitar la legibilidad del código
            (estado nil)
            (sucesores '())
            (meta-encontrada nil)
            (metodo :Astar))
        (insert-to-open *start* nil metodo); Insertamos a la frontera de búsqueda el nodo del estado inicial
        (loop until (or meta-encontrada; Entramos en un ciclo mientras no se encuentre el estado final o este vacía la frontera de búsqueda, que realize lo siguiente
                        (null *open*)) do
                          (setq nodo (get-from-open); Obtenemos el primer nodo de la frontera de búsqueda
                                estado (third nodo)); Obtenemos el estado de ese nodo
                          (push nodo *closed*); Agregamos el nodo a la memoria de intentos previos
                          (cond ((and (equal (aref *goal* 0)
                                             (aref estado 0))
                                      (equal (aref *goal* 1)
                                             (aref estado 1))); si el estado actual es igual al meta
                                 (setq *solution* (extract-solution nodo)); Desplegamos la solución
                                 (setq meta-encontrada T)); Asignamos el valor T a meta-encontrada para terminar el ciclo
                                (T (setq *id-ancestor* (first nodo)
                                         sucesores (filter-memories (expand estado) *closed*)); En caso contrario exandimos el nodo con los operadores, filtramos ese resultado con la memoria de intentos previos y se van insertando a la frontera de búsqueda
                                   (loop for elem in sucesores do
                                     (insert-to-open (first elem) (second elem) metodo))))))))
(start-maze)
