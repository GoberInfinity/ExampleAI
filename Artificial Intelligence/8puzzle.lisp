; Reyes Fragoso Roberto
(defparameter *fronteraDeBusqueda* '())
(defparameter *memoria* '())

;Definicion de Operadores, los definimos como nil por que cada operador es
; diferente dependiendo de la casilla en blanco
(defparameter *operadores* '((:Arriba nil)
                             (:Abajo nil)
                             (:Izquierda nil)
                             (:Derecha nil)))

;Definicion de parametros para el problema
(defparameter *id* 0)
(defparameter *ancestro* nil)
(defparameter *solucion* nil)
(defparameter *estadoMeta* nil)


;Contadores para poder saber como fue nuestra solucion
(defparameter *contadorNodos* 0)
(defparameter *contadorExpandir* 0)
(defparameter *contadorFronteraBusqueda* 0)
(defparameter *tiempoFinal* 0)
(defparameter *tiempoFinal* 0)

;[Funcion] Permite limpiar todas las variables
(defun limpiarVariables ()
  (setq  *fronteraDeBusqueda*  nil)
  (setq  *memoria*  nil)
  (setq  *id*  0)
  (setq *estadoMeta* 0)
  (setq *ancestro*  nil)
  (setq *solucion*  nil))

;[Validacion] Nos permite saber en donde esta el 0
(defun dondeEstaEspacioEnBlanco (estado)
  (let*((filaDelEspacioEnBlanco 0)
        (casillaDelEspacioEnBlanco 0))
    (dolist (fila estado filaDelEspacioEnBlanco)
    (if (or (zerop(first fila)) (zerop(second fila)) (zerop(third fila)))
        (cond ((zerop (first fila)) (return (list
                                             filaDelEspacioEnBlanco
                                             casillaDelEspacioEnBlanco)))
              ((zerop (second fila)) (return (list
                                              filaDelEspacioEnBlanco
                                              (setq casillaDelEspacioEnBlanco (1+ casillaDelEspacioEnBlanco)))))
              (T (return (list
                          filaDelEspacioEnBlanco
                          (setq casillaDelEspacioEnBlanco (+ 2 casillaDelEspacioEnBlanco)))))))
        (setq filaDelEspacioEnBlanco (1+ filaDelEspacioEnBlanco)))))

;[Operador] Aplicamos el operador Arriba
(defun operadorArriba (estado)
  (let* ((filaDelEspacioEnBlanco (first(dondeEstaEspacioEnBlanco estado)))
         (elementoDelEspacioEnBlanco (second(dondeEstaEspacioEnBlanco estado)))
         (filaACambiarPorEspacio (nth (1- filaDelEspacioEnBlanco) estado))
         (elementoACambiarPorEspacio (nth elementoDelEspacioEnBlanco filaACambiarPorEspacio))
         (filaACambiarSinEspacio (nth filaDelEspacioEnBlanco estado)))

    (if (= filaDelEspacioEnBlanco 2)
        (list (first estado)
              (substitute 0 elementoACambiarPorEspacio filaACambiarPorEspacio)
              (substitute elementoACambiarPorEspacio 0 filaACambiarSinEspacio))
        (list (substitute 0 elementoACambiarPorEspacio filaACambiarPorEspacio)
              (substitute elementoACambiarPorEspacio 0 filaACambiarSinEspacio)
              (third estado)))))

;[Operador] Aplicamos el operador Abajo
(defun operadorAbajo (estado)
  (let* ((filaDelEspacioEnBlanco (first(dondeEstaEspacioEnBlanco estado)))
         (elementoDelEspacioEnBlanco (second(dondeEstaEspacioEnBlanco estado)))
         (filaACambiarPorEspacio (nth (1+ filaDelEspacioEnBlanco) estado))
         (elementoACambiarPorEspacio (nth elementoDelEspacioEnBlanco filaACambiarPorEspacio))
         (filaACambiarSinEspacio (nth filaDelEspacioEnBlanco estado)))

    (if (= filaDelEspacioEnBlanco 0)
        (list (substitute elementoACambiarPorEspacio 0 filaACambiarSinEspacio)
              (substitute 0 elementoACambiarPorEspacio filaACambiarPorEspacio)
              (third estado))
        (list (first estado)
              (substitute elementoACambiarPorEspacio 0 filaACambiarSinEspacio)
              (substitute 0 elementoACambiarPorEspacio filaACambiarPorEspacio)))))

;[Operador] Aplicamos el operador Izquierda
(defun operadorDerecha (estado)
  (let* ((filaDelEspacioEnBlanco (first(dondeEstaEspacioEnBlanco estado)))
         (elementoDelEspacioEnBlanco (second(dondeEstaEspacioEnBlanco estado)))
         (filaACambiarPorEspacio (nth filaDelEspacioEnBlanco estado))
         (elementoACambiarPorEspacio (nth (1+ elementoDelEspacioEnBlanco) filaACambiarPorEspacio)))

    (cond ((= filaDelEspacioEnBlanco 0)
           (list (substitute 0 elementoACambiarPorEspacio
                             (substitute elementoACambiarPorEspacio 0 filaACambiarPorEspacio ) :count 1 :from-end t)
                 (second estado)
                 (third estado)))
          ((= filaDelEspacioEnBlanco 1)
           (list (first estado)
                 (substitute 0 elementoACambiarPorEspacio
                             (substitute elementoACambiarPorEspacio 0 filaACambiarPorEspacio ) :count 1 :from-end t)
                 (third estado)))
          (T (list (first estado)
                   (second estado)
                   (substitute 0 elementoACambiarPorEspacio
                               (substitute elementoACambiarPorEspacio 0 filaACambiarPorEspacio ) :count 1 :from-end t))))))

;[Operador] Aplicamos el operador Izquierdo
(defun operadorIzquierda (estado)
  (let* ((filaDelEspacioEnBlanco (first(dondeEstaEspacioEnBlanco estado)))
         (elementoDelEspacioEnBlanco (second(dondeEstaEspacioEnBlanco estado)))
         (filaACambiarPorEspacio (nth filaDelEspacioEnBlanco estado))
         (elementoACambiarPorEspacio (nth (1- elementoDelEspacioEnBlanco) filaACambiarPorEspacio)))

    (cond ((= filaDelEspacioEnBlanco 0)
           (list (substitute 0 elementoACambiarPorEspacio
                             (substitute elementoACambiarPorEspacio 0 filaACambiarPorEspacio ) :count 1 )
                 (second estado)
                 (third estado)))
          ((= filaDelEspacioEnBlanco 1)
           (list (first estado)
                 (substitute 0 elementoACambiarPorEspacio
                             (substitute elementoACambiarPorEspacio 0 filaACambiarPorEspacio ) :count 1)
                 (third estado)))
          (T (list (first estado)
                   (second estado)
                   (substitute 0 elementoACambiarPorEspacio
                               (substitute elementoACambiarPorEspacio 0 filaACambiarPorEspacio ) :count 1 ))))))

;[Validacion] Nos permite saber si el operador arriba se puede aplicar
(defun operadorValido? (operador estado)
  (let* ((filaDelEspacioEnBlanco (first(dondeEstaEspacioEnBlanco estado)))
         (casillaDelEspacioEnBlanco (second(dondeEstaEspacioEnBlanco estado)))
         (operador (first operador)))
    (cond ((equal operador :Arriba) (if(= filaDelEspacioEnBlanco 0) nil T))
          ((equal operador :Abajo) (if (= filaDelEspacioEnBlanco 2) nil T))
          ((equal operador :Izquierda) (if (= casillaDelEspacioEnBlanco 0) nil T))
          ((equal operador :Derecha) (if (= casillaDelEspacioEnBlanco 2) nil T))
          (T "Error"))))
          ;(T (if (= casillaDelEspacioEnBlanco 2) nil T)))))

;[Operador] Aplicamos los diferentes Operadores
(defun aplicarOperador (operador estado)
  (let* ((operador (first operador)))
    (case operador
      (:Arriba (operadorArriba estado))
      (:Abajo (operadorAbajo estado))
      (:Izquierda (operadorIzquierda estado))
      (:Derecha (operadorDerecha estado))
      (T "Error"))))

;[Busqueda] Permite saber cuantos estan desacomodados
(defun numeroDeElementosDesacomodados (estado meta)
  (1- (auxNumeroDeElementosDesacomodados (aplanaLista estado) (aplanaLista meta) 0)))

;[Aux] Permite aplanar la lista
(defun aplanaLista (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (aplanaLista (cdr l))))
        (t (append (aplanaLista (car l)) (aplanaLista (cdr l))))))

;[Aux] Permite saber el numero de desordenados segun el estado meta
(defun auxNumeroDeElementosDesacomodados (estado meta contador)
  (cond ((null estado) contador)
        ((= (car estado) 0) ( + (auxNumeroDeElementosDesacomodados (cdr estado ) (cdr meta) contador)))
        ((= (car estado) (car meta)) ( + (auxNumeroDeElementosDesacomodados (cdr estado ) (cdr meta) contador)))
        (T (+ (auxNumeroDeElementosDesacomodados(cdr estado) (cdr meta) (1+ contador))))))

;[Funcion] Permite expandir el estado
(defun expandir (estado)
  (let ((descendientes nil)
        (nuevoEstado nil))
    (dolist (operador *operadores* descendientes)
      (print operador)
      (if (operadorValido? operador estado)
          (progn
            (setq nuevoEstado (aplicarOperador operador estado))
            (setq descendientes (cons (list nuevoEstado operador) descendientes)))))))

;[Funcion] Permite obtener el ultimo elemento de la frontera de busqueda
(defun obtenerDeFronteraDeBusqueda ()
  (pop *fronteraDeBusqueda*))

;[Funcion] Permite crear el nodo con la esctructura id - estado - ancestro - operador - desacomodados
(defun crearNodo (estado operador desacomodados)
  (incf *id*)
  (incf *contadorNodos*)
  (list (1- *id*) estado *ancestro* operador desacomodados))

;[Funcion] Permite insertar a frontera de Busqueda
(defun insertarAFronteraDeBusqueda (estado operador metodoBusqueda)
;(let ((nodo (crearNodo estado operador)))
  (let ((nodo nil))
    (cond ((eql metodoBusqueda :bestFirstSearch)
           (setq nodo (crearNodo estado operador (numeroDeElementosDesacomodados estado *estadoMeta*)))
           (push nodo *fronteraDeBusqueda*)))))

;[Funcion] Permite meter a memoria de Busqueda
(defun insertarEnMemoria(nodo)
  (push nodo *memoria*))

;[Predicado] Permite saber si ya esta en la memoria
(defun recuerdasElEstado? (estado memoria)
  (print "MEMORIA (((((((((((((((((((((((((())))))))))))))))))))))))))")
  (print memoria)
  (cond ((null memoria) nil)
        ((equal estado (second (first memoria)))
         (print "SIMON")
         T)
        (T (recuerdasElEstado? estado (rest memoria)))))

;[Filtro] Permite saber si ya estaba en la memoria
(defun filtraMemoria (listaDeEstados)
  (cond ((null listaDeEstados) nil)
        ((recuerdasElEstado? (first (first listaDeEstados)) *memoria*)
         (print "--------------------------------------------------------------")
         (print (first (first listaDeEstados)))
         (print (first listaDeEstados))
         (print "--------------------------------------------------------------")
         (filtraMemoria (rest listaDeEstados)))
        (T (cons (first listaDeEstados) (filtraMemoria (rest listaDeEstados))))))

;[Main] Permite comenzar a resolver nuestro algoritmo de 8puzzle
(defun bestFirstSearch (inicio meta metodo)
  (limpiarVariables)
  (let ((nodo nil)
        (estado nil)
        (sucesores '())
        (operador nil)
        (metaEncontrada nil)
        (testing 0)
        (listaDeDesacomodados nil ))
    (setq *estadoMeta* meta)
    (insertarAFronteraDeBusqueda inicio nil metodo)
                                        ; (loop until (or metaEncontrada (null *fronteraDeBusqueda*)) do
    (loop until (or metaEncontrada (null *fronteraDeBusqueda*) (= testing 10)) do
         (setq nodo (obtenerDeFronteraDeBusqueda)
               estado (second nodo))
         (print "ESTO ES MI ESTADO")
         (print estado)
         (setq testing (1+ testing))
         (setq listaDeDesacomodados nil)
         (insertarEnMemoria nodo)
         (cond ((equal meta estado)
                (format t "Exito. Meta encontrada en ~A  intentos~%" (first  nodo))
                (setq metaEncontrada T))
               (T (setq *ancestro* (first nodo))
                  (print estado)
                  (setq sucesores (expandir estado))
                  (setq sucesores (filtraMemoria sucesores))
                  (loop for elemento in sucesores do
                       (setq listaDeDesacomodados
                             (cons
                              (list
                               (first elemento)
                               (numeroDeElementosDesacomodados (first elemento) *estadoMeta*)
                               (second elemento)
                               metodo)
                                    listaDeDesacomodados))
                       (print "/*/*/*/*/*/*/*/*/*")
                       (print listaDeDesacomodados)
                       (print "/*/*/*/*/*/*/*/*/*")
                    ;(insertarAFronteraDeBusqueda (first elemento) (second elemento) metodo)
                                        finally (setq listaDeDesacomodados (sort listaDeDesacomodados  #'< :key #'cadr)))
                  (loop for elemento in listaDeDesacomodados do
                       (insertarAFronteraDeBusqueda (first elemento) (second elemento) (fourth elemento)))
                  (print listaDeDesacomodados ))))))

(trace insertarAFronteraDeBusqueda)
(bestFirstSearch  '((2 8 3)(1 4 5)(7 0 6)) '((1 2 3)(8 0 4)(7 6 5)) :bestFirstSearch)

(sort
 '((((1 2 3) (4 5 0) (7 8 6)) 6 (:ARRIBA NIL) :BESTFIRSTSEARCH)
   (((1 2 3) (4 5 6) (7 0 8)) 4 (:IZQUIERDA NIL) :BESTFIRSTSEARCH))   #'< :key #'cadr )
(caar '(( 6 ((1 2 3) (4 5 0) (7 8 6)) (:ARRIBA NIL) 6)
        (((1 2 3) (4 5 6) (7 0 8)) (:IZQUIERDA NIL) 6)))


(third (first '((6 ((1 2 3) (4 5 0) (7 8 6)) (:ARRIBA NIL) :BESTFIRSTSEARCH))))

(sort tester #'> :key #'car)

;(trace expandir)
;(trace bestFirstSearch)
;(trace numeroDeElementosDesacomodados)

;(numeroDeElementosDesacomodados '((1 2 3)(4 5 6 )(7 8 0)) '((2 1 3)(4 5 6 )(7 8 0)))
;(operadorIzquierda 0 '((1 2 2)(2 0 3)(7 10 8)))
;(substitute 'xx 2 '(0 2 3))
;(operadorValido? :Izquierda '((1 2 3)(4 0 5)(6 7 8)))
;(aplicarOperador '((:Izquierda nil)) '((1 2 3)(4 0 5)(6 7 8)))
;(aplicarOperador '((:Derecha nil)) '((1 2 3)(4 0 5)(6 7 8)))
;(aplicarOperador '((:Arriba nil)) '((1 2 3)(4 0 5)(6 7 8)))
;(aplicarOperador '((:Abajo nil)) '((1 2 3)(4 0 5)(6 7 8)))
;(trace expandir)
;(expandir '((1 2 3)(4 0 5)(6 7 8)))
;(expandir '((1 0 3)(4 2 5)(6 7 8)))
;(dondeEstaEspacioEnBlanco '((1 0 3)(4 2 5)(6 7 8)))







