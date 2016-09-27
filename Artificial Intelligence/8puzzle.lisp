; Reyes Fragoso Roberto

;Permite saber para cada problema la frontera de busqueda y la memoria
(defparameter *fronteraDeBusqueda* '())
(defparameter *memoria* '())

;Definicion de Operadores, los definimos como nil por que cada operador es
; diferente dependiendo de la casilla en blanco
(defparameter *operadores* '((:Abajo "v")
                             (:Arriba "^")
                             (:Izquierda "<")
                             (:Derecha ">")))

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

;[Funcion] Permite reordenar la frontera de Busqueda
(defun reordenarFronteraDeBusqueda (fronteraDeBusqueda))
(setq *fronteraDeBusqueda* (sort *fronteraDeBusqueda* #'< :key #'(lambda (x) (fifth x))))

;[Funcion] Permite meter a memoria de Busqueda
(defun insertarEnMemoria(nodo)
  (push nodo *memoria*))

;[Funcion] Permite insertar a frontera de Busqueda
(defun insertarAFronteraDeBusqueda (listaDeEstados metodoBusqueda indicadorInicio)
  (let* ((nodo '())
         (listaOrdenada '()))
    (cond ((eql metodoBusqueda :bestFirstSearch)
           (if (= indicadorInicio 0)
               (progn
                 (setq nodo (crearNodo listaDeEstados nil (numeroDeElementosDesacomodados (list listaDeEstados) *estadoMeta*)))
                 (push nodo *fronteraDeBusqueda*))
               (progn
                   (setq listaOrdenada (reordenarDeMenorAMayor listaDeEstados))
                   (loop for elemento in listaDeEstados do
                        (setq nodo (crearNodo (first elemento) (third elemento) (numeroDeElementosDesacomodados (first elemento) *estadoMeta*)))
                        (push nodo *fronteraDeBusqueda*))
                   (reordenarFronteraDeBusqueda *fronteraDeBusqueda*)))))))

;[Aux] Permite reordenar lo que tenemos en nuestra lista
(defun reordenarDeMenorAMayor (listaDeEstados)
  (sort listaDeEstados  #'> :key #'second))

;[Predicado] Permite saber si ya esta en la memoria
(defun recuerdasElEstadoEnMemoria? (estado memoria)
  (cond ((null memoria) nil)
        ((equal estado (second (first memoria))) T)
        (T (recuerdasElEstadoEnMemoria? estado (rest memoria)))))

;[Predicado] Permite saber si ya esta en la frontera
(defun recuerdasElEsdadoEnFrontera? (estado frontera)
    (cond ((null frontera) nil)
          ((equal estado (second (first frontera)))T)
          (T (recuerdasElEsdadoEnFrontera? estado (rest frontera)))))

;[Filtro]Permite saber si esta en frontera
(defun filtraFrontera (listaDeEstados)
  (cond ((null listaDeEstados) nil)
        ((recuerdasElEsdadoEnFrontera? (first (first listaDeEstados)) *fronteraDeBusqueda*)
         (filtraFrontera (rest listaDeEstados)))
        (T (cons (first listaDeEstados) (filtraFrontera (rest listaDeEstados))))))

;[Filtro] Permite saber si ya estaba en la memoria
(defun filtraMemoria (listaDeEstados)
  (cond ((null listaDeEstados) nil)
        ((recuerdasElEstadoEnMemoria? (first (first listaDeEstados)) *memoria*)
         (filtraMemoria (rest listaDeEstados)))
        (T (cons (first listaDeEstados) (filtraMemoria (rest listaDeEstados))))))

;[Funcion] Permite extraer la solucion
(defun extraerSolucion (nodo)
  (labels ((localizarNodo (id lista)
             (cond ((null lista) Nil)
                   ((equal id (first (first lista))) (first lista))
                   (T (localizarNodo id (rest lista))))))
    (let ((actual (localizarNodo (first nodo) *memoria*)))
      (loop while (not (null actual)) do
           (push actual *solucion*)
           (setq actual (localizarNodo (third actual) *memoria*))))
    *solucion*))

;[Funcion] Permite mostrar la solucion bonita
(defun mostrarSolucion (listaNodos)
   (format  t "Longitud de solucion ~A ~%" (1- (length  listaNodos)))
   (let ((nodo nil))
     (dotimes (i (length listaNodos))
       (setq nodo (nth i listaNodos))
       (if (= i 0)
           (format t "Inicio en: ~A~%" (second  nodo))
           (format t "\(~A\) aplicando ~A  se  llega  a  ~A~%"  i (fourth  nodo)  (second  nodo))))))







;[Funcion] Permite saber la distancia Manhatan de cada elementos
(defun distanciaManhattan (estado meta)
  (let* ((contadorDeCadaElemento 0)
         (contadorDeEstado 0)
         (filaDelEstado 0)
         (contadorAuxiliar 0)
         (listaDeElementos (aplanaLista estado)))

    (loop for elemento in listaDeElementos do
                                        ; (obtenerMovimientosManhattan elemento estado meta 0))
         (if (or (= contadorDeEstado 3) (= contadorDeEstado 6))
             (progn
               (setq contadorAuxiliar 0)
               (setq filaDelEstado (1+ filaDelEstado))))
         (setq contadorDeCadaElemento (+ contadorDeCadaElemento (obtenerMovimientosManhattan elemento estado meta 0 filaDelEstado contadorAuxiliar)))
         (setq contadorAuxiliar (1+ contadorAuxiliar))
         (setq contadorDeEstado (1+ contadorDeEstado))
    )(print contadorDeCadaElemento)))

;[Funcion] Permite hacer toda la logita de la distancia Manhatan
(defun obtenerMovimientosManhattan (elemento estado meta contador filaDelEstado contadorAuxiliar)
  (cond (( = filaDelEstado 0)
        (cond (( member elemento (first meta))
               ( + 0 (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (first meta) elemento 0 contadorAuxiliar)))
              (( member elemento (second meta))
               ( + 1 (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (second meta) elemento 0 contadorAuxiliar)))
              ( T (1+ (1+ (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (third meta) elemento 0 contadorAuxiliar))))))
        (( = filaDelEstado 1 )
         (cond (( member elemento (first meta))
                (format t "~& SEGUNDA PRIMERA ~%")
                ( 1+ (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (first meta) elemento 0 contadorAuxiliar)))
               (( member elemento (second meta))
                (format t "~& SEGUNDA SEGUNDA ~%")
                ( + 0 (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (second meta) elemento 0 contadorAuxiliar)))
               ( T (format t "~& SEGUNDA ULTIMA ~%")
                   (1+ (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (third meta) elemento 0 contadorAuxiliar)))))
        ( T
         (cond (( member elemento (first meta))
                ( 1+ (1+  (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (first meta) elemento 0 contadorAuxiliar))))
               (( member elemento (second meta))
                ( + 1 (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (second meta) elemento 0 contadorAuxiliar)))
               ( T (+ 0 (auxObtenerMovimientosManhattan (nth filaDelEstado estado) (third meta) elemento 0 contadorAuxiliar)))))))


;[Aux] Permite saber la casilla del numero para Manthatthan&body
(defun auxObtenerMovimientosManhattan (estado meta elemento contador contadorAuxiliar)
  (print elemento)
  (print meta)
  (cond ((= contadorAuxiliar 2)
         (cond (( = elemento (third meta)) contador)
               (( = elemento (second meta)) (1+ contador))
               (T (1+ (1+ contador)))))
        ((= contadorAuxiliar 1)
         (cond (( = elemento (second meta))
                (format t "~& DENTRO PRIEMRA  ~%")
                contador)
               (T (1+ contador))))
        ( T
         (cond (( = elemento (first meta)) contador)
               (( = elemento (second meta)) (1+ contador))
               (T (1+ (1+ contador)))))))

;[Aux]

(trace auxObtenerMovimientosManhattan)
(trace obtenerMovimientosManhattan)
(distanciaManhattan '((4 5 7)(6 0 2)(1 3 8)) '((1 2 3) (8 0 4) (7 6 5)))
(member 4 '(8 0 4))
(1- (length (member 4 (reverse '(8 0 4)))))
(1- (length (member 4 (reverse '(8 4 5)))))
(1- (length (member 4 (reverse '(4 0 5)))))

(not (set-exclusive-or  '(1 2 3) '(3 2 1)))

(+ 5 6 7 8)

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
    (insertarAFronteraDeBusqueda inicio metodo 0)
    (loop until (or metaEncontrada (null *fronteraDeBusqueda*)) do
         (setq nodo (obtenerDeFronteraDeBusqueda)
               estado (second nodo))
         (print *estadoMeta*)
         (setq listaDeDesacomodados nil)
         (insertarEnMemoria nodo)
         (cond ((equal meta estado)
                (format t "Exito. Meta encontrada en ~A  intentos~%" (first  nodo))
                (mostrarSolucion (extraerSolucion nodo))
                (setq metaEncontrada T))
               (T (setq *ancestro* (first nodo))
                  (setq sucesores (expandir estado))
                  (setq sucesores (filtraFrontera sucesores))
                  (setq sucesores (filtraMemoria sucesores))
                  (loop for elemento in sucesores do
                       (setq listaDeDesacomodados
                             (cons
                              (list (first elemento)(numeroDeElementosDesacomodados (first elemento) *estadoMeta*)(second elemento) metodo)
                              listaDeDesacomodados))
                     finally (insertarAFronteraDeBusqueda listaDeDesacomodados metodo 1)))))))

(bestFirstSearch '((2 8 3)(1 4 5)(7 0 6)) '((1 2 3)(8 0 4)(7 6 5)) :bestFirstSearch )
(bestFirstSearch '((2 8 3)(1 4 5)(7 0 6)) '((1 2 3)(8 4 0)(7 6 5)) :bestFirstSearch )
(bestFirstSearch '((2 8 3)(1 4 5)(7 6 0)) '((1 2 3)(8 4 0)(7 6 5)) :bestFirstSearch )


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







