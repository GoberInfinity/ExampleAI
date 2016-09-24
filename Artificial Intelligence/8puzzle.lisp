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


;Contadores para poder saber como fue nuestra solucion
(defparameter *contadorNodos* 0)
(defparameter *contadorExpandir* 0)
(defparameter *contadorFronteraBusqueda* 0)
(defparameter *tiempoFinal* 0)
(defparameter *tiempoFinal* 0)

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
(defun operadorArriba (operador estado)
  (let ((espacioEnBlanco (dondeEstaEspacioEnBlanco (estado))))


    ))

(dondeEstaEspacioEnBlanco '((1 2 1)(1 5 2 )(7 1 0)))

(substitute 'xx 2 '(0 2 3))









