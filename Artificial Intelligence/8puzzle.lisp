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
  (let* ((filaDelEspacioEnBlanco 0))
    (dolist (fila estado filaDelEspacioEnBlanco)
      (print (first fila))
    (if (or (zerop(first fila)) (zerop(second fila)) (zerop(third fila)))
        (return)
        (setq filaDelEspacioEnBlanco (1+ filaDelEspacioEnBlanco))))
(format  t "Nodos expandidos ~A ~%" filaDelEspacioEnBlanco)))

;[Operador] Aplicamos el operador Arriba
(defun operadorArriba (operador estado)
  (let ((espacioEnBlanco (dondeEstaEspacioEnBlanco (estado))))


    ))

(dondeEstaEspacioEnBlanco '((1 1 3)(4 5 6)(7 1 0)))

(substitute 'xx 2 '(0 2 3))









