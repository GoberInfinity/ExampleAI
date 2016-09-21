(defparameter *fronteraBusqueda* '())
(defparameter *memoria* '())

(defparameter *operadores*	'( 	(:Persona-Zorro (1 1 0 0))
								(:Persona-Oveja (1 0 1 0))
								(:Persona-Pasto (1 0 0 1))
								(:Persona       (1 0 0 0))))

(defparameter *id* 0)
(defparameter *ancestro* nil)
(defparameter *solucion* nil)

(defun crearNodo (estado operador)
	(incf *id*)
	(list (1- *id*) estado *ancestro* (first operador)))

(defun dondeEstaBarca (estado)
	(if (= 1 (fifth ( first estado))) 0 1))

(defun insertarAFronteraDeBusqueda (estado operador metodoBusqueda)
	(let ((nodo (crearNodo estado operador)))
		(cond ((eql metodo :depth-first)
			(push nodo *fronteraBusqueda*))
		((eql metodo :breath-first)
			(setq *fronteraBusqueda* (append *fronteraBusqueda* (list nodo))))
		(T Nil))))

(defun obtenerDeFronteraDeBusqueda ()
	(pop *fronteraBusqueda*))

(defun operadorValido? (operador estado)
	(let* ((orillaActual (dondeEstaBarca estado))
		(granjero (first (nth orillaActual estado)))
		(zorro (second (nth orillaActual estado)))
		(oveja (third (nth orillaActual estado)))
		(pasto (fourth (nth orillaActual estado))))
	(or (= granjero (first (second op)))
		(= zorro (second (second operador)))
		(= oveja (third (second operador)))
		(= pasto (fourth (second operador))))))

(defun estadoValido? (estado)
	(let ((zorroOrigen (second (first estado)))
		(ovejaOrigen (third (first estado))))
		(lechugaOrigen (fourth (first estado)))
		(zorroDestino (second (second estado)))
		(ovejaDestino (third (second estado)))
		(lechugaDestino (fourth (second estado)))
		(and (not (= ovejaOrigen lechugaOrigen))
			 (not (= ovejaOrigen zorroOrigen))
			 (not (= ovejaDestino zorroDestino))
			 (not (= ovejaDestino lechugaDestino)))))

(defun voltear (bit) 
	(boole BOOLE-XOR bit 1))

(defun aplicarOperador (operador estado)
	(let* ((orilla0 (first estado))
		(orilla1 (second estado))
		(barca0 (first (first estado)))
		(zorro0 (second (first estado)))
		(oveja0 (third (first estado))))
		(lechuga0 (fourth (first estado)))
		(barca1 (first (first estado)))
		(zorro1 (second (second estado)))
		(oveja1 (third (second estado)))
		(lechuga1 (fourth (second estado)))
		(orillaActual (dondeEstaBarca estado))


		)))


