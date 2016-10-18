;Reyes Fragoso Roberto

;[Parametros] Definicion del tablero del enemigo y el nuestro para el juego
(defparameter *tablero* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()))
(defparameter *tirarDeNuevo* T)
(defparameter *casillasTiradas* nil)

;[Parametros] Definimos los operadores
(defparameter *id* 0)
(defparameter *ancestro* nil)
(defparameter *operadores* '((:Primero 7)
                             (:Segundo 8)
                             (:Tercero 9)
                             (:Cuarto 10)
                             (:Quinto 11)
                             (:Sexto 12)))

;[Funcion] Permite imprimir el tablero, mostrando arriba la base del oponente
; y abajo la base del jugador
(defun imprimirTablero()
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A~%"
           (nth 13 *tablero*)(nth 12 *tablero*)(nth 11 *tablero*)(nth 10 *tablero*)(nth 9 *tablero*)(nth 8 *tablero*)(nth 7 *tablero*))
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A~%"
           (nth 0 *tablero*)(nth 1 *tablero*)(nth 2 *tablero*)(nth 3 *tablero*)(nth 4 *tablero*)(nth 5 *tablero*)(nth 6 *tablero*)))

;[Funcion] Permite resetear el juego
(defun reiniciarJuego ()
  (setq *tablero* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)())))

;[Funcion] Le permite saber el numero de casillas en la posicion que eligio el usuario
(defun canicasEnCasilla (casilla)
  (nth casilla *tablero*))

;[Funcion] Te permite mover la canica a una nueva casilla
(defun moverCanicaACasilla (casillaEscogida casillaAMover)
  (let* ((canica (pop (nth casillaEscogida *tablero*))))
    (format t "~& CANICA  ~A ~%" canica)
    (format t "~& MOVER A   ~A ~%" (nth casillaAMover *tablero*))
    (push canica (nth casillaAMover *tablero*))

    ;TODO Validar tambien la inteligencia artificial si cae en contrario no tire

    (if (= casillaAMover 6)
        (setq *tirarDeNuevo* T)
        (setq *tirarDeNuevo* nil))

  (format t "~& CasillaEscogida  ~A ~%" casillaEscogida)
  (format t "~& CasillaAMover  ~A ~%" casillaAMover)
  (imprimirTablero)))

;[Predicado]Permite saber si ya se termino el juego
;TODO: Hacer recursiva esta
;; (defun juegoTerminado? ()
;;   (boole boole-and (null (nth 0 *tableroHumano*))
;;          (null (nth 1 *tableroHumano*))
;;          (null (nth 2 *tableroHumano*))
;;          (null (nth 3 *tableroHumano*))
;;          (null (nth 4 *tableroHumano*))
;;          (null (nth 5 *tableroHumano*))
;;          (null (nth 0 *tableroMaquina*))
;;          (null (nth 1 *tableroMaquina*))
;;          (null (nth 2 *tableroMaquina*))
;;          (null (nth 3 *tableroMaquina*))
;;          (null (nth 4 *tableroMaquina*))
;;          (null (nth 5 *tableroMaquina*))))

;[Predicate] Permite validar si se puede escoger la casilla
(defun casillaValida? (casillaEscogida)
  (cond ((member casillaEscogida '(6 7 8 9 10 11 12 13)) nil)
        ((null (canicasEnCasilla casillaEscogida)) nil)
        (T T)))

(defun seRepiteMovimiento? (casillaEscogida)
  (print *casillasTiradas*)
  (print casillaEscogida)
  (if (member casillaEscogida *casillasTiradas*) T nil))


(defun turnoHumano ()
  (let* ((casillaEscogida nil)
         (canicas 0)
         (casillaValida nil)
         (casillaAMover nil))
    ;quitar al final
    (reiniciarJuego)

    (loop until (null *tirarDeNuevo*) do
         ;Primero validamos que la casilla que va atirar tenga al menos una canica
         ; y si es asi, obtenermos el total de ellas en esa casilla
         (loop until casillaValida do
              (if (casillaValida? (setq casillaEscogida (read)))
                  (progn
                    (setq canicas (canicasEnCasilla casillaEscogida))
                    (setq casillaValida T))
                  (print "La casilla que escogiste no tiene canicas")))

    ;Despues para cada canica en esa casilla, le permitimos al usuario mover cada una.
    ;Asi como tambien validamos que si cae en su base alguna canica vuelve a tirar
    ;Y finalmente validamos que no se repitan sus movimientos por cada turno
         (loop for canica in canicas do
              (format t "~& ¿Para donde mover la canica?  ~A ~%" canica)
              (loop until (null (seRepiteMovimiento? (setq casillaAMover(read)))) do
                   (format t "~& No se puede mover ~%"))
              (push casillaAMover *casillasTiradas*)
              (moverCanicaACasilla casillaEscogida casillaAMover))

         ;Limpiamos las variables tomando la precaucion de si el usuario puede volver
         ; a tirar
         (imprimirTablero)
         (setq casillaEscogida nil)
         (setq casillaValida nil)
         (setq *casillasTiradas* nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;[Funcion] Permite saber si el operador es valido
(defun operadorValido? (operador estado)
  (let* ((operador (second operador)))
    (cond ((= operador 7)
           (if (null (nth 7 estado)) nil T))
          ((= operador 8)
           (if(null (nth 8 estado)) nil T))
          ((= operador 9)
           (if(null (nth 9 estado)) nil T))
          ((= operador 10)
           (if(null (nth 10 estado)) nil T))
          ((= operador 11)
           (if(null (nth 11 estado)) nil T))
          ((= operador 12)
           (if(null (nth 12 estado)) nil T))
          (T nil))))

;[Funcion] Permite hacer el movimiento no destructivo, eso devuelve del loop
; Falta validar que si cae en su base vuelve a tirar
(defun aplicarOperador (operador estado)
  (let* ((operadorEtiqueta (first operador))
         (casillaActual (second operador))
         (canicas (canicasEnCasilla casillaActual))
         (estadoFinal nil))
    (case operadorEtiqueta
      (:Primero (setq estadoFinal (aplicarOperadorAux estado casillaActual canicas)))
      (:Segundo (setq estadoFinal (aplicarOperadorAux estado casillaActual canicas)))
      (:Tercero (setq estadoFinal (aplicarOperadorAux estado casillaActual canicas)))
      (:Cuarto (setq estadoFinal (aplicarOperadorAux estado casillaActual canicas)))
      (:Quinto (setq estadoFinal (aplicarOperadorAux estado casillaActual canicas)))
      (:Sexto (setq estadoFinal (aplicarOperadorAux estado casillaActual canicas)))
      (T "error"))
    estadoFinal))

;[Auxiliar] Para evitar que se repita codigo creamos una funcion auxiliar que nos permita mover las casillas
(defun aplicarOperadorAux (estado casillaActual canicas)
  (let* ((seguirTirando nil)
         (canicaAux nil)
         (contador 0)
         (canicaMayorEnBase -1)
         (casillaAMeter (1+ casillaActual)))
    (print "Esto es mi resta para saber las casillas")
    (setq canicas(sort canicas #'>))
    (if ( >= (length canicas) (- 13 casillaActual))
        (progn
          (setq canicaMayorEnBase (first canicas))
          (push canicaMayorEnBase (nth 13 estado))
         (setq seguirTirando T)))

    (print "ESTA SON MIS CANICAS /*/*/*/*/*")
    (print canicas)

    (print "ESTE ES MI ESTADO")
    (print estado)


    (loop for canica in canicas do
         (setq canicaAux (pop (nth casillaActual estado)))
         (print canicaAux)
         (if (and ( = contador 0 ) ( = canicaMayorEnBase canicaAux))
             (setq contador (1+ contador))
             (progn
               (if (> casillaAMeter 12)
                   (setq casillaAMeter 0))
               (push canicaAux (nth casillaAMeter estado))
               (setq casillaAMeter (1+ casillaAMeter))))
       finally (return (list estado seguirTirando)))))

;[Funcion] Creamos nuestra propia heuristica que nos permitira saber cual es la mejor casilla
;(defun heuristicaMancala (estado)
;  )

;[Funcion] Permite ordenar mis canicas

;(reiniciarJuego)
;(imprimirTablero)

(first (member 2 '(1 3 2)))

(defun dummy ()
  (let* ((algo nil))
    (reiniciarJuego)
    (print "LO ANDA HACIENDO")
  (setq algo (aplicarOperador '(:Cuarto 10) *tablero*))
  (print algo)
  (print (first algo))
  (print (second algo))
  (imprimirTablero)))

(dummy)


;[Main] Programando minimax
;(defun minMax (estado profundidad maximizarJugador)
;  (if ( = profundidad 0)
;      ;Retornar la heuristica del nodo
;      (return-from minMax (heuristicaMancaa estado)))
;  (if maximizarJugador (setq mejorValor most-negative-fixnum) (setq mejorValor most-positive-fixnum))
;  (loop for operador in *operadores* do
;       (if (operadorValido? operador estado)
;           (progn
;             (setq nuevoEstado (aplicaOperador operador estado))
;             (setq valorActual (miniMax nuevoEstado (1- profundidad) nil))
;             (setq mejorValor (max mejorValor valorActual)))))
;  (list mejorValor nuevoEstado)
;
;           ;Falta regresar cual es el mejor
;))

;(turnoHumano)

