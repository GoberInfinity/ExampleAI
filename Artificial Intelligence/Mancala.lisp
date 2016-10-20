;Reyes Fragoso Roberto

;[Parametros] Definicion del tablero del enemigo y el nuestro para el juego
(defparameter *tablero* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()))
(defparameter *tirarDeNuevo* T)
(defparameter *casillasTiradas* nil)
(defparameter *tableroUniversal* '())
(defparameter *jugadorGanador* nil)
(defparameter *finDelJuego* nil)

;[Parametros] Definimos los operadores
(defparameter *id* 0)
(defparameter *infinito* most-positive-fixnum)
(defparameter *operadores* '((:Primero 7)
                             (:Segundo 8)
                             (:Tercero 9)
                             (:Cuarto 10)
                             (:Quinto 11)
                             (:Sexto 12)))

;[Funcion] Permite imprimir el tablero, mostrando arriba la base del oponente
; y abajo la base del jugador
(defun imprimirTablero()
  (format t "~%---------Tablero--------~%")
  (format  t   "~&~% |~A| |~A| |~A| |~A| |~A| |~A| |~A| ~%"
           (nth 13 *tablero*)(nth 12 *tablero*)(nth 11 *tablero*)(nth 10 *tablero*)(nth 9 *tablero*)(nth 8 *tablero*)(nth 7 *tablero*))
  (format  t   "~& |~A| |~A| |~A| |~A| |~A| |~A| |~A| ~%~%"
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
;    (format t "~& Mover a:  ~A ~%" (nth casillaAMover *tablero*))
    (push canica (nth casillaAMover *tablero*))

    ;Detectamos cuando puede volver a tirar la persona, en este caso cuando sea la casilla 6
    (if (= casillaAMover 6)
        (setq *tirarDeNuevo* T)
        (setq *tirarDeNuevo* nil))
    (format t "~& Canica Escogida:  ~A ~%" canica)
    (format t "~& Casilla Escogida  ~A ~%" casillaEscogida)
    (format t "~& Casilla A Mover  ~A ~%" casillaAMover)
    (imprimirTablero)))

;[Predicado]Permite saber si ya se termino el juego
;TODO: Hacer recursiva esta
(defun juegoTerminado? ()
  (return-from juegoTerminado? (or (and
         (null (nth 0 *tablero*))
         (null (nth 1 *tablero*))
         (null (nth 2 *tablero*))
         (null (nth 3 *tablero*))
         (null (nth 4 *tablero*))
         (null (nth 5 *tablero*)))
      (and
         (null (nth 7 *tablero*))
         (null (nth 8 *tablero*))
         (null (nth 9 *tablero*))
         (null (nth 10 *tablero*))
         (null (nth 11 *tablero*))
         (null (nth 12 *tablero*))))))

;[Predicate] Permite validar si se puede escoger la casilla
(defun casillaValida? (casillaEscogida)
  (cond ((member casillaEscogida '(6 7 8 9 10 11 12 13)) nil)
        ((null (canicasEnCasilla casillaEscogida)) nil)
        (T T)))

;[Predicate] Permite validar que no repita el mismo movimiento 2 veces
(defun seRepiteMovimiento? (casillaEscogida)
  (if (member casillaEscogida *casillasTiradas*) T nil))

;[Funcion] Permite hacer toda la logica y validaciones para que tire el ser humano
(defun turnoHumano ()
  (let* ((casillaEscogida nil)
         (canicas 0)
         (casillaValida nil)
         (casillaAMover nil))

    ;Hacemos un loop hasta que ya no pueda tirar de nuevo
    (loop until (null *tirarDeNuevo*) do

         ;Primero validamos que la casilla que va atirar tenga al menos una canica
         ; y si es asi, obtenermos el total de ellas en esa casilla
         (loop until casillaValida do
              (print "Escoge una casica")
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
         (setq *casillasTiradas* nil)
         (if (juegoTerminado?)(progn (setq *finDelJuego* (juegoTerminado?))
                                     (setq *jugadorGanador* 0)
                                     (setq *tirarDeNuevo* nil))))))

;[Predicate] Permite saber si el operador es valido
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

    ;Por seguridad creamos una tabla de respaldo, ya que usamos funciones destructivas
    (setq *tableroUniversal* estado)

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
(defun aplicarOperadorAux (tablero casillaActual canicasAux)
  (let* ((seguirTirando nil)
         (canicaAux nil)
         (contador 0)
         (estado nil)
         (canicas nil)
         (copiaTablero nil)
         (canicaMayorEnBase -1)
         (casillaAMeter (1+ casillaActual)))

    ;Usamos funciones destructivas por lo que es recomendable trabajar con copias totalmente separadas de lo por
    ; lo que estado es una copia de el estado actual de nuestro tablero
    (setq estado (copy-list tablero))

    ;Por seguridad y para que no guarde referencias a otras celdas de la lista, creamos una copia de los elementos 
    ; originales
    (loop for elemento in tablero do
         (setq copiaTablero (cons elemento copiaTablero)))
    (setq copiaTablero  (reverse copiaTablero))
    (loop for can in canicasAux do
         (setq canicas (cons can canicas)))

    ;Realizamos una resta para saber el numero de canicas que le vamos a dar al humano, quedandonos siempre
    ; las de mejor valor asi como insertando la de mayor valor en nuestra base
    (setq canicas(sort canicas #'>))
    (if ( >= (length canicas) (- 13 casillaActual))
        (progn
          (setq canicaMayorEnBase (first canicas))
          (push canicaMayorEnBase (nth 13 copiaTablero))
          (setq seguirTirando T)))

    ;Anteriormente ya hemos insertado la canica de mayor valor en nuestra base por lo que detectamos cuando
    ; se repitela canica para no insertarla, asi como debemos reiniciar la cuenta para insertar las otras
    ; canicas en la base enemiga
    (loop for canica in canicas do
         (setq canicaAux (pop (nth casillaActual copiaTablero)))
         (if (and ( = contador 0 ) ( = canicaMayorEnBase canicaAux))
             (setq contador (1+ contador))
             (progn
               (if (> casillaAMeter 12)
                   (setq casillaAMeter 0))
               (push canicaAux (nth casillaAMeter copiaTablero))
               (setq casillaAMeter (1+ casillaAMeter))))
       finally (return (list copiaTablero seguirTirando)))))

;[Funcion] Creamos nuestra propia heuristica que nos permitira saber cual es la mejor casilla
(defun heuristicaMancala (estado)
  (+ (- (apply #'+ (nth 13 estado)) (apply #'+ (nth 6 estado)))
     (- (+ (apply #'+ (nth 7 estado))(apply #'+ (nth 8 estado))(apply #'+ (nth 9 estado))
           (apply #'+ (nth 10 estado))(apply #'+ (nth 11 estado))(apply #'+ (nth 12 estado)))
        (+ (apply #'+ (nth 0 estado))(apply #'+ (nth 1 estado))(apply #'+ (nth 2 estado))
           (apply #'+ (nth 3 estado))(apply #'+ (nth 4 estado))(apply #'+ (nth 5 estado))))))

;[Funcion] Permite hacer la logica para poder hacer que la maquina de su mejor tiro
(defun minimax-alpha-beta (board depth max-depth player use-thresh pass-thresh)
  (if (= depth max-depth)
      (heuristicaMancala (first board))
      (let ((successors (aplicarOperadores board)))
        (if (null successors)
            (heuristicaMancala (first board))
            (do ((new-value nil)
                 (best-move (car successors)))
                ((null successors)
                 (if (= depth 0)
                     best-move
                     pass-thresh))
              (setf new-value
                    (- (minimax-alpha-beta
                        (car successors)
                        (1+ depth)
                        max-depth
                        (cambiarJugador player)
                        (- pass-thresh)
                        (- use-thresh))))
              (when (> new-value pass-thresh)
                (setf pass-thresh new-value)
                (setf best-move (car successors)))
              (if (>= pass-thresh use-thresh)
                  (setf successors nil)
                  (setf successors (cdr successors))))))))

;[Funcion] Permite cambiar de jugador
(defun cambiarJugador (jugador)
  (case jugador
    (0 1)
    (1 0)))

;[Funcion] Definimos una funcion que nos va a retornar todos los nodos cada uno ya aplicado el operador
(defun aplicarOperadores (tableroN)
  (let* ((listaDeEstados nil)
         (nuevoEstado nil)
         (copiaTablero nil))

    ;Por cuestiones de seguridad creamos una copia del tablero
    (loop for elemento in tableroN do
         (setq copiaTablero (cons elemento copiaTablero)))
    (setq copiaTablero  (reverse copiaTablero))

    ;Para cada operador que ya hemos definido le aplicamos todos los operadores si son validos
    (loop for operador in *operadores* do
         (if (operadorValido? operador tableroN)
             (progn
               (setq nuevoEstado (aplicarOperador operador tableroN))
               (push nuevoEstado listaDeEstados)))
       finally (return listaDeEstados))))

;[Funcion] Permite tirar a la maquina
(defun turnoMaquina ()
  (let* ((movimientoMaquina nil)
         (movimientoFinal nil)
         (vuelveATirarMaquina T))
    (loop until (null vuelveATirarMaquina) do
         (format t "~& Turno de la Computadora ~%")
         (setq movimientoMaquina (minimax-alpha-beta *tablero* 0 1 1 *infinito* (- *infinito*)))
         (setq vuelveATirarMaquina (second movimientoMaquina))
         (setq movimientoFinal (first movimientoMaquina))
         (setq *tablero* movimientoFinal)
         (if (juegoTerminado?)(progn (setq *finDelJuego* (juegoTerminado?))
                                     (setq *jugadorGanador* 1)
                                     (setq vuelveATirarMaquina nil)))
         (imprimirTablero))))

;[Funcion] Permite imprimir las intrucciones
(defun imprimirInstrucciones()
  (format t "~%Mancala Inteligencia Artificial.~%")
  (format t "~%Tablero: El talbero tiene la siguiente composicion, las casillas de la 0-5 es las que puede tirar.")
  (format t "~%         La 6ta casilla es su base, tiene que acumular el mayor numero de puntos en su base.~%")
  (format t "~%Reglas: Cada turno usted puede escoger una casilla que contenga al menos 1 canica en la casilla.")
  (format t "~%        La canica debe moverla a las casillas consecuentes, solo colocando 1 canica en las siguientes.")
  (format t "~%        Solo puede moverse hacia adelante y no puede colocar canicas en la base del enemigo .")
  (format t "~%        Si sobran casillas despues de hacer su movimiento se pasan a la base del enemigo.")
  (format t "~%        Al final el jugador que ya no tenga mas canicas en sus casillas, se apodera de las del otro.")
  (format t "~%        Gana el jugador con mejor puntuacion al final.~%~%"))

;[Main] Permite jugar
(defun jugar()
  (imprimirInstrucciones)
  (reiniciarJuego)
  (loop until (not (null *finDelJuego*)) do
       (imprimirTablero)
       (juegoTerminado?)
       (turnoHumano)
       (setq *tirarDeNuevo* T)
       (if (null *finDelJuego*)
           (turnoMaquina)))
  (format t "~& La puntuacion de la Inteligencia Artificial ~A ~%"
          (if (= *jugadorGanador* 1)
              (progn
                (+ (apply #'+ (nth 0 *tablero*))(apply #'+ (nth 1 *tablero*))(apply #'+ (nth 2 *tablero*))
                   (apply #'+ (nth 3 *tablero*))(apply #'+ (nth 4 *tablero*))(apply #'+ (nth 5 *tablero*))
                   (apply #'+ (nth 13 *tablero*))))
              (apply #'+ (nth 13 *tablero*))))
  (format t "~& La puntuacion del jugador humano ~A  ~%"
          (if (= *jugadorGanador* 0)
              (progn
                (+ (apply #'+ (nth 7 *tablero*))(apply #'+ (nth 8 *tablero*))(apply #'+ (nth 9 *tablero*))
                   (apply #'+ (nth 10 *tablero*))(apply #'+ (nth 11 *tablero*))(apply #'+ (nth 12 *tablero*))
                   (apply #'+ (nth 6 *tablero*))))
              (apply #'+ (nth 6 *tablero*)))))

;Suma 24
;(setq *tablero* '((2) NIL NIL NIL NIL NIL (2 3 2 2 1 1 2 1 1 2 2 3) NIL NIL (2) (1) NIL NIL (3 3 1 2 1 1 2 1 3 2 3 3 1 2 3 3 1 2 3 1 3 3)))

(jugar)


