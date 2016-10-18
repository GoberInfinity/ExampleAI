;Reyes Fragoso Roberto

;[Parametros] Definicion del tablero del enemigo y el nuestro para el juego
(defparameter *tablero* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()))
(defparameter *tirarDeNuevo* T)
(defparameter *casillasTiradas* nil)

(defparameter *tableroUniversal* '())

;[Parametros] Definimos los operadores
(defparameter *id* 0)
(defparameter *ancestro* nil)
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
(defun juegoTerminado? ()
  (and
         (null (nth 0 *tablero*))
         (null (nth 1 *tablero*))
         (null (nth 2 *tablero*))
         (null (nth 3 *tablero*))
         (null (nth 4 *tablero*))
         (null (nth 5 *tablero*))
         (null (nth 7 *tablero*))
         (null (nth 8 *tablero*))
         (null (nth 9 *tablero*))
         (null (nth 10 *tablero*))
         (null (nth 11 *tablero*))
         (null (nth 12 *tablero*))))

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
;TODO Recursividad
(defun heuristicaMancala (estado)
  (+ (- (apply #'+ (nth 13 estado)) (apply #'+ (nth 6 estado)))
     (- (+ (apply #'+ (nth 7 estado))(apply #'+ (nth 8 estado))(apply #'+ (nth 9 estado))
           (apply #'+ (nth 10 estado))(apply #'+ (nth 11 estado))(apply #'+ (nth 12 estado)))
        (+ (apply #'+ (nth 0 estado))(apply #'+ (nth 1 estado))(apply #'+ (nth 2 estado))
           (apply #'+ (nth 3 estado))(apply #'+ (nth 4 estado))(apply #'+ (nth 5 estado))))))

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
         (copiaTablero nil)
         (estadoRespaldo nil))

    ;Por cuestiones de seguridad creamos una copia del tablero
    (loop for elemento in tableroN do
         (setq copiaTablero (cons elemento copiaTablero)))
    (setq copiaTablero  (reverse copiaTablero))
    ;Para cada operador que ya hemos definido le aplicamos todos los operadores si son validos
    (loop for operador in *operadores* do
        ; (print "ESTO ES MI COPIA TABLERO")
        ; (print copiaTablero)
        ; (print "ESTO ES MI TABLERON")
        ; (setq tableroN copiaTablero)
        ; (print tableroN)
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
         (setq movimientoMaquina (minimax-alpha-beta *tablero* 0 1 1 *infinito* (- *infinito*)))
         (setq vuelveATirarMaquina (second movimientoMaquina))
         (setq movimientoFinal (first movimientoMaquina))
         (setq *tablero* movimientoFinal)
         (imprimirTablero))
    ))


;El tercer uno es que es la pc
;(print (minimax-alpha-beta *tablero* 0 1 1 *infinito* (- *infinito*)))
(defparameter testing 0)
;[Maain] Permite jugar
(defun jugar()
  (reiniciarJuego)
    (loop until (= testing 15) do
       (imprimirTablero)
       (turnoHumano)
       (setq *tirarDeNuevo* T)
       (turnoMaquina)
         (setq testing (1+ testing))))


(jugar)



