;Reyes Fragoso Roberto


(defparameter *baseEnemiga* '())
(defparameter *baseHumana* '())
(defparameter *tableroMaquina* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)))
(defparameter *tableroHumano* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)))


;[Funcion] Permite imprimir el tablero, mostrando arriba la base del oponente
; y abajo la base del jugador
(defun imprimirTablero()
  (format  t  " ~A ~%"  *baseEnemiga*)
  (format  t  " ~A ~%" *tableroMaquina*)
  (format  t  " ~A ~%" *tableroHumano*)
  (format  t  " ~A ~%"  *baseHumana*))

(imprimirTablero)

;[Funcion] Permite firmatear la entrada del usuario para saber a donde mover la bolita
(defun convertirEntradaUsuario (entrada)

  )


;[Funcion] Permite mover las casillas dependiendo de lo que eliga el usuario en
; teclado
(defun moverCanicas (primeraPosicion segundaPosicion terceraPosicion))


(defvar answer 0)
(defvar an2 0)

(defun limpiarEntrada ()
  (setq answer 0)
  (setq an2 0))

;[Funcion] Permite resetear el juego
(defun reiniciarJuego ()
  (setq *baseEnemiga* '())
  (setq *baseHumana* '())
  (setq *tableroMaquina* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)))
  (setq *tableroHumano* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3))))

;[Funcion] Permite leer la casilla que el usuario desea mover
(defun leerUsuario()
  (let*((casillaEscogida 0))
    (setq casillaEscogida (read))
    casillaEscogida))

;[Funcion] Le permite saber el numero de casillas en la posicion que eligio el usuario
(defun canicasEnCasilla (casilla)
  (nth casilla *tableroHumano*))

;[Funcion] Te permite mover la canica a una nueva casilla
(defun moverCanicaACasilla (casillaEscogida casillaAMover)
  (let* ((canica (pop (nth casillaEscogida *tableroHumano*))))
    (format t "~& CANICA  ~A ~%" canica)
    (format t "~& MOVER A   ~A ~%" (nth casillaAMover *tableroHumano*))
    (push canica (nth casillaAMover *tableroHumano*))

  (format t "~& CasillaEscogida  ~A ~%" casillaEscogida)
  (format t "~& CasillaAMover  ~A ~%" casillaAMover)
  (format t "~& FINAL HUMANO  ~A ~%" *tableroHumano*)))

;[Funcion] Es el inicio del Juego
(defun Mancala()
  (reiniciarJuego)
  (let* ((casillaEscogida 0)
         (canicas 0)
         (casilla))

    (setq casillaEscogida (leerUsuario))
    (setq canicas (canicasEnCasilla casillaEscogida))
    (loop for canica in canicas do
         (format t "~& ¿Para donde mover la canica?  ~A ~%" canica)
         (moverCanicaACasilla casillaEscogida (leerCasilla)))))

(Mancala)


(defun imprimirInstrucciones ()
  (format t "~& Testing printing ~%"))

(defun algo ()
  (format t "~& INGRES ALOS DATOS POR FAVOR ~%")
  (setq answer (read))
  (setq an2 (read)))

(defun dummy ()
  (limpiarEntrada)
  (algo)
  (format t "~& Los datos ingregados fueron  ~A ~%" answer) 
  (format t "~& Los datos ingregados fueron  ~A ~%" an2))

(dummy) 
