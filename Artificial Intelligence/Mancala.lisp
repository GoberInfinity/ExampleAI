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

(defun leerCasilla())

(defun imprimirInstrucciones ()
  (print "Para mover las casillas primero seleccione la casilla")
  (leerCasilla()))

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
