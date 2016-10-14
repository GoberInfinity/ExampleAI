;Reyes Fragoso Roberto

;[Parametros] Definicion del tablero del enemigo y el nuestro para el juego
(defparameter *tableroMaquina* '(()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)))
(defparameter *tableroHumano* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()))


;[Funcion] Permite imprimir el tablero, mostrando arriba la base del oponente
; y abajo la base del jugador
(defun imprimirTablero()
  (format  t  " ~A ~%" *tableroMaquina*)
  (format  t  " ~A ~%" *tableroHumano*))

;[Funcion] Permite resetear el juego
(defun reiniciarJuego ()
  (setq *tableroMaquina* '(()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)))
  (setq *tableroHumano* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)())))

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
  (if (null (canicasEnCasilla casillaEscogida)) nil T))


;[Funcion] Es el inicio del Juego
(defun Mancala()
  (reiniciarJuego)
  (let* ((casillaEscogida 0)
         (canicas 0)
         (casillaValida nil)
         (casilla))
    (imprimirTablero)

    (loop until casillaValida do
         (setq casillaEscogida (leerUsuario))
         (if (casillaValida? casillaEscogida)
             (setq casillaValida T)
             (print "Casilla Invalida")))

    ;(setq casillaEscogida (leerUsuario))
    (setq canicas (canicasEnCasilla casillaEscogida))
;    (loop until (juegoTerminado?) do
       (loop for canica in canicas do
         (format t "~& ¿Para donde mover la canica?  ~A ~%" canica)
            (moverCanicaACasilla casillaEscogida (leerCasilla)))))

()

(Mancala)


