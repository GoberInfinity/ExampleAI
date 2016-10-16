;Reyes Fragoso Roberto

;[Parametros] Definicion del tablero del enemigo y el nuestro para el juego
(defparameter *tablero* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()))
(defparameter *tirarDeNuevo* T)
(defparameter *casillasTiradas* nil)

;[Funcion] Permite imprimir el tablero, mostrando arriba la base del oponente
; y abajo la base del jugador
(defun imprimirTablero()
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A~%"
           (nth 13 *tablero*)(nth 12 *tablero*)(nth 11 *tablero*)(nth 10 *tablero*)(nth 9 *tablero*)(nth 8 *tablero*)(nth 7 *tablero*))
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A~%"
           (nth 0 *tablero*)(nth 1 *tablero*)(nth 2 *tablero*)(nth 3 *tablero*)(nth 4 *tablero*)(nth 5 *tablero*)(nth 6 *tablero*)))

;[Funcion] Permite resetear el juego
(defun reiniciarJuego ()
  *tablero* '((1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)(1 2 3)()))

;[Funcion] Permite leer la casilla que el usuario desea mover
(defun leerUsuario()
  (let*((casillaEscogida 0))
    (setq casillaEscogida (read))
    casillaEscogida))

;[Funcion] Le permite saber el numero de casillas en la posicion que eligio el usuario
(defun canicasEnCasilla (casilla)
  (nth casilla *tablero*))

;[Funcion] Te permite mover la canica a una nueva casilla
(defun moverCanicaACasilla (casillaEscogida casillaAMover)
  (let* ((canica (pop (nth casillaEscogida *tablero*))))
    (format t "~& CANICA  ~A ~%" canica)
    (format t "~& MOVER A   ~A ~%" (nth casillaAMover *tablero*))
    (push canica (nth casillaAMover *tablero*))
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
;TODO Hacerlo recursivo
(defun casillaValida? (casillaEscogida)
  (cond ((member casillaEscogida '(6 7 8 9 10 11 12 13)) nil)
        ((null (canicasEnCasilla casillaEscogida)) nil)
        (T T)))

(defun seRepiteMovimiento? (casillaEscogida)
  (print *casillasTiradas*)
  (print casillaEscogida)
  (if (member casillaEscogida *casillasTiradas*) T nil))

;[Funcion] Es el inicio del Juego
;TODO limpiar las variables y hacer bottom up 
(defun Mancala()
  (reiniciarJuego)
  (let* ((casillaEscogida 0)
         (canicas 0)
         (casillaValida nil)
         (casilla)
         (casillaAMover nil))
    (imprimirTablero)

    (loop until casillaValida do
         (setq casillaEscogida (leerUsuario))
         (if (casillaValida? casillaEscogida)
             (setq casillaValida T)
             (print "Casilla Invalida")))

    ;(setq casillaEscogida (leerUsuario))
    (setq canicas (canicasEnCasilla casillaEscogida))
                                        ;    (loop until (juegoTerminado?) do
    (loop until (null *tirarDeNuevo*) do
       (loop for canica in canicas do
            (format t "~& ¿Para donde mover la canica?  ~A ~%" canica)
            (setq casillaAMover (leerUsuario))
            (loop until (null (seRepiteMovimiento? casillaAMover)) do
                 (format t "~& No se puede mover ~%")
                 (setq casillaAMover (leerUsuario)))
            (push casillaAMover *casillasTiradas*)
            (moverCanicaACasilla casillaEscogida casillaAMover)))))

;TODO NO TIENE SENTIDO HACER LA FUNCION LEER USUARIO CORREGIR
(Mancala)

