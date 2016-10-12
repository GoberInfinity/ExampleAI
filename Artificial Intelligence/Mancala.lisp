;Reyes Fragoso Roberto





;[Funcion] Permite imprimir el tablero, mostrando arriba la base del oponente
; y abajo la base del jugador
(defun imprimirTablero()
  (format  t   " ~A ~%"  '(9 8 7 6 5))
  (format  t  " ~A ~A ~A ~A ~%"  '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3))
  (format  t  " ~A ~A ~A ~A ~%"  '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3))
  (format  t  " ~A ~%"  '(9 8 7 6 5)))

(imprimirTablero)

(defvar answer 0)

(defun algo ()
  (format t "~& INGRES ALOS DATOS POR FAVOR ~%")
  (setq answer (read)))

(defun dummy ()
  (algo)
  (format t "~& Los datos ingregados fueron  ~A ~%" answer))

(dummy)
