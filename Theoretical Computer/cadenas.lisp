;; Reyes Fragoso Roberto

(defun readUser ()
  (let* ((primeraCadena nil)
         (segundaCadena nil)
         (opcionEscogida nil))

    (print " Inserte Primera Cadena")
    (setq primeraCadena (read))
    (print " Inserte Segunda Cadena")
    (setq segundaCadena (read))

    (print " Escoge una opcion
1.-Calcular longitud de cadena.
2.-Concatenar cadenas.
3.-Potencia de cadena.
4.-Inverso.
5-Prefijos.
6.-Sufijos.
7.-Subcadenas.
8.-Salir.")
    (setq opcionEscogida (read))
    (longitud primeraCadena segundaCadena)
    (concatena primeraCadena segundaCadena)
    (inversa primeraCadena segundaCadena)
    (impPalindromo primeraCadena segundaCadena)
    (prefijo primeraCadena segundaCadena)

    ))

(defun palindromo? (cadena1)
  (equal (write-to-string cadena1) (reverse (write-to-string cadena1))))

(defun impPalindromo (cadena1 cadena2)
  (format t "~& Cadena 1 Palindromo: ~A ~%" (palindromo? cadena1))
  (format t "~& Cadena 1 Palindromo: ~A ~%" (palindromo? cadena2)))

(defun longitud (cadena1 cadena2)
  (format t "~& Longitud De Cadena 1  ~A ~%" (length (write-to-string cadena1)))
  (format t "~& Longitud De Cadena 2  ~A ~%" (length (write-to-string cadena2))))

(defun concatena (cadena1 cadena2)
  (format t "~& Cadena Concatenada 1  ~A~A ~%" cadena1 cadena2)
  (format t "~& Cadena Concatenada 2  ~A~A ~%" cadena2 cadena1))

(defun potencia ())

(defun inversa (cadena1 cadena2)
  (if (not (palindromo? cadena1))
      (format t "~& Reversa De Cadena 1  ~A ~%" (reverse (write-to-string cadena1))))
  (if (not (palindromo? cadena2))
      (format t "~& Reversa De Cadena 2  ~A ~%" (reverse (write-to-string cadena2)))))

(defun prefijo(cadena1 cadena2)
  (let* ((listaCadena1 nil)
         (listaCadena2 nil)
         (v (make-array 2 :element-type 'character :adjustable t :fill-pointer 0)))

    (loop for caracter across (reverse (write-to-string cadena1)) do
         (setq listaCadena1 (cons caracter listaCadena1)))

         (loop until (null listaCadena1) do
              (vector-push (first listaCadena1) v)
              (pop listaCadena1)
              (loop for elemento in listaCadena1 do
                   (print elemento)
                   (vector-push-extend elemento v)
                   (print v))
              )
         ))

(defun sufijo(cadena1 cadena2))
(defun subcadena(cadena1 cadena2))

(readUser)




