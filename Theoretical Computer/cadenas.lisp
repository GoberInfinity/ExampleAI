;; Reyes Fragoso Roberto

(defun readUser ()
  (let* ((primeraCadena nil)
         (segundaCadena nil)
         (opcionEscogida nil))

    (print " Inserte Primera Cadena")
    (setq primeraCadena (write-to-string (read)))
    (print " Inserte Segunda Cadena")
    (setq segundaCadena (write-to-string (read)))

    (longitud primeraCadena segundaCadena)
    (concatena primeraCadena segundaCadena)
    (inversa primeraCadena segundaCadena)
    (impPalindromo primeraCadena segundaCadena)

    (print "Subcadena")
    (subcadena (stringComoLista primeraCadena))
    (subcadena (stringComoLista segundaCadena))

    (print "Prefijos")
    (prefijo (stringComoLista primeraCadena) '())
    (prefijo (stringComoLista segundaCadena) '())

    (print "Sufijo")
    (sufijo (reverse (stringComoLista primeraCadena)) '())
    (sufijo (reverse (stringComoLista segundaCadena)) '())

    (auxPotencia primeraCadena)
    (auxPotencia segundaCadena)))

(defun palindromo? (cadena1)
  (equal cadena1 (reverse cadena1)))

(defun impPalindromo (cadena1 cadena2)
  (format t "~& Cadena 1 Palindromo: ~A ~%" (palindromo? cadena1))
  (format t "~& Cadena 1 Palindromo: ~A ~%" (palindromo? cadena2)))

(defun longitud (cadena1 cadena2)
  (format t "~& Longitud De Cadena 1  ~A ~%" (length cadena1))
  (format t "~& Longitud De Cadena 2  ~A ~%" (length cadena2)))

(defun concatena (cadena1 cadena2)
  (format t "~& Cadena Concatenada 1  ~A~A ~%" cadena1 cadena2)
  (format t "~& Cadena Concatenada 2  ~A~A ~%" cadena2 cadena1))

(defun potencia (veces cadena)
  (loop for i from 1 to veces do
       (format t "~a" cadena)))

(defun auxPotencia (cadena1)
  (let* ((potencia nil))
    (print "Inserte la potencia")
    (setq potencia (parse-integer (write-to-string (read))))

    (cond ((= potencia 0) (print nil))
          ((> 0 potencia) (potencia (abs potencia) (reverse cadena1)))
          (t (potencia potencia  cadena1)))))

(defun inversa (cadena1 cadena2)
  (if (not (palindromo? cadena1))
      (format t "~& Reversa De Cadena 1  ~A ~%" (reverse cadena1)))
  (if (not (palindromo? cadena2))
      (format t "~& Reversa De Cadena 2  ~A ~%" (reverse cadena2))))

(defun stringComoLista (cadena)
  (let* ((listaCadena nil))
    (loop for caracter across (reverse cadena) do
         (setq listaCadena (cons caracter listaCadena)))
    listaCadena))

(defun prefijo (cadena cprefijo)
  (cond ((null cadena) (print (reverse cprefijo)))
        (t (progn
             (print (reverse cprefijo))
             (prefijo (rest cadena) (cons (first cadena) cprefijo))))))

(defun sufijo (cadena csufijo)
  (cond ((null cadena) (print csufijo))
        (t (progn
             (print csufijo)
             (sufijo (rest cadena) (cons (first cadena) csufijo))))))

(defun subcadena (cadena)
  (let* ((primerCaracter nil))
    (loop until (null cadena) do
         (setq primerCaracter (cons (first cadena) primerCaracter))
         (pop cadena)
         (prefijo cadena primerCaracter)
         (setq primerCaracter nil))))

(readUser)

