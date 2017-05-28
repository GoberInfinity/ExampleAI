;;Permiten saber en que estado y caracter estamos analizando de la cinta
(defparameter *string* nil)
(defparameter *currentState* 0)
(defparameter *currentChar* 0)
;;Lista que almacena las transiciones
(defparameter *rulesOfProduction* nil)
;;Parametro que permite saber si hemos acabado de analizar la cita
(defparameter *stop* nil)

(defun stringAsList (cadena)
  "Permite convertir de tipo cadena a tipo lista"
  (let* ((listaCadena nil))
    (loop for caracter across (reverse cadena) do
         (setq listaCadena (cons caracter listaCadena)))
    listaCadena))

(defun cleanParameters ()
  "Permite limpiar las variables globales"
  (setq *string* nil)
  (setq *currentState* 0)
  (setq *currentState* 0)
  (setq *stop* nil))


(defun generateNextState (transitions)
  "Permite recursivamente generar el siguiente estado de la maquina y remplazar los simbolos"
  (let* ((transition (car transitions))
         (currentStateT (first transition))
         (simbolT (second transition))
         (nextStateT (third transition))
         (nextSimbolT (fourth transition))
         (directionT (fifth transition))
         (comparedSybols nil)
         (isANumber nil)
         )


    ;; (format t "~& Comparacion  State:~A Simbol:~A State:~A Simbol:~A Direction:~A  ~%" currentStateT simbolT nextStateT nextSimbolT directionT)
    ;; (format t "~& Usuaro  Stirng:~A State:~A Char:~A  ~%" *string* *currentState* *currentChar*)
    ;; (format t "~& Tupo de Compraracion ~A Tipo de Usuario ~A  ~%" (type-of simbolT) (type-of (nth *currentChar* *string*)))

    ;; (format t "~& ----------------------------------------- ~%")

    (if (equalp (type-of (nth *currentChar* *string*)) 'NULL)
        (return-from generateNextState nil))



    (if (numberp (nth *currentChar* *string*))
        (setq isANumber t))


    (cond ((and (equalp(type-of (nth *currentChar* *string*)) 'SYMBOL)
                (equalp (type-of simbolT) 'SYMBOL))
           (progn
             (if (equal
                  (coerce simbolT 'character)
                  (coerce (nth *currentChar* *string*) 'character)
                  )
                 (setq comparedSybols t))))

          ((and (equalp(type-of (nth *currentChar* *string*)) 'SYMBOL)
                (equalp (type-of simbolT) 'BIT))
           (progn
             (if (equal
                  simbolT
                  (nth *currentChar* *string*)
                  )
                 (setq comparedSybols t))))

          ((and (equalp (type-of simbolT) 'SYMBOL)
                (equalp (type-of (nth *currentChar* *string*)) 'STANDARD-CHAR))
           (progn
             (if (equal
                  (coerce simbolT 'character)
                  (nth *currentChar* *string*)
                  )
                 (setq comparedSybols t))))

          ((equalp isANumber t)
           (progn
             (if (equal
                  simbolT
                  (nth *currentChar* *string*))
                 (setq comparedSybols t))))
          ((equalp (type-of simbolT) 'NULL)
           (print "ESTO ES NULLL"))

          (T (progn
               (if (equal
                    simbolT
                    (digit-char-p (nth *currentChar* *string*)))
                   (setq comparedSybols t)))))

    (cond ((null transitions)
           nil)
          ((and (equalp *currentState* currentStateT)
                comparedSybols)
           (progn
             (format t "~& .,.,.,.,..,.,.,.,.,.ESTO ENTRO AL TRUE ~%")
             (setq *currentState* nextStateT)
             (setf (nth *currentChar* *string*) nextSimbolT)
             (cond ((equal directionT 'R)
                    (setq *currentChar* (1+ *currentChar*)))
                   (T (setq *currentChar* (1- *currentChar*))))))
          (T (generateNextState (cdr transitions))))))

(defun turingMachine (tuple)
  (let* ((listWord (stringAsList(read-line)))
         (generated nil))
    (cleanParameters)
    (setq *string* listWord)
    (loop until *stop* do
         (setq generated (generateNextState tuple))

         (if (equalp *currentState* 4)
             (progn
               (format t "~& ACEPTADO ~%")
               (return)))

         (if (null generated)
             (progn
               (format t "~& String did not accept ~%")
               (return)))

         (format t "~& Tape ~A ~%" *string*))))

(turingMachine '((0 0 1 X R)
                 (0 Y 3 Y R)
                 (1 0 1 0 R)
                 (1 1 2 Y L)
                 (1 Y 1 Y R)
                 (2 0 2 0 L)
                 (2 X 0 X R)
                 (2 Y 2 Y L)
                 (3 Y 3 Y R)
                 (3 B 4 B R)))

