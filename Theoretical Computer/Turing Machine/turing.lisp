(compile-file "ltk")
(load "ltk")
(in-package :ltk)
(defparameter *stringTape* nil)
(defparameter *stringActualState* nil)
(defparameter *stringActualCharacter* nil)
(defparameter *stringAccepted* "No Aceptada")


(defun hello-1()
  (with-ltk ()
    (let* ((f (make-instance 'frame :height 20 :width 1200))
           (entrada (make-instance 'label :text " "))
           (entradaS (make-instance 'label :text "Entrada Usuario"))

           (cinta (make-instance 'label :text " "))
           (cintaS (make-instance 'label :text "Cinta: "))

           (eActualS (make-instance 'label :text "Estados: "))
           (eActual (make-instance 'label :text " "))

           (cActualS (make-instance 'label :text "Caracteres: "))
           (cActual (make-instance 'label :text " "))

           (cAceptadaS (make-instance 'label :text "La cadena fue:"))
           (cAceptada (make-instance 'label :text " "))

           (tipo (make-instance 'entry))
           (b (make-instance 'button
                             :text "Iniciar Maquina"
                             :command (lambda ()
                                        (format t "the content of enry is:~a~%" (text tipo))(finish-output)
                                        (setf (text entrada) (text tipo))
                                        (turingMachine '((0 0 1 X R)
                                                         (0 Y 3 Y R)
                                                         (1 0 1 0 R)
                                                         (1 1 2 Y L)
                                                         (1 Y 1 Y R)
                                                         (2 0 2 0 L)
                                                         (2 X 0 X R)
                                                         (2 Y 2 Y L)
                                                         (3 Y 3 Y R)
                                                         (3 B 4 B R))
                                                       (arrayToList (text tipo)))
                                        (setf (text cinta) (reverse *stringTape*))
                                        (setf (text eActual) (reverse *stringActualState*))
                                        (setf (text cActual) (reverse *stringActualCharacter*))
                                        (setf (text cAceptada) *stringAccepted*)
                                        )
                                             )))
      (pack f)
      (pack entradaS :side :top :fill :x :expand t)
      (pack entrada :side :top :fill :x :expand t)

      (pack cintaS :side :top :fill :x :expand t)
      (pack cinta :side :top :fill :x :anchor :w)

      (pack eActualS :side :top :fill :x :expand t)
      (pack eActual :side :top :fill :x :expand t)

      (pack cActualS :side :top :fill :x :expand t)
      (pack cActual :side :top :fill :x :expand t)

      (pack cAceptadaS :side :top :fill :x :expand t)
      (pack cAceptada :side :top :fill :x :expand t)

      
      (pack tipo :side :left :fill :x :expand t)
      (pack b :side :left :fill :x :expand t)
      )))


(defun arrayToList (array)
  (map 'list #'identity array))

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
  (setq *currentChar* 0)
  (setq *stringTape* nil)
  (setq *stop* nil)
  (setq *stringActualState* nil)
  (setq *stringActualCharacter* nil)
  (setq *stringAccepted* "Cadena no aceptada"))


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


     (format t "~& Comparacion  State:~A Simbol:~A State:~A Simbol:~A Direction:~A  ~%" currentStateT simbolT nextStateT nextSimbolT directionT)
    (format t "~& Usuaro  Stirng:~A State:~A Char:~A  ~%" *string* *currentState* *currentChar*)
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
             (setq *currentState* nextStateT)
             (setf (nth *currentChar* *string*) nextSimbolT)
             (cond ((equal directionT 'R)
                    (setq *currentChar* (1+ *currentChar*)))
                   (T (setq *currentChar* (1- *currentChar*))))))
          (T (generateNextState (cdr transitions))))))

(defun turingMachine (tuple input)
  (let* ((listWord input)
         (newList nil)
         (generated nil))
    (cleanParameters)
    (setq *string* listWord)


    (setq *stringActualCharacter* (cons *currentChar* *stringActualCharacter*))

    (loop until *stop* do
         (setq generated (generateNextState tuple))

         (setq *stringActualState* (cons *currentState* *stringActualState*))
         (print *stringActualState*)

         (if (equalp *currentState* 4)
             (progn
               (setq *stringAccepted* "Cadena Aceptada")
               (return)))

         (if (null generated)
             (progn
               (format t "~& String did not accept ~%")
               (return)))

         (setq newList (copy-list *string*))

         (setq *stringTape* (cons newList *stringTape*))

         (setq *stringActualCharacter* (cons *currentChar* *stringActualCharacter*))
         )))

(hello-1)


;; (turingMachine '((0 0 1 X R)
;;                  (0 Y 3 Y R)
;;                  (1 0 1 0 R)
;;                  (1 1 2 Y L)
;;                  (1 Y 1 Y R)
;;                  (2 0 2 0 L)
;;                  (2 X 0 X R)
;;                  (2 Y 2 Y L)
;;                  (3 Y 3 Y R)
;;                  (3 B 4 B R)))

