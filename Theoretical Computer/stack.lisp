(defparameter *rulesOfProduction* '())
(defparameter *stack* '())
(defparameter *currentState* 0)

(defun stringAsList (cadena)
  "Allow to coerce a string word to list"
  (let* ((listaCadena nil))
    (loop for caracter across (reverse cadena) do
         (setq listaCadena (cons caracter listaCadena)))
    listaCadena))

(defun generateNextState (rulesOfProduction input)
  "Recursively across the rules of production, we produce the next state of the stack machinek"
  (let* ((rule (first (car rulesOfProduction)))
         (production (second (car rulesOfProduction)))
         (stateOfRule (first rule))
         (characterOfRule (second rule))
         (stackOfRule (third rule))
         (stateOfProduction (first production))
         (stackOfProduction (second production)))

    (cond ((null rulesOfProduction)
           nil)
          ((and (equal *currentState* stateOfRule)
                (equal (first *stack*) stackOfRule)
                (equal (coerce characterOfRule 'character) input))
           (progn
             (setq *currentState* stateOfProduction)
             (cond ((equal stackOfProduction 'epis)
                    (print "Epsilon"))
                   ((equal stackOfProduction 'lamb)
                    (pop *stack*))
                   (T (push stackOfProduction *stack*)))))
          (T (generateNextState (cdr rulesOfProduction) input)))))

(defun stackmachine (initialState finalState rulesOfProduction alphabet stackSimbols)
  "Allow to check if a string is accepted by the stack machine"
  (let* ((listWord (stringAsList(write-to-string(read))))
         (generated nil))

    (setq *stack* 'nil)
    (push (first stackSimbols) *stack*)
    (setq *currentState* 0)

    (format t "~& This is the rules ~A ~%" rulesOfProduction)
    (format t "~& -- Stack -- ~A ~%" *stack*)


    (loop for char in listWord do
         (setq generated (generateNextState rulesOfProduction char))
         (if (null generated)
             (progn
               (format t "~& String did not accept ~%")
               (return)))
         (format t "~& State: ~A -- Stack -- ~A ~%" *currentState* *stack*) )

    (format t "~& Final State ~A ~%" *currentState*)
    (if (and (equal finalState *currentState*) (equal *stack* '(z0)))
        (format t "~& String accepted ~%")
        (format t "~& String did not accept ~%"))

    )
  )


(stackmachine '0 '2 '(((0 a z0)(0 AA))
                      ((0 b z0)(0 BB))
                      ((0 a AA)(0 A))
                      ((0 a BB)(0 A))
                      ((0 c A)(1 epis))
                      ((1 a A)(1 lamb))
                      ((1 a AA)(2 lamb))) '(a b) '(z0 A B))

;;Accepted: aacaa


