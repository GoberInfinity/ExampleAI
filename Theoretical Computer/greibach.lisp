(defparameter *greibach* nil)
(defparameter *final* nil)

;;Funcion recursiva que permite sunstituir los no terminales con sus producciones
(defun substitution (firstElement productionRules)
  (let* ((simbol (first (first (first productionRules))))
         (firstSimbolOfRule (second (first productionRules)))
         (element (first firstElement)))
  (cond ((null productionRules)
         nil)
        ((equalp simbol element)
         (progn
           (pop firstElement)
           (if (numberp (first (third (first productionRules))))
               (progn
                 (append (listToAtom (reverse firstSimbolOfRule) firstElement)
                                     '(--)
                                     (listToAtom (reverse (third (first productionRules))) firstElement)))
               (listToAtom (reverse firstSimbolOfRule) firstElement))
           ))
        (T (substitution firstElement (cdr productionRules))))))

;; Permite pasar una lista completa a atomos de lisp para poder generar una nueva produccion
(defun listToAtom (list finalList)
  (cond ((null list)
        finalList)
        (T (listToAtom (cdr list) (cons (first list) finalList)))))

;; Funcion que permite saber si el primer elemento de nuestra produccion es un terminal
;; ya que Greinach necesita que el primero elemento sea un terminal y los demas no terminales
(defun isTerminal? (firstElement)
  (numberp (first firstElement)))

(defun convertToGreibach (rulesOfProduction)
  (setq *greibach* nil)
  (setq *final* nil)
  (loop for rule in rulesOfProduction do
       (let* ((firstElement (second rule))
              (terminal (first (third rule))))
         (if (not (isTerminal? firstElement))
             (loop until (isTerminal? firstElement)do
                  (setq firstElement (substitution firstElement rulesOfProduction))))
         (setq *final* (cons terminal firstElement))
         (setq *greibach* (cons *final* *greibach*))
         ))

  (setq *final* (delete 2 (nth 5 *greibach*)))
  (format t "~& Forma ~% Y: ~A ~% X: ~A - ~A ~% C: ~A - ~A ~% B: ~A -- ~A ~A ~% A: ~A --  ~A  ~A~%" (second (first *greibach*))
          (second (second *greibach*))
          (cdr (nth 3 *greibach*))
          (first (nth 4 *greibach*))
          (cdr (nth 4 *greibach*))
          (first (nth 5 *greibach*))
          (subseq (nth 5 *greibach*) 1 5)
          (subseq (nth 5 *greibach*) 8 11)
          (first (nth 6 *greibach*))
          (subseq (nth 6 *greibach*) 1 4)
          (subseq (nth 6 *greibach*) 5)
          ))

;;Produccion de ejemplo de la presentacion
(convertToGreibach '(
                     ((A)(C B)(2))
                     ((B)(A Y)(1))
                     ((C)(D X)(0))
                     ((X)(E X)(1))
                     ((D)(0))
                     ((E)(1))
                     ((Y)(1))
                     ))



