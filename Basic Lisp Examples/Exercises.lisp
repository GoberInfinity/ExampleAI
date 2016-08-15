;Reyes Fragoso Roberto
;Ejercicios parte 1
;a) El quinto elemento de la lista (((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))) sin usar la función FIFTH
(nth 4 '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))))
;b) El numero de segundos que tiene el año bisiesto 2004
(* 60 60 24 366)
;c) Si el valor numerico asociado a la variable x es diferente de cero y además menor o igual que el valor asociado a la variable y (x =2, y=1)
(AND (NOT (EQUAL 2 0)) (<= 2 1 ))
;d) Una lista con las dos soluciones reales de la ecuación
(complex x y)

;Ejercicios parte 2
a) (+ (* 2 4)(- 6 8))
b) (/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))
c) (sqrt (/ (+ 1.4502 (* -1 (- -4 (/ 3 8)))) (expt -1 (expt (- 3 5) (/ 1 3)))))
d) (expt (/ (expt (/ 65.402 (sqrt -1)) (/ 5)) 0.17) (/ 7))

;Ejercicios parte 3
;a) (cdar '((one two) three four)))
TWO
;b) (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
((EVA LISA) KARL SVEN EVA LISA KARL SVEN) 
;c) (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
(EVA GITAN LISA GITAN KARIN)
;d) (remove 'sven '(eva sven lisa sven anna))
(EVA LISA ANA)
;e) (butlast '(karl adam nilsson gregg alisson vilma) 3)
(KARL ADAM NILSSON)
;f) (nth 2 '(a b c d e))
C
;g) (nthcdr 2 '(a b c d e))
(C D E)
;h) (intersection '(a b c) '(x b z c))
(B C)
;i) (cdadar '(((((1 2 3) z) y)(x 4)) 7 8 (a b c (5 (6 7 8)))))
(4)