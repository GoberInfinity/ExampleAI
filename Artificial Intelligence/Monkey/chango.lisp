;; Reyes Fragoso Roberto

;;[Paquete] Seguimos el ejemplo de retriever-tests.lisp, y definimos un paquete que use nuestro retriever
(defpackage #:chango
  (:use #:common-lisp #:retriever))
(in-package :chango)

;; [Parametro] Comenzamos con definir un parámetro que será el conjunto de reglas para
;;  dar solución al problema del chango.
(defparameter *chango* nil)

;;[Auxiliar] Permite limpiar las variables globales
(defun limpiarVariables () (setq *chango* nil))

;;[Principal] Funcion principal para dar respuesta al problema
(defun problemaChango ( )
  ;;Limpiamos nuestras reglas y hechos
  (limpiarVariables)
  ;;Comenzamos leyendo desde archivo nuestras reglas y hechos
  ;; recordar que "retriever" se declara tambien los hechos como reglas
  (setq *chango* (cargar-reglas "reglas.txt"))
  (consultarReglas))

;;[Funcion] Permite consultar al retriever
(defun consultarReglas ()
	;; Dada las definiciones del problema debemos dar solucion al problema con la especificacion de una 
	;;  sola pregunta por lo que preguntamos solamente una vez y dejamos que el arbol lo construya nuestro
	;;  retriever.
	(print (with-kb *chango* (ask '(puedeTener (estado puerta ventana piso nopalito) ?pasos)))))

;; Inicio del programa, nota: Compilar primero retriever.lisp; para ver el resultado ver en la pantalla de "slime-repl sbcl"
(problemaChango)
