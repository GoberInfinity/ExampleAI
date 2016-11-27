;; Reyes Fragoso Roberto

;;[Paquete] Seguimos el ejemplo de retriever-tests.lisp, y definimos un paquete que use nuestro retriever
(defpackage #:zebra
  (:use #:common-lisp #:retriever))
(in-package :zebra)

;;[Parametro] Como vamos a utilizar encadenamiento hacia atras, tenemos que partir de
;; nuestra hipotesis y buscamos probarla, supondremos una permutacion de hipotesis con
;; los parametros, nacionalidad, color, mascota, cigarros y bebida
(defparameter *colores* '(Casa-Azul Casa-Verde Casa-Amarilla Casa-Roja Casa-Blanca))
(defparameter *nacionalidades* '(Ucraniano Ingles Noruego Japones Espanol))
(defparameter *bebidas* '(Leche Jugo Cafe Te Agua))
(defparameter *cigarros* '(Kools Old-Gold Chesterfields Lucky-Strike Parlaments))
(defparameter *mascotas* '(perro zorro caracol caballo zebra))
;; ;;[Parametro] Comenzamos con definir un parámetro que será el conjunto de reglas para
;; ;; dar solución al problema de la zebra.
(defparameter *zebra* nil)

;;[Principal] Funcion principal para dar respuesta al problema
(defun problemaZebra ( )
  ;;Limpiamos nuestras reglas y hechos
  (limpiarVariables)
  ;;Comenzamos leyendo desde archivo nuestras reglas y hechos
  ;; recordar que "retriever" se declara tambien los hechos como reglas
  (setq *zebra* (cargar-reglas "reglas.txt"))
  (consultarReglas))

;;[Funcion] Genera todas las combinaciones y pregunta si esa combinacion es la que posee la zebra o toma agua
(defun consultarReglas ()
  ;; Hacemos un loop que nos va a generar todas nuestras hipotesis, por lo que mandamos a llamar a combinaciones
  (loop for elemento in (combinaciones *colores* *nacionalidades* *bebidas* *cigarros* *mascotas*) do
       (let* ((colorCasa (nth 0 elemento))
              (nacionalidad (nth 1 elemento))
              (bebida (nth 2 elemento))
              (cigarros (nth 3 elemento))
              (mascota (nth 4 elemento))
              ;; Creamos una variable local con todos los atributos que vamos a incluir en nuestra hipotesis
              (persona (list 'tomaAgua colorCasa nacionalidad bebida cigarros mascota))
              ;; Asi como tambien supondremos que la misma persona tiene la zebra
              (personaZ (list 'tieneZebra colorCasa nacionalidad bebida cigarros mascota))
              ;;Creamos una variable que cambiara con cada iteracion, donde sabremos si nuestra hipotesis fue correcta
              (respuestaAPersona (with-kb *zebra* (ask persona)))
              (respuestaAPersonaZebra (with-kb *zebra* (ask personaZ))))

         ;; Si la respuesta que nos envia no es nula, significa que es verdad, nuestra hipotesis era correcta, por lo
         ;;  cual la imprimiremos en pantalla
         (if (not (null respuestaAPersona))
             (format t "~& La persona que vive en ~A es ~A toma ~A fuma ~A y tiene de mascota ~A ~%"
                     (second (first respuestaAPersona))
                     (third (first respuestaAPersona))
                     (fourth (first respuestaAPersona))
                     (fifth (first respuestaAPersona))
                     (sixth (first respuestaAPersona))))
         (if (not (null respuestaAPersonaZebra))
             (format t "~& La persona que vive en ~A es ~A toma ~A fuma ~A y tiene de mascota ~A ~%"
                     (second (first respuestaAPersonaZebra))
                     (third (first respuestaAPersonaZebra))
                     (fourth (first respuestaAPersonaZebra))
                     (fifth (first respuestaAPersonaZebra))
                     (sixth (first respuestaAPersonaZebra)))))))

;;[Auxiliar] Permite limpiar las variables globales
(defun limpiarVariables () (setq *zebra* nil))

;;[Auxiliar] Permite realizar todas las combinaciones posibles, que van a ser nuestras hipotesis
(defun combinaciones (&rest listas)
  (if (car listas)
      (mapcan (lambda (valorEntrada)
                (mapcar (lambda (valorSalida)
                          (cons valorSalida
                                valorEntrada))
                        (car listas)))
              (apply #'combinaciones (cdr listas)))
      (list nil)))

;; Inicio del programa, nota: Compilar primero retriever.lisp; para ver el resultado ver en la pantalla de "slime-repl sbcl"
(problemaZebra)

