(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

(defun leerUsuario ()
  (let* ((primeraCadena nil)
	 (cadenaFinal nil)
	 (x nil))
    (print "La expresion regular es: (A|B)A*")
    (loop for x from 1 to 5 do
	 (setq primeraCadena (write-to-string (read)))
	 (setq cadenaFinal  (scan-to-strings "(A|B)A*" primeraCadena))
	 (if (equal primeraCadena cadenaFinal)
	     (print "Valida")
	     (print "No valida")))
    (print "Programa terminado")
    ))

(leerUsuario)
