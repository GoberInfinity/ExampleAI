;;Reyes Fragoso Roberto

;; Definimos como parametro el vector en donde vayamos a almacenar nuestra base de conocimiento
(defparameter *vector-conocimiento* (make-array 100
                                            :adjustable T
                                            :element-type 'list))
(defparameter *inicio* '((T.T)))											
(defconstant fail nil
  "Indicates pat-match failure")
  
;;[Funcion] Permite leer de archivo nuestra base de conocimiento
(defun leerConocimiento (filename)
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
          (adjust-array *vector-conocimiento* numrows)
          (read-line stream nil nil)
          (dotimes (row numrows *vector-conocimiento*)
            (setf (aref *vector-conocimiento* row) (read stream nil nil))))))

(leerConocimiento "baseDeConocimiento.txt")

(defun miniSistemaExperto ()
	(loop
		(print "Escriba la consulta que desea realizar")
		(let* ((entrada (read))
           (salida (patternMatch input))))
      (if (equal entrada '(Salir)) (RETURN))))
	#|  
(defun patternMatch (patron expresion &optional(alist *inicio*))
		(cond ((esVariable? patron)
				(matchearVariable patron expresion alist))
			  ((esConstante? patron)
				(matchearConstante patron expresion alist))
		((and (consp pat) (consp exp))
(patternMatch (rest pat) (rest exp)
(patternMatch (first pat) (first exp) alist))
(T alist))))

(defun esVariable? (x)
  (and (symbolp x)
       (equal (char (symbol-name x) 0)
              #\?)))
			  
(defun segment-pattern-p (pat)
  "Is this a segment-matching pattern: (?*var ...)"
  (and (listp pat)
       (>= (length (symbol-name (car pat))) 2)
       (equal (char (symbol-name (car pat)) 0) #\?)
       (equal (char (symbol-name (car pat)) 1) #\*)))			  

		|#	  

(print *vector-conocimiento*)

