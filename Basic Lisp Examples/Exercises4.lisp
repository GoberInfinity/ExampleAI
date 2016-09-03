;Reyes Fragoso Roberto

;Problema 1
(defun ultimoElemento (l)
  (car (last l)))

(defun palindromo (l)
  (if (equal nil l)
      t
      (let ((inicio (first l))
	    (final (ultimoElemento l)))
	(if (equal inicio final)
	    (palindromo (cdr (butlast l)))
	    nil))))

(palindromo '(o o s o o)) ; => T

;Problema 5
(defun listRotate (l &key (right nil) (left nil))
  (if right
      (rotate l right)
      (rotate l (* -1 left))))

(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))

(listRotate '(a b c d e f g h) :right 3) ; => (D E F G H A B C)

;Problema 7 
(defun Combine (fun l)
  (cond ((null l) 0)
	( t (funcall fun (Combine fun (cddr l)) (car l) (cadr l)))))

(Combine #'+ '(1 2 3 4)); => 7

;Problema 9
(defun encode (list)
  (labels ((encode-run (element count list)
             (cond
               ((null list) (list (list count element)))
               ((eql element (first list)) (encode-run element (1+ count) (rest list)))
               (t (cons (list count element) (encode-run (first list) 1 (rest list)))))))
    (if (null list)
        '()
        (encode-run (first list) 1 (rest list)))))

(encode '(a a a a b c c a a d e e e e)) ; => ((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))


;Problema 14
(defun combinations (count list)
  (cond
    ((zerop count) '(())) 
    ((endp list)   '())   
    (t (nconc (mapcar (let ((item (first list))) (lambda (combi) (cons item combi)))
                      (combinations (1- count) (rest list)))
              (combinations count (rest list))))))

(combinations 5 '(a b c d e f)); => ((A B C D E) (A B C D F) (A B C E F) (A B D E F) (A C D E F) (B C D E F))

(subseq "Hola prro" 0 1)


