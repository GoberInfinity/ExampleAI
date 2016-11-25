
;;; A simplified version of the Deductive Data Retriever
;;;
;;; Limitations:
;;;
;;; All assertions are rules, e.g., (<- (mortal ?x) (human ?x)),
;;; including "facts," e.g., (<- (human socrates)).
;;;
;;; No forward chaining rules.
;;;
;;; No (tell ...) function. Use (with-kb kb (ask ...)) 
;;; where kb should evaluate to a list of rules. See
;;; retriever-tests.lisp for examples.
;;;
;;; No ASK-TRACE


(defpackage :retriever
  (:use :common-lisp)
  (:export #:<- #:ask #:unify #:with-kb #:cargar-reglas))

(in-package :retriever)

(defvar *kb* nil)

(defmacro with-kb (kb &body body)
  `(let ((*kb* ,kb)) ,@body))

(defun ask (pat &optional (form pat))
  (mapcar #'(lambda (blist) (replace-vars form blist))
          (find-blists pat (list nil))))

(trace replace-vars)
(trace find-blists )

(defun find-blists (pat blists)
  (and blists
       (mapcan #'(lambda (rule)
                   (try-rule pat (rename-vars rule) blists))
               *kb*)))

;; (trace try-rule)
;; (trace rename-vars)
;; (trace prove-and)
;; (trace vars-in)

(defun try-rule (pat rule blists)
  (prove-and (cddr rule)
             (unify pat (cadr rule) blists)))

(defun prove-and (conjuncts blists)
  (and blists
       (if (null conjuncts) blists
         (prove-and (cdr conjuncts)
                    (find-blists (car conjuncts) blists)))))

;;; Recursively all variables with the values, if any. Leave
;;; unbound variables alone.
(defun replace-vars (form blist)
  (cond ((var-p form)  (replace-var form blist))
        ((atom form) form)
        (t (cons (replace-vars (car form) blist)
                 (replace-vars (cdr form) blist)))))

(defun replace-var (var blist)
  (let ((bdg (var-binding var blist)))
    (if (null bdg) var (replace-vars (binding-value bdg) blist))))

;;; Replace all variables in form with newly generated ones.
(defun rename-vars (form)
  (print "//////////////////")
  (print form)
  (print "//////////////////")
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")))
                  (vars-in form))
          form))

(defun vars-in (form &optional vars)
  (cond ((var-p form) (adjoin form vars))
        ((atom form) vars)
        (t (vars-in (car form)
                    (vars-in (cdr form) vars)))))

(defun unify (x y &optional (blists '(nil)))
  (cond ((null blists) nil)
        ((eql x y) blists)
        ((var-p x) (var-unify x y blists))
        ((var-p y) (var-unify y x blists))
        ((atom x) nil)
        ((atom y) nil)
        (t (unify (cdr x) (cdr y)
                  (unify (car x) (car y) blists)))))

(defun var-unify (var pat blists)
  (mapcan #'(lambda (blist) (bind-var var pat blist))
          blists))

(defun bind-var (var pat blist)
  (if (and (var-p pat)
           (var-equalp var pat blist))
      (list blist)
      (let ((bdg (var-binding var blist)))
        (cond (bdg (unify (binding-value bdg) pat (list blist)))
              ((contained-in-p var pat blist) nil)
              (t (list (add-var-binding var pat blist)))))))

(defun var-equalp (var1 var2 blist)
  (and (var-p var2)
       (or (eql (var-name var1) (var-name var2))
           (var-equalp var1 (var-value var2 blist) blist))))

(defun contained-in-p (var pat blist)
  (if (var-p pat)
      (or (eql (var-name var) (var-name pat))
          (contained-in-p var
                          (binding-value (var-binding pat blist))
                          blist))
      (and (consp pat)
           (or (contained-in-p var (car pat) blist)
               (contained-in-p var (cdr pat) blist)))))

(defun var-p (x)
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))

(defun var-name (x) x)

(defun add-var-binding (var val blist)
  (cons (list (var-name var) val) blist))

(defun var-binding (var blist)
  (assoc (var-name var) blist))

(defun var-value (var blist)
  (binding-value (var-binding var blist)))

(defun binding-value (bdg)
  (cadr bdg))

(defun cargar-reglas (filename)
  (with-open-file (stream filename)
    (let ((numrows (read stream))
          (lista nil))
      (read-line stream nil nil)
      (dotimes (row numrows lista)
        (setq lista (cons (read stream nil nil) lista))))))

;; (defparameter *tiger-kb*
;;   '(
    ;; (<- (can-satisfy-hunger-with ?x ?y)
    ;;  (eats ?x ?y)
    ;;  (can-bite ?x ?y))

    ;; (<- (eats ?y ?x)
    ;;  (predator-of ?x ?y))

    ;; (<- (can-bite ?x ?y)
    ;;  (isa ?x animal)
    ;;  (near ?x ?y))

    ;; (<- (near ?x ?y)
    ;;  (at ?x ?loc)
    ;;  (at ?y ?loc))

;;     (<- (eats antelope grass))
;;     (<- (eats antelope ferns))
;;     (<- (predator-of antelope tiger))
;;     (<- (predator-of zebra tiger))

;;     (<- (at antelope savannah))
;;     (<- (at tiger savannah))
;;     (<- (at grass savannah))

;;     (<- (isa tiger animal))
;;     (<- (isa antelope animal))
;;     ))

;; (with-kb *tiger-kb* (ask '(at antelope savannah)))
;; (with-kb *tiger-kb* (ask '(at tiger savannah)))
;; (with-kb *tiger-kb* (ask '(can-bite grass antelope)))


