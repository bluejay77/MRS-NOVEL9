;;; -*- Mode: Common-LISP  -*-
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-09
;;;
;;; ------------------------------------------------------------
;;;
;;; Various MACLISP -- Common LISP compatibility issues
;;;
;;;


;;; The Maclisp delq is missing in several Common LISP implementations

(defun delq (x y) (delete x y :test #'equal)) ; The variant of the theme... 

;;; The Maclisp addq also is missing in several Common LISP
;;; implementations
;;; (addq where-to-insert what-to-insert)
;;;

(defun addq (x y) (if (member y x :test #'equal) ; The variant of the theme 
		      x
		      (append x (list y))))


;;; The Maclisp assq is missing in the Common LISP implementations

(defun assq (x y)
  (assoc x y :test #'equal)) ; The variant of the theme


;;; The Maclisp memq is missing in the Common LISP implementations

(defun memq (x y) (member x y :test #'equal))


;;; The LSH, Left Shift Binary an integer
;;;
;;; AJY 2015-05-09
;;;
;;; TODO: shiftaamisen jälkeen kerro 2:lla
;;;

(defun log2 (n) (/ (log n) (log 2.0))) ; Base 2 logarithm

(defun lsh1 (int) ; Simple way to Left Shift by one bit
  (let ((order-of-magnitude (floor (log2 int))))
    (- int (round (expt 2 order-of-magnitude)))))

(defun lsh (int n) ; Left Shift by n bits
  (cond ((<= int 0)
	 0)
	(t
	 (let ((aux int))
	   (loop
	      for i from 1 to n
	      do
		(if (<= aux 0)
		    0
		    (setf aux (lsh1 aux)))
	      finally (return aux))))))



;;; MACLISP compatibility functions
;;;
;;; ------------------------------------------------------------
;;;
;;; Antti J Ylikoski 2015-05-09 from the Pitmanual
;;;
;;; ------------------------------------------------------------
;;;


(DEFUN EXPLODE (OBJ) 
  (MAP 'LIST #'(LAMBDA (X) (INTERN (STRING X))) (FORMAT NIL "~S" OBJ)))

(DEFUN EXPLODEC (OBJ)
  (MAP 'LIST #'(LAMBDA (X) (INTERN (STRING X))) (FORMAT NIL "~A" OBJ)))

(DEFUN EXPLODEN (OBJ)
  (MAP 'LIST #'STRING (FORMAT NIL "~A" OBJ)))

#|

(DEFUN IMPLODE (CH-LIST)
  (INTERN (MAKNAM CH-LIST)))

|#


;;; This was wrong in the MACLISP Pitmanual

(defun implode (l)
  (let ((sym (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop for i in l do (setf sym
			      (concatenate 'string
					   sym
					   (format nil "~A" i))))
    (intern sym)))


(DEFUN MAKNAM (CH-LIST)
  (MAKE-SYMBOL (FORMAT NIL "~{~C~}" CH-LIST)))

(DEFUN READLIST (X)
  (READ-FROM-STRING (APPLY #'STRING-APPEND (MAPCAR #'STRING X))))

(DEFUN ASCII (N)
  (COND ((NUMBERP N) (ASCII (CODE-CHAR N)))
	(T (INTERN (STRING N)))))

(DEFUN GETCHARN (X N)
  (LET ((IDX (- N 1))
	(STR (STRING X)))
    (UNLESS (OR (>= IDX (LENGTH STR))
		(< IDX 0))
      (CHAR-CODE (AREF STR IDX)))))

(DEFUN STRING-LENGTH (STR)
  (LENGTH STR))

(DEFUN STRING-APPEND (STR1 STR2)
  (CONCATENATE 'STRING
	       STR1
	       STR2))


;;; The MACLISP putprop
;;;

(defun putprop (obj val propert)
  (setf (get obj propert) val))

