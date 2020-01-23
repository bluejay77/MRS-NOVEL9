;;; -*- Mode: Common-LISP -*-
;;;
;;; MACLISP compatibility functions
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-09 from the Pitmanual
;;;
;;; ------------------------------------------------------------
;;;


(DEFUN EXPLODE (OBJ) 
  (MAP 'LIST #'(LAMBDA (X) (INTERN (STRING X))) (FORMAT NIL "~S" OBJ)))

(DEFUN EXPLODEC (OBJ)
  (MAP 'LIST #'(LAMBDA (X) (INTERN (STRING X))) (FORMAT NIL "~A" OBJ)))

(DEFUN EXPLODEN (OBJ)
  (MAP 'LIST #'STRING (FORMAT NIL "~A" OBJ)))

(DEFUN IMPLODE (CH-LIST)
  (INTERN (MAKNAM CH-LIST)))

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
    (UNLESS (OR (>= IDX (STRING-LENGTH STR))
		(< IDX 0))
      (AREF STR IDX))))

