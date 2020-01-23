;;; -*- Mode: Common-LISP -*-
;;;
;;;MACROS.LSP    1.2 18-Mar-83 0121 000   1 ML E      18-Mar-83   
;;;	Defines macros used by other files for compiling
;;;
;;;perm filename MACROS.LSP[MRS,LSP] blob sn#702098 filedate
;;;	1983-03-18 generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00005 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002	             -*- Mode:LISP  -*-                                
;;;     
;;;C00009 00003
;;;C00019 00004
;;;C00020 00005
;;;C00021 ENDMK
;;;C⊗;
;;;             -*- Mode:LISP;  -*-                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981 Michael R. Genesereth			 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; AJY 2015-05-02
;;;
;;; A number of modifications in various points.
;;;
;;; ------------------------------------------------------------
;;;




;;; The LOCAL macro.  Substituted this w/ the (LET ...) form instead
;;; of (LOCAL ...)


(defmacro local (x)
  `(do ((l (cadr ,x) ; Initial value
	   (cdr l))  ; Next value
	(nl)  ; Initial value shall be NIL
	(vl)) ; Initial value shall be NIL
       ((null l) ; Loop end test
	(progn
	  (setf vl (nreverse vl))
	  (setf nl (nreverse nl))) ; Loop body follows
	`((lambda ,vl . ,(cddr ,x)) . ,nl) ; Another sexp in loop body
	) ; End of DO loop
     (cond ((atom (car l))
	    (setf vl (cons (car l) vl))
	    (setf nl (cons nil nl)))
	   (t
	    (setf vl (cons (caar l) vl))
	    (setf nl (cons (cadar l) nl))))))



(defmacro mapand (fun list)
  `(do ((l ,list (cdr l))) ; Var init repeat
       ((null l) t) ; Termination condition
       (unless (funcall ,fun (car l))
	 (return nil)))) ; If NIL, return immediately



(defmacro mapor (fun list)
  `(do ((l ,list (cdr l)) ; Var init repeat
       (dum)) ; Dummy
       ((null l)) ; Term condition
       (if (setf dum (funcall ,fun (car l)))
	   (return dum))))



(defmacro maplac (fun list)
  `(do ((l ,list (cdr l)))
       ((null l))
     (rplaca l (funcall fun (car l)))))


#|
;;; This had to be corrected.

(defmacro ifn (x)
  `(cond ((null (cdddr ,x))
	  `(cond ((not ,(cadr ,x)) ,(caddr ,x))))
	 (t `(cond ((not ,(cadr ,x)) ,(caddr ,x)) (t . ,(cdddr ,x))))))

|#

;;; (defun put macro (x) `(putprop . ,(cdr x)))

(defmacro put (x) `(setf (get (car ,x) (cadr ,x)) (caddr ,x)))

;;; (defun iand macro (x) `(boole 1 . ,(cdr x)))

(defmacro iand (x) `(boole 6 1 (cdr ,x)))

;;; (defun ior macro (x) `(boole 7 . ,(cdr x)))

(defmacro ior (x) `(boole 7 (cdr ,x)))

;;; (defun xor macro (x) `(not (eq . ,(cdr x))))

(defmacro xor (x) `(boole 8 (cdr ,x)))

;(defun copy macro (x) `(subst nil nil ,(cadr x)))

(defmacro copyp (x) `(cons (car (cadr ,x)) (cdr (cadr ,x))))
(defmacro copylm (x) `(append  ,x nil))


(defmacro caaadar (x) `(caaadr (car (cadr ,x))))
(defmacro caaaddr (x) `(caaadr (cdr (cadr ,x))))
(defmacro caadaar (x) `(caadar (car (cadr ,x))))
(defmacro caadadr (x) `(caadar (cdr (cadr ,x))))
(defmacro caaddar (x) `(caaddr (car (cadr ,x))))
(defmacro cadaaar (x) `(cadaar (car (cadr ,x))))
(defmacro cadaddr (x) `(cadadr (cdr (cadr ,x))))
(defmacro caddaar (x) `(caddar (car (cadr ,x))))
(defmacro caddadr (x) `(caddar (cdr (cadr ,x))))
(defmacro cadddar (x) `(cadddr (car (cadr ,x))))
(defmacro caddddr (x) `(cadddr (cdr (cadr ,x))))
(defmacro cdadadr (x) `(cdadar (cdr (cadr ,x))))
(defmacro cdadddr (x) `(cdaddr (cdr (cadr ,x))))
(defmacro cdddddr (x) `(cddddr (cdr (cadr ,x))))

;;; aset is some sorta ASSOCIATION LIST SET
;;; (defun aset macro (x) `(store ,(cddr x) ,(cadr x)))

(defmacro aset (&rest x)
  `(store '(cadr ,x) '(car ,x)))

(defun aset-f (datum data index)
  (setf (aref data index) datum))


(defun store (item alist)
  (append alist (list item)))



;;; Duplicate definition.

#|

(defmacro pset (&rest x) `(cons
			   (cons (cadr ,x)
				 (caddr ,x))
			   (cadddr ,x)))
|#



