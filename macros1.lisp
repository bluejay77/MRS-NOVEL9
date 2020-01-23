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



;;; The LOCAL macro.  This is important

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
     (cond ((atom (car l)) (setq vl (cons (car l) vl) nl (cons nil nl)))
	   (t (setq vl (cons (caar l) vl) nl (cons (cadar l) nl))))))

(defmacro mapand (x)
  ``(do ((l ,(caddr ,x) (cdr l))) ((null l) t)
      (ifn (funcall ,(cadr ,x) (car l)) (return nil))))

(defmacro mapor (x)
  ``(do ((l ,(caddr ,x) (cdr l)) (dum)) ((null l))
      (if (setq dum (funcall ,(cadr ,x) (car l))) (return dum))))

(defmacro maplac (x)
  ``(do l ,(caddr ,x) (cdr l) (null l) (rplaca l (,(cadr ,x) (car l)))))

(defmacro ifn (x)
  `(cond ((null (cdddr ,x)) `(cond ((not ,(cadr ,x)) ,(caddr ,x))))
	 (t `(cond ((not ,(cadr ,x)) ,(caddr ,x)) (t . ,(cdddr ,x))))))

;;; (defun put macro (x) `(putprop . ,(cdr x)))

(defmacro put (x) `(setf (get (car ,x) (cadr ,x)) (caddr ,x)))

;;; (defun iand macro (x) `(boole 1 . ,(cdr x)))

(defmacro iand (x) `(boole 6 1 (cdr ,x)))

;;; (defun ior macro (x) `(boole 7 . ,(cdr x)))

(defmacro ior (x) `(boole 7 (cdr ,x)))

;;; (defun xor macro (x) `(not (eq . ,(cdr x))))

(defmacro xor (x) `(boole 8 (cdr ,x)))

;(defun copy macro (x) `(subst nil nil ,(cadr x)))
(defmacro copyp (x) ``(cons (car ,(cadr ,x)) (cdr ,(cadr ,x))))
(defmacro copyl (x) ``(append ,(cadr ,x) nil))


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

;;; #-lispm(defun aset macro (x) `(store ,(cddr x) ,(cadr x)))
;;; #-lispm(defun aref macro (x) (cdr x))

(defmacro impvar (x) ``(special . (cdr ,x)))
(defmacro expvar (x) ``(special . (cdr ,x)))
(defmacro impfun (ignore) nil)
(defmacro expfun (ignore) nil)

(defmacro pset (x) ``(cons (cons (cadr ,x) (caddr ,x)) (cadddr ,x)))

