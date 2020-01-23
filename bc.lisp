;;; -*- Mode: Common-LISP -*-
;;;
;;;BC.LSP    0.4 18-Mar-83 0108 000   1 ML E      18-Mar-83   
;;;	Functions such as &trueps, bc-&truep.
;;;
;;;perm filename BC.LSP[MRS,LSP] blob sn#702111 filedate 1983-03-18
;;;	generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00003 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00004 00003
;;;C00007 ENDMK
;;;C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; Converted this to the Common LISP.  AJY 2015-05-09
;;;
;;; ------------------------------------------------------------
;;;
;;; Changed the use of the & symbol to the prefix META since the
;;; & char is being used for lambda list keywords, and it will confuse
;;; the compilers.  AJY 2015-05-09


; Defined in macros
;(defun pset (x y al) (cons (cons x y) al))

#|
(defun punset (x al)
  (if (eq x (caar al)) (cdr al)
      (do ((l al (cdr l)))
	  ((null (cdr al)))
	(cond ((eq x (caadr l)) (rplacd l (cddr l)) (return al))))))
|#


(defun meta-truep (p) 
  (cond ((eq 'and (car p)) (meta-truep-and p))
	((eq 'or (car p)) (meta-truep-or p))
	((fb 'pr-lookup p))
	((bc-meta-truep p))))

(defun meta-trueps (p)
  (Cond ((groundp p) (if (setq p (meta-truep p)) (list p)))
	((eq 'and (car p)) (meta-trueps-and p))
	((eq 'or (car p)) (meta-trueps-or p))
	(t (nconc (fbs 'pr-lookup p) (bc-meta-trueps p)))))

(defun bc-meta-truep (q)
  (do ((l (fbs 'pr-lookup `(if $p ,q)) (cdr l))
       ;; (al) ; This was an unused variable
       (dum))
      ((null l))
      (cond ((setq dum (meta-truep (getvar '$p (car l))))
	     (setq dum (alconc (punset '$p (car l)) dum))
	     (return dum)))))

(defun bc-meta-trueps (q)
  (do ((l (fbs 'pr-lookup `(if $p ,q)) (cdr l)) (lhs) (dum) (nl))
      ((null l) nl)
    (progn
      (setq lhs (getvar '$p (car l)) dum (punset '$p (car l)))
      (mapc #'(lambda (m) (setq nl (cons (alconc m dum) nl)))
	    (meta-trueps lhs)))))


(defun meta-truep-and (p)
  (declare (special truth))
  (meta-truep-and1 (cdr p) truth))

(defun meta-truep-and1 (cs al)
  (cond ((null (cdr cs))
	 (if (setq cs (meta-truep (plug (car cs) al))) (alconc cs al)))
	(t (do ((l (meta-trueps (plug (car cs) al)) (cdr l)) (dum))
	       ((null l) nil)
	       (if (setq dum (meta-truep-and1 (cdr cs) (alconc (car l) al)))
		   (return dum))))))

(defun meta-trueps-and (p)
  (declare (special truth))
  (meta-trueps-and1 (cdr p) truth nil))

(defun meta-trueps-and1 (cs al nl)
  (cond ((null cs) (cons al nl))
	(t (do ((l (meta-trueps (plug (car cs) al)) (cdr l)))
	       ((null l) nl)
	       (setq nl (meta-trueps-and1 (cdr cs) (alconc (car l) al) nl))))))


(defun meta-truep-or (p) (mapor #'meta-truep (cdr p)))

(defun meta-trueps-or (p)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) nl)
      (setq nl (nconc (meta-trueps (car l)) nl))))

