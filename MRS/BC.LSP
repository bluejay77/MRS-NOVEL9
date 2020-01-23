
perm filename BC.LSP[MRS,LSP] blob sn#702111 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00003 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00004 00003
C00007 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp (load '|macros.fasl|)
	   #+franz (load 'macros)
	   (impvar truth))

;defined in macros
;(defun pset (x y al) (cons (cons x y) al))

(defun punset (x al)
  (if (eq x (caar al)) (cdr al)
      (do l al (cdr l) (null (cdr al))
	  (cond ((eq x (caadr l)) (rplacd l (cddr l)) (return al))))))



(defun &truep (p) 
  (cond ((eq 'and (car p)) (&truep-and p))
	((eq 'or (car p)) (&truep-or p))
	((fb 'pr-lookup p))
	((bc-&truep p))))

(defun &trueps (p)
  (Cond ((groundp p) (if (setq p (&truep p)) (list p)))
	((eq 'and (car p)) (&trueps-and p))
	((eq 'or (car p)) (&trueps-or p))
	(t (nconc (fbs 'pr-lookup p) (bc-&trueps p)))))

(defun bc-&truep (q)
  (do ((l (fbs 'pr-lookup `(if $p ,q)) (cdr l)) (al) (dum)) ((null l))
      (cond ((setq dum (&truep (getvar '$p (car l))))
	     (setq dum (alconc (punset '$p (car l)) dum))
	     (return dum)))))

(defun bc-&trueps (q)
  (do ((l (fbs 'pr-lookup `(if $p ,q)) (cdr l)) (lhs) (dum) (nl))
      ((null l) nl)
      (setq lhs (getvar '$p (car l)) dum (punset '$p (car l)))
      (mapc '(lambda (m) (setq nl (cons (alconc m dum) nl)))
	    (&trueps lhs))))

(defun &truep-and (p) (&truep-and1 (cdr p) truth))
(defun &truep-and1 (cs al)
  (cond ((null (cdr cs))
	 (if (setq cs (&truep (plug (car cs) al))) (alconc cs al)))
	(t (do ((l (&trueps (plug (car cs) al)) (cdr l)) (dum))
	       ((null l) nil)
	       (if (setq dum (&truep-and1 (cdr cs) (alconc (car l) al)))
		   (return dum))))))

(defun &trueps-and (p) (&trueps-and1 (cdr p) truth nil))
(defun &trueps-and1 (cs al nl)
  (cond ((null cs) (cons al nl))
	(t (do ((l (&trueps (plug (car cs) al)) (cdr l)))
	       ((null l) nl)
	       (setq nl (&trueps-and1 (cdr cs) (alconc (car l) al) nl))))))


(defun &truep-or (p) (mapor '&truep (cdr p)))

(defun &trueps-or (p)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) nl)
      (setq nl (nconc (&trueps (car l)) nl))))

