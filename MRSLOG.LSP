
perm filename MRSLOG.LSP[MRS,LSP] blob sn#620900 filedate 1981-10-23 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00003 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002	               -*-Mode:LISP Package:MACSYMA -*-                      
C00009 00003
C00013 ENDMK
C⊗;
;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '|<csd.Genesereth>macros.fasl|)
	 (impfun falsep) (expfun truep-not thnot truep-and truep-or))

($assert '(all p (MyToTruep (not p) truep-not)))
($assert '(all p (MyToTrueps (not p) trueps-not)))

(defun truep-not (p)
  (cond ((eq 'not (caadr p)) (truep (cadadr p)))
	((eq 'or (caadr p))
	 (truep (cons 'and (mapcar '(lambda (l) (list 'not l)) (cdadr p)))))
	((eq 'and (caadr p))
	 (truep (cons 'or (mapcar '(lambda (l) (list 'not l)) (cdadr p)))))
	(t (bs-truep p))))

(defun thnot (p) (ifn (truep (cadr p)) truth))

(defun trueps-not (p)
  (cond ((eq 'not (caadr p)) (trueps (cadadr p)))
	((eq 'or (caadr p))
	 (trueps (cons 'and (mapcar '(lambda (l) (list 'not l)) (cdadr p)))))
	((eq 'and (caadr p))
	 (trueps (cons 'or (mapcar '(lambda (l) (list 'not l)) (cdadr p)))))
	(t (bs-trueps p))))

(defun thnots (p) (ifn (trueps (cadr p)) (list truth)))


;------------------------------------------------------------------------
($assert '(all p q (MyToAssert (and p q) assert-and)))
($assert '(all p q r (MyToAssert (and p q r) assert-and)))
($assert '(all p q r s (MyToAssert (and p q r s) assert-and)))
($assert '(all p q r s t (MyToAssert (and p q r s t) assert-and)))
($assert '(all p q r s t u (MyToAssert (and p q r s t u) assert-and)))
($assert '(all p q r s t u v (MyToAssert (and p q r s t u v) assert-and)))
($assert '(all p q r s t u v w (MyToAssert (and p q r s t u v w) assert-and)))
($assert '(all p q r s t u v w x
	       (MyToAssert (and p q r s t u v w x) assert-and)))
($assert '(all p q r s t u v w x y
	       (MyToAssert (and p q r s t u v w x y) assert-and)))
;------------------------------------------------------------------------
($assert '(all p q (MyToTruep (and p q) truep-and)))
($assert '(all p q r (MyToTruep (and p q r) truep-and)))
($assert '(all p q r s (MyToTruep (and p q r s) truep-and)))
($assert '(all p q r s t (MyToTruep (and p q r s t) truep-and)))
($assert '(all p q r s t u (MyToTruep (and p q r s t u) truep-and)))
($assert '(all p q r s t u v (MyToTruep (and p q r s t u v) truep-and)))
($assert '(all p q r s t u v w (MyToTruep (and p q r s t u v w) truep-and)))
($assert '(all p q r s t u v w x
	       (MyToTruep (and p q r s t u v w x) truep-and)))
($assert '(all p q r s t u v w x y
	       (MyToTruep (and p q r s t u v w x y) truep-and)))
;------------------------------------------------------------------------
($assert '(all p q (MyToUnassert (and p q) unassert-and)))
($assert '(all p q r (MyToUnassert (and p q r) unassert-and)))
($assert '(all p q r s (MyToUnassert (and p q r s) unassert-and)))
($assert '(all p q r s t (MyToUnassert (and p q r s t) unassert-and)))
($assert '(all p q r s t u (MyToUnassert (and p q r s t u) unassert-and)))
($assert '(all p q r s t u v (MyToUnassert (and p q r s t u v) unassert-and)))
($assert '(all p q r s t u v w
	       (MyToUnassert (and p q r s t u v w) unassert-and)))
($assert '(all p q r s t u v w x
	       (MyToUnassert (and p q r s t u v w x) unassert-and)))
($assert '(all p q r s t u v w x y
	       (MyToUnassert (and p q r s t u v w x y) unassert-and)))
;------------------------------------------------------------------------
($assert '(all p q (MyToTrueps (and p q) trueps-and)))
($assert '(all p q r (MyToTrueps (and p q r) trueps-and)))
($assert '(all p q r s t (MyToTrueps (and p q r s t) trueps-and)))
($assert '(all p q r s t u (MyToTrueps (and p q r s t u) trueps-and)))
($assert '(all p q r s t u v (MyToTrueps (and p q r s t u v) trueps-and)))
($assert '(all p q r s t u v w (MyToTrueps (and p q r s t u v w) trueps-and)))
($assert '(all p q r s t u v w x
	       (MyToTrueps (and p q r s t u v w x) trueps-and)))
($assert '(all p q r s t u v w x y
	       (MyToTrueps (and p q r s t u v w x y) trueps-and)))


(defun assert-and (p) (mapcar 'assert (cdr p)))

(defun truep-and (p) (truep-and1 (cdr p)))
(defun truep-and1 (cs)
  (cond ((null (cdr cs)) (truep (car cs)))
	(t (do ((l (trueps (car cs)) (cdr l)) (dum))  ((null l))
	       (if (setq dum (truep-and1 (plug (cdr cs) (car l))))
		   (return (alconc (car l) dum)))))))

(defun trueps-and (p) (trueps-and1 (cdr p)))
(defun trueps-and1 (cs)
  (cond ((null (cdr cs)) (trueps (car cs)))
	(t (do ((l (trueps (car cs)) (cdr l)) (dum) (nl))
	       ((null l) nl)
	       (if (setq dum (trueps-and1 (plug (cdr cs) (car l))))
		   (mapc '(lambda (m) (setq nl (cons (alpend (car l) m) nl)))
			 dum))))))

(defun alpend (al1 al2)
  (cond ((null al1) nil)
	((null (cdr al1)) al2)
	(t (do () ((null (cdr al1)) al2)
	       (setq al2 (cons (car al1) al2) al1 (cdr al1))))))

(defun alconc (al1 al2)
  (cond ((null al1) nil)
	((null (cdr al1)) al2)
	(t (do ((l al1 (cdr l)))
	       ((null (cddr l)) (rplacd l al2)))
	   al1)))

($assert '(all p q (MyToTruep (or p q) truep-or)))
($assert '(all p q (MyToTrueps (or p q) trueps-or)))
($assert '(all p q r (MyToTruep (or p q r) truep-or)))
($assert '(all p q r (MyToTrueps (or p q r) trueps-or)))
($assert '(all p q r s (MyToTruep (or p q r s) truep-or)))
($assert '(all p q r s (MyToTrueps (or p q r s) trueps-or)))

(defun truep-or (p)
  (do l (cdr p) (cdr l) (null l)
      (if (setq p (truep (car l))) (return p))))

(defun trueps-or (p)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) nl)
      (setq nl (nconc (trueps (car l)) nl))))

($assert '(all x y (MyToTruep (= x y) truep-=)))
($assert '(all x y (MyToTrueps (= x y) trueps-=)))

(defun truep-= (p)
  (progb (x y)
    (cond ((and (setq x (canref (cadr p))) (setq y (canref (caddr p)))
		(eq x y))
	   truth)
	((lookup p)))))

(defun trueps-= (p)
  (cond ((setq p (truep-= p)) (list p))
	((lookups p))))

(defun canref (x) (if (symbolp x) x (getval x)))


