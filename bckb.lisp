;;; -*- Mode: Common-LISP -*-
;;;
;;;BCKB.LSP    0.4 18-Mar-83 0109 000   1 ML E      18-Mar-83   
;;;	Functions such as bc-truep, thnot, etc.
;;;
;;;perm filename BCKB.LSP[MRS,LSP] blob sn#702112 filedate 1983-03-18 generic text, type C, neo UTF8
;;;
;;;COMMENT ⊗   VALID 00006 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00004 00003
;;;C00005 00004
;;;C00006 00005
;;;C00007 00006
;;;C00008 ENDMK
;;;C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




#|
(declare (special number0)
	 (special truth))
|#

;;; Double definition

#|
(defun bs-truep (p)
  (declare (special number0))
  (or (notes (lookupn p number0)) (bc-truep p)))
|#

;;; Double definition

#|
(defun maknot (p) (if (eq 'not (car p)) (cadr p) (list 'not p)))
|#

;;; Double definition

#|

(defun bc-truep (q)
  (declare (special number0)
	   (special cache)
	   (special justify))
  (do ((l (bclookups `(if $p ,q)) (cdr l)) (r) (x) (al) (bl))
      ((null l) nil)
      (setq r (caar l) x (getvar '$p (cdar l)) al (punset '$p (cdar l)))
      (do ((m (truepn x number0) (cdr m)))
	  ((null m))
	  (setq bl (alconc (car m) al))
	  (when cache (thstash (plug q bl) 'cache))
	  (when justify (just (datum q) 'bs-truep r (datum (plug x (car m)))))
	  (when (note bl) (setq l nil) (return t)))))
|#


(defun just (n) (thstash (cons 'just (listify n)) 'justify))

(defun groundp (x)
  (cond ((varp x) nil)
	((atom x))
	(t (mapand #'groundp x))))


(pr-stash '(totruep (prop ^= $x $y) truep-=))

(defun truep-= (p)
  (let ((x) (y))
    (cond ((and (setq x (getval (cadr p))) (setq y (getval (caddr p))))
	   (unifyp x y)))))


(pr-stash '(totruep (prop ^not $p) truep-not))

(defun truep-not (p)
  (declare (special number0))
  (cond ((eq 'not (caadr p)) (notes (truepn (cadadr p) number0)))
	((eq 'or (caadr p))
	 (setq p (cons 'and (mapcar #'(lambda (l) (list 'not l)) (cdadr p))))
	 (notes (truepn p number0)))
	((eq 'and (caadr p))
	 (setq p (cons 'or (mapcar #'(lambda (l) (list 'not l)) (cdadr p))))
	 (notes (truepn p number0)))
	(t (bs-truep p))))

(defun thnot (p)
  (declare (special truth))
  (ifn (truep (cadr p)) truth))


(pr-stash '(toassert (prop ^and . $p) assert-and))
(pr-stash '(tounassert (prop ^and . $p) unassert-and))
(pr-stash '(totruep (prop ^and . $p) truep-and))

(defun assert-and (p) (mapcar #'mrs-assert (cdr p)))

(defun unassert-and (p) (mapcar #'unassert (cdr p)))

(defun truep-and (p)
  (declare (special truth))
  (truep-and1 (cdr p) truth))

(defun truep-and1 (cs al)
  (declare (special number0))
  (cond ((null cs) (note al))
	((null (cdr cs))
	 (do ((l (truepn (plug (car cs) al) number0) (cdr l)))
	     ((null l))
	   (note (alconc (car l) al))))
	(t (do ((l (trueps (plug (car cs) al)) (cdr l)))
	       ((null l))
	     (if (truep-and1 (cdr cs) (alconc (car l) al)) (return t))))))


(pr-stash '(totruep (prop ^or . $p) truep-or))

(defun truep-or (p)
  (declare (special number0))
  (do ((l (cdr p) (cdr l)))
      ((null l))
    (if (notes (truepn (car l) number0)) (return t))))

