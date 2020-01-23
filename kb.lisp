;;; -*- Mode: Common-LISP -*-
;;;
;;;KB.LSP    0.5 18-Mar-83 0135 000   1 ML E      18-Mar-83   
;;;	Defines assert, etc in terms of KB and QB *** this might be new
;;;             and not yet integrated****
;;;
;;;perm filename KB.LSP[MRS,LSP] blob sn#702126 filedate 1983-03-18
;;;	generic text, type C, neo UTF8
;;;
;;;COMMENT ⊗   VALID 00005 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00004 00003
;;;C00007 00004
;;;C00009 00005
;;;C00010 ENDMK
;;;C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; AJY 2015-05-10
;;;
;;; ------------------------------------------------------------
;;;






;;; (declare (special goals possibilities number theory))

(setq goals nil)

(defun snoc (l x) (append l (list x)))

(defun achieve (p) (kb 'toachieve p))

(defun perceive (p) (qb 'toperceive p))

(defun perceives (p) (qbs 'toperceive p))

(defun stash (p) (kb 'tostash p))

(defun thstash (p theory) (stash p))

(defun unstash (p) (kb 'tounstash p))

(defun thunstash (p theory) (unstash p))

(defun lookup (p) (qb 'tolookup p))

(defun lookups (p) (qbs 'tolookup p))

(defun lookupn (p n) (qbn 'tolookup p n))

(defun lookupval (p) (qb 'tolookupval p))

(defun lookupvals (p) (qbs 'tolookupval p))

(defun lookupval-lookup (x) (getvar '$ (lookup (snoc x '$))))


(defun mrs-assert (p) (kb 'toassert p))

(defun thassert (p theory) (mrs-assert p))

(defun unassert (p) (kb 'tounassert p))

(defun thunassert (p theory) (unassert p))

(defun truep (p) (qb 'totruep p))

(defun trueps (p) (qbs 'totruep p))

(defun truepn (p n) (qbn 'totruep p n))

(defun residue (p) (qb 'toresidue p))

(defun residues (p) (qbs 'toresidue p))

(defun getbdg (v p) (getvar v (truep p)))

(defun getbdgs (v p) (mapcar #'(lambda (l) (getvar v l)) (trueps p)))

(defun getval (x) (qb 'togetval x))

(defun getvals (x) (qbs 'togetval x))

(defun getval-truep (x)
  (if (atom x) x
      (do ((l (truepn (snoc x '$) number) (cdr l)))
	  ((null l))
	(if (note (getvar '$ (car l))) (return t)))))


(defun kb (&rest argsl) ; This is WRONG must correct
  (do* ((i (length argsl) (1- i))
        (g '($))
        (l)
        (arguments argsl (cdr arguments))
        (arg-i (car arguments) (car arguments)))
       ((null arguments))
    (if (= 1 (length argsl))
        (if (setq g (getvar '$ (meta-truep (cons (car arg-i) g)))) (apply g arg-i))
        (setq g (cons (datum arg-i g) l (cons arg-i l))))))

(defun qb (g x) (fb (getvar '$ (meta-truep (list g (datum x) '$))) x))

(defun qbs (g x) (fbs (getvar '$ (meta-truep (list g (datum x) '$))) x))

(defun qbn (g x n) (fbn (getvar '$ (meta-truep (list g (datum x) '$))) x n))

(defun fb (f x) (car (fbn f x t)))

(defun fbs (f x) (fbn f x nil))

(defun fbn (f x number)
  (let (possibilities)
       (setq x (funcall f x))
       (cond (possibilities (nreverse possibilities))
	     (x (list x)))))

(defun memsamep (x l)
  (do () ((null l)) (if (samep x (car l)) (return l) (setq l (cdr l)))))

(defun note (x)
  (setq possibilities (cons x possibilities))
  number)

(defun notes (l)
  (and (setq possibilities (nreconc l possibilities)) number))


(pr-stash '(tostash $x pr-stash))
(pr-stash '(tounstash $x pr-unstash))
(pr-stash '(tolookup $x pr-lookup))
(pr-stash '(tolookupval $x lookupval-lookup))

(pr-stash '(toassert $x stash))
(pr-stash '(tounassert $x unstash))
(pr-stash '(totruep $x bs-truep))
(pr-stash '(toresidue $x backresidue))
(pr-stash '(togetval $x getval-truep))

