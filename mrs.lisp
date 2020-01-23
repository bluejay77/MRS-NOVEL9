;;; -*- Mode: Common-LISP -*-
;;;
;;; the mrs.lisp file
;;;
;;; The main functions of the MRS system
;;;
;;; ------------------------------------------------------------
;;;
;;; AJY 2015-03-16 
;;;
;;; ------------------------------------------------------------
;;;
;;; perm filename MRS.LSP[MRS,LSP] blob sn#616163 filedate 1981-10-03
;;; generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00007 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002	               -*-Mode:LISP Package:MACSYMA -*-
;;;
;;;C00004 00003
;;;C00007 00004
;;;C00010 00005
;;;C00014 00006
;;;C00016 00007
;;;C00017 ENDMK
;;;C⊗;
;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; Converted this into the Common LISP.  AJY 2015-05-09
;;;
;;; ------------------------------------------------------------
;;;



(defvar goals nil)

(defvar cache nil)

(defparameter truth '((t . t)))

;;; Discovered a duplicate definition.  AJY 2015-05-09
;;;

;;; (defun varp macro (x) `(getl ,(cadr x) '(un ex)))

;;; (defmacro varp (x) `(or (get (cadr ,x) 'un)
;;;			(get (cadr ,x) 'ex)))

;;; (defun unvarp macro (x) `(get ,(cadr x) 'un))

;;; (defmacro unvarp (x) `(get (cadr ,x) 'un))
;;;

;;; (defun exvarp macro (x) `(get ,(cadr x) 'ex))

;;; (defmacro exvarp (x) `(get (cadr ,x) 'ex))

(defun subvar (x al) (cdr (assq x al)))

;;; (defun pset macro (x) `(cons (cons ,(cadr x) ,(caddr x)) ,(cadddr x)))

(defmacro pset (&rest x) `(cons
			   (cons '(car ,x)
				 '(cadr ,x))
			   '(caddr ,x)))

(defun punset (x al)
  (if (equal x (caar al))
      (cdr al)
      (catch '*exit-punset*
	(do ((l al (cdr l)))
	    ((null (cdr al)))
	  (cond ((equal x (caadr l))
		 (rplacd l (cddr l))
		 (throw '*exit-punset* al)))))))

#| AJY

(defun snoc (l x) (append l (list x)))

|#




#| AJY

(defun mrs-assert (p) (kb 'MyToAssert p))

(defun unassert (p) (kb 'MyToUnassert p))

(defun truep (p) (kb 'MyToTruep p))

(defun trueps (p) (kb 'MyToTrueps p))

|#

#| AJY

(defun getbdg (x) (cdar (truep x)))

(defun getbdgs (x) (mapcar #'cdar (trueps x)))

(defun getval (x) (kb 'MyToGetval x))

(defun getvals (x) (kb 'MyToGetvals x))

|#

(defun truep-getval (x) (subvar '$ (truep (snoc x '$))))

(defun trueps-getvals (x)
  (mapcar #'(lambda (l) (subvar '$ l)) (trueps (snoc x '$))))

#|

(defun stash (p) (kb 'MyToStash p))


(defun unstash (p) (kb 'MyToUnstash p))

(defun lookup (p) (kb 'MyToLookup p))

(defun lookups (p) (kb 'MyToLookups p))

(defun lookups-lookup (p) (if (setq p (lookup p)) (list p)))

(defun lookupval (p) (kb 'MyToLookupval p))

(defun lookupvals (p) (kb 'MyToLookupvals p))

(defun lookup-lookupval (x) (subvar '$ (lookup (snoc x '$))))

(defun lookups-lookupvals (x)
  (mapcar #'(lambda (l) (subvar '$ l)) (lookups (snoc x '$))))

|#

#| AJY

(defun kb (g x) (setq g `(,g ,x $))
  (if (memsamep g goals) nil
      (let ((goals (cons g goals)))
	(funcall (subvar '$ (or (bc-truep g) (ex-lookup g))) x))))

|#

#| AJY

(defun memsamep (x l)
  (do () ((null l)) (if (somber x (car l)) (return l) (setq l (cdr l)))))

|#

(defun somber (x y) (somber1 x y truth))

(defun somber1 (x y al)
  (cond ((and (varp x) (varp y))
	 (let ((dum))
	   (if (setq dum (assq x al))
	       (if (eq y (cdr dum))
		   al)
	       (cons (cons x y) al))))
	((atom x) (if (eq x y) al))
	((atom y) nil)
	((mapand2 '(lambda (l m) (setq al (somber1 l m al))) x y) al)))




(setf (get '$ 'un) t)
(setf (get '$xx 'un) t)
(setf (get '$yy 'un) t)
(setf (get '$pp 'un) t)
(setf (get '$ff 'un) t)

(pr-stash '(MyToAssert $xx stash))
(pr-stash '(MyToUnassert $xx unstash))
(pr-stash '(MyToTruep $xx bs-truep))
(pr-stash '(MyToTrueps $xx bs-trueps))
(pr-stash '(MyToGetval $xx truep-getval))
(pr-stash '(MyToGetvals $xx trueps-getvals))

(pr-stash '(MyToStash $xx pr-stash))
(pr-stash '(MyToUnstash $xx pr-unstash))
(pr-stash '(MyToLookup $xx ex-lookup))
(pr-stash '(MyToLookups $xx ex-lookups))
(pr-stash '(MyToLookupval $xx lookup-lookupval))
(pr-stash '(MyToLookupvals $xx lookups-lookupvals))

(pr-stash '(MyToTruep (expression $xx) tfun))
(pr-stash '(MyToLookups (if $xx $yy) ex-lookups))

(defun tfun (ignore) truth)
(defun nilfun (ignore) nil)
(defun unkfun (ignore) 'unknown)

(pr-stash '(MyToTruep (rel $pp $ff) lookup))
(pr-stash '(MyToTruep (arg $pp $xx) lookup))
(pr-stash '(MyToTruep (val $pp $yy) lookup))
(pr-stash '(MyToTrueps (rel $pp $ff) lookups))
(pr-stash '(MyToTrueps (arg $pp $xx) lookups))
(pr-stash '(MyToTrueps (val $pp $yy) lookups))
(pr-stash '(MyToLookup (rel $pp $ff) lookup-rel))
(pr-stash '(MyToLookup (arg $pp $xx) lookup-arg))
(pr-stash '(MyToLookup (val $pp $yy) lookup-val))
(pr-stash '(MyToLookups (rel $pp $ff) lookups-lookup))
(pr-stash '(MyToLookups (arg $pp $xx) lookups-lookup))
(pr-stash '(MyToLookups (val $pp $yy) lookups-lookup))
(pr-stash '(MyToLookupval (rel $pp) car))
(pr-stash '(MyToLookupval (arg $pp) cadr))
(pr-stash '(MyToLookupval (val $pp) caddr))

(defun lookup-rel (p) (matchp (caddr p) (caadr p)))
(defun lookup-arg (p) (matchp (caddr p) (cadadr p)))
(defun lookup-val (p) (matchp (caddr p) (caddadr p)))


;;; This code assumes (MyToLookup (not $p) ex-lookup)

(defun bs-truep (p)
  (cond ((lookup p))
	((ex-lookup (maknot p)) nil)
	((bc-truep p))))

;;; Putting the groundp check here is dirty.  It should be done higher up,
;;; but doing it at the meta-level is too costly.
;;; This code assumes (MyToLookup (not $p) ex-lookup)

(defun bs-trueps (p)
  (cond ((groundp p) (if (setf p (bs-truep p)) (list p)))
	((ex-lookup (maknot p)) nil)
	(t (nconc (lookups p) (bc-trueps p)))))

(defun maknot (p) (if (eq 'not (car p)) (cadr p) (list 'not p)))

;;; This is what's intended ultimately:
;;; (defun bc-truep (q) (subgoal truep (subgoal fetch p `(if p ,q))))
;;;
;;; The following definition presumes (MyToLookups (if $x $y) ex-lookups)

(defun bc-truep (q)
  (worldmark)
  (do ((l (ua-getfacts (car q)) (cdr l)) (al) (dum)) ((null l))
      (cond ((and (cntp (car l)) (setq al (matchp `(if $pp ,q) (car l)))
		  (setq dum (truep (subvar '$pp al))))
	     (setq dum (plugal (punset '$pp al) dum))
	     (if cache (stash (plug q dum)))
	     (return dum)))))

;(defun bc-truep (q)
;  (do ((l (ex-lookups `(if $pp ,q)) (cdr l)) (dum)) ((null l))
;      (cond ((setq dum (truep (subvar '$pp (car l))))
;	     (setq dum (plugal (punset '$pp (car l)) dum))
;	     (if cache (stash (plug q dum)))
;	     (return dum)))))

;;; The following definition presumes (MyToLookups (if $x $y) ex-lookups)

(defun bc-trueps (q)
  (worldmark)
  (do ((l (ua-getfacts (car q)) (cdr l)) (al) (lhs) (dum) (nl))
      ((null l) nl)
    (cond ((and (cntp (car l)) (setq al (matchp `(if $pp ,q) (car l))))
	   (setf lhs (subvar '$pp al))
	   (setf dum (punset '$pp al)))
	  (mapc #'(lambda (m) (setq nl (cons (plugal dum m) nl)))
		(trueps lhs)))))

#|

(defun groundp (x)
  (cond ((unvarp x) nil)
	((atom x))
	(t (and
	    (groundp (car x))
	    (groundp (cdr x))))))

(defun plugal (al1 al) 
  (mapcar #'(lambda (l) (cons (car l) (plug (cdr l) al))) al1))

(defun plug (x al) (if (null al) x (plug1 x al)))

(defun plug1 (x al)
  (cond ((unvarp x)
	 (let ((dum))
	   (cond ((null (setq dum (assq x al)))
		  x)
		 ((eq x (cdr dum))
		  x)
		 (t
		  (plug1 (cdr dum) al)))))
	((atom x)
	 x)
	(t
	 (cons (plug1 (car x) al) (plug1 (cdr x) al)))))

|#


(defun fs-assert (p)
  (cond ((pr-lookup p))
	(t (fa-assert (stash p)))))

(defun fc-assert (p)
  (do ((l (ex-lookups `(if ,p $q)) (cdr l)))
      ((null l))
    (mrs-assert (subvar '$q (car l)))))

(defun fa-assert (p)
  (worldmark)
  (do ((l (ua-getfacts (car (ua-pattern p))) (cdr l)) (rule) (al))
      ((null l) p)
    (cond ((not (eq 'if (car (ua-pattern (car l))))))
	  ((setq al (matchp (cadr (setq rule (unsemant (car l)))) p))
	   (mrs-assert (plug (caddr rule) al)))
	  ((eq 'and (caadr rule))
	   (do (m (cdadr rule) (cdr m))
	       (null m)
	     (if (setq al (matchp (car m) p))
		 (mapc #'(lambda (n) (mrs-assert
				     (plug (caddr rule) (alconc n al))))
		       (lookups-and1 (plug (cdadr rule) al)))))))))

(defun lookups-and1 (cs)
  (cond ((null (cdr cs)) (lookups (car cs)))
	(t (do ((l (lookups (car cs)) (cdr l)) (dum) (nl))
	       ((null l) nl)
	       (if (setq dum (lookups-and1 (plug (cdr cs) (car l))))
		   (mapc #'(lambda (m) (setq nl (cons (alpend (car l) m) nl)))
			 dum))))))


(defun ps-truep (p) (or (lookup p) (pi-truep p)))

(defun pi-truep (p)
  (do ((l (pr-lookupvals `(mem ,(cadr p))) (cdr l)) (dum)) ((null l))
      (if (setq dum (pi-truep1 p (car l))) (return dum))))

(defun pi-truep1 (p s)
  (cond ((lookup `(func ,(car p) ,s . ,(cddr p))))
	(t (do ((l (pr-lookupvals `(subclass ,s)) (cdr l)) (dum)) ((null l))
	       (if (setq dum (pi-truep1 p (car l))) (return dum))))))

