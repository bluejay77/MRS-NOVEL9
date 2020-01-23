
perm filename BCKB.LSP[MRS,LSP] blob sn#702112 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00006 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00004 00003
C00005 00004
C00006 00005
C00007 00006
C00008 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp (load '|macros.fasl|)
	   #+franz (load 'macros)
	   (*lexpr just)
	   (impvar cache justify number truth)
	   (expfun bs-truep truep-not thnot truep-and truep-or))

(defun bs-truep (p) (or (notes (lookupn p number)) (bc-truep p)))

(defun maknot (p) (if (eq 'not (car p)) (cadr p) (list 'not p)))

(defun bc-truep (q)
  (do ((l (bclookups `(if $p ,q)) (cdr l)) (r) (x) (al) (bl))
      ((null l) nil)
      (setq r (caar l) x (getvar '$p (cdar l)) al (punset '$p (cdar l)))
      (do m (truepn x number) (cdr m) (null m)
	  (setq bl (alconc (car m) al))
	  (when cache (thstash (plug q bl) 'cache))
	  (when justify (just (datum q) 'bs-truep r (datum (plug x (car m)))))
	  (when (note bl) (setq l nil) (return t)))))

(defun just n (thstash (cons 'just (listify n)) 'justify))

(defun groundp (x)
  (cond ((varp x) nil)
	((atom x))
	(t (mapand 'groundp x))))


(pr-stash '(totruep (prop ↑= $x $y) truep-=))

(defun truep-= (p)
  (local (x y)
    (cond ((and (setq x (getval (cadr p))) (setq y (getval (caddr p))))
	   (unifyp x y)))))


(pr-stash '(totruep (prop ↑not $p) truep-not))

(defun truep-not (p)
  (cond ((eq 'not (caadr p)) (notes (truepn (cadadr p) number)))
	((eq 'or (caadr p))
	 (setq p (cons 'and (mapcar '(lambda (l) (list 'not l)) (cdadr p))))
	 (notes (truepn p number)))
	((eq 'and (caadr p))
	 (setq p (cons 'or (mapcar '(lambda (l) (list 'not l)) (cdadr p))))
	 (notes (truepn p number)))
	(t (bs-truep p))))

(defun thnot (p) (ifn (truep (cadr p)) truth))


(pr-stash '(toassert (prop ↑and . $p) assert-and))
(pr-stash '(tounassert (prop ↑and . $p) unassert-and))
(pr-stash '(totruep (prop ↑and . $p) truep-and))

(defun assert-and (p) (mapcar 'assert (cdr p)))

(defun unassert-and (p) (mapcar 'unassert (cdr p)))

(defun truep-and (p) (truep-and1 (cdr p) truth))

(defun truep-and1 (cs al)
  (cond ((null cs) (note al))
	((null (cdr cs))
	 (do l (truepn (plug (car cs) al) number) (cdr l) (null l)
	     (note (alconc (car l) al))))
	(t (do l (trueps (plug (car cs) al)) (cdr l) (null l)
	       (if (truep-and1 (cdr cs) (alconc (car l) al)) (return t))))))


(pr-stash '(totruep (prop ↑or . $p) truep-or))

(defun truep-or (p)
  (do l (cdr p) (cdr l) (null l)
      (if (notes (truepn (car l) number)) (return t))))

