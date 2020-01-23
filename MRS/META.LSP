
perm filename META.LSP[MRS,LSP] blob sn#702128 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00007 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00005 00003
C00007 00004
C00008 00005
C00009 00006
C00013 00007
C00015 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp (load '|macros.fas|)
	   #+franz (load 'macros)
	   (impvar truth))

(pr-stash '(tolookup (prop ↑objconst $x) tl-lookup))
(pr-stash '(tolookup (prop ↑funconst $x) tl-lookup))
(pr-stash '(tolookup (prop ↑relconst $x) tl-lookup))
(pr-stash '(tolookup (prop ↑variable $x) tl-lookup))


(pr-stash '(tolookup (prop ↑rel $p $f) perceive-rel))
(pr-stash '(tolookup (prop ↑arg $p $x) perceive-arg))
(pr-stash '(tolookup (prop ↑val $p $y) perceive-val))
(pr-stash '(tolookup (prop ↑argn $n $p $y) perceive-argn))
(pr-stash '(toperceive (prop ↑rel $p $f) perceive-rel))
(pr-stash '(toperceive (prop ↑arg $p $x) perceive-arg))
(pr-stash '(toperceive (prop ↑val $p $y) perceive-val))
(pr-stash '(toperceive (prop ↑argn $n $p $y) perceive-argn))

(defun perceive-rel (p) (perargn 0 (cadr p) (caddr p)))
(defun perceive-arg (p) (perargn 1 (cadr p) (caddr p)))
(defun perceive-val (p) (perargn 2 (cadr p) (caddr p)))
(defun perceive-argn (p) (perargn (cadr p) (caddr p) (cadddr p)))

(defun perargn (n x z)
  (cond ((unvarp x) nil)
	((and (atom x) (setq x (pattern x)))
	 (cond ((numberp n) (unifyp (nth n x) z))
	       ((unvarp n)
		(do ((l x (cdr l)) (i 0 (1+ i))) ((null l))
		    (if (and (setq x (unifyp (car l) z)) (note (pset n i x)))
			(return t))))))
	((eq 'prop (car x))
	 (cond ((numberp n) (unifyp (nth n (cdr x)) z))
	       ((unvarp n)
		(do ((l (cdr x) (cdr l)) (i 0 (1+ i))) ((null l))
		    (if (and (setq x (unifyp (car l) z)) (note (pset n i x)))
			(return t))))))))


(pr-stash '(tolookup (prop ↑intheory $p $t) perceive-intheory))
(pr-stash '(toperceive (prop ↑intheory $p $t) perceive-intheory))

(defun perceive-intheory (p) (perintheory (cadr p) (caddr p)))

(defun perintheory (p h)
  (cond ((unvarp p)
	 (cond ((unvarp h) nil)
	       ((atom h) (do l (contents h) (cdr l) (null l)
			     (if (note (pset p (car l) truth)) (return t))))))
	((atom p)
	 (cond ((unvarp h) (do l (get p 'theory) (cdr l) (null l)
			       (if (note (pset h (car l) truth)) (return t))))
	       ((atom h) (if (memq h (get p 'theory)) truth))))))


(pr-stash '(tostash (prop ↑indb $p) achieve-indb))
(pr-stash '(toachieve (prop ↑indb $p) achieve-indb))
(pr-stash '(tolookup (prop ↑indb $p) perceive-indb))
(pr-stash '(toperceive (prop ↑indb $p) perceive-indb))

(defun achieve-indb (p)
  (if (and (symbolp (cadr p)) (pattern (cadr p))) (stash (cadr p))))

(defun perceive-indb (p)
  (if (and (symbolp (cadr p)) (pattern (cadr p))) (lookup (cadr p))))


(pr-stash '(tostash (prop ↑includes $c $d) achieve-includes))
(pr-stash '(tounstash (prop ↑includes $c $d) unstash-includes))
(pr-stash '(toachieve (prop ↑includes $c $d) achieve-includes))
(pr-stash '(tolookup (prop ↑includes $c $d) dl-lookup))
(pr-stash '(toperceive (prop ↑includes $c $d) dl-lookup))
(pr-stash '(tolookupval (prop ↑includes $t) dl-lookupval))

(defun achieve-includes (p) (includes (cadr p) (caddr p)))
(defun unstash-includes (p) (unincludes (cadr p) (caddr p)))


(pr-stash '(tolookup (prop ↑number $x) perceive-symbol))
(pr-stash '(tolookup (prop ↑number $x) perceive-symbol))

(defun perceive-number (p) (pernumber (cadr p)))

(defun pernumber (x)
  (cond ((unvarp x) nil)
	((atom x) (if (and (setq x (pattern x)) (numberp x)) truth))))



(pr-stash '(tolookup (prop ↑symbol $x) perceive-symbol))
(pr-stash '(toperceive (prop ↑symbol $x) perceive-symbol))

(defun perceive-symbol (p) (persymbol (cadr p)))

(defun persymbol (x)
  (cond ((unvarp x) (mapatoms '(lambda (z) (note (pset x z truth)))))
	((atom x) (if (and (setq x (pattern x)) (symbolp x)) truth))))


(pr-stash '(tostash (prop ↑value $x $y) achieve-value))
(pr-stash '(toachieve (prop ↑value $x $y) achieve-value))
(pr-stash '(tolookup (prop ↑value $x $y) perceive-value))
(pr-stash '(toperceive (prop ↑value $x $y) perceive-value))

(defun achieve-value (p)
  (if (and (groundp p) (symbolp (cadr p))) (set (cadr p) (caddr p))))

(defun perceive-value (p) (pervalue (cadr p) (caddr p)))

(defun pervalue (v x)
  (cond ((unvarp v)
	 (local (dum)
	   (mapatoms '(lambda (l)
			(if (and (boundp l) (setq dum (unifyp (symeval l) x)))
			    (note (pset v l dum)))))))
	((and (atom v) (boundp v)) (unifyp (symeval v) x)))) 


(pr-stash '(tostash (prop ↑property $x $y $z) achieve-property))
(pr-stash '(toachieve (prop ↑property $x $y $z) achieve-property))
(pr-stash '(tolookup (prop ↑property $x $y $z) perceive-property))
(pr-stash '(toperceive (prop ↑property $x $y $z) perceive-property))

(defun achieve-property (p)
  (if (groundp p) (putprop (cadr p) (cadddr p) (caddr p))))

(defun perceive-property (p) (perproperty (cadr p) (caddr p) (cadddr p)))

(defun perproperty (f x y)
  (cond ((unvarp x)
	 (local (dum)
	   (cond ((unvarp f)
		  (mapatoms '(lambda (z)
			       (do l (plist z) (cddr l) (null l)
				   (if (setq dum (unifyp (cadr l) y))
				       (note (pset x z
						   (pset f (car l) dum))))))))
		 ((atom f)
		  (mapatoms '(lambda (z)
			       (if (and (get z f)
					(setq dum (unifyp (get z f) y)))
				   (note (pset x z dum)))))))))
	((atom x)
	 (cond ((unvarp f) (do l (plist x) (cddr l) (null l)
			       (if (and (setq x (unifyp (cadr l) y))
					(note (pset f (car l) x)))
				   (return t))))
	       ((atom f) (unifyp (get x f) y))))))


(pr-stash '(tolookup (prop ↑islist $x) perceive-islist))
(pr-stash '(toperceive (prop ↑islist $x) perceive-islist))

(defun perceive-islist (p) (perislist (cadr p)))

(defun perislist (x)
  (cond ((unvarp x) nil)
	((atom x) (if (and (setq x (pattern x)) (eq 'list (typep x))) truth))))


(pr-stash '(tolookup (prop ↑length  $x $y) perceive-length ))
(pr-stash '(toperceive (prop ↑length  $x $y) perceive-length))

(defun perceive-length  (p) (perlength  (cadr p) (caddr p)))

(defun perlength  (x y)
  (cond ((unvarp x) nil)
	((and (atom x) (setq x (pattern x)))
	 (cond ((unvarp y) (matchp y (length x)))
	       ((and (atom y) (setq y (pattern y)))
		(if (equal y (length x)) truth))))))


(pr-stash '(tolookup (prop ↑member $x $y) perceive-member))
(pr-stash '(toperceive (prop ↑member $x $y) perceive-member))

(defun perceive-member (p) (permember (cadr p) (caddr p)))

(defun permember (x y)
  (cond ((unvarp x)
	 (cond ((unvarp y) nil)
	       ((atom y)
		(do l (pattern y) (cdr l) (null l)
		    (note (pset x (car l) truth))))))
	((and (atom x) (setq x (pattern x)) (atom y) (setq y (pattern y)))
	 (if (member x y) truth))))

