
perm filename MRSS.LSP[MRS,LSP] blob sn#616167 filedate 1981-10-03 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00005 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002	               -*-Mode:LISP Package:MACSYMA -*-                      
C00004 00003
C00006 00004
C00010 00005
C00012 ENDMK
C⊗;
;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '(macros fasl)) 
	 (special uns) (fixnum newsymbol)
	 (expfun $defunit $assert $assert* $unassert $unassert*
		 $truep $truep* $trueps $trueps*
		 $getbdg $getbdg* $getbdgs $getbdgs*
		 $getval $getval* $getvals $getvals*
		 $stash $unstash $lookup $lookups $lookupval $lookupvals
		 $facts $load))


(defun $defunit fexpr (x) (mapc '$assert (cdr x)) (car x))

(defun $assert (p) (assert (semant p)))

(defun $assert* n ($assert (listify n)))

(defun $unassert (p) (unassert (semant p)))

(defun $unassert* n ($unassert (listify n)))

(defun $truep (p) (truep (semant p)))

(defun $truep* n ($truep (listify n)))

(defun $trueps (p) (trueps (semant p)))

(defun $trueps* n ($trueps (listify n)))

(defun $getbdg (p) (getbdg (semant p)))

(defun $getbdg* n ($getbdg (listify n)))

(defun $getbdgs (p) (getbdgs (semant p)))

(defun $getbdgs* n ($getbdgs (listify n)))

(defun $getval (x) (getval (semant x)))

(defun $getval* n ($getval (listify n)))

(defun $getvals (x) (getvals (semant x)))

(defun $getvals* n ($getvals (listify n)))


(defun $stash (p) (stash (semant p)))

(defun $unstash (p) (unstash (semant p)))

(defun $lookup (p) (lookup (semant p)))

(defun $lookups (p) (lookups (semant p)))

(defun $lookupval (p) (lookupval (semant p)))

(defun $lookupvals (p) (lookupvals (semant p)))


(defun semant (pat) (semant1 pat nil nil))
(defun unsemant (x)
  (cond ((get x 'semant))
	((null (ua-pattern x)) x)
	(t (mapcar 'unsemant (ua-pattern x)))))

(defun semant1 (pat flip uns) 
  (cond ((symbolp pat)
	 (cond ((eq 36. (getcharn pat 1)) (put pat t 'un) pat)
	       ((eq 63. (getcharn pat 1)) (put pat t 'ex) pat)
	       ((get pat 'var))
	       (t pat)))
	((numberp pat) (semnum pat))
	((eq 'not (car pat))
	 (list 'not (semant1 (cadr pat) (not flip) uns)))
	((eq 'if (car pat)) (semif pat flip uns))
	((eq 'iff (car pat))
	 (list 'and (list 'if (semant1 (cadr pat) (not flip) uns)
			      (semant1 (caddr pat) flip uns))
	            (list 'if (semant1 (caddr pat) (not flip) uns)
			      (semant1 (cadr pat) flip uns))))
	((eq 'all (car pat))
	 (ifn flip (semall pat flip uns) (semexist pat flip uns)))
	((eq 'exist (car pat))
	 (ifn flip (semexist pat flip uns) (semall pat flip uns)))
	(t (mapcar '(lambda (l) (semant1 l flip uns)) pat))))

(defun semnum (p)
  (progb (s)
    (setq s (implode (exploden p)))
    (put s p 'semant)
    s))

(defun semif (p flip uns)
  (if (not (eq 'and (caaddr p)))
      (list 'if (semant1 (cadr p) (not flip) uns) (semant1 (caddr p) flip uns))
      (do ((l (cdaddr p) (cdr l)) (d (semant1 (cadr p) (not flip) uns)) (nl))
	  ((null l) (cons 'and (nreverse nl)))
	  (setq nl (cons (list 'if d (semant1 (car l) flip uns)) nl)))))

(defun semall (p flip uns)
  (prog2 (do l (cdr p) (cdr l) (null (cdr l))
	     (setq uns (cons (unvar (car l)) uns))
	     (put (car l) (car uns) 'var))
	 (semant1 (car (last p)) flip uns)
	 (do l (cdr p) (cdr l) (null (cdr l)) (rem (car l) 'var))))

(defun semexist (p flip uns)
  (prog2 (do l (cdr p) (cdr l) (null (cdr l))
	     (put (car l) (exvar (car l) uns) 'var))
	 (semant1 (car (last p)) flip uns)
	 (do l (cdr p) (cdr l) (null (cdr l)) (rem (car l) 'var))))

(defun unvar (x)
  (if (not (eq '$ (getchar x 1))) (setq x (implode (cons '$ (exploden x)))))
  (put x t 'un)
  x)

(defun exvar (x l)
  (if (not (eq '? (getchar x 1))) (setq x (implode (cons '? (exploden x)))))
  (if l (put x l 'uns))
  (put x t 'ex)
  x)


(defun $load (f)
  (setq f (open f 'in))
  (do a (read f nil) (read f nil) (null a) ($assert a))
  (close f)
  'done)

(defun $facts fexpr (x) (prfacts (semant (car x))))

(defun prfacts (x)
  (do l (pr-getfacts x) (cdr l) (null l)
      (terpri) (princ (unsemant (car l)))))

(defun $apropos (s)
  (progb (nl)
    (mapatoms '(lambda (l) (if (substringp s l) (setq nl (cons l nl)))))
    nl))

(defun substringp (r s)
  (progb (i j)
    (setq i (flatc r) j (flatc s))
    (do k (1+ (- j i)) (1- k) (<= k 0)
	(if (do ((m 1 (1+ m)) (n k (1+ n)))
		((> m i) t)
	      (ifn (= (getcharn r m) (getcharn s n)) (return nil)))
	    (return t)))))

