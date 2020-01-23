
perm filename PROPRE.LSP[MRS,LSP] blob sn#702133 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00006 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00004 00003
C00006 00004
C00009 00005
C00014 00006
C00017 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp(load '|macros.fas|)
	   #+franz(load 'macros)
	   (special current best score)
	   (expvar theory activetheories truth)
	   (expfun pr-stash pr-unstash pr-indbp pr-lookup pr-getfacts
		   activate deactivate includes unincludes contents empty
		   pattern datum varp unvarp exvarp))

(setq theory 'global current 'global activetheories nil truth '((t . t)))

(defprop global 100000 cmark)
(defprop cache 1 cmark)
(defprop justify 1 cmark)

(array data t 1000.)


(defun pr-stash (p) (cntxt (pr-index p) theory))

(defun pr-unstash (p)
  (cond ((not (setq p (pr-datump p))) nil)
	((kcntxt p theory) p)
	(t (pr-unindex p) (undatum p))))

(defun pr-indbp (p)
  (theorymark)
  (if (cntp (pr-datump p)) truth))

(defun pr-lookup (p)
  (theorymark)
  (do ((l (pr-indexp p) (cdr l)) (dum) (nl))
      ((null l) (nreverse nl))
      (if (and (cntp (car l)) (setq dum (matchp p (pattern (car l))))
	       (note dum))
	  (return t))))

(defun pr-getfacts (n)
  (theorymark)
  (do ((l (plistp n) (cddr l)) (nl))
      ((null l) nl)
      (if (numberp (car l))
	  (do m (cdadr l) (cdr m) (or (null m) (numberp (car m)))
	      (if (cntp (car m)) (setq nl (addq (car m) nl)))))))


(defun pattern (n) (and (symbolp n) (get n 'pattern)))

(defun datump (p) (cdr (assoc p (aref data (remainder (abs (sxhash p)) 1000.)))))

(defun datum (p)
  (local (n b d)
    (setq n (remainder (abs (sxhash p)) 1000.) b (aref data n))
    (cond ((setq d (cdr (assoc p b))))
	  (t (setq d (if (atom p) (implode (cons '↑ (exploden p)))
			          (maksym 'p)))
	     (put d p 'pattern)
	     (cdar (aset (cons (cons p d) b) data n))))))

(defun undatum (d)
  (local (n b p)
    (setq p (pattern d) n (remainder (abs (sxhash p)) 1000.) b (aref data n))
    (when (setq p (assoc p b))
	  (aset (delq p b 1) data n)
	  (remprop d 'pattern)
	  d)))

(defun varp (x) (and (symbolp x) (get x 'variable)))
(defun unvarp (x) (and (symbolp x) (eq 'un (get x 'variable))))
(defun exvarp (x) (and (symbolp x) (eq 'ex (get x 'variable))))

(defprop $  un variable)
(defprop $a un variable)
(defprop $b un variable)
(defprop $c un variable)
(defprop $d un variable)
(defprop $e un variable)
(defprop $f un variable)
(defprop $g un variable)
(defprop $h un variable)
(defprop $i un variable)
(defprop $j un variable)
(defprop $k un variable)
(defprop $l un variable)
(defprop $m un variable)
(defprop $n un variable)
(defprop $o un variable)
(defprop $p un variable)
(defprop $q un variable)
(defprop $r un variable)
(defprop $s un variable)
(defprop $t un variable)
(defprop $u un variable)
(defprop $v un variable)
(defprop $w un variable)
(defprop $x un variable)
(defprop $y un variable)
(defprop $z un variable)


(defun pr-index (p)
  (cond ((pr-datump p))
	(t (pr-index1 p (setq p (datum p)) 1 nil) p)))

(defun pr-index1 (p d n vl)
  (setq vl (prchk 'var n vl))
  (cond ((varp p) (pradd d vl))
	((atom p)
	 (if (and (symbolp p) (= 94. (getcharn p 1)) (not (pattern p)))
	     (datum (implode (cdr (exploden p)))))
	 (pradd d (prchk p n vl)))
	((commutativep (car p))
	 (setq n (lsh n 1))
	 (do ((l p (cdr l)))
	     ((atom l) (pr-index1 l d n vl))
	     (pr-index1 (car l) d n vl)))
	(t (do ((l p (cdr l)) (m (lsh n 1) (lsh (1+ m) 1)))
	       ((atom l) (pr-index1 l d n vl))
	       (pr-index1 (car l) d m vl)))))

(defun prchk (p n vl) (or (prget p n) (prput p (cons 0 vl) n)))

(defun pradd (d vl)
  (rplaca vl (1+ (car vl)))
  (rplacd vl (cons d (cdr vl))))


(defun pr-unindex (d) (pr-unindex1 (pattern d) d 1) d)

(defun pr-unindex1 (p d n)
  (cond ((varp p) (prdel d (prget 'var n)))
	((atom p) (local (dum) (setq dum (prget p n))
			 (if (= 1 (car dum)) (prrem p n) (prdel d dum))))
	((commutativep (car p))
	 (setq n (lsh n 1))
	 (do ((l p (cdr l)))
	     ((atom l) (pr-unindex1 l d n))
	     (pr-unindex1 (car l) d n)))
	(t (do ((l p (cdr l)) (m (lsh n 1) (lsh (1+ m) 1)))
	       ((atom l) (pr-unindex1 l d n))
	       (pr-unindex1 (car l) d m)))))

(defun prdel (d vl)
  (rplaca vl (1- (car vl)))
  (delq d vl 1))


(defun pr-datump (p)
  (do l (pr-indexp p) (cdr l) (or (null l) (numberp (car l)))
      (if (samep p (pattern (car l))) (return (car l)))))

(defun pr-indexp (p) 
  (let (best (score 999999999)) (pr-indexp1 p 1 0 nil) (cdr best)))

(defun pr-indexp1 (p n s vl)
  (local (dum)
    (cond ((null (setq dum (prget 'var n))) (setq score s best vl))
	  ((>= (setq s (+ s (car dum))) score))
	  ((atom p)
	   (cond ((or (varp p) (pattern p)))
		 ((null (setq vl (prget p n))) (setq score s best dum))
		 ((< (setq s (+ s (car vl))) score) (setq score s best vl))))
	  ((commutativep (car p))
	   (setq n (lsh n 1))
	   (do l p (cdr l) (atom l) (pr-indexp1 (car l) n s dum)))
	  (t (do l p (cdr l) (atom l)
		 (pr-indexp1 (car l) (setq n (lsh n 1)) s dum)
		 (setq n (1+ n)))))))

(defun prget (x n)
  (do l (plistp x) (cddr l) (null l)
      (if (equal n (car l)) (return (cadr l)))))

(defun prrem (x n)
  (local ((dum (plistp x)))
    (if (equal n (car dum)) (setplistp x (cddr dum))
	(do l (cdr dum) (cddr l) (null (cdr l))
	    (if (equal n (cadr l)) (return (rplacd l (cdddr l))))))))

(defun prput (x v n)
  (do ((l (plistp x) (cddr l)))
      ((null l) (setplistp x (list* n v (plistp x))))
      (if (equal n (car l)) (return (rplaca (cdr l) v))))
  v)

(defun commutativep (x) (memq x '(and or set + *)))



(defun activate n 
  (do i 1 (1+ i) (> i n)
      (if (memq (arg i) activetheories) nil
	  (setq activetheories (cons (arg i) activetheories))
	  (cmark (arg i))))
  'done)

(defun deactivate n 
  (do i 1 (1+ i) (> i n)
      (if (not (memq (arg i) activetheories)) nil
	  (cunmrk (arg i))
	  (setq activetheories (delq (arg i) activetheories))))
  'done)

(defun includes (t1 t2)
  (cunmrk current)
  (putp t1 (addq t2 (getp t1 'includes)) 'includes)
  (cmark current)
  'done)

(defun unincludes (t1 t2)
  (cunmrk current)
  (putp t1 (delq t2 (getp t1 'includes)) 'includes)
  (cmark current)
  'done)

(defun contents (c) (getp c 'contents))

(defun empty (con)
  (do l (getp con 'contents) (cdr l) (null l)
      (when (not (kcntxt (car l) con))
	    (pr-unindex (car l)) (undatum (car l))))
  'done)

(defun cntxt (dat con)
  (if (memq con (get dat 'theory)) dat
      (putp con (cons dat (getp con 'contents)) 'contents)
      (put dat (cons con (get dat 'theory)) 'theory)
      dat))

(defun kcntxt (dat con)
  (putp con (delq dat (getp con 'contents)) 'contents)
  (putc dat (delq con (get dat 'theory)) 'theory))

(defun putc (x y z)
  (cond ((null y) (remprop x z) nil)
	(t (put x y z))))

(defun cntp (x)
  (do l (get x 'theory) (cdr l) (null l)
      (if (and (setq x (getp (car l) 'cmark)) (> x 0)) (return t))))

(defun theorymark ()
  (when (not (eq current theory))
	(cunmrk current) (setq current theory) (cmark theory)))

(defun cmark (con)
  (local ((cm (getp con 'cmark)))
    (putp con (if cm (1+ cm) 1) 'cmark)
    (mapc 'cmark (getp con 'includes))))

(defun cunmrk (con)
  (local ((cm (getp con 'cmark)))
    (if cm (putp con (1- cm) 'cmark))
    (mapc 'cunmrk (getp con 'includes))))

