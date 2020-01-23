;;;PROPRE.LSP    0.9 18-Mar-83 0142 000   1 ML E      18-Mar-83   
;;;	Activate, Include, and pr- functions
;;;
;;;perm filename PROPRE.LSP[MRS,LSP] blob sn#702133 filedate
;;;	1983-03-18 generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00006 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00004 00003
;;;C00006 00004
;;;C00009 00005
;;;C00014 00006
;;;C00017 ENDMK
;;;C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modified this file for Common LISP.
;;;
;;; ------------------------------------------------------------
;;;
;;; AJY 2015-03-18
;;;
;;; ------------------------------------------------------------



(defun setplist (x l)
  (setf (symbol-plist x) l))


(defvar current nil)
(defvar best nil)
(defvar score nil)

(defvar theory nil)
(defvar activetheories nil)
(defvar truth nil)

(setf theory 'global)
(setf current 'global)
(setf activetheories nil)
(setf truth '((t . t)))

(defvar global nil)
(defvar cache nil)
(defvar justify nil)

(setf (get global 'cmark) 100000)
(setf (get cache 'cmark) 1)
(setf (get justify 'cmark) 1)

(defvar data (make-array 1000.))

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
  (do ((l (pr-indexp p) (cdr l)) ; Var init next
       (dum) ; Var init NIL
       (nl)) ; Var init NIL
      ((null l) (nreverse nl)) ; Term test
      (if (and (cntp (car l)) (setf dum (matchp p (pattern (car l))))
	       (note dum))
	  (return t))))

(defun pr-getfacts (n)
  (theorymark)
  (do ((l (plistp n) (cddr l)) ; Var init next
       (nl)) ; Var whose init is NIL
      ((null l) nl)
      (if (numberp (car l))
	  (do ((m (cdadr l) (cdr m))) ; Var init next
	      ((or (null m) (numberp (car m)))) ; Term test
	    (if (cntp (car m))
		(setf nl (addq (car m) nl))))))) ; Ret val


(defun pattern (n) (and (symbolp n) (get n 'pattern)))

(defun datump (p)
  (cdr (assoc p (aref data (remainder (abs (sxhash p)) 1000.)))))

;;; Define REMAINDER from the Common LISP REM

(defun remainder (x y)
  (rem x y))


(defun datum (p)
  (declare (special data)) ; External array
  (let ((n nil) (b nil) (d nil))
    ;; n is the hashed address in the hash table
    (setf n (remainder (abs (sxhash p)) 1000.))
    ;; b is the data there, if there is some data	  
    (setf b (aref data n))
    (cond ((equal b 0) ; No data there?
	   (cdar (setf (aref data n) ; place the data in the array
		       (cons (cons p d) b))))
	  ((atom p)
	   (setf d (if (atom p) ; Is the datum an atom?
		       (progn
			 (setf p (implode (cons '^ (explode p)))) ; EXPLODE(N)
			 (maksym 'p))))
	   (setf (get d 'pattern) p)
	   (cdar
	    (setf (aref data n) ; place the data in the array
		  (cons (cons p d) b))))
	  (t	   
	   (setf d (cdr (assoc p b))))))) ; if the data is there, return it


(defun undatum (d)
  (let ((n nil) (b nil) (p nil))
    (setf p (pattern d))
    (setf n (remainder (abs (sxhash p)) 1000.))
    (setf b (aref data n))
    (when (setf p (assoc p b))
	  (aset-f (delq p b) data n)
	  (remprop d 'pattern)
	  d)))

;;; Redid these due to a duplicate definition.  AJY 2015-05-09
;;;

(defun varp (x) (and (symbolp x) (get x 'variable)))
(defun unvarp (x) (and (symbolp x) (eq 'un (get x 'variable))))
(defun exvarp (x) (and (symbolp x) (eq 'ex (get x 'variable))))


(defvar $ nil)
(setf (get '$ 'variable) 'un)
(defvar $a nil)
(setf (get '$a 'variable) 'un)
(defvar $b nil)
(setf (get '$b 'variable) 'un)
(defvar $c nil)
(setf (get '$c 'variable) 'un)
(defvar $d nil)
(setf (get '$d 'variable) 'un)
(defvar $e nil)
(setf (get '$e 'variable) 'un)
(defvar $f nil)
(setf (get '$f 'variable) 'un)
(defvar $g nil)
(setf (get '$g 'variable) 'un)
(defvar $h nil)
(setf (get '$h 'variable) 'un)
(defvar $i nil)
(setf (get '$i 'variable) 'un)
(defvar $j nil)
(setf (get '$j 'variable) 'un)
(defvar $k nil)
(setf (get '$k 'variable) 'un)
(defvar $l nil)
(setf (get '$l 'variable) 'un)
(defvar $m nil)
(setf (get '$m 'variable) 'un)
(defvar $n nil)
(setf (get '$n 'variable) 'un)
(defvar $o nil)
(setf (get '$o 'variable) 'un)
(defvar $p nil)
(setf (get '$p 'variable) 'un)
(defvar $q nil)
(setf (get '$q 'variable) 'un)
(defvar $r nil)
(setf (get '$r 'variable) 'un)
(defvar $s nil)
(setf (get '$s 'variable) 'un)
(defvar $t nil)
(setf (get '$t 'variable) 'un)
(defvar $u nil)
(setf (get '$u 'variable) 'un)
(defvar $v nil)
(setf (get '$v 'variable) 'un)
(defvar $w nil)
(setf (get '$w 'variable) 'un)
(defvar $x nil)
(setf (get '$x 'variable) 'un)
(defvar $y nil)
(setf (get '$y 'variable) 'un)
(defvar $z nil)
(setf (get '$z 'variable) 'un)


(defun pr-index (p)
  (cond ((pr-datump p))
	(t (pr-index1 p (setf p (datum p)) 1 nil) p)))

(defparameter uparrow 94.) ; ASCII code of #\^


(defun pr-index1 (p d n vl)
  (setf vl (prchk 'var n vl))
  (cond ((varp p) (pradd d vl))
	((atom p)
	 (if (and (symbolp p) (= uparrow (getcharn p 1)) (not (pattern p)))
	     (datum (implode (cdr (explode p))))) ; EXPLODEN ==> EXPLODE
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
	((atom p) (let ((dum nil))
		    (setf dum (prget p n))
		    (if (= 1 (car dum))
			(prrem p n)
			(prdel d dum))))
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
  (delq d vl))


(defun pr-datump (p)
  (do ((l (pr-indexp p) (cdr l))) ; Var init next
      ((or (null l) (numberp (car l))) ; Termination test
      (if (samep p (pattern (car l))) (return (car l))))))

(defun pr-indexp (p) 
  (let (best (score 999999999)) (pr-indexp1 p 1 0 nil) (cdr best)))

(defun pr-indexp1 (p n s vl)
  (let ((dum))
    (cond ((null (setf dum (prget 'var n)))
	   (setf score s best vl))
	  ((>= (setf s (+ s (car dum))) score))
	  ((atom p)
	   (cond ((or (varp p) (pattern p)))
		 ((null (setf vl (prget p n))) (setf score s best dum))
		 ((< (setf s (+ s (car vl))) score) (setf score s best vl))))
	  ((commutativep (car p))
	   (setq n (lsh n 1))
	   (do ((l p (cdr l)))
	       ((atom l))
	     (pr-indexp1 (car l) n s dum)))
	  (t (do ((l p (cdr l)))
		 ((atom l))
	       (pr-indexp1 (car l) (setf n (lsh n 1)) s dum)
	       (setf n (1+ n)))))))

(defun prget (x n)
  (catch '*exit-prget*
    (do ((l (symbol-plist x) (cddr l)))
	((null l))
      (if (equal n (car l)) (throw '*exit-prget* (cadr l))))))

(defun prrem (x n)
  (let ((dum (plistp x)))
    (if (equal n (car dum))
	(setplistp x (cddr dum))
	(catch '*exit-prrem*
	  (do ((l (cdr dum) (cddr l)))
	      ((null (cdr l)))
	    (if (equal n (cadr l)) (throw '*exit-prrem*
				     (rplacd l (cdddr l)))))))))

(defun prput (x v n)
  (catch '*exit-prput*
    (do ((l (symbol-plist x) (cddr l)))
	((null l) (setplistp x (list* n v (plistp x))))
      (if (equal n (car l)) (throw '*exit-prput* (rplaca (cdr l) v)))))
  v)

(defun commutativep (x) (memq x '(and or set + *)))

(defun activate (n) 
  (do ((i 1 (1+ i)))
      ((> i n))
    (if (memq (arg i) activetheories)
	nil
	(progn
	  (setq activetheories (cons (arg i) activetheories))
	  (cmark (arg i)))))
  'done)

(defun deactivate (n) 
  (do ((i 1 (1+ i)))
       ((> i n))
    (if (not (memq (arg i) activetheories))
	nil
	(progn
	  (cunmrk (arg i))
	  (setf activetheories (delq (arg i) activetheories))))
    'done))

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
  (do ((l (getp con 'contents) (cdr l)))
      ((null l))
    (when (not (kcntxt (car l) con))
      (pr-unindex (car l)) (undatum (car l))))
  'done)

(defun cntxt (dat con)
  (if (memq con (get dat 'theory))
      dat
      (progn
	(putp con (cons dat (getp con 'contents)) 'contents)
	(setf (get dat 'theory) (cons con (get dat 'theory)))
	dat)))

(defun kcntxt (dat con)
  (putp con (delq dat (getp con 'contents)) 'contents)
  (putc dat (delq con (get dat 'theory)) 'theory))

(defun putc (x y z)
  (cond ((null y) (setf (get x z) nil))
	(t (setf (get x z) y))))

(defun cntp (x)
  (do ((l (get x 'theory) (cdr l)))
      ((null l))
    (if (and (setq x (getp (car l) 'cmark)) (> x 0)) (return t))))

(defun theorymark ()
  (when (not (eq current theory))
	(cunmrk current) (setq current theory) (cmark theory)))

(defun cmark (con)
  (let ((cm (getp con 'cmark)))
    (putp con (if cm (1+ cm) 1) 'cmark)
    (mapc 'cmark (getp con 'includes))))

(defun cunmrk (con)
  (let ((cm (getp con 'cmark)))
    (if cm (putp con (1- cm) 'cmark))
    (mapc 'cunmrk (getp con 'includes))))

