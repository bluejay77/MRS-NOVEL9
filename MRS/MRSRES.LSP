
perm filename MRSRES.LSP[MRS,LSP] blob sn#620901 filedate 1981-10-23 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00004 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002	               -*-Mode:LISP Package:MACSYMA -*-                      
C00007 00003
C00008 00004
C00009 ENDMK
C⊗;
;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '(macros fasl)))

(defun $residue (p) (residue (semant p)))

(defun $residues (p) (residues (semant p)))

(defun residue (p) (kb 'MyToResidue p))

(defun residues (p) (kb 'MyToResidues p))

(defun bs-residue (p) (or (ex-residue p) (bc-residue p)))

(defun bs-residues (p)
  (if (groundp p) (if (setq p (bs-residue p)) (list p))
      (nconc (lookups p) (bc-residues p))))

(defun ex-residue (p)
  (worldmark)
  (do ((l (ua-getfacts (car p)) (cdr l)) (dum)) ((null l))
      (cond ((and (cntp (car l)) (setq dum (resp p (car l))))
	     (if (assumptionp (car l))
		 (setq dum (vset t (list (car l)) dum)))
	     (return dum)))))

(defun bc-residue (q)
  (worldmark)
  (do ((l (ua-getfacts (car q)) (cdr l)) (lhs)) ((null l))
      (cond ((and (cntp (car l)) (setq al (resp `(if $pp ,q) (car l)))
		  (setq lhs (residue (subvar '$pp al))))
	     (setq lhs (vset t (subvar t lhs) (plugal al lhs)))
	     (return (punset '$pp lhs))))))

(defun bc-residues (q)
  (worldmark)
  (do ((l (ua-getfacts (car q)) (cdr l)) (al) (dum) (nl))
      ((null l) nl)
    (cond ((and (cntp (car l)) (setq al (resp `(if $pp ,q) (car l))))
	   (setq q (subvar '$pp al) dum (punset '$pp al))
	   (mapc '(lambda (m) (setq nl (cons (plugal dum m) nl)))
		 (residues q))))))

(defun residue-and (p) (residue-and1 (cdr p)))

(defun residue-and1 (cs)
  (cond ((null (cdr cs)) (residue (car cs)))
	(t (do ((l (residues (car cs)) (cdr l)) (dum))  ((null l))
	       (if (setq dum (residue-and1 (plug (cdr cs) (car l))))
		   (return (resconc (car l) dum)))))))

(defun residues-and (p) (residues-and1 (cdr p) truth))
(defun residues-and1 (cs al)
  (cond ((null (cdr cs)) (residues (plug (car cs) al)))
	(t (do ((l (residues (plug (car cs) al)) (cdr l)) (dum) (nl))
	       ((null l) nl)
	       (if (setq dum (residues-and1 (cdr cs) (car l)))
		   (mapc '(lambda (m) (setq nl (cons (alpend (car l) m) nl)))
			 dum))))))

(defun vset (x y al) (rplacd (assq x al) y) al)

(defun resp (p x)
  (let (pp px xp xx) (if (setq x (mgupx p x)) (vars p (list (cons t nil))))))

(defun resconc (al1 al2)
  (cond ((null al1) nil)
	((null (cdr al1)) (vset t (append (subvar t al1) (subvar t al2)) al2))
	(t (do ((l al1 (cdr l)))
	       ((null (cddr l)) (rplacd l al2)))
	   (vset t (append (subvar t al1) (subvar t al2)) al1))))

(defun assumptionp (p) (lookup `(just ,p assume)))  


($assert '(MyToResidue $xx bs-residue))
($assert '(MyToResidues $xx bs-residues))


($assert '(all p q (MyToResidue (and p q) residue-and)))
($assert '(all p q (MyToResidues (and p q) residues-and)))
($assert '(all p q r (MyToResidue (and p q r) residue-and)))
($assert '(all p q r (MyToResidues (and p q r) residues-and)))
($assert '(all p q r s (MyToResidue (and p q r s) residue-and)))
($assert '(all p q r s (MyToResidues (and p q r s) residues-and)))


(defun $just (p) (pr-just (datum (semant p)) 0) (terpri))

(defun datum (p) (if (atom p) p (ua-datum (mapcar 'datum p))))

(defun pr-just (p n)
  (progb (j)
    (terpri) (tyotbsp n) (princ p)
    (if (not (setq j (get-just p))) t
	(princ '| by |) (princ (caddr j))
	(do l (cdddr j) (cdr l) (null l) (pr-just (car l) (+ 4 n))))))

(defun get-just (p)
  (do ((l (ua-getfacts p) (cdr l)) (dum)) ((null l))
      (setq dum (ua-pattern (car l)))
      (if (and (eq 'just (car dum)) (eq p (cadr dum))) (return dum))))

(defun tyotbsp (n) (do i 1 (1+ i) (> i n) (tyo 32.)))

