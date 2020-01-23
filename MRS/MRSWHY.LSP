
perm filename MRSWHY.LSP[MRS,LSP] blob sn#613752 filedate 1981-09-30 generic text, type T, neo UTF8

;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '(macros fasl)))

(defun $why (p) (why (semant p)))

(defun $whys (p) (whys (semant p)))

(defun why (p) (kb 'MyToWhy p))

(defun whys (p) (kb 'MyToWhys p))

(defun bs-why (p) (or (ex-why p) (bc-why p)))

(defun bs-whys (p)
  (if (groundp p) (if (setq p (bs-why p)) (list p))
      (nconc (lookups p) (bc-whys p))))

(defun ex-why (p)
  (worldmark)
  (do ((l (ua-getfacts (car p)) (cdr l)) (dum)) ((null l))
      (cond ((and (cntp (car l)) (setq dum (matchp p (car l))))
	     (if (not (groundp (unsemant (car l))))
		 (stash (list 'just (plug p dum) 'ex-lookup (car l))))
	     (return dum)))))

(defun bc-why (q)
  (worldmark)
  (do ((l (ua-getfacts (car q)) (cdr l)) (lhs) (rhs)) ((null l))
      (cond ((and (cntp (car l)) (setq al (matchp `(if $pp ,q) (car l)))
		  (setq lhs (why (subvar '$pp al))))
	     (setq rhs (plugal al lhs))
	     (stash (list 'just (plug q rhs) 'bc-truep
			        (car l)
				(plug (subvar '$pp al) lhs)))
	     (return (punset '$pp rhs))))))

(defun bc-whys (q)
  (worldmark)
  (do ((l (ua-getfacts (car q)) (cdr l)) (al) (dum) (nl))
      ((null l) nl)
    (cond ((and (cntp (car l)) (setq al (matchp `(if $pp ,q) (car l))))
	   (setq q (subvar '$pp al) dum (punset '$pp al))
	   (mapc '(lambda (m) (setq nl (cons (plugal dum m) nl)))
		 (whys q))))))

(defun why-and (p)
  (progb (al)
    (cond ((setq al (why-and1 (cdr p) truth))
	   (stash `(just ,(plug p al) truep-and
			 . ,(mapcar '(lambda (l) (plug l al)) (cdr p))))
	   al))))

(defun why-and1 (cs al)
  (cond ((null (cdr cs)) (why (plug (car cs) al)))
	(t (do ((l (whys (plug (car cs) al)) (cdr l)) (dum))  ((null l))
	       (if (setq dum (why-and1 (cdr cs) (car l)))
		   (return (alconc (car l) dum)))))))

(defun whys-and (p) (whys-and1 (cdr p) truth))
(defun whys-and1 (cs al)
  (cond ((null (cdr cs)) (whys (plug (car cs) al)))
	(t (do ((l (whys (plug (car cs) al)) (cdr l)) (dum) (nl))
	       ((null l) nl)
	       (if (setq dum (whys-and1 (cdr cs) (car l)))
		   (mapc '(lambda (m) (setq nl (cons (alpend (car l) m) nl)))
			 dum))))))


($assert '(MyToWhy $xx bs-why))
($assert '(MyToWhys $xx bs-whys))


($assert '(all p q (MyToWhy (and p q) why-and)))
($assert '(all p q (MyToWhys (and p q) whys-and)))
($assert '(all p q r (MyToWhy (and p q r) why-and)))
($assert '(all p q r (MyToWhys (and p q r) whys-and)))
($assert '(all p q r s (MyToWhy (and p q r s) why-and)))
($assert '(all p q r s (MyToWhys (and p q r s) whys-and)))


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

