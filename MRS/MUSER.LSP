
perm filename MUSER.LSP[MRS,LSP] blob sn#588783 filedate 1981-09-30 generic text, type T, neo UTF8

;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '|<csd.genesereth>macros.fasl|) (special actions))

(defun define macro (x) (cons 'defun (cdr x)))

(defun asetq macro (x) (cons 'setq (cdr x)))


($assert '(MyToAchieve $g alreadytrue))
(define alreadytrue (g) ($truep g))



($assert '(MyToAchieve (opr $a $c) achopr))
($assert '(MyToAchieve (output $n $a $c) achopr))

(define achopr (g) (if (groundp g) ($assert g) truth))

(defun groundp (p) (if (atom p) (not (varp p)) (mapand 'groundp p)))



($assert '(MyToAchieve (input $n $c $x) achinput))
(define achinput (p)
  (let (x)
       (if (setq x (sb 'obtain (cadddr p)))
	   ($assert (list 'input (cadr p) (caddr p) x)))))



($assert '(MyToAchieve (prc $c $p) achprc))

(define achprc (g)
  ($assert `(requires ,(cadr g) ,(caddr g)))
  (sb 'plan (caddr g)))



;($assert '(MyToAchieve (occurs $c $p) achoccurred))

;(defun achoccurred (g)
;  (let (c al)
;       (setq c (exvar (cadr g) nil) g (subst c (cadr g) (caddr g)))
;       (if (and (setq al ($truep (cons 'or (cdr g))))
;		(sb 'achieve (pateval g al)))
;	   (subvar c al))))

(defun pateval (x al) (sublis al x))



($assert '(if (rel $p and) (MyToAchieve $p achand)))

(define achand (g)
  (do ((l (cdr g) (cdr l)) (dum) (al truth))
      ((null l) t)
      (if (setq dum (sb 'achieve (mtcheval (car l) al)))
	  (setq al (alpend dum al))
	  (return nil))))


($assert '(MyToObtain $x obtsymbol))
(define obtsymbol (x) (if (atom x) x))



($assert '(MyToObtain $x getval))



($assert '(MyToObtain $x construct))

(define construct (x)
  (let (c al)
    (cond ((and (asetq al ($lookup `(if ?p (output ?c ,x))))
		(asetq c (sb 'plan (list 'occurs (subvar '?c al)
					 (mtcheval '?p al)))))
	   ($assert (list 'produces c (asetq c (ua-datum `(output ,c)))))
	   c))))


($assert '(MyToPlan $g planphantom))
(define planphantom (g) ($truep g))



($assert '(MyToPlan $g alreadyplanned))
(define alreadyplanned (g) ($truep `(achieves ?a ,g)))



($assert '(MyToPlan $g plansea))

(define plansea (g)
  (let (al c)
    (cond ((and (asetq al ($lookup `(if ?p (sea ?c ,g))))
		(asetq c (sb 'plan
			     `(occurs ,(subvar '?c al),(mtcheval '?p al)))))
	   (do l (getvals `(sea ,c)) (cdr l) (null l)
	       (if (groundp (setq g (mtcheval (car l) al)))
		   ($assert `(achieves ,c ,g))))
	   truth))))



($assert '(MyToPlan $g planred))

(define planred (g)
  (if (asetq g ($lookup `(if ?p ,g)))
      (sb 'plan (mtcheval '?p g))))			; here



($assert '(if (rel $p and) (MyToPlan $p planand)))

(define planand (g)
  (do ((l (cdr g) (cdr l)) (dum) (al))
      ((null l) al)
      (if (setq dum (sb 'plan (mtcheval (car l) al)))	; here
	  (setq al (alpend dum al))
	  (return nil))))



($assert '(MyToPlan (occurs $c $p) planoccurs))

(define planoccurs (g)
  (let ((c (newsymbol 'a)) al)
    (cond ((setq al (sb 'achieve (subst c (cadr g) (caddr g))))
	   (asetq actions (cons c actions))
	   c))))


($assert '(MyToPerform $c perform))


(defprop ?p t ex)
(defprop ?c t ex)
($assert '(mytolookup (if $xx $yy) ex-lookup))
(asetq methods (nconc (mapcar '(lambda (l) (subvar '?y l))
			      ($lookups '(MyToAchieve ?x ?y)))
		      (mapcar '(lambda (l) (subvar '?y l))
			      ($lookups '(MyToPlan ?x ?y)))
		      (mapcar '(lambda (l) (subvar '?y l))
			      ($lookups '(MyToObtain ?x ?y)))))

