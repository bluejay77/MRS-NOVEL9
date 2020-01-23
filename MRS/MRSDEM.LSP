
perm filename MRSDEM.LSP[MRS,LSP] blob sn#702099 filedate 1983-03-17 generic text, type T, neo UTF8

;;;               -*-mode:lisp; package:macsyma -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '|<csd.genesereth>macros.fasl|) (special alist)
	 (expfun demon-assert defasserted defunasserted) (expvar demontrace))

(setq demontrace nil)

(defun demon-assert (p) (prog2 nil (stash p) (rundemons p 'preadded)))
(defun demon-unassert (p) (prog2 (rundemons p 'preremoved) (unstash p)))

(defun rundemons (p s)
  (do ((l (lookups (list s '? p)) (cdr l)) (d)) ((null l))
      (setq d (subvar '? (car l)))
      (rundemon (lookupvals (list s d)) d (car l))))

(defun rundemon (ps d al)
  (let (alist)
    (setq ps (mapcar '(lambda (l) (substpattern l al)) ps))
    (do l (chklhs ps) (cdr l) (null l)
      (cond (demontrace (terpri) (Princ '|Firing |) (princ d)))
      (funcall d (nconc (sublis (car l) alist) al)))))

(defun chklhs (cs)
  (cond ((null (cdr cs)) (lookups (car cs)))
	(t (do ((l (lookups (car cs)) (cdr l)) (nl))
	       ((null l) nl)
	       (mapc '(lambda (m) (setq nl (cons (alpend (car l) m) nl)))
		     (chklhs (sublis (car l) (cdr cs))))))))

(defun $defassert fexpr (x)
  (mapc '(lambda (l) ($assert `(preadded ,(car x) ,l))) (cadr x))
  (put (car x)
       `(lambda (al) (progv (mapcar 'car al) (mapcar 'cdr al) . ,(cddr x)))
       'expr)
  (car x))

(defun $defunassert fexpr (x)
  (mapc '(lambda (l) ($assert `(preremoved ,(car x) ,l))) (cadr x))
  (put (car x)
       `(lambda (al) (progv (mapcar 'car al) (mapcar 'cdr al) . ,(cddr x)))
       'expr)
  (car x))


($assert '(all x (if (expression x) (MyToAssert x demon-assert))))
($assert '(all x (if (expression x) (MyToUnassert x demon-unassert))))
($unassert '(all x (if (expression x) (MyToAssert x stash))))
($unassert '(all x (if (expression x) (MyToUnassert x unstash))))

