
perm filename BCSB.LSP[MRS,LSP] blob sn#702113 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00004 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00004 00003
C00007 00004
C00009 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1982  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp (load '|macros.fasl|)
	   #+franz (load 'macros)
	   (*lexpr tb)
	   (impfun maknot) (impvar theory agenda truth task)
	   (expfun backchain backresidue) (expvar cache justify))

(setq cache nil justify nil)

(defun pat (x) (cadr x))
(defun gl (x) (caddr x))
(defun al (x) (cadddr x))
(defun getjust (x) (caddddr x))
(defun cont (x) (caddddr (cdr x)))


(defun backchain (x)
  (setq agenda nil)
  (tb 'addbc x nil truth nil nil)
  (do x (scheduler) (scheduler) (or (null x) (note x))))

(defun addbc (p gl al j c)
  (cond ((eq 'and (car p)) (bcand p gl al j c))
	((eq 'or (car p)) (bcor p gl al j c))
	(t (bcbc p gl al j c) (bclookup p gl al j c))))

(defun bcand (p gl al j c)
  (tb 'addbc (cadr p) (cddr p) truth (list 'and) task))

(defun bcor (p gl al j c)
  (do l (cdr p) (cdr l) (null l)
      (tb 'addbc (car l) gl al (list 'or) c)))

(defun bclookup (p gl al j c)
  (do l (lookups p) (cdr l) (null l)
      (result (car l) (list 'lookup) p gl al j c)))

(defun bcbc (q gl al j c)
  (do l (bclookups `(if $p ,q)) (cdr l) (null l)
      (tb 'addbc (getvar '$p (cdar l)) gl
	         (punset '$p (cdar l)) (list (caar l) 'mp) task)))

(defun bclookups (p)
  (theorymark)
  (do ((l (pr-indexp p) (cdr l)) (dum) (nl))
      ((null l) (nreverse nl))
      (if (and (cntp (car l)) (setq dum (matchp p (pattern (car l)))))
	  (setq nl (cons (cons (car l) dum) nl)))))

(defun result (ans n p gl al j c)
  (setq p (datum (plug p ans)) ans (alpend al ans))
  (if cache (thstash (pattern p) 'cache))
  (if justify (thstash `(just ,p . ,(reverse n)) 'justify))
  (cond ((and (eq 'and (car (last j))) (not (null gl)))
	 (tb 'addbc (plug (car gl) ans) (cdr gl) ans (cons p j) c))
	((null c) (tb 'succeed ans))
	(t (result ans (cons p j)
		   (pat c) (gl c) (al c) (getjust c) (cont c)))))


(defun backresidue (x)
  (setq agenda nil)
  (tb 'addbr (list x) '((= t t)))
  (do x (scheduler) (scheduler) (or (null x) (note x))))

(defun addbr (gl cl)
  (cond ((null gl) (tb 'succeed cl))
	((eq 'and (caar gl)) (brand gl cl))
	((eq 'or (caar gl)) (bror gl cl))
	(t (brbc gl cl) (brprim gl cl) (brold gl cl) (brlookup gl cl))))

(defun brand (gl cl) (tb 'addbr (append (cdar gl) (cdr gl)) cl))

(defun bror (gl cl)
  (do l (cdar gl) (cdr l) (null l)
      (tb 'addbr (cons (car l) (cdr gl)) cl)))

(defun brlookup (gl cl)
  (do l (lookups (car gl)) (cdr l) (null l)
      (tb 'addbr (plug (cdr gl) (car l)) (plug cl (car l)))))

(defun brold (gl cl)
  (do ((l cl (cdr l)) (al))
      ((null l))
      (if (setq al (unifyp (car gl) (car l)))
	  (tb 'addbr (plug (cdr gl) al) (plug cl al)))))

(defun brprim (gl cl)
  (local (al)
    (if (setq al (lookup `(primitive ,(car gl))))
	(tb 'addbr (plug (cdr gl) al) (plug (cons (car gl) cl) al)))))

(defun brbc (gl cl)
  (do l (fbs 'pr-lookup `(if $p ,(car gl))) (cdr l) (null l)
      (tb 'addbr (cons (getvar '$p (car l)) (plug (cdr gl) (car l)))
	         (plug cl (car l)))))

