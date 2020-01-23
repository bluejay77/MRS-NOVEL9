
perm filename RESOLU.LSP[MRS,LSP] blob sn#702134 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00003 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00005 00003	 
C00007 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1982  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+maclisp (eval-when (compile)  (load '|macros.fas|))
#+franz(eval-when (compile) (load 'macros))
(eval-when (compile)
           (special alist)
	   (*lexpr tb)
	   (impvar theory truth agenda)
	   (impfun maknot))


(defun resolution (x)
  (local ((theory (newtheory theory)) gl vl ans)
	 (setq gl (cnf (maknot x)) vl (variables x truth))
	 (do l gl (cdr l) (null l)
	     (if (null (cdar l)) (stash (caar l))
		 (assert-or (cons 'or (car l)))))
	 (do l gl (cdr l) (null l)
	     (setq agenda nil) (tb 'addrs (car l) vl)
	     (if (setq ans (scheduler)) (return ans)))
	 (empty theory)
	 ans))

(defun newtheory (d) (local ((c (maksym 't))) (includes c d) c))

(defun variables (x al)
  (cond ((unvarp x) (if (assq x al) al (pset x x al)))
	((atom x) al)
	(t (mapc '(lambda (l) (setq al (variables l al))) x)  al)))

(defun addrs (gl al)
  (cond ((null gl) (tb 'succeed al))
	(t (rsres gl al) (rslookup gl al))))

(defun rslookup (gl al)
  (do l (lookups (maknot (car gl))) (cdr l) (null l) 
      (tb 'addrs (plug (cdr gl) (car l)) (plugal al (car l)))))

(defun rsres (gl al)
  (do ((l (fbs 'pr-lookup `(or ,(maknot (car gl)) . $q)) (cdr l)))
      ((null l) nil)
      (tb 'addrs (plug (nconc (cdr (getvar '$q (car l))) (cdr gl)) al)
	         (plugal al (car l)))))
 

(defun resolutionresidue (x)
  (local ((theory (newtheory theory)) gl ans)
	 (setq gl (cnf (maknot x)))
	 (do l gl (cdr l) (null l)
	     (if (null (cdar l)) (stash (caar l))
		 (assert-or (cons 'or (car l)))))
	 (do l gl (cdr l) (null l)
	     (setq agenda nil) (tb 'addrs (car l) '((= t t)))
	     (if (setq ans (scheduler)) (return ans)))
	 (empty theory)
	 ans))

(defun addrr (gl cl)
  (cond ((null gl) (tb 'addrr nil cl))
	(t (rrres gl cl) (rrprim gl cl) (rrlookup gl cl))))

(defun rrlookup (gl cl)
  (do l (lookups (maknot (car gl))) (cdr l) (null l) 
      (tb 'addrr (plug (cdr gl) (car l)) (plug cl (car l)))))

(defun rrprim (gl cl)
  (local (al)
    (if (setq al (lookup `(primitive ,(car gl))))
	(tb 'addrr (plug (cdr gl) al) (plug (cons (car gl) cl) al)))))


(defun rrres (gl cl)
  (do ((l (fbs 'pr-lookup `(or ,(maknot (car gl)) . $q)) (cdr l)))
      ((null l) nil)
      (tb 'addrs (plug (nconc (cdr (getvar '$q (car l))) (cdr gl)) (car l))
	         (plug cl (car l)))))

