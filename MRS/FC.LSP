
perm filename FC.LSP[MRS,LSP] blob sn#702121 filedate 1983-03-18 generic text, type T, neo UTF8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
           #+maclisp (load '|macros.fas|)
           #+franz (load 'macros)
	   )

(defun &assert (p)
  (cond ((pr-indbp p))
	(t (prog2 nil (stash p)
		  (&assert1 p) (&assert2 p)))))

(defun &assert1 (p)
  (do l (fbs 'pr-lookup `(if ,p $q)) (cdr l) (null l)
      (&assert (getvar '$q (car l)))))

(defun &assert2 (p)
  (theorymark)
  (do ((l (pr-indexp `(if (and ,p) $q)) (cdr l)) (rule) (al))
      ((null l) p)
    (if (and (cntp (car l)) (setq rule (pattern (car l)))
	     (eq 'if (car rule)) (eq 'and (caadr rule)))
	(do m (cdadr rule) (cdr m) (null m)
	    (if (setq al (matchp (car m) p))
		(mapc '(lambda (n) (&assert (plug (caddr rule) (alconc n al))))
		      (lookups-and1 (cdadr rule) al nil)))))))

(defun lookups-and1 (cs al nl)
  (cond ((null cs) (cons al nl))
	(t (do ((l (lookups (plug (car cs) al)) (cdr l)))
	       ((null l) nl)
	       (setq nl (lookups-and1 (cdr cs) (alpend (car l) al) nl))))))
;; milt  2/25 changed alconc to alpend

