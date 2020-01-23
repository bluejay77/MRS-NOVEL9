
perm filename TM.LSP[MRS,LSP] blob sn#702140 filedate 1983-03-18 generic text, type T, neo UTF8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
           #+maclisp (load '|macros.fas|)
           #+franz (load 'macros)
	   )

(defun tm-unassert (p)
  (cond ((not (pr-indbp p)) nil)
	(t (do ((l (fbs 'pr-lookup '(just . $p)) (cdr l)) (d (datum p)) (j))
	       ((null l) (unstash p))
	       (setq j (getvar '$p (car l)))
	       (if (memq d (cddr j)) (unassert (pattern (cadr j))))))))

