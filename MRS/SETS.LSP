
perm filename SETS.LSP[MRS,LSP] blob sn#702135 filedate 1983-03-18 generic text, type T, neo UTF8

(pr-stash '(totruep (prop ↑setof $x $p $s) truep-setof))

(defun truep-setof (p)
  (cond ((unvarp (cadddr p))
	 (do ((l (trueps (caddr p)) (cdr l)) (x (cadr p)) (nl))
	     ((null l) (pset (cadddr p) nl truth))
	     (setq nl (cons (plug x (car l)) nl))))))

