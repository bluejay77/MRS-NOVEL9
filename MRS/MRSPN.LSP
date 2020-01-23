
perm filename MRSPN.LSP[MRS,LSP] blob sn#612038 filedate 1981-09-30 generic text, type T, neo UTF8

;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pn-truep (p)
  (cond ((lookup p))
	((lookup (maknot p)) nil)
	((bc-truep p))
	((bc-truep (maknot p)) nil)
	(t 'unknown)))

(defun maknot (p)
  (cond ((atom p) (list 'not p))
	((eq 'not (car p)) (cadr p))
	(t (list 'not p))))

