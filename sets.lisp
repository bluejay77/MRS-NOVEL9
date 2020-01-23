;;; -*- Mode: Common-LISP -*-
;;;
;;;SETS.LSP    0.1 18-Mar-83 0057 000   1 ML PUPFTP 18-Mar-83   
;;;	truep-setof (only function in file)
;;;
;;;perm filename SETS.LSP[MRS,LSP] blob sn#702135 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-10
;;;
;;; ------------------------------------------------------------
;;;




(pr-stash '(totruep (prop setof $x $p $s) truep-setof))

(defun truep-setof (p)
  (declare (special truth))
  (cond ((unvarp (cadddr p))
	 (do ((l (trueps (caddr p)) (cdr l))
	      (x (cadr p)) (nl))
	     ((null l) (pset (cadddr p) nl truth))
	     (setf nl (cons (plug x (car l)) nl))))))

