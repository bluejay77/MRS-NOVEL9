
perm filename PLIST.LSP[MRS,LSP] blob sn#702132 filedate 1983-03-18 generic text, type T, neo UTF8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
           #+maclisp (load '|macros.fas|)
           #+franz (load 'macros)
	   (impvar truth))



(defun tl-stash (p) (put (cadr p) t (car p)))

(defun tl-unstash (p) (remprop (cadr p) (car p)))

(defun tl-lookup (p) (if (get (cadr p) (car p)) truth))



(defun pl-stash (p) (put (cadr p) (caddr p) (car p)))

(defun pl-unstash (p) (remprop (cadr p) (car p)))

(defun pl-lookup (p) (matchp (caddr p) (get (cadr p) (car p))))

(defun pl-lookupval (p) (get (cadr p) (car p)))



(defun dl-stash (p)
  (put (cadr p) (addq (caddr p) (get (cadr p) (car p))) (car p)))

(defun dl-unstash (p)
  (put (cadr p) (delq (caddr p) (get (cadr p) (car p))) (car p)))

(defun dl-lookup (p)
  (do ((l (get (cadr p) (car p)) (cdr l)) (val (caddr p)) (dum))
      ((null l))
      (if (and (setq dum (matchp val (car l))) (note dum)) (return t))))

(defun dl-lookupval (x) (notes (get (cadr x) (car x))))

