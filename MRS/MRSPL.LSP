
perm filename MRSPL.LSP[MRS,LSP] blob sn#612317 filedate 1981-09-30 generic text, type T, neo UTF8

;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '(macros fasl)))

(defun pl-stash (p) (put (cadr p) (caddr p) (car p)))
(defun pl-unstash (p) (rem (cadr p) (car p)))
(defun pl-lookup (p) (if (eq (caddr p) (get (cadr p) (car p))) truth))
(defun pl-lookupval (p) (get (cadr p) (car p)))


(defun dl-stash (p)
  (put (cadr p) (addq (caddr p) (get (cadr p) (car p))) (car p)))

(defun dl-unstash (p)
  (put (cadr p) (delq (caddr p) (get (cadr p) (car p))) (car p)))

(defun dl-lookup (p) (if (memq (caddr p) (get (cadr p) (car p))) truth))

(defun dl-lookupval (x) (car (get (cadr x) (car x))))

(defun dl-lookupvals (x) (get (cadr x) (car x)))

