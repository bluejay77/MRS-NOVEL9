;;; -*- Mode: Common-LISP -*-
;;;
;;;FC.LSP    0.2 18-Mar-83 0133 000   1 ML E      18-Mar-83   
;;;	&ASSERT - presumably hooks for forward chaining
;;;
;;;perm filename FC.LSP[MRS,LSP] blob sn#702121 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-10
;;;
;;; ------------------------------------------------------------
;;;




(defun meta-assert (p)
  (cond ((pr-indbp p))
	(t (prog2 nil (stash p)
		  (meta-assert1 p) (meta-assert2 p)))))

(defun meta-assert1 (p)
  (do ((l (fbs 'pr-lookup `(if ,p $q)) (cdr l)))
      ((null l))
    (meta-assert (getvar '$q (car l)))))

(defun meta-assert2 (p)
  (theorymark)
  (do ((l (pr-indexp `(if (and ,p) $q)) (cdr l)) (rule) (al))
      ((null l) p)
    (if (and (cntp (car l)) (setq rule (pattern (car l)))
	     (eq 'if (car rule)) (eq 'and (caadr rule)))
	(do ((m (cdadr rule) (cdr m)))
	    ((null m))
	  (if (setq al (matchp (car m) p))
	      (mapc '(lambda (n) (meta-assert (plug (caddr rule) (alconc n al))))
		    (lookups-and1 (cdadr rule) al nil)))))))

;;; Duplicate definition

#|

(defun lookups-and1 (cs al nl)
  (cond ((null cs) (cons al nl))
	(t (do ((l (lookups (plug (car cs) al)) (cdr l)))
	       ((null l) nl)
	       (setq nl (lookups-and1 (cdr cs) (alpend (car l) al) nl))))))

;; milt  2/25 changed alconc to alpend

|#


