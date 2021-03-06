;;; -*- Mode: Common-LISP -*-
;;;
;;;FCKB.LSP    0.2 18-Mar-83 0133 000   1 ML E      18-Mar-83   
;;;	FL-ASSERTS presumably forward chaining
;;;
;;;perm filename FCKB.LSP[MRS,LSP] blob sn#702122 filedate 1983-03-18
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




(defun fl-assert (p)
  (cond ((pr-indbp p))
	(t (prog2 nil (stash p) (fl-assert1 p) (fl-assert2 p)))))

(defun fl-assert1 (p)
  (declare (special justify))
  (do ((l (bclookups `(if ,p $q)) (cdr l)) (q))
      ((null l))
      (setq q (getvar '$q (cdar l)))
      (when justify (just (datum q) 'fl-assert (caar l) (datum p)))
      (mrs-assert q)))

(defun fl-assert2 (p)
  (theorymark)
  (do ((l (pr-indexp `(if (and ,p) $q)) (cdr l)) (rule) (al))
      ((null l) p)
    (if (and (cntp (car l)) (setq rule (pattern (car l)))
	     (eq 'if (car rule)) (eq 'and (caadr rule)))
	(do ((m (cdadr rule) (cdr m)))
	    ((null m))
	  (if (setq al (matchp (car m) p))
	      (mapc '(lambda (n)
		      (setq n (plug (caddr rule) (alpend n al)))
		      (when justify (just (datum n) 'fl-assert
					  (car l) (datum p)))
		      (mrs-assert n))
		    (lookups-and1 (cdadr rule) al nil)))))))

;; milt  2/25 changed alconc to alpend

