;;; -*- Mode: Common-LISP -*-
;;;
;;;FCSB.LSP    0.2 18-Mar-83 0133 000   1 ML E      18-Mar-83   
;;;	FORCHAIN - and code to add task to agenda when chaining
;;;
;;;perm filename FCSB.LSP[MRS,LSP] blob sn#702123 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




#|
(declare (special agenda)
	 (special justify))
|#


(defun forchain (p)
  (setq agenda nil)
  (tb 'addfc p)
  (scheduler)
  (datum p))

(defun addfc (p)
  (cond ((pr-indbp p))
	(t (stash p) (addfc2 p) (addfc1 p))))

(defun addfc1 (p)
  (do ((l (bclookups `(if ,p $q)) (cdr l)) (q))
      ((null l))
      (setq q (getvar '$q (cdar l)))
      (when justify (just (datum q) 'forchain (caar l) (datum p)))
      (tb 'addfc q)))

(defun addfc2 (p)
  (theorymark)
  (do ((l (pr-indexp `(if (and ,p) $q)) (cdr l)) (rule) (al))
      ((null l) p)
    (if (and (cntp (car l)) (setq rule (pattern (car l)))
	     (eq 'if (car rule)) (eq 'and (caadr rule)))
	(do ((m (cdadr rule) (cdr m)))
	    ((null m))
          (if (setq al (matchp (car m) p))
	      (mapc '(lambda (n)
		       (setq n  (plug (caddr rule) (alconc n al)))
		       (when justify (just (datum n) 'forchain
					   (car l) (datum p)))
		       (tb 'addfc n))
		    (lookups-and1 (cdadr rule) al nil)))))))

