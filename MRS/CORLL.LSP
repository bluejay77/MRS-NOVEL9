
perm filename CORLL.LSP[MRS,LSP] blob sn#612316 filedate 1981-09-30 generic text, type T, neo UTF8

;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '|<csd.genesereth>macros.fasl|) (special units)
	 (expfun ua-pattern ua-datum ua-put ua-rem ua-getfacts ua-list))

(setq units nil)

(defmode node () (atom <data>))
(defmode link () (atom <pattern>))

(defun a-atom (x s v) (if v (put x v s) (rem x s)))

(defun ua-pattern (n) :n:pattern)

(defun ua-datum (p)
  (progb (s)
    (setq s (implode (exploden p)))
    (put s p 'pattern)
    s))

(defun ua-put (n lk)
  (if (null :n:data) (setq units (cons n units)))
  (← :n:data (cons lk :n:data)))

(defun ua-rem (n lk)
  (← :n:data (delq lk :n:data))
  (if (null :n:data) (setq units (delq n units))))

(defun ua-getfacts (n) :n:data)

(defun ua-list (n) units)

