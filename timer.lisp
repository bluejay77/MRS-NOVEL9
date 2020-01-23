;;; -*- Mode: Common-LISP -*-
;;;
;;;TIMER.LSP    0.2 18-Mar-83 0156 000   1 ML E      18-Mar-83   
;;;	timing function
;;;
;;;perm filename TIMER.LSP[MRS,LSP] blob sn#702139 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-10
;;;
;;; ------------------------------------------------------------
;;;




(defun runtime ()
  (let ((ticks (get-internal-run-time)))
    (/ (coerce ticks 'float)
       (coerce internal-time-units-per-second 'float))))


(defmacro timer (&rest x)
  `(time ,x))
