;;; -*- Mode: Common-LISP -*-
;;;
;;;DEMO.LSP    0.1 18-Mar-83 0129 000   1 ML E      18-Mar-83   
;;;	DEMO will run the file demo.mrs (** need to retrieve**)
;;;
;;;perm filename DEMO.LSP[MRS,LSP] blob sn#702096 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; AJY 2015-05-10 on Mother's Day
;;;
;;; ------------------------------------------------------------
;;;




(defun $demo () (demo '|C:\\MRS\\mrs.demo|))


(defun demo (f)
  (terpri) (princ '|Type a carriage return to advance.|)
  (setq f (open f 'in))
  (do ((s (read f 'end) (read f 'end)))
      ((eq 'end s))
    (do ((c (tyi t) (tyi t)))
	((= 13. c)))
    (print s)
    (print (eval s)))
  (terpri)
  (close f)
  'done)

