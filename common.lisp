;;;COMMON.LSP    0.3 18-Mar-83 0126 000   1 ML E      18-Mar-83   
;;;	Property lists - also plists for arrays, etc.
;;;
;;;perm filename COMMON.LSP[MRS,LSP] blob sn#702115 filedate
;;;	1983-03-18 generic text, type T, neo UTF8 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modified this file for Common LISP:
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-03-18
;;;
;;; ------------------------------------------------------------
;;;



;;; (defun putp (x y z) (putprop (unique x) y z))

(defun putp (x y z) (setf (get (unique x) z) y))

;;; (defun getp (x z) (get (unique x) z))

(defun getp (x z) (get (unique x) z))

(defun plistp (x) (if (atom (setf x (unique x)))
		      (symbol-plist x)
		      (cdr x)))

(defun setplistp (x l)
       (if (atom (setf x (unique x)))
	   (setplist x l)
	   (rplacd x l)))

;;; Already done.
#|

(defun addq (x l) (if (memq x l) l (cons x l)))

|#

;;; This array and the next function are used to give property lists to
;;; Lisp objects that don't normally have plists (eg strings and fixnums)
;;;


(defvar uniques (make-array 1000.))

(defun unique (n)
  (cond ((symbolp n) n)
	(t (let ((b))
	     (setf b (remainder (abs (sxhash n)) 100.))
	     (cond ((assoc n (aref uniques b)))
		   (t (car (aset (cons (list n) (aref uniques b))
				     uniques b))))))))

(defun maksym (c)
  (let ((s nil) (n nil))
    (if (setf n (get 'maksym c)) ; Condition
	(setf (get c 'maksym) (1+ n)) ; IF True clause
        (progn (setf n 1) (setf (get c 'maksym) 2)) ; ELSE clause
	)
    (implode (cons c (exploden n)))))

(defvar oblist nil) ; Temporary hack for the MACLISP OBLIST

(defun mapatoms (fcn) (mapc #'(lambda (x) (funcall fcn x)) (oblist)))



