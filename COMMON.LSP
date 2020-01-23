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

;(defun addq (x l) (if (memq x l) l (cons x l)))

;;; This array and the next function are used to give property lists to
;;; Lisp objects that don't normally have plists (eg strings and fixnums)
;;;

(defvar uniques (make-array 1000.))

(defun unique (n)
  (cond ((symbolp n) n)
	(t (let ((b nil) (a-obj nil))
	     ;; Calculate b = the hash value for the objcet n
	     (setf b (remainder (abs (sxhash n)) 1000.))
	     ;; Find a-obj = the object and its values in b
	     ;; The array uniques contains (object . value-list) pairs
	     (setf a-obj (assoc n (aref uniques b)))
	     (setf value (cadr a-obj)) ; This is the list of values
	     (cond (a-obj) ; If it is there return it
		   (t
		    (setf (aref uniques b) ; The place of the alist
			  (acons Rubbitsch))))))))


(defun unique (n) n)

(defun maksym (c)
  (let ((s nil) (n nil))
    (if (setf n (get 'maksym c)) ; Condition
	(setf (get c 'maksym) (1+ n)) ; IF True clause
        (progn (setf n 1) (setf (get c 'maksym) 2)) ; ELSE clause
	)
    (implode (cons c (exploden n)))))

(defun mapatoms (fcn) (mapc '(lambda (x) (funcall fcn x)) (oblist)))



