
perm filename COMMON.LSP[MRS,LSP] blob sn#702115 filedate 1983-03-18 generic text, type T, neo UTF8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
           #+maclisp (load '|macros.fasl|)
           #+franz (load 'macros)
	   )

(defun putp (x y z) (putprop (unique x) y z))
(defun getp (x z) (get (unique x) z))

#+maclisp (defun plistp (x) (plist (unique x)))
#-maclisp (defun plistp (x) (if (atom (setq x (unique x))) (plist x) (cdr x)))

#+maclisp
(defun setplistp (x l) (setplist (unique x) l))
#-maclisp
(defun setplistp (x l)
       (if (atom (setq x (unique x))) (setplist x l) (rplacd x l)))


(defun addq (x l) (if (memq x l) l (cons x l)))

;;; This sxhash only understands symbols/strings, cells, and numbers.
#+franz (defun sxhash (x)
	 (cond ((numberp x) (boole 9 x #o251634))  ; choose better number
               ((dtpr x) (boole 9 (rot (sxhash (car x)) 6) (sxhash (cdr x))))
	       (t (apply #'boole (append '(6 127.) (substringn x 1 20.))))))

;;; This array and the next function are used to give property lists to
;;; Lisp objects that don't normally have plists (eg strings and fixnums)
(array uniques t 100.)

(defun unique (n)
  (cond ((symbolp n) n)
	(t (local (b)
	     (setq b (remainder (abs (sxhash n)) 100.))
	     (cond ((assoc n (aref uniques b)))
		   (t (car (aset (cons (list n) (aref uniques b))
				     uniques b))))))))

(defun maksym (c)
  (local (s n)
    (if (setq n (get 'maksym c)) (put 'maksym (1+ n) c)
        (setq n 1) (put 'maksym 2 c))
    (implode (cons c (exploden n)))))

;;; This is NOT a Common Lisp compatible function.  It is only for use in
;;; tutor.l
#+franz(defun readline (&optional stream eof)
       (do ((in (tyi) (tyi)) (res))
	   ((eq in 10) (readlist (cons #/| (nreverse (cons #/| res)))))
	 (setq res (cons in res))))

;;; I/O compatability
#+franz(defun open (file type)
	      (cond ((atom type)
		     (cond ((eq type 'out) (outfile file))
			   ((eq type 'in) (infile file))
			   ((eq type 'append) (outfile file 'a))))
		    (t
		     (cond ((memq 'append type) (outfile file 'a))
			   ((memq 'out type)
			    (cond ((memq 'append type) (outfile file 'a))
				  (t (outfile file))))
			   ((memq 'in type) (infile file))))))
#+lispm(defun filepos (file) (send file ':read-pointer))

#+franz(defun mapatoms (fcn) (mapc '(lambda (x) (funcall fcn x)) (oblist)))

#+franz(defun nth (n lst) (nthelem (1+ n) lst))

#+franz(defun prin1 (ele &optional file)
	      (cond ((null file) (print ele))
		    (t (print ele file))))

