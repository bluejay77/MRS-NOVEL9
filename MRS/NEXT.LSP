
perm filename NEXT.LSP[MRS,LSP] blob sn#702130 filedate 1983-03-18 generic text, type T, neo UTF8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1982  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
           #+maclisp (load '|macros.fas|)
           #+franz (load 'macros)
	   (*lexpr tb)
	   (special task better agenda advice))

(setq advice nil better nil)

(pr-stash '(tostash (prop ↑better $x $y) stash-better))
(pr-stash '(tostash (prop ↑if $p (prop ↑better $x $y)) stash-better))

(defun stash-better (p) (setq better t) (pr-stash p))

;;; Format of a task:
;;;      (<opr> . <args>)

(defun getopr macro (x) `(car ,(cadr x)))
(defun getargs macro (x) `(cdr ,(cadr x)))
(defun maktask n (listify n))

(defun scheduler ()
  (do ((task (next) (next)))
      ((null task) nil)
      (if advice (mapc '(lambda (f) (funcall f task)) advice))
      (cond ((eq 'succeed (getopr task)) (return (car (getargs task))))
	    (t (apply (getopr task) (getargs task))))))

(defun add (x ol) (cons x ol))

(defun sb (g x) (tb (getvar '$ (&truep (list g x '$))) x))

(defun tb n (setq agenda (add (listify n) agenda)))

(defun next () 
  (if better (best)
      (prog2 nil (car agenda) (setq agenda (cdr agenda)))))

(defun best ()
  (do ((best (car agenda)) (l (cdr agenda) (cdr l)))
      ((null l) (setq agenda (delq best agenda 1)) best)
      (if (betterp (car l) best) (setq best (car l)))))

(defun betterp (x y)
  (and (eq 'addbc (getopr x)) (eq 'addbc (getopr y))
       (&truep `(better ,(car (getjust x)) ,(car (getjust y))))))

;;; Assumptions for next:
;;;
;;;     (if (and (applicable $z $t)
;;;              (every x (if (applicable x $t) (not (better x $z)))))
;;;         (recommended $z $t))
;;;
;;;     (iff (applicable $z $t) (true (elt $z (value agenda)) $t))
;;;
;;;     (if (and (relevant $z $t) (not (executed $z)))
;;;         (applicable $z $t))

