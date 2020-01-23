;;; -*- Mode: Common-LISP -*-
;;;
;;;DEMONS.LSP    0.4 18-Mar-83 0129 000   1 ML E      18-Mar-83   
;;;	DEMON related functions (forward chaining?)
;;;
;;;perm filename DEMONS.LSP[MRS,LSP] blob sn#702117 filedate
;;;	1983-03-18 generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00003 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00005 00003
;;;C00007 ENDMK
;;;C⊗;
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




;;; (declare (special agenda))


;;; General compilation of applicability relies on the following fact.
;;;
;;;     (if (applicable $z) (elt $z (value agenda)))
;;;
;;; Each demon can be represented by a rule of the following form:
;;;
;;;     (if (and (demon $p $d) (indb $p $al))
;;;         (applicable (ev $d $al))

(defun demons (p)
  (cond ((pr-indbp p) p)
	(t (let (agenda)
	        (stash p)
		(schdemons p 'demon)
		(scheduler)
		(datum p)))))

(defun undemons (p)
  (cond ((not (pr-indbp p)) nil)
	(t (let (agenda)
	        (schdemons p 'undemon)
		(scheduler)
		(unstash p)))))

(defun schdemons (p s)
  (theorymark)
  (do ((l (pr-indexp `(,s (and ,p) $z)) (cdr l)) (d))
      ((null l))
      (if (and (cntp (car l)) (eq s (car (setq d (pattern (car l))))))
	  (schdemon p (cdadr d) (caddr d)))))

(defun schdemon (p ts d)
  (do ((l ts (cdr l)) (al)) ((null l))
      (if (setq al (matchp (car l) p))
	  (mapc '(lambda (bl) (tb d bl))
		(lookups-and1 ts al nil)))))


(defmacro $defdemon (&rest x)
  `(defdemon (cond ((car ,x)) (t (maksym 'd)))
             (mapcar '(lambda (l) (internal l nil nil)) (cadr ,x))
	     (cddr ,x)))

(defun defdemon (name ts body)
  (pr-stash `(demon (and . ,ts) ,name))
  (setf (fdefinition name)
	`(lambda (al) (progv (mapcar 'car al) (mapcar 'cdr al) . ,body)))
  name)

(defmacro $defundemon (&rest x)
  `(defundemon (cond ((car ,x)) (t (maksym 'd)))
       (mapcar '(lambda (l) (internal l nil nil)) (cadr ,x))
     (cddr ,x)))

(defun defundemon (name ts body)
  (pr-stash `(undemon (and . ,ts) ,name))
  (setf (fdefinition name)
	`(lambda (al) (progv (mapcar 'car al) (mapcar 'cdr al) . ,body)))
  name)

