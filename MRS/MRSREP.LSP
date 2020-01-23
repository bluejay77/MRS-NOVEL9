
perm filename MRSREP.LSP[MRS,LSP] blob sn#617881 filedate 1981-10-07 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00006 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002	               -*-Mode:LISP Package:MACSYMA -*-                      
C00004 00003
C00005 00004
C00007 00005
C00012 00006
C00017 ENDMK
C⊗;
;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (load '(macros fasl))
	 (special current conindex connumber pp px xp xx)
	 (fixnum conindex connumber)
	 (expvar currenttheory activetheories truth)
	 (expfun pr-stash pr-unstash pr-lookup pr-lookupval pr-lookupvals
		 ut-activate ut-deactivate ut-newtheory ut-empty ut-kill))

(defmode dnode () (atom <myworld>))
(defmode world () (atom <cmark fixnum 0> <subworld> <contents>))

(defun pset macro (x) `(cons (cons ,(cadr x) ,(caddr x)) ,(cadddr x)))

(setq currenttheory 'global current 'global activetheories nil truth '((t . t))
      conindex 0 connumber 50)

(defprop global 2 cmark)

(array conunmrk nil (1+ connumber))
(array conmark t (1+ connumber))

(defun varp (x) (getl x '(ex un)))
(defun unvarp (x) (get x 'un))
(defun exvarp (x) (get x 'ex))

(defun addq (x l) (if (memq x l) l (cons x l)))


(defun pr-stash (p) (cntxt (pr-index p) currenttheory))

(defun pr-index (p) (pr-index1 p (ua-datum p)))

(defun pr-index1 (p d)
  (cond ((atom p) (ifn (memq d (ua-getfacts p)) (ua-put p d)) p)
	(t (pr-index1 (ua-datum (mapcar '(lambda (l) (pr-index1 l d)) p)) d))))


(defun pr-unstash (p)
  (cond ((not (setq p (pr-lookup p))) nil)
	((kcntxt p currenttheory) p)
	(t (pr-unindex p))))

(defun pr-unindex (d)
  (mapc 'pr-unindex (ua-pattern d))
  (mapc '(lambda (l) (ua-rem l d)) (ua-pattern d))
  d)


(defun pr-lookup (p)
  (worldmark)
  (do l (ua-getfacts (car p)) (cdr l) (null l)
      (if (and (cntp (car l)) (samep p (car l)))
	  (return (car l)))))

(defun samep (x y) (samep1 x y truth))

(defun samep1 (x y al)
  (cond ((and (varp x) (varp y))
	 (progb (dum)
	   (if (setq dum (assq x al)) (if (eq y (cdr dum)) al)
	       (cons (cons x y) al))))
	((atom x) (if (eq x y) al))
	((mapand2 '(lambda (l m) (setq al (samep1 l m al)))
		  x (ua-pattern y))  al)))


(defun pr-lookupval (x)
  (worldmark)
  (do l (ua-getfacts (cadr x)) (cdr l) (null l)
      (if (and (cntp (car l))
	       (eq (car x) (car (ua-pattern (car l))))
	       (eq (cadr x) (cadr (ua-pattern (car l)))))
	  (return (caddr (ua-pattern (car l)))))))

(defun pr-lookupvals (x)
  (worldmark)
  (do ((l (ua-getfacts (cadr x)) (cdr l)) (nl))
      ((null l) nl)
      (if (and (cntp (car l))
	       (eq (car x) (car (ua-pattern (car l))))
	       (eq (cadr x) (cadr (ua-pattern (car l)))))
	  (setq nl (cons (caddr (ua-pattern (car l))) nl)))))


(defun ex-lookup (p)
  (worldmark)
  (do ((l (ua-getfacts (car p)) (cdr l)) (dum)) ((null l))
      (if (and (cntp (car l)) (setq dum (matchp p (car l))))
	  (return dum))))

(defun ex-lookups (p) 
  (worldmark)
  (do ((l (ua-getfacts (car p)) (cdr l)) (dum) (nl))
      ((null l) (nreverse nl))
      (if (and (cntp (car l)) (setq dum (matchp p (car l))))
	  (setq nl (cons dum nl)))))

(defun matchp (p x)
  (let (pp px xp xx) (if (setq x (mgupx p x)) (vars p truth))))

(defun mgupx (p x)
  (cond ((unvarp p) (mguvarpx p x))
	((unvarp x) (mguvarxp x p))
	((atom p) (eq p x))
	(t (do ((l p (cdr l)) (m (ua-pattern x) (cdr m))) (nil)
	       (cond ((null l) (return (null m)))
		     ((null m) (return nil))
		     ((mgupx (car l) (car m)))
		     (t (return nil)))))))

(defun mgupp (p x)
  (cond ((eq p x))
	((unvarp p) (mguvarpp p x))
	((unvarp x) (mguvarpp x p))
	((or (atom p) (atom x)) nil)
	(t (do ((l p (cdr l)) (m x (cdr m))) (nil)
	       (cond ((null l) (return (null m)))
		     ((null m) (return nil))
		     ((mgupp (car l) (car m)))
		     (t (return nil)))))))

(defun mguxx (p x)
  (cond ((eq p x))
	((unvarp p) (mguvarxx p x))
	((unvarp x) (mguvarxx x p))
	((or (not (setq p (ua-pattern p))) (not (setq x (ua-pattern x)))) nil)
	(t (do ((l p (cdr l)) (m x (cdr m))) (nil)
	       (cond ((null l) (return (null m)))
		     ((null m) nil)
		     ((mguxx (car l) (car m)))
		     (t (return nil)))))))


(defun mguvarpx (vr vl)
  (progb ((dum (assq vr px)))
    (cond ((null dum) (setq px (pset vr vl px)))
	  (t (mguxx (cdr dum) vl)))))

(defun mguvarxp (vr vl)
  (progb ((dum (assq vr xp)))
    (cond ((null dum) (setq xp (pset vr vl xp)))
	  (t (mgupp (cdr dum) vl)))))

(defun mguvarpp (vr vl)
  (progb ((dum (assq vr pp)))
    (cond ((null dum) (setq pp (pset vr vl pp)))
	  (t (mgupx vl (cdr dum))))))

(defun mguvarxx (vr vl)
  (progb ((dum (assq vr xx)))
    (cond ((null dum) (setq xx (pset vr vl xx)))
	  (t (mgupx (cdr dum) vl)))))

(defun vars (p al)
  (cond ((unvarp p) (if (cdr (assq p al)) al (pset p (plugp p) al)))
	((atom p) al)
	(t (do l p (cdr l) (null l) (setq al (vars (car l) al))) al)))

(defun plugp (p)
  (cond ((unvarp p)
	 (progb (dum)
	   (cond ((setq dum (assq p pp)) (plugp (cdr dum)))
		 ((setq dum (assq p px)) (plugx (cdr dum)))
		 (t p))))
	((atom p) p)
	(t (mapcar 'plugp p))))

(defun plugx (p)
  (cond ((unvarp p)
	 (progb (dum)
	   (cond ((setq dum (assq p xp)) (plugp (cdr dum)))
		 ((setq dum (assq p xx)) (plugx (cdr dum)))
		 (t p))))
	((atom (ua-pattern p)) p)
	(t (mapcar 'plugx (ua-pattern p)))))

(defun subrplac (al x)
  (cond ((atom x) (if (setq al (assq x al)) (cdr al) x))
	(t (mapcar '(lambda (l) (subrplac al l)) (ua-pattern x)))))

(defun mtcheval (p al)
  (cond ((varp p)
	 (progb ((dum (assq p al)))
	   (cond ((null dum) p)
		 (t (mtcheval (cdr dum) al)))))
	((atom (ua-pattern p)) p)
	(t (mapcar '(lambda (l) (mtcheval l al)) (ua-pattern p)))))


(defun pr-getfacts (n)
  (worldmark)
  (do ((l (ua-getfacts n) (cdr l)) (nl))
      ((null l) nl)
      (if (cntp (car l)) (setq nl (cons (car l) nl)))))


(defun ut-activate n 
  (do i 1 (1+ i) (> i n)
      (if (memq (arg i) activetheories) nil
	  (setq activetheories (cons (arg i) activetheories))
	  (cmark (arg i)))))

(defun ut-deactivate n 
  (do i 1 (1+ i) (> i n)
      (if (not (memq (arg i) activetheories)) nil
	  (cunmrk (arg i))
	  (setq activetheories (delq (arg i) activetheories)))))

(defun ut-subworld (t1 t2)
  (cunmrk current)
  (put t1 (addq t2 (get t1 'subworld)) 'subworld)
  (cmark current))

(defun ut-unsubworld (t1 t2)
  (cunmrk current)
  (put t1 (delq t2 (get t1 'subworld)) 'subworld)
  (cmark current))

(defun ut-kill (con)
  (do l (get con 'contents) (cdr l) (null l)
      (ifn (kcntxt (car l) con) (pr-unindex (car l))))
  (setq activetheories (delq con activetheories))	;added by Jay 7/25
  (rem con 'contents)
  (rem con 'cmark)
  (rem con 'subworld) t)

(defun ut-empty (con)
  (do l (get con 'contents) (cdr l) (null l)
      (ifn (kcntxt (car l) con) (pr-unindex (car l)))))

(defun ut-newtheory n (newcon (listify n)))

(defun newcon (c)
  (if (> conindex connumber) (gccon))
  (setq c (if (null c) (list '*gc nil) (list '*gc nil 'subworld c)))
  (store (conunmrk conindex) c) (store (conmark conindex) (cdr c))
  (setq conindex (1+ conindex))  c)

(defun gccon () (gccon1)
  (cond ((<= conindex connumber))
	((progn (gc) (gccon1)))
	((<= conindex connumber))
	(t (terpri) (princ '|too many worlds.|) (err))))

(defun gccon1 ()
  (setq conindex 0)
  (do i 0 (1+ i) (> i connumber)
      (if (not (eq (conmark i) (cdr (conunmrk i)))) (killc (conmark i))
	  (store (conunmrk conindex) (conunmrk i))
	  (store (conmark conindex) (conmark i))
	  (setq conindex (1+ conindex)))))

(defun cntxt (dat con)
  (if (not (atom con)) (setq con (cdr con)))
  (if (memq con (get dat 'myworld)) dat
      (put con (cons dat (get con 'contents)) 'contents)
      (put dat (cons con (get dat 'myworld)) 'myworld)
      dat))

(defun kcntxt (dat con)
  (if (not (atom con)) (setq con (cdr con)))
  (put con (delq dat (get con 'contents)) 'contents)
  (put dat (delq con (get dat 'myworld)) 'myworld))

(defun cntp (x)
  (do l (get x 'myworld) (cdr l) (null l)
      (if (and (setq x (get (car l) 'cmark)) (> x 0)) (return t))))

(defun worldmark ()
  (progb (con)
    (setq con (if (atom currenttheory) currenttheory (cdr currenttheory)))
    (if (eq current con) t
	(cunmrk current) (setq current con) (cmark con))))

(defun cmark (con)
  (ifn (atom con) (setq con (cdr con)))
  (progb ((cm (get con 'cmark)))
    (put con (if cm (1+ cm) 1) 'cmark)
    (mapc 'cmark (get con 'subworld))))

(defun cunmrk (con)
  (ifn (atom con) (setq con (cdr con)))
  (progb ((cm (get con 'cmark)))
    (if cm (put con (1- cm) 'cmark))
    (mapc 'cunmrk (get con 'subworld))))

