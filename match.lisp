;;; -*- Mode: Common-LISP -*-
;;;
;;;MATCH.LSP    0.9 18-Mar-83 0137 000   1 ML E      18-Mar-83   
;;;	Matcher, Unifier, PlugP
;;;
;;;perm filename MATCH.LSP[MRS,LSP] blob sn#702127 filedate 1983-03-18
;;;	generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00009 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00003 00003
;;;C00004 00004
;;;C00005 00005
;;;C00007 00006
;;;C00009 00007
;;;C00012 00008
;;;C00014 00009
;;;C00016 ENDMK
;;;C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-02
;;;
;;; ------------------------------------------------------------
;;;


(defvar pp nil)
(defvar px nil)
(defvar xp nil)
(defvar xx nil)
(defvar vp nil)
(defvar vx nil)

(defvar truth '((t. t)))



(defun samep (x y) (samep1 x y truth))

(defun samep1 (x y al)
  (cond ((equal x y) al)
	((varp x)
	 (let ((dum))
	   (cond ((setf dum (assq x al)) ; Is x in the al assoc list?
		  (if (equal y (cdr dum))
		      al)) ; Is it bound to y?  Then return the orig assoc l
		 ((varp y)
		  (pset x y al))))) ; Bind x to y in the al assoc l
	((or (atom x) (atom y))
	 nil)
	((setf al (samep1 (car x) (car y) al))
	 (setf al (samep1 (cdr x) (cdr y) al)))))


(defun unifyp (p q) (mgu p q truth))

(defun mgu (x y al)
  (cond ((eq x y) al)
	((unvarp x) (mguvar x y al))
	((unvarp y) (mguvar y x al))
	((or (atom x) (atom y)) (if (equal x y) al))
	((setq al (mgu (car x) (car y) al)) (mgu (cdr x) (cdr y) al))))

(defun mguvar (p q al)
  (cond ((assq p al)
	 (setf q al))
	((not (mguchk p q al))
	 (pset p q al))))

(defun mguchk (p q al)
  (cond ((eq p q))
	((unvarp q) (mguchk p (cdr (assq q al)) al))
	((atom q) nil)
	((or (mguchk p (car q) al) (mguchk p (cdr q) al)))))


(defun matchp (p x)
  (let ((pp)
	(px)
	(xp)
	(xx)
	(vp truth)
	(vx truth))
    (cond ((mgupx p x)
	   (do ((l pp (cdr l)))
	       ((null l))
	     (or (assq (caar l) vp)
		 (setf vp (pset (caar l) (plugp (cdar l)) vp))))
	   (do ((l px (cdr l)))
	       ((null l))
	     (or (assq (caar l) vp)
		 (setf vp (pset (caar l) (plugx (cdar l)) vp))))
	   vp))))


(defun plugp (p)
  (cond ((unvarp p)
	 (let ((dum))
	   (cond ((setq dum (assq p vp)) (cdr dum))
		 ((setq dum (assq p pp))
		  (cdar (setq vp (pset p (plugp (cdr dum)) vp))))
		 ((setq dum (assq p px))
		  (cdar (setq vp (pset p (plugx (cdr dum)) vp))))
		 (t p))))
	((atom p) p)
	(t (cons (plugp (car p)) (plugp (cdr p))))))

(defun plugx (p)
  (cond ((unvarp p)
	 (let ((dum))
	   (cond ((setq dum (assq p vx)) (cdr dum))
		 ((setq dum (assq p xp))
		  (cdar (setq vx (pset p (plugp (cdr dum)) vx))))
		 ((setq dum (assq p xx))
		  (cdar (setq vx (pset p (plugx (cdr dum)) vx))))
		 (t (cdar (setq vx (pset p (newvar) vx)))))))
	((atom p) p)
	(t (cons (plugx (car p)) (plugx (cdr p))))))

(defun newvar ()
  (let ((s))
    (setq s (maksym '$))
    (setf (get s 'variable) 'un) s))


(defun mgupx (p x)
  (cond ((unvarp x) (mguvarxp x p))
	((unvarp p) (mguvarpx p x))
	((atom p) (or (equal p x) (mguspec x p 'xp)))
	((atom x) (mguspec p x 'px))
	((mgupx (car p) (car x)) (mgupx (cdr p) (cdr x)))))

(defun mgupp (p x)
  (cond ((eq p x))
	((unvarp p) (mguvarpp p x))
	((unvarp x) (mguvarpp x p))
	((atom p) (or (equal p x) (mguspec x p 'pp)))
	((atom x) (mguspec p x 'pp))
	((mgupp (car p) (car x)) (mgupp (cdr p) (cdr x)))))

(defun mguxx (p x)
  (cond ((eq p x))
	((unvarp p) (mguvarxx p x))
	((unvarp x) (mguvarxx x p))
	((atom p) (or (equal p x) (mguspec x p 'xx)))
	((atom x) (mguspec p x 'xx))
	((mguxx (car p) (car x)) (mguxx (cdr p) (cdr x)))))


(defun mguvarpp (vr vl)
  (let ((dum))
    (cond ((setq dum (assq vr pp)) (mgupp (cdr dum) vl))
	  ((setq dum (assq vr px)) (mgupx vl (cdr dum)))
	  ((not (mguchkpp vr vl)) (setq pp (pset vr vl pp))))))

(defun mguvarpx (vr vl)
  (let ((dum))
    (cond ((setq dum (assq vr px)) (mguxx (cdr dum) vl))
	  ((setq dum (assq vr pp)) (mgupx (cdr dum) vl))
	  ((not (mguchkpx vr vl)) (setq px (pset vr vl px))))))

(defun mguvarxp (vr vl)
  (let ((dum))
    (cond ((setq dum (assq vr xp)) (mgupp (cdr dum) vl))
	  ((setq dum (assq vr xx)) (mgupx vl (cdr dum)))
	  ((not (mguchkxp vr vl)) (setq xp (pset vr vl xp))))))

(defun mguvarxx (vr vl)
  (let ((dum))
    (cond ((setq dum (assq vr xx)) (mguxx (cdr dum) vl))
	  ((setq dum (assq vr xp)) (mgupx (cdr dum) vl))
	  ((not (mguchkxx vr vl)) (setq xx (pset vr vl xx))))))

(defun mguspec (x y al)
  (cond ((unvarp x) (mgusvar x y al))
	((atom x) (equal (pattern x) y))
	((eq 'prop (car x))
	 (do ((l (cdr x) (cdr l))
	      (m (pattern y) (cdr m)))
	     (nil)
	   (cond ((unvarp l) (return (mgusvar l m al)))
		 ((unvarp m) (return (mgusvar m l al)))
		 ((null l) (return (null m)))
		 ((null m) (return nil))
		 ((mguspec (car l) (car m) al))
		 (t (return nil)))))))

(defun mgusvar (x y al)
  (let ((dum))
    (cond ((setf dum (assq x (symeval al)))
	   (equal (pattern (cdr x)) y))
	  (t (set al (pset x (datum y) (symeval al))) t))))


(defun mguchkpp (vr vl)
  (cond ((eq vr vl))
	((unvarp vl)
	 (let ((dum))
	   (cond ((setq dum (assq vl px)) (mguchkpx vr (cdr dum)))
		 ((setq dum (assq vl pp)) (mguchkpp vr (cdr dum))))))
	((atom vl) nil)
	((or (mguchkpp vr (car vl)) (mguchkpp vr (cdr vl))))))

(defun mguchkpx (vr vl)
  (cond ((unvarp vl)
	 (let ((dum))
	   (cond ((setq dum (assq vl xp)) (mguchkpp vr (cdr dum)))
		 ((setq dum (assq vl xx)) (mguchkpx vr (cdr dum))))))
	((atom vl) nil)
	((or (mguchkpx vr (car vl)) (mguchkpx vr (cdr vl))))))

(defun mguchkxp (vr vl)
  (cond ((unvarp vl)
	 (let ((dum))
	   (cond ((setq dum (assq vl px)) (mguchkxx vr (cdr dum)))
		 ((setq dum (assq vl pp)) (mguchkxp vr (cdr dum))))))
	((atom vl) nil)
	((or (mguchkxp vr (car vl)) (mguchkxp vr (cdr vl))))))

(defun mguchkxx (vr vl)
  (cond ((eq vr vl))
	((unvarp vl)
	 (let ((dum))
	   (cond ((setq dum (assq vl xp)) (mguchkxp vr (cdr dum)))
		 ((setq dum (assq vl xx)) (mguchkxx vr (cdr dum))))))
	((atom vl) nil)
	((or (mguchkxx vr (car vl)) (mguchkxx vr (cdr vl))))))


(defun getvar (x al) (plug (cdr (assq x al)) al))

(defun plug (x al) (if (null (cdr al)) x (plug1 x al)))

(defun plug1 (x al)
  (cond ((varp x)
	 (let ((dum))
		(cond ((null (setq dum (assq x al))) x)
		      ((eq x (cdr dum)) x)
		      (t (plug1 (cdr dum) al)))))
	((atom x) x)
	(t (cons (plug1 (car x) al) (plug1 (cdr x) al)))))

(defun plugal (al1 al) 
  (mapcar #'(lambda (l) (cons (car l) (plug (cdr l) al)))
	  al1))

(defun alconc (al1 al2)
  (cond ((null al1) nil)
	((null (cdr al1)) al2)
	(t (do ((l al1 (cdr l)))
	       ((null (cddr l)) (rplacd l al2)))
	   al1)))

(defun alpend (al1 al2)
  (cond ((null al1) nil)
	((null (cdr al1)) al2)
	(t (do () ((null (cdr al1)) al2)
	       (setq al2 (cons (car al1) al2) al1 (cdr al1))))))

