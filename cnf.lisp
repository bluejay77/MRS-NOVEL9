;;; -*- Mode: Common-LISP -*-
;;;
;;;CNF.LSP    0.5 18-Mar-83 0125 000   1 ML E      18-Mar-83   
;;;	CNF and DNF (conjunctive normal form?)
;;;
;;;perm filename CNF.LSP[MRS,LSP] blob sn#702114 filedate 1983-03-18
;;;	generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00004 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00004 00003
;;;C00006 00004
;;;C00009 ENDMK
;;;C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; AJY 2015-0510
;;;
;;; ------------------------------------------------------------
;;;







(defun cnf-assert (p)
  (do ((l (cnf p) (cdr l)))
      ((null l) 'done)
      (if (null (cdar l)) (stash (caar l))
	  (mrs-assert-ot (cons 'or (car l))))))

(defun mrs-assert-ot (p)
  (setq p (cdr p))
  (do ((l p (cdr l)))
      ((null l))
    (stash (cons 'or p))
    (setq p (rotate p))))

(defun rotate (l) (append (cdr l) (list (car l))))


(defun cnf (p)
  (cond ((atom p) (list (list p)))
	((eq 'not (car p))
	 (mapc '(lambda (l) (maplac (function (lambda (m) (maknot m))) l))
	       (dnf (cadr p))))
	((eq 'and (car p))
	 (do ((l (cdr p) (cdr l)) (nl))
	     ((null l) nl)
	     (setq nl (cnfand (cnf (car l)) nl))))
	((eq 'or (car p))
	 (do ((l (cdr p) (cdr l)) (nl))
	     ((null l) nl)
	     (setq nl (cnfor (cnf (car l)) nl))))
	((eq 'if (car p))(cnf (list 'or (list 'not (cadr p)) (caddr p))))
	((eq 'iff (car p))
	 (cnf (list 'and (list 'or (list 'not (cadr p)) (caddr p))
		         (list 'or (cadr p) (list 'not (caddr p))))))
	(t (list (list p)))))

(defun cnfand (c d) (mrs-merge c d))

(defun cnfor (c d)
  (cond ((null c) d)
	((null d) c)
	(t (do ((res)) ((null c) res)
	       (do ((m d (cdr m)))
		   ((null m))
		 (setq res (addm (cerge (car c) (car m)) res)))
	     (setq c (cdr c))))))


(defun dnf (pat)
  (cond ((atom pat) (list (list pat)))
	((eq 'not (car pat))
	 (mapc '(lambda (l) (maplac (function (lambda (m) (maknot m))) l))
	       (cnf (cadr pat))))
	((eq 'and (car pat))
	 (do ((l (cdr pat) (cdr l)) (nl))
	     ((null l) nl)
	     (setq nl (dnfand (dnf (car l)) nl))))
	((eq 'or (car pat))
	 (do ((l (cdr pat) (cdr l)) (nl))
	     ((null l) nl)
	     (setq nl (dnfor (dnf (car l)) nl))))
	((eq 'if (car pat))(dnf (list 'or (list 'not (cadr pat)) (caddr pat))))
	((eq 'iff (car pat))
	 (dnf (list 'and (list 'or (list 'not (cadr pat)) (caddr pat))
		         (list 'or (cadr pat) (list 'not (caddr pat))))))
	(t (list (list pat)))))

(defun dnfand (c d)
  (cond ((null c) d)
	((null d) c)
	(t (do ((res)) ((null c) res)
	       (do ((m d (cdr m)))
		   ((null m))
		 (setq res (addm (cerge (car c) (car m)) res)))
	     (setq c (cdr c))))))

(defun dnfor (c d) (mrs-merge c d))


(defun mrs-merge (l m)
  (cond ((null l) m)
	((null m) l)
	(t (let ((beg) (end) (oth))
	     (if (lsp (car l) (car m)) (setq beg l end l oth m)
		 (setq beg m end m oth l))
	     (do ((dum)) ((null (cdr end)) (rplacd end oth) beg)
	       (if (lsp (cadr end) (car oth))
		   (setq end (cdr end))
		   (progn
		     (setq dum (cdr end))
		     (rplacd end oth)
		     (setq end oth oth dum))))))))

;;; Copy list
;;;

(defun copyl (x)
  (append x nil))

(defun cerge (l m) (mrs-merge (copyl l) (copyl m)))

(defun addm (x l)
  (cond ((null l) (list x))
	((equal x (car l)) l)
	((lsp x (car l)) (cons x l))
	(t (do ((m l (cdr m)))
	       ((null (cdr m)) (rplacd m (list x)))
	       (cond ((equal x (cadr m)) (return m))
		     ((lsp x (cadr m)) (return (rplacd m (cons x (cdr m)))))))
	   l)))

(defun lsp (x y) (< (sxhash x) (sxhash y)))

