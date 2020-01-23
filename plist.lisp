;;;PLIST.LSP    0.2 18-Mar-83 0142 000   1 ML E      18-Mar-83   
;;;	tl-,dl-,pl- lookup,stash, and unstash
;;;
;;;perm filename PLIST.LSP[MRS,LSP] blob sn#702132 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modified this file for the Common LISP:
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-03-18
;;;
;;; ------------------------------------------------------------
;;;

;;; Truth PLIST funcs

(defun tl-stash (p) (setf (get (car p) (cadr p)) t))

(defun tl-unstash (p) (setf (get (car p) (cadr p)) nil))

(defun tl-lookup (p) (if (get (car p) (cadr p)) truth))

;;; Property list funcs

(defun pl-stash (p) (setf (get (car p) (cadr p)) (caddr p)))

(defun pl-unstash (p) (setf (get (car p) (cadr p)) NIL))

(defun pl-lookup (p) (matchp (caddr p) (get (car p) (cadr p))))

(defun pl-lookupval (p) (get (car p) (cadr p)))

;;; Delete list funcs

(defun dl-stash (p)
  (setf (get (car p) (cadr p)) (addq (caddr p) (get (car p) (cadr p)))))

(defun dl-unstash (p)
  (setf (get (car p) (cadr p)) (delq (caddr p) (get (car p) (cadr p)))))

(defun dl-lookup (p)
  (do ((l (get (car p) (cadr p)) (cdr l)) (val (caddr p)) (dum))
      ((null l))
      (if (and (setq dum (matchp val (car l))) (note dum)) (return t))))

(defun dl-lookupval (x) (notes (get (car x) (cadr x))))

