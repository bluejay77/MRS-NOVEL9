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


;(defun tl-stash (p) (put (cadr p) t (car p)))

(defun tl-stash (p) (setf (get (cadr p) (car p)) t))

;(defun tl-unstash (p) (remprop (cadr p) (car p)))

(defun tl-unstash (p) (setf (get (cadr p) (car p)) nil))

(defun tl-lookup (p) (if (get (cadr p) (car p)) truth))




;(defun pl-stash (p) (put (cadr p) (caddr p) (car p)))

(defun pl-stash (p) (setf (get (cadr p) (car p)) (caddr p)))

;(defun pl-unstash (p) (remprop (cadr p) (car p)))

(defun pl-unstash (p) (setf (get (cadr p) (car p)) nil))

;(defun pl-lookup (p) (matchp (caddr p) (get (cadr p) (car p))))

(defun pl-lookup (p) (matchp (caddr p) (get (cadr p) (car p))))

;(defun pl-lookupval (p) (get (cadr p) (car p)))

(defun pl-lookupval (p) (get (cadr p) (car p)))


;(defun dl-stash (p)
;  (put (cadr p) (addq (caddr p) (get (cadr p) (car p))) (car p)))

(defun dl-stash (p)
  (setf (get (cadr p) (car p)) (addq (caddr p) (get (cadr p) (car p)))))

;(defun dl-unstash (p)
;  (put (cadr p) (delq (caddr p) (get (cadr p) (car p))) (car p)))

(defun dl-unstash (p)
  (setf (get (cadr p) (car p)) (delq (caddr p) (get (cadr p) (car p)))))


;(defun dl-lookup (p)
;  (do ((l (get (cadr p) (car p)) (cdr l)) (val (caddr p)) (dum))
;      ((null l))
;      (if (and (setq dum (matchp val (car l))) (note dum)) (return t))))

(defun dl-lookup (p)
  (do ((l (get (car p) (cadr p)) (cdr l)) (val (caddr p)) (dum))
      ((null l))
      (if (and (setq dum (matchp val (car l))) (note dum)) (return t))))


;(defun dl-lookupval (x) (notes (get (cadr x) (car x))))

(defun dl-lookupval (x) (notes (get (car x) (cadr x))))

