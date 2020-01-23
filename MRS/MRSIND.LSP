
perm filename MRSIND.LSP[MRS,LSP] blob sn#615047 filedate 1981-09-30 generic text, type T, neo UTF8


(defun indexp (p) (let (best (score 999999)) (indexp1 p 1 0) best))

(defun indexp1 (p pos n)
  (cond ((atom p) (setq p (getposp p pos) n (+ (car p) n))
	 (if (< n score) (setq score n best (cdr p))))
	(t (do l p (cdr l) (null l)
	       (indexp1 (car l) (lsh pos 1) (+ n (car (getposp 'un pos))))
	       (setq pos (1+ (lsh pos 1)))))))

(defun getposp (p pos) (cond ((get p pos)) (t '(0))))


(defun index (p) (index1 p (ua-datum p) 1 nil))

(defun index1 (p d pos vl)
  (cond ((unvarp p) (setq vl (getposv pos vl))
		    (do ((l vl (cdr l)))
			((numberp (cadr l)) (rplaca vl (1+ (car vl)))
					    (rplacd l (cons d (cdr l))))
		        (if (eq d (car l)) (return p))))
	((atom p) (setq vl (getpos p pos vl))
		  (do ((l (cdr vl) (cdr l)))
		      ((numberp (car l)) (rplaca vl (1+ (car vl)))
					 (rplacd vl (cons d (cdr vl))))
		      (if (eq d (car l)) (return p))))
	(t (setq vl (getposv pos vl))
	   (do l p (cdr l) (null l)
	       (index1 (car l) d (lsh pos 1) vl)
	       (setq pos (1+ (lsh pos 1)))))))


(defun getpos (p pos vl)
  (cond ((get p pos)) (t (put p (cons 0 (getposv pos vl)) pos))))

(defun getposv (pos vl) (cond ((get 'un pos)) (t (put 'un (cons 0 vl) pos))))

