
perm filename DESCRI.LSP[MRS,LSP] blob sn#702118 filedate 1983-03-18 generic text, type T, neo UTF8

;;; This file contains the $describe package for mrs.  It builds
;;; an index of the positions (and files) of forms like:
;;;	(DESCRIPTION <key> <text>)
;;; and puts them on a list pointed to by $describe-position-alist.

;;; The text is just long atom names that should be thrown out
;;; after they are read.
(gctwa t)

(eval-when (compile)
  (special $describe-position-alist))

(defun $describe (a)
  (let ((position (assq a $describe-position-alist)))
    (cond (position (apply 'filepos (cdr position))
		    (princ (caddr (read (cadr position))))
		    (terpri))
	  (t (princ '|No description.|)
	     (terpri)))))

;;; This reads a file and returns an index for it.
(defun read-dat-file (file)
  (let ((handle (open file '(in ascii block))))
    (do ((position 0 (filepos handle))
	 (descr (read handle) (read handle))
	 (index))
	((eq descr 'stop) index)
      (cond ((eq (car descr) 'description)
	     (setq index (cons (cons (cadr descr) (list handle position))
			       index)))))))

(setq $describe-position-alist (read-dat-file '|mrs:describe.dat|))

