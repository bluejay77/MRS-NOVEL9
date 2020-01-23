
perm filename ASK.LSP[MRS,LSP] blob sn#702108 filedate 1983-03-18 generic text, type T, neo UTF8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp (load '|macros.fasl|)
	   #+franz (load 'macros)
	   (impvar truth theory) (impfun varp groundp plug note stash))

(defun ask (p)
  (princ p) (terpri)
  (cond ((groundp p)
	 (princ '|True or false?|) (terpri)
	 (yesorno))
	(t (princ '|Give some binding lists for which this is true.|) (terpri)
	   (princ '|Enter one per line and type "false" when done.|) (terpri)
	   (hear p))))

(defun yesorno ()
  (do ans (read) (read) nil
      (cond ((memq ans '(t tr tru true y ye yes)) (return truth))
	    ((memq ans '(f fa fal fals false n no)) (return nil))
	    ((memq ans '(u un unk unkn unkno unknow unknown)) (return nil))
	    (t (princ '|Please type "true", "false", or "unknown"|)(terpri)))))

(defun hear (p)
  (do ans (read) (read) nil
      (cond ((memq ans '(f fa fal fals false n no)) (return nil))
	    ((and (not (atom ans))
		  (mapand '(lambda (l) (or (eq 't (car l)) (varp (car l))))
			  ans))
	     (stash (plug p ans))
	     (if (note ans) (return t)))
	    (t (princ '|Please type a binding list.|) (terpri)))))

