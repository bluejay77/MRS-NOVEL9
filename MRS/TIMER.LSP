
perm filename TIMER.LSP[MRS,LSP] blob sn#702139 filedate 1983-03-18 generic text, type T, neo UTF8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp(load '|macros.fas|)
	   #+franz(load 'macros))

(defun timer fexpr (x)
  (local (time gc form answer number)
    (setq form (car x)
	  number (cond ((numberp (cadr x)) (cadr x)) (t 1))
	  gc (status gctime) time (runtime))
    (do i 1 (1+ i) (> i number) (setq answer (eval form)))
    (setq time (quotient (- (- (runtime) time) (- (status gctime) gc)) 1000.))
    (princ '|Overall time = |) (princ time) (princ '| msec.|) (terpri)
    answer))

