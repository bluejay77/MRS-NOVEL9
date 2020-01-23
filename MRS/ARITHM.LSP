
perm filename ARITHM.LSP[MRS,LSP] blob sn#702107 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00003 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00005 00003
C00007 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile) 
	   (load '|macros.fasl|)
	   (impvar truth))

(pr-stash '(tostash (prop ↑lisp $x $s) pl-stash))

(pr-stash '(togetval (prop ↑+ $x $y) getval-arith))
(pl-stash '(lisp + plus))

(pr-stash '(togetval (prop ↑- $x) getval-arith))
(pr-stash '(togetval (prop ↑- $x $y) getval-arith))
(pl-stash '(lisp - difference))

(pr-stash '(togetval (prop ↑* $x $y) getval-arith))
(pl-stash '(lisp * times))

(pr-stash '(togetval (prop ↑// $x $y) getval-arith))
(pl-stash '(lisp // quotient))
#+franz(pr-stash '(togetval (prop ↑/ $x $y) getval-arith)) ;milt 2/25/83
#+franz(pl-stash '(lisp / quotient)) ;milt 2/25/83

;;; The following assumes (togetval (prop ↑lisp $x) lookupval)
;;;                   and (tolookupval (prop ↑lisp $x) pl-lookupval)

(defun getval-arith (x)
  (if (atom x) x
      (do ((l (cdr x) (cdr l)) (dum) (nl))
	  ((null l) (apply (get (car x) 'lisp) (nreverse nl)))
	(if (setq dum (getval (car l))) (setq nl (cons dum nl))
	    (return nil)))))


#-franz(pr-stash '(totruep (prop ↑/> $x $y) truep-arith))
#+franz(pr-stash '(totruep (prop ↑\> $x $y) truep-arith))
(pl-stash '(lisp > greaterp))

#-franz(pr-stash '(totruep (prop ↑/< $x $y) truep-arith))
#+franz(pr-stash '(totruep (prop ↑\< $x $y) truep-arith))
(pl-stash '(lisp < lessp))

;;; The following assumes (togetval (prop ↑lisp $x) lookupval)
;;;                   and (tolookupval (prop ↑lisp $x) pl-lookupval)

(defun truep-arith (p)
  (do ((l (cdr p) (cdr l)) (dum) (nl))
      ((null l) (if (apply (get (car p) 'lisp) (nreverse nl)) truth))
      (if (setq dum (getval (car l))) (setq nl (cons dum nl))
	  (return (bs-truep p)))))

