;;; -*- Mode: Common-LISP -*-
;;;
;;;OUTPUT.LSP    1.5 18-Mar-83 0140 000   1 ML E      18-Mar-83   
;;;	First page is same as ASK.LSP, the rest is the NAt LAng Interface
;;;
;;;perm filename OUTPUT.LSP[MRS,LSP] blob sn#702131 filedate
;;;	1983-03-18 generic text, type C, neo UTF8 
;;;
;;;COMMENT ⊗   VALID 00004 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002
;;;C00005 00003
;;;C00020 00004
;;;C00028 ENDMK
;;;C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-10
;;;
;;; ------------------------------------------------------------
;;;




#|
(declare (special path vars))
(declare (special truth theory))
(declare (special outbuf nested))
|#

(defun ask (p)
  (princ (output p)) (terpri)
  (cond ((groundp p)
	 (princ '|True or false?|) (terpri)
	 (yesorno))
	(t (princ '|Give some binding lists for which this is true.|) (terpri)
	   (princ '|Enter one per line and type "false" when done.|) (terpri)
	   (hear p))))

(defun yesorno ()
  (do ((ans (read) (read)))
      (nil) ; Infinite loop
      (cond ((memq ans '(t tr tru true y ye yes)) (return truth))
	    ((memq ans '(f fa fal fals false n no)) (return nil))
	    ((memq ans '(u un unk unkn unkno unknow unknown)) (return nil))
	    (t (princ '|Please type "true", "false", or "unknown"|)(terpri)))))

(defun hear (p)
  (do ((ans (read) (read)))
      (nil) ; Infinite loop
      (cond ((memq ans '(f fa fal fals false n no)) (return nil))
	    ((and (not (atom ans))
		  (mapand #'(lambda (l) (or (eq 't (car l)) (varp (car l))))
			  ans))
	     (stash (plug p ans))
	     (if (note ans) (return t)))
	    (t (princ '|Please type a binding list.|) (terpri)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;								    	;;;
;;;		Natural Language output package for MRS			;;;
;;;								    	;;;
;;;			(MACLISP and FRANZ-LISP)			;;;
;;;								    	;;;
;;;			    Terry  A  Barnes				;;;
;;;								    	;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








(defmacro mrsconcat (&rest x)
  `(implode (mapcan 'explodec
		    (mapcar 'eval ',x))))


(defmacro mrsprint (&rest x)
  `(setf outbuf (cons ,(car x) outbuf)))


(defmacro mrswrite (&rest x)
  `(do ((tb-unique ,x (cdr tb-unique))
	(prints))
       ((null tb-unique)
	(append '(progn nil) (reverse prints)))
     (cond ((atom (car tb-unique))
	    (setq prints (cons `(mrsprint ',(car tb-unique)) prints)))
		 (t (setq prints (cons `(toutput ,(car tb-unique)) prints))))
     (cond ((and (cdr tb-unique)
		 (not (or (stringp (car tb-unique))
			  (stringp (cadr tb-unique)))))
	    (setq prints (cons '(mrsprint " ") prints))))))



(defmacro mrspwrite (&rest x)
  `(do ((tb-unique ,x (cdr tb-unique))
	(prints))
       ((null tb-unique)
	(append '(progn nil) (reverse prints)))
     (cond ((atom (car tb-unique))
	    (setq prints (cons `(mrsprint ',(car tb-unique)) prints)))
	   (t (setq prints (cons `(poutput ,(car tb-unique)) prints))))
     (cond ((and (cdr tb-unique)
		 (not (or (stringp (car tb-unique))
			  (stringp (cadr tb-unique)))))
	    (setq prints (cons '(mrsprint " ") prints))))))


(defun output (p) ($poutput p))

(defun $poutput (p)
       (let (nested outbuf)
	    (poutput (unvarize p))
	    (princ (makesentence outbuf))
	    (terpri)))



(defun $toutput (p)
       (let (nested outbuf)
	    (toutput (unvarize p))
	    (princ (implode (mapcan 'exploden (nreverse outbuf))))
	    (terpri)))



(defun poutput (p)
       (cond ((atom p) (mrsprint p))
	     (t (kb 'topoutput p))))


(defun toutput (p)
       (cond ((atom p)
	      (cond ((testf p '$proper)
		     (mrsprint (capitalize p)))
		    ((testf p '$noun)
		     (cond ((memq (getchar p 1) '(a e i o u))
			    (mrswrite an " "))
			   (t (mrswrite a " ")))
		     (mrsprint p))
		    (t (mrsprint p))))
	     (t (kb 'totoutput p))))



(defun makesentence (l)
       (setq l (mapcan 'exploden (nreverse l)))
       (implode (nconc (cond ((and (> (car l) (1- #\a))
				   (< (car l) (1+ #\z)))
			      (rplaca l (- (car l) (- #\a #\A))))
			     (t l))
		       (ncons #\.))))


(defun $ltemplate (prop pattern)
       (let ((fname (mrsconcat 'poutput "-" (gensym))))
	    ($assert `(topoutput ,prop ,fname))
	    (eval (append `(defun ,fname (x))
			  (makeprints 'poutput
				       (sublis (findvars prop 'x)
					       pattern))))))


(defun $ptemplate (prop pattern)
       (let ((fname (mrsconcat 'poutput "-" (gensym))))
	    ($assert `(topoutput ,prop ,fname))
	    (eval (append `(defun ,fname (x))
			  (makeprints 'toutput
				       (sublis (findvars prop 'x)
					       pattern))))))


(defun $ttemplate (term pattern)
       (let ((fname (mrsconcat 'toutput "-" (gensym))))
	    ($assert `(totoutput ,term ,fname))
	    (eval (append `(defun ,fname (x))
			  (makeprints 'toutput
				       (sublis (findvars term 'x)
					       pattern))))))


(defun makeprints (routine args)
       (do ((l (reverse args) (cdr l))
	    (arg)  (prints))
	   ((null l) prints)
	   (setq arg (car l))
	   (cond ((stringp arg)
		  (setq prints (cons `(mrsprint ,arg) prints)))
		 ((atom arg)
		  (setq prints (cons `(mrsprint ',arg) prints)))
		 (t (setq prints (cons `(,routine ,arg)
				       prints))))
	   (cond ((and (cdr l)
		       (not (or (stringp arg)
				(stringp (cadr l)))))
		  (setq prints (cons `(mrsprint " ") prints))))))



(defun capitalize (word)
       (let ((letters (exploden word)))
	    (cond ((and (> (car letters) (1- #\a))
			(< (car letters) (1+ #\z)))
		   (implode (rplaca letters (- (car letters) (- #\a #\A)))))
		  (t word))))



(defun flatten (p)
       (cond ((null p) nil)
	     ((atom p) (ncons p))
	     (t (nconc (flatten (car p)) (flatten (cdr p))))))



(defun drop$? (var)
       (implode (cdr (explode var))))



(defun unvarize (p)
  (do ((l (flatten p) (cdr l))
       (ex)  (un))
      ((null l)
       (setq p (sublis (append ex un) p))
       (setq l (cond (ex `(existout ,(nreverse (mapcar 'cdr ex)) ,p))
		     (t p)))
       (cond (un `(allout ,(nreverse (mapcar 'cdr un)) ,l))
	     (t l)))
    (cond ((and (unvarp (car l))
		(not (assq (car l) un)))
	   (setq un (cons (cons (car l) (drop$? (car l)))
			  un)))
	  ((and (exvarp (car l))
		(not (assq (car l) ex)))
	   (setq ex (cons (cons (car l) (drop$? (car l)))
			  ex))))))


;;; Here there is a true error there.
;;;

#|
(mapc '$assert
       '(
	 (topoutput $x poutput-reln)
	 (topoutput (prop $x $y) poutput-rel1)

	 (topoutput (prop ↑and *p) poutput-pcon)
	 (topoutput (prop ↑or *p) poutput-pcon)
	 (topoutput (prop ↑allout $x $y) poutput-all)
	 (topoutput (prop ↑existout $x $y) poutput-exist)
	 (topoutput (prop ↑if $x $y) poutput-if)
	 (topoutput (prop ↑not $x) poutput-not)
	 (topoutput (prop ↑= $x $y) poutput-rel-infix)
	 (topoutput (prop ↑> $x $y) poutput-rel-infix)
	 (topoutput (prop ↑< $x $y) poutput-rel-infix)


	 (totoutput $x toutput-pfun)
	 (totoutput (prop $x) toutput-const)
	 (totoutput (prop ↑- $x $y) toutput-pinfix)
	 (totoutput (prop ↑+ *x) toutput-pinfix)
	 (totoutput (prop ↑* *x) toutput-pinfix)
	))
|#


(defun prinlist (prop)		; (term1 term2 term3 ...)
  (toutput (car prop)) 
  (cond ((cdr prop)
         (do ((l (cdr prop) (cdr l)))
             ((null (cdr l)))
           ;; (mrswrite " " (car l))
           )
         ;; (mrswrite ", " (car l))
         )))



(defun prinplist (prop)		; (prop1 prop2 prop3 ...)
  (poutput (car prop))
  (cond ((cdr prop)
         (do ((l (cdr prop) (cdr l)))
             ((null (cdr l)))
           ;; (mrspwrite " " and (car l))
           )
         ;; (mrspwrite ", " (car l))
         )))



(defun poutput-con (prop)	; (and <p> <q> <r> ...)
  (poutput (cadr prop))
  (do ((l (cddr prop) (cdr l)))
      ((null l))
    ;; (mrspwrite " " (car prop) (car l))
    ))




(defun poutput-pcon (prop)	; (and <p> <q> <r> ...) grouped with parens
  (cond (nested (mrsprint "(")
                (poutput (cadr prop))
                (do ((l (cddr prop) (cdr l)))
			 ((null l))
                  ;; (mrspwrite " " (car prop) (car l))
                  )
                ;; (mrsprint ")")
                )
        (t (setq nested t)
           (poutput-con prop))))


#|
(defun poutput-all (prop)	; (allout (<vars>) <p>)
       (mrswrite for every " ")
       (prinlist (cadr prop))
       (mrspwrite " : " (caddr prop)))



(defun poutput-exist (prop)	; (existout (<vars>) <p>)
       (mrswrite for some " ")
       (prinlist (cadr prop))
       (mrspwrite " : " (caddr prop)))



(defun poutput-if (prop)	; (if <p> <q>)
       (mrspwrite if (cadr prop) then (caddr prop)))



(defun poutput-not (prop)	; (not <p>)
       (mrspwrite it is not the case that (cadr prop)))



(defun poutput-reln (prop)	; (color clyde grey)
       (let (value)		; (between item left right)
	    (cond ((and (testf (car prop) '$verb)
			(= (length prop) 3))
		   (poutput-rel-infix prop))
		  (t (setq value (car (last prop)))
		     (setq prop (reverse (cdr (reverse prop))))
		     (mrswrite the (car prop) of " ")
		     (prinlist (cdr prop))
		     (mrswrite " " is " ")
		     (mrsprint value)))))



(defun poutput-rel1 (prop)	; (blue sky)
       (mrswrite (cadr prop) is (car prop)))



(defun poutput-rel-infix (prop)		; (loves x y)
       (mrswrite (cadr prop) (car prop) (caddr prop)))



(defun toutput-infix (prop)	; (+ a b c ...)
       (toutput (cadr prop))
       (do ((l (cddr prop) (cdr l)))
	   ((null l))
	   (mrswrite " " (car prop) (car l))))


(defun toutput-pinfix (prop)		; (+ a b c ...) grouped with parens
       (cond (nested (mrsprint "(")
		     (toutput (cadr prop))
		     (do ((l (cddr prop) (cdr l)))
			 ((null l))
			 (mrswrite " " (car prop) (car l)))
		     (mrsprint ")"))
	     (t (setq nested t)
		(toutput-infix prop))))



(defun toutput-posessive (prop)		; (father jenny)
       (mrswrite (cadr prop) "'" s (car prop)))



(defun toutput-pfun (prop)	; (child jane third)
       (cond ((and nested (cddr prop))
	      (mrswrite "[ " the (car prop) of " ")
	      (prinlist (cdr prop))
	      (mrswrite " ]"))
	     (t (setq nested t)
		(toutput-fun prop))))


(defun toutput-fun (prop)	; (child jane)
       (mrswrite the (car prop) of " ")
       (prinlist (cdr prop)))



(defun toutput-const (prop)	; (skolem)
       (toutput (car prop)))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;									;;;
;;;    These are the utilities for the MRS natural language package.	;;;
;;;									;;;
;;;			(FRANZ LISP and MACLISP)			;;;
;;;									;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmacro myconcat (&rest x)
  `(implode (mapcan 'explodec
		    (mapcar 'eval ',x))))

;;; This is there only in the Franzl
#|

(defun getf (x) (cond ((symbolp x)
		       (get x 'features))))
|#


(defun testf (node features)
       (cond ((null features))
	     ((atom features)
	      (memq features (getf node)))
	     (t (do ((l features (cdr l))
		     (node-features (getf node)))
		    ((null l) t)
		    (cond ((not (memq (car l) node-features))
			   (return nil)))))))



(defun add-feature (word feature)
       (cond ((not (memq feature (get word 'features)))
	      (putprop word
		       (cons feature (get word 'features))
		       'features))))



(defmacro proper (&rest pnouns)
  (cond ((null pnouns)
	 (princ "This function defines words as proper nouns.")
	 (terpri))
	(t (do ((l pnouns (cdr l)))
	       ((null l) pnouns)
	     (add-feature (car l) '$proper)
	     (add-feature (car l) '$noun)))))



(defmacro noun (&rest nouns)
  `(cond ((null ,nouns)
	  (princ "This function defines words as common nouns.")
	  (terpri))
	 (t (do ((l ,nouns (cdr l)))
		((null l) ,nouns)
	      (add-feature (car l) '$noun)))))



(defmacro adjective (&rest adjectives)
  `(cond ((null ,adjectives)
	  (princ "This function defines words as adjectives.")
	  (terpri))
	 (t (do ((l ,adjectives (cdr l)))
		((null l) ,adjectives)
	      (add-feature (car l) '$adjective)))))



(defmacro verb (&rest verbs)
  `(cond ((null ,verbs)
	  (princ "This function defines words as verbs.")
	  (terpri))
	 (t (do ((l ,verbs (cdr l)))
		((null l) ,verbs)
	      (add-feature (car l) '$verb)))))



(defmacro aux (&rest auxs)
  `(cond ((null ,auxs)
	  (princ "This function defines words as auxillary verbs.")
	  (terpri))
	 (t (do ((l ,auxs (cdr l)))
		((null l) ,auxs)
	      (add-feature (car l) '$aux)
	      (add-feature (car l) '$verb)))))



(defmacro determiner (&rest determiners)
  `(cond ((null ,determiners)
	  (princ "This function defines words as determiners.")
	  (terpri))
	 (t (do ((l ,determiners (cdr l)))
		((null l) ,determiners)
	      (add-feature (car l) '$determiner)))))



(defmacro preposition (&rest prepositions)
  `(cond ((null ,prepositions)
	  (princ "This function defines words as prepositions.")
	  (terpri))
	 (t (do ((l ,prepositions (cdr l)))
		((null l) ,prepositions)
	      (add-feature (car l) '$preposition)))))



(defmacro keyword-r (&rest keywords)
  `(cond ((null ,keywords)
	  (princ "This function allows the parser to recognize words.")
	  (terpri))
	 (t (do ((l ,keywords (cdr l)))
		((null l) ,keywords)
	      (add-feature (car l) (car l))))))




(defmacro rephrase (&rest patterns)
  `(let ((oldpat (car ,patterns))
	 (newpat (cadr ,patterns)))
     (cond ((atom oldpat)
	    (setq oldpat (list oldpat))))
     (cond ((atom newpat)
	    (setq newpat (list newpat))))
     (cond ((not (symbolp (car oldpat)))
	   (setq oldpat (cons (implode (explodec (car oldpat)))
			      (cdr oldpat)))))
     (setf (get (car oldpat)
		'rephrases
		(cons (list oldpat newpat)
		      (get (car oldpat) 'rephrases))))))



(defmacro root (&rest pair)
  `(cond ((null ,pair)
	  (princ "This function replaces words by their root words.")
	  (terpri))
	 ((and (= (length pair) 2)
	       (symbolp (car ,pair))
	       (symbolp (cadr ,pair)))
	  (putprop (car ,pair) (cadr pair) 'root))
	 (t (princ "Improper arguments to root.")
	    (terpri))))



(defun findvars (prop loc)
       (let (path vars)
	    (findvars1 prop loc)
	    vars))


(defun findvars1 (p loc)
       (let ((path path))
	    (cond ((atom p)
		   (cond ((and (eq (getchar p 1) '$)
			       (not (assq p vars)))
			  (setq vars (cons (cons p (makepath path loc))
					   vars)))))
		  (t (setq path (cons 'a path))
		     (findvars1 (car p) loc)
		     (setq path (cons 'd (cdr path)))
		     (findvars1 (cdr p) loc)))))


(defun makepath (path loc)
       (cond ((null path) loc)
	     ((> (length path) 4)
	      (list (myconcat 'c
				(car path)
				(cadr path)
				(caddr path)
				(cadddr path)
				'r)
		    (makepath (cddddr path) loc)))
	     (t (list (myconcat 'c (implode path) 'r)
		      loc))))

