
perm filename INTERF.LSP[MRS,LSP] blob sn#702125 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00009 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002
C00004 00003
C00007 00004
C00011 00005
C00013 00006
C00015 00007
C00018 00008
C00021 00009
C00023 ENDMK
C⊗;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1980  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
	   #+maclisp (load '|macros.fas|)
	   #+franz (load 'macros)
	   (special tracegoals traceprops describe-position-alist)
	   (impvar theory advice)
	   (expfun $assert $unassert $truep $trueps $residue $residues
		   $getbdg $getbdgs $getval $getvals
		   $stash $unstash $lookup $lookups $lookupval $lookupvals
		   $achieve $perceive $perceives
		   $defunit $facts $save $load $dump $apropos
		   $traceprop $untraceprop $tracegoal $untracegoal
		   $why $where $describe $demo))

#+franz(setsyntax  '|↑| 'macro '(lambda () (datum (read))))
#+maclisp(setsyntax '|↑| 'macro '(lambda () (datum (read))))
#+lispm(setsyntax '|↑| ':macro '(lambda () (datum (read))))

(defun wna (x)
  (princ '|Wrong number of args to |) (princ x) (princ '|.|) (terpri))

(defun udf (x) 
  (princ x) (princ '| is not available in this version.|) (terpri))

(setq traceprops nil tracegoals nil)


(defun $defunit fexpr (x) (mapc '$assert (cdr x)) (car x))

(defun $assert n
  (cond ((= 1 n) (assert (internal (arg 1) nil nil)))
	((= 2 n) (thassert (internal (arg 1) nil nil) (arg 2)))
	(t (wna '$assert))))

(defun $unassert n
  (cond ((= 1 n) (unassert (internal (arg 1) nil nil)))
	((= 2 n) (thunassert (internal (arg 1) nil nil) (arg 2)))
	(t (wna '$unassert))))

(defun $truep (p) (truep (internal p t nil)))

(defun $trueps (p) (trueps (internal p t nil)))

(defun $residue (p) (residue (internal p t nil)))

(defun $residues (p) (residues (internal p t nil)))

(defun $getbdg (v p) (getbdg v (internal p t nil)))

(defun $getbdgs (v p) (getbdgs v (internal p t nil)))

(defun $getval (x) (getval (internal x t nil)))

(defun $getvals (x) (getvals (internal x t nil)))


(defun $stash n
  (cond ((= 1 n) (stash (internal (arg 1) nil nil)))
	((= 2 n) (thstash (internal (arg 1) nil nil) (arg 2)))
	(t (wna '$stash))))

(defun $unstash n
  (cond ((= 1 n) (unstash (internal (arg 1) nil nil)))
	((= 2 n) (thunstash (internal (arg 1) nil nil) (arg 2)))
	(t (wna '$unstash))))

(defun $lookup (p) (lookup (internal p t nil)))

(defun $lookups (p) (lookups (internal p t nil)))

(defun $lookupval (p) (lookupval (internal p t nil)))

(defun $lookupvals (p) (lookupvals (internal p t nil)))


(defun $achieve (p) (achieve (internal p t nil)))

(defun $perceive (p) (perceive (internal p nil nil)))

(defun $perceives (p) (perceives (internal p nil nil)))


(defun internal (pat flip uns) 
  (cond #-maclisp((stringp pat) pat)
	((symbolp pat)
	 (cond ((= 36. (getcharn pat 1)) (put pat 'un 'variable) pat)
	       ((= 63. (getcharn pat 1)) (put pat 'ex 'variable) pat)
	       ((get pat 'temp))
	       (t pat)))
	((numberp pat) pat)
	((eq 'not (car pat))
	 (list 'not (internal (cadr pat) (not flip) uns)))
	((eq 'if (car pat)) (semif pat flip uns))
	((eq 'iff (car pat))
	 (list 'and (semif pat flip uns)
	            (semif (list 'if (caddr pat) (cadr pat)) flip uns)))
	((eq 'all (car pat))
	 (ifn flip (semall pat flip uns) (semexist pat flip uns)))
	((eq 'exist (car pat))
	 (ifn flip (semexist pat flip uns) (semall pat flip uns)))
	(t (internals pat flip uns))))

(defun internals (pat flip uns)
  (cond ((atom pat) (internal pat flip uns))
	(t (cons (internal (car pat) flip uns)
		 (internals (cdr pat) flip uns)))))

(defun semif (p flip uns)
  (local (lhs rhs)
    (setq lhs (internal (cadr p) (not flip) uns)
	  rhs (internal (caddr p) flip uns))
    (if (not (eq 'and (car rhs))) (list 'if lhs rhs)
	(do ((l (cdr rhs) (cdr l)) (nl))
	    ((null l) (cons 'and (nreverse nl)))
	    (setq nl (cons (list 'if lhs (car l)) nl))))))

(defun semall (p flip uns)
  (prog2 (do l (cdr p) (cdr l) (null (cdr l))
	     (setq uns (cons (unvar (car l)) uns))
	     (put (car l) (car uns) 'temp))
	 (internal (car (last p)) flip uns)
	 (do l (cdr p) (cdr l) (null (cdr l)) (remprop (car l) 'temp))))

(defun semexist (p flip uns)
  (prog2 (do l (cdr p) (cdr l) (null (cdr l))
	     (put (car l) (cons (maksym 'f) uns) 'temp))
	 (internal (car (last p)) flip uns)
	 (do l (cdr p) (cdr l) (null (cdr l)) (remprop (car l) 'temp))))

(defun unvar (x)
  (if (not (eq '$ (getchar x 1))) (setq x (implode (cons '$ (exploden x)))))
  (put x 'un 'variable)
  x)

(defun exvar (x)
  (if (not (eq '? (getchar x 1))) (setq x (implode (cons '? (exploden x)))))
  (put x 'ex 'variable)
  x)


(defun $compile (p) (udf '$compile))

(defun $save n
  (local (f)
    (setq f (open (arg n) 'out))
    (do i 1 (1+ i) (= i n)
	(do l (contents (arg i)) (cdr l) (null l)
	    (prin1 (pattern (car l)) f) (terpri f)))
    (close f)
    'done))

(defun $load (f)
  (setq f (open f 'in))
  (do a (read f nil) (read f nil) (null a) ($assert a))
  (close f)
  'done)

(defun $dump n
  (local (f)
    (setq f (open (arg n) 'out))
    (do i 1 (1+ i) (= i n)
	(do l (contents (arg i)) (cdr l) (null l)
	    (prin1 `(pr-stash ',(pattern (car l))) f) (terpri f)))
    (close f)
    'done))

(defun $facts (x) (prfacts (internal x nil nil)))

(defun prfacts (x)
  (do l (pr-getfacts x) (cdr l) (null l)
      (princ (pattern (car l))) (terpri))
  'done)

(defun $apropos (s)
  (local (nl)
    (mapatoms (function (lambda (l) (if (substringp s l) (setq nl (cons l nl))))))
    nl))

(defun substringp (r s)
  (local (i j)
    (setq i (flatc r) j (flatc s))
    (do k (1+ (- j i)) (1- k) (<= k 0)
	(if (do ((m 1 (1+ m)) (n k (1+ n)))
		((> m i) t)
	      (ifn (= (getcharn r m) (getcharn s n)) (return nil)))
	    (return t)))))


(defun $demo n
  (cond ((= 0 n) (demo '|nmrs:d74.demo|))
	(t (do i 1 (1+ i) (> i n) (demo (arg i))))))

(defun demo (f)
  (terpri) (princ '|Type a carriage return to advance.|) (terpri)
  (setq f (open f 'in))
  (do s (read f 'end) (read f 'end) (eq 'end s)
      (do c (tyi t) (tyi t) (= 13. c))
      (princ s) (terpri)
      (princ (eval s)) (terpri))
  (terpri)
  (close f)
  'done)

(defun $why (p)
  (cond ((atom p) (why p))
	(t (why (datum p)))))

(defun why (p)
  (do ((l (lookups `(just ,p $m . $j)) (cdr l)))
      ((null l) 'done)
      (prjust p (getvar '$m (car l)) (cdr (getvar '$j (car l))))
      (terpri)))

(defun $where (p)
  (cond ((atom p) (where p))
	(t (where (datum p)))))

(defun where (d)
  (do ((l (fbs 'pr-lookup '(just . $p)) (cdr l)) (j))
      ((null l) 'done)
      (setq j (getvar '$p (car l)))
      (if (memq d (cdddr j)) (prjust (cadr j) (caddr j) (cdddr j)))))

(defun prjust (g m jl)
  (princ g) (princ '|: |) (princ (pattern g)) (princ '| by |) (princ m)
  (do l jl (cdr l) (null l)
      (terpri)
      (princ '|    |) (princ (car l)) (princ '|: |) (princ (pattern (car l))))
  (terpri))


(defun $traceprop n
  (cond ((= 0 n) traceprops)
	(t (do i 1 (1+ i) (> i n) (traceprop (arg i)))
	   'done)))

(defun traceprop (p)
  (setq advice (addq 'tracetask advice)
	traceprops (addq p traceprops))
  p)

(defun $untraceprop n
  (cond ((= 0 n) (mapc 'untraceprop traceprops))
	(t (do i 1 (1+ i) (> i n) (untraceprop (arg i)))
	   'done)))

(defun untraceprop (g)
  (setq traceprops (delete g traceprops 1))
  (if (and (null traceprops) (null tracegoals))
      (setq advice (delq 'tracetask advice)))
  g)

(defun $tracegoal n
  (cond ((= 0 n) tracegoals)
	(t (do i 1 (1+ i) (> i n) (tracegoal (arg i)))
	   'done)))

(defun tracegoal (g)
  (setq advice (addq 'tracetask advice)
	tracegoals (addq g tracegoals))
  g)

(defun $untracegoal n
  (cond ((= 0 n) (mapc 'untracegoal tracegoals))
	(t (do i 1 (1+ i) (> i n) (untracegoal (arg i)))
	   'done)))

(defun untracegoal (g)
  (setq tracegoals (delete g tracegoals 1))
  (if (and (null traceprops) (null tracegoals))
      (setq advice (delq 'tracetask advice)))
  g)

(defun tracetask (k)
  (if (and (eq 'addbc (car k))
	   (or (memq (car (caddddr k)) traceprops)
	       (and (cont k) (memmatchp (cadr (cont k)) tracegoals))))
      (prjust (datum (cadr (cont k))) 'backchain
	      (cdr (reverse (cons (datum (cadr k)) (caddddr k)))))))

(defun memmatchp (x l)
  (do () ((null l) nil)
      (if (matchp x (car l)) (return t)
	  (setq l (cdr l)))))


(defun $help n 
  (cond ((= 0 n) (mrshelper 'help))
	((= 1 n) (mrshelper (arg 1)))
	(t (wna '$help))))

(defun mrshelper (x)
  (cond ((eq 'debugging x) (helpdebugging))
	((eq 'help x) (helphelp))
	((eq 'helpfacilities x) (helphelpfacilities))
	((eq 'mrs x) (helpmrs))
	(t ($describe x))))

(defun helpdebugging ()
  (princ '|    Since MRS is written in LISP, all of LISP's debugging
    capabilities are available for debugging user code.  For debugging
    data bases  and inferences, the following commands are also available:

	 $tracegoal        $untracegoal     $where
	 $traceprop        $untraceprop     $why|)
  (terpri)
  'done)

(defun helphelp ()
  (princ '|    ($help <c>) prints a description of the concept <c> and,
    where appropriate, lists all instances.  Currently available concepts:

	 debugging
	 help
	 helpfacilities
	 MRS|)
  (terpri)
  'done)

(defun helphelpfacilities ()
  (princ '|     MRS includes a number of commands to help users learn
    more about the system.  Currently available commands:

	 $describe
	 $help
	 $tutor|)
  (terpri)
  'done)

(defun helpmrs ()
  (princ '|    MRS is a knowledge representation system intended for use
    by AI researchers in buildings expert systems.

        What makes MRS special among knowledge representation systems is
    its ability to reason about and control its own problem solving
    activity.  

        Please read the architecture paper for further information.|)
  (terpri)
  'done)

;;; This code implements the $describe package for mrs.  It builds
;;; an index of the positions (and files) of forms like:
;;;	(DESCRIPTION <key> <text>)
;;; and puts them on a list pointed to by $describe-position-alist.
;;; The text is just long atom names that should be thrown out
;;; after they are read.

(setq describe-position-alist nil)

(defun $describe (a)
  (if (null describe-position-alist)
      (setq describe-position-alist (read-dat-file '|nmrs:describe.dat|)))
  (let ((position (assq a describe-position-alist)))
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

