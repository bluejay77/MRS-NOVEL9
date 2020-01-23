
perm filename MACROS.LSP[MRS,LSP] blob sn#702098 filedate 1983-03-18 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00005 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002	             -*- Mode:LISP  -*-                                      
C00009 00003
C00019 00004
C00020 00005
C00021 ENDMK
C⊗;
;;;             -*- Mode:LISP;  -*-                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981 Michael R. Genesereth			 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-lispm (eval-when (compile) (macros t))
#+franz (eval-when (eval load) (sstatus uctolc t))

(eval-when (compile load eval)
  #+maclisp (setsyntax '@ 'macro 'tyi)

(comment
 #+maclisp
  (setsyntax '/< 'macro
    '(lambda () (cond ((= @  (tyipeek)) '|<|)
		      ((= @= (tyipeek)) (tyi) '|<=|)
		      )))
  #+maclisp
  (setsyntax '/> 'macro
    '(lambda () (cond ((not (= @= (tyipeek))) '/>)
		      (t (tyi) '|>=|))))
)
) ;end of eval-when


(defun local macro (x)
  (do ((l (cadr x) (cdr l)) (nl) (vl))
      ((null l)
       (setq vl (nreverse vl) nl (nreverse nl))
       `((lambda ,vl . ,(cddr x)) . ,nl))
      (cond ((atom (car l)) (setq vl (cons (car l) vl) nl (cons nil nl)))
	    (t (setq vl (cons (caar l) vl) nl (cons (cadar l) nl))))))

;unused
;(defun mrg-values macro (x)
;  (do ((l (cdr x) (cdr l)) (i 1 (1+ i)) (nl))
;      ((null l) `(setq . ,(nreverse nl)))
;      (setq nl (cons (car l)
;		     (cons (implode (nconc (exploden 'reg) (exploden i)))
;			   nl)))))

;unused
;(defun setqs macro (x)
;  (do ((l (cadr x) (cdr l)) (i 1 (1+ i)) (nl))
;      ((null l) `(if ,(caddr x) (setq . ,(nreverse nl))))
;      (setq nl (cons (implode (nconc (exploden 'reg) (exploden i)))
;		     (cons (car l) nl)))))

(defun mapand macro (x)
  `(do ((l ,(caddr x) (cdr l))) ((null l) t)
       (ifn (funcall ,(cadr x) (car l)) (return nil))))

;unused
;(defun mapand2 macro (x)
;  `(do ((l ,(caddr x) (cdr l)) (m ,(cadddr x) (cdr m)))
;       ((or (null l) (null m))
;	(eq l m))
;       (ifn (funcall ,(cadr x) (car l) (car m)) (return nil))))

(defun mapor macro (x)
  `(do ((l ,(caddr x) (cdr l)) (dum)) ((null l))
       (if (setq dum (funcall ,(cadr x) (car l))) (return dum))))

(defun maplac macro (x)
  `(do l ,(caddr x) (cdr l) (null l) (rplaca l (,(cadr x) (car l)))))

#-lispm
(defun when macro (x) `(cond ,(cdr x)))

#-lispm
(defun if macro (x)
  (cond ((null (cdddr x)) `(cond (,(cadr x) ,(caddr x))))
	(t `(cond (,(cadr x) ,(caddr x)) (t . ,(cdddr x))))))

(defun ifn macro (x)
  (cond ((null (cdddr x)) `(cond ((not ,(cadr x)) ,(caddr x))))
	(t `(cond ((not ,(cadr x)) ,(caddr x)) (t . ,(cdddr x))))))

(defun put macro (x) `(putprop . ,(cdr x)))

(defun iand macro (x) `(boole 1 . ,(cdr x)))
(defun ior macro (x) `(boole 7 . ,(cdr x)))

(defun xor macro (x) `(not (eq . ,(cdr x))))

;(defun copy macro (x) `(subst nil nil ,(cadr x)))
(defun copyp macro (x) `(cons (car ,(cadr x)) (cdr ,(cadr x))))
(defun copyl macro (x) `(append ,(cadr x) nil))

#-lispm
(defun >= macro (x) `(not (< ,(cadr x) ,(caddr x))))
#-lispm
(defun <= macro (x) `(not (> ,(cadr x) ,(caddr x))))

;unused
;(defun econs macro (x) `(append ,(cadr x) (list ,(caddr x))))

#-franz(defun caaadar macro (x) `(caaadr (car ,(cadr x))))
#-franz(defun caaaddr macro (x) `(caaadr (cdr ,(cadr x))))
#-franz(defun caadaar macro (x) `(caadar (car ,(cadr x))))
#-franz(defun caadadr macro (x) `(caadar (cdr ,(cadr x))))
#-franz(defun caaddar macro (x) `(caaddr (car ,(cadr x))))
#-franz(defun cadaaar macro (x) `(cadaar (car ,(cadr x))))
#-franz(defun cadaddr macro (x) `(cadadr (cdr ,(cadr x))))
#-franz(defun caddaar macro (x) `(caddar (car ,(cadr x))))
#-franz(defun caddadr macro (x) `(caddar (cdr ,(cadr x))))
#-franz(defun cadddar macro (x) `(cadddr (car ,(cadr x))))
#-franz(defun caddddr macro (x) `(cadddr (cdr ,(cadr x))))
#-franz(defun cdadadr macro (x) `(cdadar (cdr ,(cadr x))))
#-franz(defun cdadddr macro (x) `(cdaddr (cdr ,(cadr x))))
#-franz(defun cdddddr macro (x) `(cddddr (cdr ,(cadr x))))

#-lispm(defun aset macro (x) `(store ,(cddr x) ,(cadr x)))
#-lispm(defun aref macro (x) (cdr x))

;(declare (special name bas mobjects mrg-selector) (*expr mode))
;
;(setq mobjects nil)
;
;(defprop mode (c-mode s-mode a-mode) mode)
;
;(defun c-mode macro (x) `(list . ,(cdr x)))
;
;(defun s-mode macro (x)
;  (cond ((eq 'c (caddr x)) `(car ,(cadr x)))
;	((eq 'sel (caddr x)) `(cadr ,(cadr x)))
;	((eq '← (caddr x)) `(caddr ,(cadr x)))))
;
;(defun a-mode macro (x)
;  (cond ((eq 'c (caddr x)) `(rplaca (cadr x) ,(cadddr x)))
;	((eq 'sel (caddr x)) `(rplaca (cdr ,(cadr x)) ,(cadddr x)))
;	((eq '← (caddr x)) `(rplaca (cddr ,(cadr x)) ,(cadddr x)))))
;
;
;
;(defun defmode macro (x)
;  (let ((mrg-selector (memq 'mrg-selector (cddddr x))))
;    (define-mode (cadr x) (cadddr x))
;    (mapc 'eval (cddddr x))
;    `',(cadr x)))
;
;(defun define-mode (name desc)
;  (prog (c s a dummy)
;    (setq dummy (explodec name)
;	  c (implode (append '(c -) dummy))
;	  s (implode (append '(s -) dummy))
;	  a (implode (append '(a -) dummy)))
;    (define-macro c (defc desc))
;    (define-macro s (defs desc))
;    (define-macro a (defa desc))
;    (put name (c-mode c s a) 'mode)
;    (return name)))
;
;
;(defun defc (desc) (let ((bas 'x)) `(lambda (x) ,(defc1 desc))))
;
;(defun defc1 (desc)
;  (cond ((atom desc) (list 'quote desc))
;	((eq 'mrg-selector (car desc))
;	 (cond ((not (null (cdddr desc))) (list 'quote (cadddr desc)))
;	       (t (setq bas (list 'cdr bas))
;		  (list 'car bas))))
;	((eq 'atom (car desc))
;	 `(list 'c-atom '',(mapcar 'cadr (cdr desc)) (cons 'list (cdr x))))
;	((eq 'cons (car desc)) `(list 'cons ,(defc1 (cadr desc)) ,(defc1 (caddr desc))))
;	((eq 'list (car desc))
;	 (do ((l (cdr desc) (cdr l)) (nl))
;	     ((null l) `(list 'list . ,(nreverse nl)))
;	     (setq nl (cons (defc1 (car l)) nl))))
;	((eq 'struct (car desc)) (defc1 (cons 'list (cdr desc))))
;	(t (list 'quote desc))))
;
;
;(defun defs (desc)
;  `(lambda (x) (cond . ,(nreverse (defs1 desc '(cadr x) nil)))))
;
;(defun defs1 (desc bas result)
;  (cond ((atom desc) result)
;	((eq 'mrg-selector (car desc))
;	 (put (cadr desc) (cons (cons name (caddr desc)) (get (cadr desc) 'modes)) 'modes)
;	 (put name (cons (cons (cadr desc) (caddr desc)) (get name 'sels)) 'sels)
;	 (if mrg-selector (define-macro (cadr desc) 'mrg-selector))
;	 (cons `((eq ',(cadr desc) (caddr x)) ,bas) result))
;	((eq 'atom (car desc))
;	 (do l (cdr desc) (cdr l) (null l)
;	     (put (cadar l) (cons (cons name (caddar l)) (get (cadar l) 'modes)) 'modes)
;	     (put name (cons (cons (cadar l) (caddar l)) (get name 'sels)) 'sels)
;	     (if mrg-selector (define-macro (cadar l) 'mrg-selector)))
;	 (cons `((memq (caddr x) ',(mapcar 'cadr (cdr desc))) (list 'get ,bas (list 'quote (caddr x))))
;	       result))
;	((eq 'cons (car desc))
;	 (setq result (defs1 (cadr desc) `(list 'car ,bas) result))
;	 (defs1 (caddr desc) `(list 'cdr ,bas) result))
;	((eq 'list (car desc))
;	 (do l (cdr desc) (cdr l) (null l)
;	     (setq result (defs1 (car l) `(list 'car ,bas) result)
;		   bas `(list 'cdr ,bas)))
;	 result)
;	((eq 'struct (car desc)) (defs1 (cons 'list (cdr desc)) bas result))
;	(t result)))
;
;(defun defa (desc)
;  `(lambda (x) (cond . ,(nreverse (defa1 desc '(cadr x) nil nil)))))
;
;(defun defa1 (desc bas cdr result)
;  (cond ((atom desc) result)
;	((eq 'mrg-selector (car desc))
;	 (setq bas (cond ((not cdr) `(list 'car (list 'rplaca ,(caddr bas) (cadddr x))))
;			 (t `(list 'cdr (list 'rplacd ,(caddr bas) (cadddr x))))))
;	 (cons `((eq ',(cadr desc) (caddr x)) ,bas) result))
;	((eq 'atom (car desc))
;	 (list `(t (list 'a-atom (cadr x) (list 'quote (caddr x)) (cadddr x)))))
;	((eq 'cons (car desc))
;	 (setq result (defa1 (cadr desc) `(list 'car ,bas) nil result))
;	 (defa1 (caddr desc) `(list 'cdr ,bas) t result))
;	((eq 'list (car desc))
;	 (do l (cdr desc) (cdr l) (null l)
;	     (setq result (defa1 (car l) `(list 'car ,bas) nil result)
;		   bas `(list 'cdr ,bas)))
;	 result)
;	((eq 'struct (car desc)) (defa1 (cons 'list (cdr desc)) bas cdr result))
;	(t result)))
;
;
;(defun define-macro (name lambda-exp) (put name lambda-exp 'macro))
;
;(defun mode (x) (cdr (assoc x mobjects)))
;
;(defun modedeclare fexpr (x)
;  (mapc '(lambda (l) (mapc '(lambda (v) (setq mobjects (cons (cons v (car l)) mobjects)))
;			   (cdr l)))
;	x))
;
;(defun ndm-err (x)
;  (terpri)
;  (princ '|cannot determine the mode of |) (princ x)
;  (err))
;
;(defun nsm-err (x)
;  (terpri)
;  (princ '|no such mode as |) (princ x)
;  (err))
;
;(defun sel-err (b s)
;  (terpri)
;  (princ '|:|) (princ b)
;  (do () ((null s)) (princ '|:|) (princ (car s)) (setq s (cdr s)))
;  (princ '|is an impossible selection|)
;  (err))
;
;(defun ia-err (x)
;  (terpri)
;  (princ '|cannot assign |) (princ x)
;  (err))
;
;
;(defun sel macro (x)
;  (local ((s (fsel (mode (cadr x)) (cddr x))))
;    (cond ((null s) (sel-err (cadr x) (cddr x)))
;	  (t (setq x (cadr x))
;	     (do () ((null (cdr s)) x)
;		 (setq x (cons (cadr (get (car s) 'mode)) (rplaca s x)) s (cddr s))
;		 (rplacd (cddr x) nil))))))
;
;(defun fsel (m sels)		; this has a bug in it.
;  (cond ((null sels) (list m))
;	((null m)
;	 (do l (get (car sels) 'modes) (cdr l) (null l)
;	     (if (setq m (fsel (cdar l) (cdr sels)))
;		 (return (cons (caar l) (cons (car sels) m))))))
;	((local (dum)
;	   (if (setq dum (assq (car sels) (get m 'sels)))
;	       (cons m (cons (car sels) (fsel (cdr dum) (cdr sels)))))))
;	(t (do ((l (get m 'sels) (cdr l)) (dum)) ((null l))
;	       (if (setq dum (fsel (cdar l) sels))
;		   (return (cons m (cons (caar l) dum))))))))
;
;(defun mrg-selector (x)
;  (if (null (cddr x)) `(sel ,(cadr x) ,(car x))
;      `(← (sel ,(cadr x) ,(car x)) ,(caddr x))))
;
;
;(defun ← macro (x) `(sto . ,(cdr x)))
;
;(defun sto macro (x)
;  (do ((l (cdr x) (cddr l)) (s) (nl))
;      ((null l) `(progn . ,(nreverse nl)))
;      (cond ((atom (car l)) (setq nl (cons `(setq ,(car l) ,(cadr l)) nl)))
;	    ((and (eq 'sel (caar l)) (setq s (fsel (mode (cadar l)) (cddar l))))
;	     (setq x (cadar l))
;	     (do l (cddr s) (cddr l) (null (cdr l))
;		 (setq x (cons (cadr (get (car l) 'mode)) (rplaca l x)))
;		 (rplacd (cddr x) nil))
;	     (setq nl (cons (list (caddr (get (car s) 'mode)) x (cadr s) (cadr l)) nl)))
;	    (t (ia-err (car l))))))


;(defun c-atom (sels args)
;  (do ((nl)) ((null sels) (rplacd (intern (gensym)) (nreverse nl)))
;      (if (car args) (setq nl (cons (car args) (cons (car sels) nl))))
;      (setq sels (cdr sels) args (cdr args))))

;(defun a-atom (bas sel val)
;  (cond ((null val) (remprop bas sel) nil)
;	(t (putprop bas val sel))))

;(defun dssq (x l)
;  (do () ((null l))
;      (cond ((eq x (cdar l)) (return (car l))) (t (setq l (cdr l))))))


(defun impvar macro (x) `(special . ,(cdr x)))
(defun expvar macro (x) `(special . ,(cdr x)))
(defun impfun macro (x) `(*expr . ,(cdr x)))
(defun expfun macro (x) `nil)

#+franz
(defmacro list* (first &rest rest)
	  (if rest `(cons ,first (list* ,@ rest))
	      first))

;moved from match and bc
(defun pset macro (x) `(cons (cons ,(cadr x) ,(caddr x)) ,(cadddr x)))

