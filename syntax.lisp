;;; -*- Mode: Common-LISP -*-
;;;
;;;SYNTAX.LSP    0.7 18-Mar-83 0058 000   1 ML PUPFTP 18-Mar-83   
;;;	MRS TUTOR - checks syntax of stud input, such as arity
;;;
;;;perm filename SYNTAX.LSP[MRS,LSP] blob sn#702137 filedate
;;;	1983-03-18 generic text, type T, neo UTF8 
;;;
;**********************************************************
;*                                                        *
;*              SYNTAXCHECKER.LSP                         *
;*   Routines for checking the syntax of student input    *
;*                The MRS tutor                           *
;*        (c) Copyright 1982, Jan Clayton                 *
;*                                                        *
;**********************************************************
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-11
;;;
;;; ------------------------------------------------------------
;;;


;;; (declare (special ErrorMessages input))




(defun Checksyntax (input)
  ;; ErrorMessages is a global variable and will remain that waN
  ;; The reason I am going to all this trouble is to make sure that
  ;; the error messages get printed out in a top down manner.  Without
  ;; reversing the order that the errors are found they are printed in
  ;; a bottom up fashion.  The Special check in the COND is to handle TERPRI.
  (setq errorMessages NIL)
  (PropositionChecker input)

  (mapcar '(lambda (x) (cond ((Eq X 'CRLF) (terpri))
			 (T (princ x))))
	 errormessages)

  (Cond ((null errormessages))))



(defun PropositionChecker (Prop)
  ;; Hopefully this finds the syntax errors of the proposition that the
  ;; student has typed in.  Error messages are printed accordingly.
  ;; Check ERROR# functions for information about the errors being caught.
  ;;
  ;; May 26 - Arity checks entered
  ;; General restructuring for generality
  ;; Type of error messages changed
  ;; First round of student modelling put in.

  (Cond ((Atom prop) (SymbolsNotProps prop))

	(T (Let ((FA (car prop))
		 RelFlg Arity)
	 (Cond ((not (atom FA)) (FirstArgError Prop))

		 (($truep `(Quantifier ,FA))
		 (Cond ((null (cadr prop)) (QuantNoArgs prop))
			 ((Not (atom (cadr prop)))
			 (QuantVarsMissing prop))
			 (T (Do ((Rprop (cddr prop) (cdr rprop))
				 (PropFound NIL)
				 (NextArg))
				 ((Null rprop) T)

				(Cond (PropFound (QuantPropMissing prop))
				 ((not (atom (setq NextArg (car Rprop))))
				 (Setq PropFound 'T)
				(PropositionChecker NextArg)))))))

		 ((or ($truep `(LogicalOperator ,FA))
			 (setq RelFlg ($truep `(Relation ,FA))))

		 (Cond ((not (aritycheck prop (setq arity
						 ($getval `(arity ,FA)))))
			 (ArityError prop arity))
			 ((and RelFlg
				 (not (AllOkay (mapcar 'termchecker
						 (cdr prop)))))
			 (TermArgError prop))
			 ((and (not relflg)
				 (not (AllOkay (mapcar 'propositionchecker
						 (cdr prop)))))
			 (PropositionArgError prop))
			 (T)))
		 (T (UnknownRelation prop)))))))


(defun TermChecker (Term)
  ; There are two cases to check for with terms.  Either a term is an
  ; atom (symbol).  Or it is (<function> <termlist>).  We don't really need
  ; to check for all the weird cases, but we better make sure that the
  ; think is not a proposition!

  (Cond ((Atom term) 'T)
	((member (car term) '(ALL EXIST OR AND IF NOT =))
	 (ComplexTermError term))
	((cadr term) (AllOkay (mapcar 'Termchecker (cdr term))))
	(T (TermArgError term))))


(Defun AllOkay (L)
  (not (member 'NIL L)))


(defun Aritycheck (prop arityNum)
  ;; If we don't know the arity of a relation, or if the arity is arbitrary
  ;; then we will allow anything.  Otherwise we will have to test to see if
  ;; things are the right length.

    (OR (null arityNum)
	(eq (1- (length prop)) arityNum)))



;;;; Now we will put in some information about the errors that are
;;;; trapped by the syntax checker.  This information will be the 
;;;; the error messages to be typed, and eventually, what information 
;;;; should be added to the student model.

; FirstArgError - The first argument of the proposition was not an atom, 
; therefore we know already that it can't be a correctly formed MRS 
; proposition.


(defun FirstArgError (prop)
  (BuildErrMess
   (list (car prop) 'CRLF
	 '| The first argument of a proposition must be symbol.  More
    specifically either a Logical Operator, a quantifier, or a Relation.|))
  (AddToModel `(Misconception |(FIRSTARG PROPOSITION SYMBOL)|
			 ,(ua-datum input)))
  NIL)


(defun UnknownRelation (prop)
  (BuildErrMess
   (list '| | (car prop) '| is not a known relation.|))
  (AddToModel `(Misconception |(MUSTBEDEFINED RELATION)| ,(ua-datum input)))
  NIL)



(defun QuantNoArgs (prop)
  (buildErrMess
   (list '| | prop 'CRLF
	 '| Exist and All must be followed by a arbitrary list and a proposition.|))
  (AddTomodel `(Misconception|(ARITY QUANTIFIER GEQ-2)| ,(ua-datum input)))
  NIL)



(defun QuantVarsMissing (prop)
  (buildErrMess
   (list '|    | prop 'CRLF
	 '|    Quantifiers must be followed by one or more variables.|))

  (AddToModel `(Misconception |(MIDDLEARGS QUANTIFIER VARS)| ,(ua-datum input)))
  NIL)


(defun QUANTIFIERMissing (prop)
  (buildErrMess
   (list '|    | prop 'CRLF
	 '|    Only one proposition is allowed in a quantified proposition.|))

  (AddToModel `(Misconception |(LASTARG QUANTIFIER PROP)| ,(ua-datum input)))
  NIL)


(defun SymbolsNotProps (prop)
  (BuildErrMess
   (list '|    | prop '| is a term, not a proposition.|))

  (AddToModel `(Misconception |(IF (SYMBOL $X) (TERM $X))| ,(ua-datum input)))
  NIL)



(defun ArityError (prop arity)
  (buildErrMess
   (list '|    | prop 'CRLF
	 '|    | (car prop) '| must be followed by | arity  '| argument(s).|))
  (let ((concept (ua-datum (list 'Arity (car prop) arity))))
    (AddToModel `(Misconception ,concept ,(ua-datum prop))))
  NIL)



(defun PropositionArgError (prop)
  (BuildErrmess
   (list '|    | prop 'CRLF
	 '|    | (car prop)  '| must have propositional arguments.|))

  (AddToModel `(Misconception |(ARGTYPE LOGICALOPERATOR PROP)| 
			      ,(ua-datum input)))
  NIL)



(defun TermArgError (prop)
  (BuildErrMess
   (list '|    | prop 'CRLF 
	 '|    The relation, | (car prop) '|, must have term arguments.|))

  (AddToModel `(Misconception |(ARGTYPE RELATION TERM)| ,(ua-datum input)))
  NIL)



(defun ComplexTermError (prop)
  (BuildErrMess
   (list '|    | prop 'CRLF
	 '|    Complex Terms cannot begin with quantifiers or logical operators.|))

  (AddToModel `(Misconception |(FIRSTARG COMPLEXTERM FUNCTION)| 
			      ,(ua-datum Input)))
  NIL)


(defun TermArityError (prop)
  (BuildErrMess 
   (List '|    | prop 'CRLF 
	 '|    Complex terms must have 1 or more term arguments.|))

  (AddToModel `(Misconception |(ARITY FUNCTIONS GEQ-1)| ,(ua-datum prop)))
  NIL)
  
 
(Defun BuildErrMess (ErrList)
  (Setq ErrorMessages
	(append ErrList '(CRLF) ErrorMessages)))

