;;; -*- Mode: Common-LISP -*-
;;;
;;;EXERCI.LSP    2.4 18-Mar-83 0053 000   1 ML PUPFTP 18-Mar-83   
;;;	Problem information for TUTOR.LSP
;;;
;;;
;;;perm filename EXERCI.LSP[MRS,LSP] blob sn#702120 filedate
;;;	1983-03-18 generic text, type T, neo UTF8 
;;;
;**********************************************************
;*                                                        *
;*                EXERCISES.LSP                           *
;*           Exercises for the MRS tutor                  *
;*        (c) Copyright 1982, Jan Clayton                 *
;*                                                        *
;**********************************************************
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr AJY 2015-05-11
;;;
;;; ------------------------------------------------------------
;;;

;;;(declare (special syntaxElist1 SemanticElist SyntaxElist2
;;;		  AccessingElist Elist4A))


;  This is the section of the MRS tutor that has all the problem information.
;  This includes the problems themselves, the assertions that need to go
;  along with them.  The general exercise functions are in the file
;  tutor.lsp.

;;; This is faulty, needs to be corrected later on

#|


(defun SyntaxExercises1 ()
  (PresentExercises SyntaxEList1 (Get 'SyntaxElist1 'Directions)
		    (Get 'SyntaxElist1 'Prompt) 'GEN T)
  (CorrectMisConceptions (Get 'SyntaxElist1 'Directions)
		    (Get 'SyntaxElist1 'Prompt) 'GEN))

(defun SyntaxExercises2 ()
  (PresentExercises SyntaxElist2 (Get 'SyntaxElist2 'Directions)
		    (Get 'SyntaxElist2 'Prompt) 'GEN T)
  (CorrectMisconceptions (Get 'SyntaxElist2 'Directions)
		    (Get 'SyntaxElist2 'Prompt) 'GEN))

(defun SemanticExercises ()
  (PresentExercises SemanticElist (Get 'SemanticElist 'directions)
		    (Get 'SemanticElist 'Prompt) 'SEM T))

(defun AccessingExercises ()
  (PresentExercises AccessingElist (Get 'AccessingElist 'Directions)
		    (Get 'AccessingElist 'Prompt) 'SEM T))


(defun interactiveExercises ()
  (Let ((theory 'Exercises))

    ($Stash '(Relation Frog))
    ($Stash '(Relation Hates))
    ($Stash '(Relation Giraffe))
    ($Stash '(Relation Herbivore))
    ($Stash '(Relation Vegetarian))

    (PresentExercises Elist4A 
		      (Get 'EList4A 'Directions)
		      (Get 'Elist4A 'Prompt) 
		      'INT T)
    ;Add the other sections to this exercise here.


    ($Unstash '(Relation Frog))
    ($Unstash '(Relation Hates))
    ($Unstash '(Relation Giraffe))
    ($Unstash '(Relation Herbivore))
    ($Unstash '(Relation Vegetarian))
))


(setq SyntaxElist1 '(GP1 GP2 GP4 GP33 GP5 GP28 GP34 GP3 GP15 GP16
			 GP27 GP17 GP20	GP21 GP19))


(putprop 'SyntaxElist1 '"
    For the following problems, indicate which are Terms (T), Propositions (P),
    or Illegal (I) constructions according to the rules of MRS syntax. " 
	 'Directions)
(Putprop 'SyntaxElist1 '"Term, Proposition or Illegal? " 'prompt)

(Setq SyntaxElist2 '(GP6 GP11 GP10 GP29 GP12 GP13 GP14 GP8
			 GP7 GP35 GP30 GP31))
(putprop 'SyntaxElist2 '"
    For the following problems, indicate which are Propositions (P),
    or Illegal (I) constructions according to the rules of MRS syntax. " 
	 'Directions)
(Putprop 'SyntaxElist2 '"Proposition or Illegal? (P or I) " 'Prompt)


(setq SemanticElist '(Prob2-11 Prob2-12 Prob2-13 Prob2-14 Prob2-15 Prob2-16))
(putprop 'SemanticElist '"
    Are these two statements equivalent?  Assuming the usual meaning for
    the English words, answer ``Y'' if the MRS proposition and the English
    sentence have the same meaning." 'Directions)
(putprop 'SemanticElist '"Equivalent? (Y or N) " 'Prompt)

(Setq Prob2-11 '"Frogs are green.  ==  (Exist x (If (Frog x) (Green x)))")
    (putprop 'Prob2-11 'N 'Answer)
    (putprop 'Prob2-11 '"
    Exist is the existential quantifier.  In the sentence, ``Frogs are green''
    we are stating (implicitly) that ``all frogs are green'', therefore the
    correct MRS proposition is (All x (If (Frog x) (Green x)))." 'Explanation)

(Setq Prob2-12 '"No coat is waterproof unless it has been specially treated.
        ==  (All x (If (And (Coat x) 
                            (Waterproof x))
                       (SpeciallyTreated x))) ")
    (putprop 'Prob2-12 'Y 'Answer)
    (putprop 'Prob2-12 '"
    Another English sentence that is closer to this MRS proposition is 
    ``All waterproof coats have been specially treated.'' ." 'Explanation)

(Setq Prob2-13 '"Some houses are cold if they are not well insulated.
        ==  (Exist x (If (house x) 
                         (If (cold x)
                             (not (Well-Insulated x))))) ")
    (putprop 'Prob2-13 'N 'Answer)
    (putprop 'prob2-13 '"
    Although this looks right on first examination it actually has the 
    unintuitive meaning of ``If everything is a house, and everything is cold
    then there exists something that is not well insulated''. " 'Explanation)

(setq Prob2-14 '"Houses are cold only if they are not well insulated.
        ==  (All x (If (house x) 
                       (If (cold x) 
                           (not (Well-Insulated x))))) ")
    (putprop 'Prob2-14  'Y 'Answer)
    (putprop 'Prob2-14 '"
    The correct MRS proposition is not ``(All x (If (house x) (If (not 
    (Well-Insulated x)) (cold x))))'' since with that statement it is possible
    to have a house that is cold that is well insulated." 'Explanation)

(setq Prob2-15 '"Everyone buys hamburgers from MacDonalds.
	== (All x (And (Person x) 
                       (Buys x Hamburgers MacDonalds)))")
    (putprop 'Prob2-15 'N 'answer)
    (putprop 'prob2-15 '"
    The MRS proposition stated says that everything in the universe is a person
    and that everything buys hamburgers at MacDonalds.  The correct MRS
    proposition is (ALL X (IF (PERSON X) (BUYS X HAMBURGERS MACDONALDS))) ."
	     'Explanation)

(Setq Prob2-16 '"All Canaries are yellow.  ==  (ALL CANARIES (YELLOW CANARIES))")
    (putprop 'Prob2-16 'N 'Answer)
    (putprop 'Prob2-16 '"
    This MRS propositions says that everything is yellow.  Remember that 
    the symbols following the quantifier are VARIABLES not individuals or 
    sets.  The proper MRS statement is (ALL C (IF (CANARY C) (YELLOW C))). "
	     'Explanation)

(setq AccessingElist '(Prob3-1 Prob3-2 Prob3-3 Prob3-4))
(putprop 'AccessingElist '"
    In the problems below there are a set of propositions that are in the 
    data base.  For the command, indicate, with either Y or N, whether the
    given response is what MRS will return." 'Directions)
(putprop 'AccessingElist '"The right response? (Y or N) " 'Prompt)

(Setq Prob3-1 '"(IF (FROG $X) (GREEN $X))
	  (FROG FRED)
  User:   ($LOOKUP '(GREEN FRED)) 
  MRS:    ((T . T)) ")
    (putprop 'Prob3-1 'N 'answer)
    (putprop 'prob3-1 '"
    $Lookup only looks for a fact that is in the data base.  No deduction
    (backward chaining) is performed. " 'Explanation)
        ($stash '(Concept Prob3-1 "(ONLYLOOKSUP LOOKUP)"))


(Setq Prob3-2 '"(If (Frog $X) (Green $X))
	  (Frog Fred)
  User:   ($Truep '(Green Fred))
  MRS:    ((T . T)) ")
    (putprop 'Prob3-2 'Y 'Answer)
    (putprop 'Prob3-2 '"
    $Truep backward chains through the data base to deduce the fact if it
    is not in the data base.  " 'Explanation)
        ($Stash '(Concept Prob3-2 "(BACKWARDCHAINS TRUEP)"))


(Setq Prob3-3 '"(If (Bird $X) (Blue $X))
	  (If (Canary $X) (Bird $X))
	  (Canary Tweety)
	  (Canary Twiddle)
  User:   ($Truep (Blue $Y)) 
  MRS:    ((($Y . Tweety) (T . T)) (($Y . Twiddle) (T . T)))  ")
    (putprop 'Prob3-3 'N 'Answer)
    (putprop 'prob3-3 '"
    If one asks for $Truep to return a binding for a variable, it will only
    return the first binding it finds in the database that satisfies the
    proposition. " 'Explanation)
        ($stash '(Concept Prob3-3 "(RETURNSONLYONE TRUEP)"))


(Setq prob3-4 '"(If (Bird $X) (Blue $X))
	  (Bird Tweety)
	  (Bird Twiddle)
  User:   ($Trueps '(Blue $Y))
  MRS:    ((($Y . Tweety) (T . T)) (($Y . Twiddle) (T . T)))  ")
    (putprop 'Prob3-4 'Y 'answer)
    (putprop 'prob3-4 '"
    The only difference between $Truep and $Trueps is that $Trueps
    will return all bindings for variables. " 'Explanation)
        ($stash '(Concept Prob3-4 "(RETURNSALL TRUEP)"))


(setq Elist4A '(Prob4-1 Prob4-2 Prob4-3 Prob4-4 Prob4-5 
		Prob4-11 Prob4-12 Prob4-13 Prob4-14 Prob4-15
		Prob4-21 Prob4-22 Prob4-23 Prob4-24 Prob4-25 Prob4-26))

(putprop 'Elist4A '"
    Using the relations (Frog x), (Hates a b), (Giraffe x), (Herbivore x),
    (Vegetarian x) meaning ``x is a Frog'', ``a hates b'', etc., enter the 
    following statements as propositions into MRS. "
	 'Directions)

(putprop 'Elist4A '"MRS statement: " 'Prompt)

(Setq EnteringFactsCommands '($Stash $Assert Stash Assert))
(Setq AskingCommands '($Truep $Trueps $Lookup $Lookups Truep Trueps Lookup 
			      Lookups))

(Setq Prob4-1 '"Fred is a frog.")
    (putprop 'Prob4-1 '(Frog Fred) 'Answer)
    (putprop 'Prob4-1 '"
    Either stash or assert can be used to enter this fact.  No quantification
    or variables are needed to state that the object denoted by ``Fred'' is a
    frog. " 'Explanation)

    (putprop 'Prob4-1 EnteringFactsCommands 'PossibleCommands)

(Setq Prob4-2 '"Sally is a frog.")
    (putprop 'Prob4-2 '(Frog Sally) 'Answer)
    (putprop 'Prob4-2 '"
    Either stash or assert can be used to enter this fact.  No quantification
    or variables are need to state that the object denoted by ``Sally'' is a
    frog. " 'Explanation)

    (putprop 'Prob4-2 EnteringFactsCommands 'PossibleCommands)

(setq Prob4-3 '"Sally hates Fred.")
    (putprop 'Prob4-3 '(Hates Sally Fred) 'Answer)
    (putprop 'Prob4-3 '"
    No variables are needed.  This is a simple proposition." 'Explanation)
    (putprop 'Prob4-3 EnteringFactsCommands 'PossibleCommands)

(setq Prob4-4 '"Fred hates everything.") 
    (putprop 'Prob4-4 '(Hates Fred $x) 'Answer)
    (putprop 'prob4-4 '"
    A universally quantified variable is needed to represent everything in the
    world." 'Explanation)
    (putprop 'Prob4-4 EnteringFactsCommands 'PossibleCommands)

(setq Prob4-5 '"Frogs do exist.")
    (putprop 'Prob4-5 '(Frog ?x) 'Answer)
    (putprop 'Prob4-5 '"
    A existentially quantified variable is needed to say that something
    (not everything) is a frog." 'Explanation)

    (putprop 'Prob4-5 EnteringFactsCommands 'PossibleCommands)


        

(Setq Prob4-11 '"Is Sally a frog?")
    (putprop 'Prob4-11 '(Frog Sally) 'Answer)
    (putprop 'prob4-11 '"
    No variables are needed to make a simple database inquiry.  Truep and
    Lookup are equivalent commands since no inference need be done to 
    answer the question. " 'Explanation)
    (putprop 'Prob4-11 AskingCommands 'PossibleCommands)

(Setq Prob4-12 '"Is George a frog?")
    (putprop 'Prob4-12 '(Frog George) 'answer)
    (putprop 'Prob4-12 '"
    Your query to MRS should be almost identical to the previous question,
    but the response from MRS will be false, since that fact has not been 
    entered into the database. " 'Explanation)
    (putprop 'Prob4-12 AskingCommands 'PossibleCommands)

(Setq Prob4-13 '"Does George hate Sally?")
    (putprop 'Prob4-13 '(Hates George Sally) 'answer)
    (putprop 'Prob4-13 '"
    Any of the fact retrieval commands are okay to use for this, however,
    the best is probably Truep, since it will backward chain if it is
    needed." 'Explanation)
    (putprop 'Prob4-13 AskingCommands 'PossibleCommands)

(Setq Prob4-14 '"Who hates Fred?")
    (putprop 'Prob4-14 '(Hates $X Fred) 'answer)
    (putprop 'Prob4-14 '"
    For this particular problem, it is not necessary to do any inference
    to answer the question.  However, it is probably best to use Truep
    just in case.  Because of the ambiguous nature of the question either
    the singular or plural forms of the commands are accepted."
'Explanation)
    (putprop 'Prob4-14 AskingCommands 'PossibleCommands)

(Setq Prob4-15 '"Who are all the frogs?")
    (putprop 'Prob4-15 '(Frog $X) 'answer)
    (putprop 'Prob4-15 '"
    Since no backward chaining is needed for answering this question,
    either Trueps or Lookups will do.  The plural forms must be used,
    however, since we want all individuals that fit this description."
	 'Explanation)
    (putprop 'Prob4-15 '($Lookups $Trueps Lookups Trueps)
'PossibleCommands)



(Setq Prob4-21 '"George is a giraffe.")
    (putprop 'Prob4-21 '(Giraffe George) 'answer)
    (putprop 'Prob4-21 '"
    This problem is totally analogous to ``Fred is a frog.'' "
'Explanation)
    (putprop 'Prob4-21 EnteringFactsCommands 'PossibleCommands)

(Setq Prob4-22 '"All Giraffes are herbivores.")
    (putprop 'Prob4-22 '(If (Giraffe $x) (Herbivore $x)) 'Answer)
    (putprop 'Prob4-22 '"
    Quantification is necessary to describe the set of individuals that
    are giraffes. " 'Explanation) (putprop 'Prob4-22
EnteringFactsCommands 'PossibleCommands)

(Setq Prob4-23 '"All herbivores are vegetarians.")
    (putprop 'Prob4-23 '(If (Herbivore $x) (Vegetarian $x)) 'Answer)
    (putprop 'Prob4-23 '"
    Quantification must be used to be able to describe a set of individual,
    in this case the set of herbivores." 'Explanation)
    (putprop 'Prob4-23 EnteringFactsCommands 'PossibleCommands)


(setq prob4-24 '"Is George a herbivore?")
    (putprop 'Prob4-24 '(Herbivore George) 'answer)
    (putprop 'Prob4-24 '"
    Since MRS must do some backward chaining to answer this question,
    a form of Truep must be used instead of Lookup." 'Explanation)
    (putprop 'Prob4-24 AskingCommands 'PossibleCommands)

(setq Prob4-25 '"Gertrude is a herbivore.")
    (putprop 'Prob4-25 '(Herbivore Gertrude) 'Answer)
    (putprop 'Prob4-25 '"
    This is totally analogous to saying Fred is a frog." 'Explanation)
    (putprop 'Prob4-25 EnteringFactsCommands 'PossibleCommands) 

(setq Prob4-26 '"Who is a vegetarian?")
    (putprop 'Prob4-26 '(Vegetarian $x) 'Answer)
    (putprop 'Prob4-26 '"
    Backward chaining must take place for MRS to answer this question
    properly.  Because of that, Truep or Trueps must be used instead	
    of lookup. " 'Explanation)
    (putprop 'Prob4-26 '($Truep Truep $Trueps Trueps) 'PossibleCommands)


; Symbols as terms

; SALLY
(setq GP1 '((Symbol p1)))
  (putprop 'GP1 'T 'Answer)
  (putprop 'GP1 '"
    All symbols are terms by definition. " 'Explanation)
  ($stash '(Concept GP1 "(IF (SYMBOL $X) (TERM $X))"))

; IF
(setq GP2 '((LogicalOperator p1)))
  (putprop 'GP2 'T 'Answer)
  (putprop 'GP2 '"
    All symbols are terms by definition. " 'Explanation)
  ($stash '(Concept GP2 "(IF (SYMBOL $X) (TERM $X))"))

; COLOR-OF
(setq GP25 '((Function p1)))
  (putprop 'GP25 'T 'Answer)
  (putprop 'GP25 '"
    All symbols are terms by definition. " 'Explanation)
  ($STASH '(Concept GP25 "(IF (SYMBOL $X) (TERM $X))"))


;  Complex Term stuff

; (COLOR-OF JOHN)
(setq GP4 '((Firstarg p1 function)(ARgtype p1 symbol)))
  (putprop 'GP4 'T 'Answer)
  (putprop 'GP4 '"
    Although this has the same form as a proposition, it is a term because
    it represents an object (person, place or thing) and not a fact."
	     'Explanation)
  ($Stash '(Concept GP4 "(DENOTES OBJECTS TERMS)"))


; (TALLER-THAN (SISTER-OF FRED))
(setq GP33 '((Firstarg p1 function)(argtype p1 complexterm)))
  (putprop 'GP33 'T 'Answer)
  (putprop 'GP33 '"
    This expression is a term, not a proposition.  It refers to a specific
    person or thing.  And each argument also refers to a specific thing. "
	     'Explanation)
  ($Stash '(Concept GP33 "(ARGTYPE FUNCTION TERM)"))
  ($Stash '(Concept GP33 "(DENOTES OBJECTS TERMS)"))



;  Regular Propositions

; (HATES JOE FRED)
(setq GP34 '((FirstArg p1 relation)(Argtype p1 symbol)))
  (putprop 'GP34 'P 'Answer)
  (putprop 'GP34 '"
    This is the simplest form of a proposition, a relation followed by symbols.
    It is not a term because it states a fact. " 'Explanation)
  ($STash '(Concept GP34 "(ARGTYPE RELATION TERMS)"))
  ($Stash '(Concept GP34 "(DENOTES FACTS PROPOSITIONS)"))


; (LIKES PETER (SISTER-OF SUZY))
(setq GP5 '((ReGProp P1)))
  (putprop 'GP5 'P 'Answer)
  (putprop 'GP5 '"
    This expression states a fact about the relationship between two things 
    which are represented as terms.  Facts  about objects are always 
    propositions. " 'Explanation)
  ($STASH '(Concept GP5 "(DENOTES FACTS PROPOSITIONS)"))
  ($STASH '(Concept GP5 "(FIRSTARG RELPROP RELATION)"))  
  ($STASH '(Concept GP5 "(DENOTES OBJECTS TERMS)"))

;  (LIKES (HATES PETER JOE)(CARPENTER FRED))
(setq GP3 '((Firstarg p1 relation)(Argtype p1 RegProp)))
  (putprop 'GP3 'i 'answer)
  (putprop 'GP3 '"
    All relations take terms as arguments.  In this case, both arguments are
    propositions." 'Explanation)
  ($Stash '(Concept GP3 "(ARGTYPE RELATION TERM)"))



; LogicalPropositions

;  (IF (HATES SUZY JIM) (EATS-WITH JIM (SISTER-OF SALLY)))
(setq GP14 '((logicalProp p1)))
  (putprop 'GP14 'p 'answer)
  (Putprop 'GP14 '"
    This is a legal logical proposition." 'Explanation)
  ($stash '(Concept GP14 "(ARGTYPE LOGICALPROP PROP)"))


;  (AND (HATES SUZY JIM) (EATS-WITH JIM (SISTER-OF SALLY)))
(setq GP21 '((Gargi 0 p1 And)(Argtype p1 RegProp)))
  (putprop 'GP21 'p 'answer)
  (putprop 'GP21 '"
    This is a correct form of a proposition.  AND can take any number of
    propositional arguments. " 'Explanation)
  ($Stash '(Concept GP21 "(PROPOSITION LOGICALPROP)"))
  ($Stash '(Concept GP21 "(LOGICALOPERATOR AND)"))
  ($Stash '(Concept GP21 "(ARGTYPE LOGICALOPERATOR PROP)"))


;  (OR (HATES SUZY JIM) (EATS-WITH JIM (SISTER-OF SALLY)))
(setq GP22 '((gargi 0 p1 OR)(Argtype p1 RegProp)))
  (putprop 'GP22 'p 'answer)
  (putprop 'GP22 '"
    This is a legal logical proposition." 'Explanation)
  ($Stash '(Concept GP22 "(LOGICALOPERATOR OR)"))
  ($Stash '(Concept GP22 "(ARGTYPE LOGICALPROP PROP)"))

;  (NOT (HATES SUZY JIM))
(setq GP23 '((gargi 0 p1 not)(Argtype p1 RegProp)))
  (putprop 'GP23 'p 'answer)
  (putprop 'GP23 '"
    This is a legal logical proposition." 'Explanation)
  ($Stash '(Concept GP23 "(LOGICALOPERATOR NOT)"))
  ($Stash '(Concept GP23 "(ARGTYPE LOGICALPROP PROP)"))


;  (IF (HATES SUZY JIM) (EATS-WITH JIM (SISTER-OF SALLY)))
(setq GP24 '((gargi 0 p1 if)(argtype p1 RegProp)))
  (putprop 'GP24 'p 'answer)
  (putprop 'GP24 '"
    This is a legal logical proposition." 'Explanation)
  ($Stash '(Concept GP24 "(LOGICALOPERATOR IF)"))
  ($Stash '(Concept GP24 "(ARGTYPE LOGICALPROP PROP)"))


;  (AND (SISTER-OF JOE)(TALLER-THAN SUZY))
(setq GP15 '((firstarg p1 logicaloperator)(Argtype p1 ComplexTerm)))
  (Putprop 'GP15 'i 'answer)
  (putprop 'GP15 '"
    Logical operators must have propositional arguments.  In this problem
    the arguments are terms. " 'Explanation)
  ($Stash '(Concept GP15 "(ARGTYPE LOGICALPROP PROP)"))


;  (IF JOE FRED)
(setq GP16 '((firstarg p1 logicaloperator)(Argtype p1 Symbol)))
  (putprop 'GP16 'i 'answer)
  (putprop 'GP16 '"
    This is an illegal statement because logical operators must
    have propositions (facts) as their arguments.  Symbols are terms, 
    not propositions. " 'Explanation)
  ($stash '(Concept GP16 "(IF (SYMBOL $X) (TERM $X))"))
  ($stash '(Concept GP16 "(ARGTYPE LOGICALOPERATOR PROP)"))


;  (HATES FRED (AND GEORGE SUZY))
(setq GP17 '((firstarg p1 relation)(argstruc 1 p1 symbol) (gargi 2 p1 p2)
				   (firstarg p2 logicaloperator)
				   (argtype p2 symbol)))
  (putprop 'GP17 'i 'answer)
  (putprop 'GP17 '"
    There are two errors here.  First of all relations have arguments which are
    terms.  The second argument of this propositions starts out like it is 
    going to be a logical proposition.  Second, all logical operators must have
    propositional arguments.  In this case the arguments are symbols."
	   'Explanation)
  ($stash '(Concept GP17 "(ARGTYPE RELATION TERM"))
  ($stash '(Concept GP17 "(ARGTYPE LOGICALOPERATOR PROP)"))
  ($stash '(Concept GP17 "(IF (SYMBOL $X) (TERM $X))"))


;   (NOT (SISTER-OF GEORGE) SUZY)
(setq GP26 '((gargi 0 p1 not) (argstruc 1 p1 ComplexTerm)
			      (argstruc 2 p1 symbol)))
  (putprop 'GP26 'i 'answer)
  (putprop 'GP26 '"
    There are two errors here.  First of all, NOT can only have one argument.
    Secondly, NOT must negate a proposition.  In this problem, both arguments
    are terms.  The first is a complex term, the second is a symbol. "
	     'Explanation)
  ($Stash '(Concept GP26 "(LOGICALOPERATOR NOT)"))
  ($Stash '(Concept GP26 "(ARITY NOT 1)"))
  ($Stash '(Concept GP26 "(ARGTYPE LOGICALOPERATOR PROP)"))


;   (NOT (= (SISTER-OF GEORGE) FRED))
(setq GP27 '((Gargi 0 p1 not)(Gargi 1 p1 p2)(Gargi 0 p2 =)
	     (Argstruc 1 p2 Complexterm)(ArgStruc 2 p2 SyMBOL)))
  (putprop 'GP27 'P 'answer)
  (putprop 'GP27 '"
    This is a legal statement.  Because it states a fact it is a proposition.
    = is a relation that takes two arguments with must be terms, and NOT
    must modify a proposition. " 'Explanation)
  ($Stash '(Concept GP27 "(ARITY NOT 1)"))
  ($Stash '(Concept GP27 "(ARGTYPE LOGICALOPERATOR PROP)"))
  ($Stash '(Concept GP27 "(ARGTYPE RELATION TERM)"))


;   (LOGICALOPERATOR NOT)
(setq GP28 '((Gargi 0 p1 LogicalOperator) (gargi 1 p1 not)))
  (putprop 'GP28 'P 'answer)
  (putprop 'GP28 '"
    Although Not is a reserved word in MRS, this does not mean that you
    can't make statements about it. " 'Explanation)
  ($STASH '(Concept GP28 "(IF (SYMBOL $X) (TERM $X))"))
  ($STASH '(Concept GP28 "(DENOTES FACTS PROPOSITION)"))


;  Now arity stuff

;   (NOT (HATES SUZY SALLY)(LOVES JIM SUZY))
(setq GP19 '((Gargi 0 p1 NOT)(ArgStruc 1 p1 RegProp)
			     (ARGSTRUC 2 P1 RegProp)))
  (putprop 'GP19 'i 'answer)
  (putprop 'GP19 '"
    The Logical operator NOT can only have one propositional argument."
	     'Explanation)
  ($Stash '(Concept GP19 "(ARITY NOT 1)"))

;   (IF (LOVES SALLY JIM))
(setq GP20 '((Gargi 0 p1 IF)(Argstruc 1 p1 RegProp)))
  (putprop 'GP20 'i 'answer)
  (Putprop 'GP20 '"
    The Logical operator IF can only have two propositional arguments."
	     'Explanation)
  ($Stash '(concept GP20 "(ARITY IF 2)"))


; Quantified Propositions

;  (ALL X (LIKES X SUZY))
(Setq GP6 '((QuantifiedProp p1)))
  (putprop 'GP6 'P 'Answer)
  (putprop 'GP6 '"
    This is the proper form of a proposition with a quantifier.  It's last
    argument states a fact. " 'Explanation)
  ($Stash '(Concept GP6 "(ARITY QUANTIFIER GEQ-2)"))


(Setq GP35 '((FirstArg P1 Quantifier) (ArgStruc 1 p1 variable)
	     (ArgStruc 2 p1 variable) (ArgStruc 3 p1 variable)
	     (Argstruc 4 p1 RegProp)))
  (putprop 'GP35 'P 'answer)
  (putprop 'GP35 '"
    As many variables as needed can be quantified with one quantifier."
	     'Explanation)
  ($Stash '(Concept GP35 "(MIDDLEARGS QUANTIFIER VARS)"))


;  (EXISTS)
(Setq GP7 '((FirstARg p1 Quantifier)))
  (putprop 'GP7 'i 'answer)
  (putprop 'GP7 '"
    A quantifier must have at least one variable and one proposition as 
    arguments.  " 'Explanation)
  ($STASH '(Concept GP7 "(ARITY QUANTIFIER GEQ-2)"))


;  (ALL (HATES JOE MARY))
(setq GP8 '((firstarg p1 quantifier)(ArgStruc 1 p1 reGProp)))
  (putprop 'GP8 'i 'answer)
  (putprop 'GP8 '"
    A quantifier must have at least one variable to quantify." 'Explanation)
  ($Stash '(Concept GP8 "(ARITY QUANTIFIER GEQ-2)"))
  ($Stash '(Concept GP8 "(MIDDLEARGS QUANTIFIER VARS)"))


;  (ALL (COLOR-OF SUZY) (TALLER-THAN FRED) (HATES SUZY FRED))
(Setq GP9 '((firstarg p1 quantifier)(Middleargs p1 complexterm)
				    (lastarg p1 RegProp)))
  (putprop 'GP9 'i 'answer)
  (Putprop 'GP9 '"
    A quantifier must quantify one or more variables.  Although any
    symbol can be used in the variable position, complex terms like the
    ones in this problem are not allowed. " 'Explanation)
  ($Stash '(Concept GP9 "(MIDDLEARGS QUANTIFIERS VARS)"))


;  (ALL X2 (LIKES JOE FRED)(HATES SUZY CAROL))
(setq GP10 '((firstarg p1 quantifier) (ArgStruc 1 p1 variable)
	     (ArgStruc 2 p1 regprop) (ARgStruc 3 p1 regprop)))
  (putprop 'GP10 'i 'answer)
  (putprop 'GP10 '"
    A quantifier can only quantify one proposition." 'Explanation)
  ($Stash '(Concept GP10 "(MIDDLEARGS QUANTIFIER VARS)"))


;   (ALL FRED (COLOR-OF JOE))
(setq GP11 '((firstarg p1 quantifier)(Argtype p1 term)))
  (putprop 'GP11 'i 'answer)
  (putprop 'GP11 '"
    Quantifiers (ALL and EXIST) must have one or more variables to quantify, 
    as well as a proposition.  All the arguments in the example above are 
    terms, and although symbols can double as variables, complex or functional
    terms cannot.  Finally, the last argument must be a proposition."
	     'Explanation)
  ($Stash '(Concept GP11 "(LASTARG QUANTIFIER PROP)"))
  ($Stash '(Concept Gp11 "(MIDDLEARGS QUANTIFIER VARS)"))


;   (ALL SUZY FRED JIM)
(setq GP12 '((FirstArg p1 Quantifier)(Argtype p1 symbol)))
  (putprop 'GP12 'i 'answer)
  (putprop 'GP12 '"
    The last argument to a quantifier must be a proposition.  In this case
    the last argument is a term, not a proposition." 'Explanation)
  ($Stash '(Concept GP12 "(QUANTIFIER EXIST)"))
  ($Stash '(Concept GP12 "(LASTARG QUANTIFIER PROP)"))


;   (IF (HATES BILL CAROL)(ALL X (LOVES X (SISTER-OF SUZY))
(setq GP13 '((IMbed p1) (QuantifiedProp p1)))
  (putprop 'GP13 'P 'answer)
  (Putprop 'Gp13 '"
    If takes 2 propositional arguments.  Both of these arguments are legal 
    propositions." 'Explanation)
  ($Stash '(Concept GP13 "(ARGTYPE LOGICALOPERATOR PROP)"))
  ($Stash '(Concept GP13 "(PROPOSITION QUANTIFIEDPROP)"))


;   (ALL X Y (COLOR-OF Y))
(setq GP30 '((Firstarg p1 quantifier)(MiddleArgs p1 variable)
				     (lastarg p1 ComplexTerm)))
  (putprop 'GP30 'I 'answer)
  (putprop 'GP30 '"
    At first glance, this statement looks legal within the constraints of
    the MRS syntax.  However, under normal interpretations the last argument
    refers to a thing, and because of that, it is a term, not a proposition."
	     'Explanation)
  ($Stash '(Concept GP30 "(LASTARG QUANTIFIER PROP)"))


;   (HATES (ALL HORSES) SUZY)
(Setq GP31 '((firstarg p1 relation)(Gargi 1 p1 p2) (Gargi 0 P2 All)
	     (Gargi 1 p2 Horses)(Argstruc 2 p1 Symbol)))
  (Putprop 'GP31 'i 'answer)
  (putprop 'GP31 '"
    Although quantifiers do not have to be on the outermost level of a
    proposition, they must quantify a proposition.  (All Horses) is
    the illegal part of this statement.	" 'Explanation)
  ($Stash '(Concept GP31 "(ARITY QUANTIFIER GEQ-2)"))
  ($Stash '(Concept GP31 "(LASTARG QUANTIFIER PROP)"))


;   (ALL X Y (EXIST Z (AND (HATES X Y) (LOVES Z SUZY))))
(setq GP32 '((Firstarg p1 quantifier)(middleargs p1 variable)
				     (lastarg p1 quantifiedProp)))
  (putprop 'GP32 'P 'answer)
  (putprop 'GP32 '"
    Since a proposition that is quantified is a proposition,
    nesting quantified propositions is legal. " 'Explanation)
  ($STASH '(Concept GP32 "(PROPOSITION QUANTIFIEDPROP)"))


;   (EXISTS FROGS (HATES SALLY SUZY))
(setq GP29 '((Firstarg p1 quantifier) (Gargi 1 p1 Frogs)
				      (Argstruc 2 p1 ReGProp)))
  (putprop 'GP29 'p 'answer)
  (putprop 'GP29 '"
    The variables that a quantifier quantifies do not have to be found in
    the following proposition. (But it makes more sense if they do!) "
	     'Explanation)
  ($Stash '(Concept GP29 "(LASTARG QUANTIFIER PROP)"))
  ($Stash '(Concept GP29 "(MIDDLEARGS QUANTIFIER VARS)"))


|#


