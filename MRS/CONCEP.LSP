
perm filename CONCEP.LSP[MRS,LSP] blob sn#702116 filedate 1983-03-18 generic text, type T, neo UTF8


;**********************************************************
;*                                                        *
;*                 CONCEPTS.LSP                           *
;*                 The MRS tutor                          *
;*     Concept and exercise templates for the generator   *
;*        (c) Copyright 1982, Jan Clayton                 *
;*                                                        *
;**********************************************************

(Putprop '|(IF (SYMBOL $X) (TERM $X))|
	 '(GP1 GP2 GP25 GP16 GP17 GP28) 
	 'GenProbList)

(Putprop '|(IF (SYMBOL $X) (TERM $X))|
	 '|
    Symbols are terms by definition.  In MRS even symbols which have
other functions can also be used as terms.  For example, it is legal
to make statements like "(LOGICALOPERATOR AND)" and "(RELATION HATES)".|
	 'RemedialMaterial)

    
(putprop '|(LOGICALOPERATOR AND)|
	 '(GP21)
	 'GenProbList)

(putprop '|(LOGICALOPERATOR AND)|
	 '|
    AND is a logical operator.  The structure for all propositions that
begin with AND is (AND p1 p2 p3 ...).|
	 'RemedialMaterial)

(putprop '|(LOGICALOPERATOR OR)|
	 '(GP22)
	 'GenProbList)

(putprop '|(LOGICALOPERATOR OR)|
	 '|
    OR is a logical operator.  The structure for all propositions that
begin with OR is (OR p1 p2 p3 ...).|
	 'RemedialMaterial)

(putprop '|(LOGICALOPERATOR NOT)|
	 '(GP23 GP26)
	 'GenProbList)

(putprop '|(LOGICALOPERATOR NOT)|
	 '|
    NOT is a logical operator.  The structure for all propositions that
begin with NOT is (NOT p), where p is proposition.|
	 'RemedialMaterial)


(putprop '|(LOGICALOPERATOR IF)|
	 '(GP24)
	 'GenProbList)

(putprop '|(LOGICALOPERATOR IF)|
	 '|
    IF is a logical operator.  The structure for all propositions that
begin with IF is (IF p1 p2), where p1 and p2 are propositions.|
	 'RemedialMaterial)


(Putprop '|(DENOTES OBJECT TERMS)|
	 '(GP4 GP33 GP5)
	 'GenProbList)

(Putprop '|(DENOTES OBJECT TERMS)|
	 '|
    One of the distinguising features between terms and propositions are
that terms represent a thing (person, object, place, concept, etc.) in the
world.  (Sister-of Suzy) refers to a person.  On the otherhand, propositions 
are statements of facts.|
	 'RemedialMaterial)


(putprop '|(DENOTES FACTS PROPOSITIONS)|
	 '(GP34 GP5 GP28)
	 'GenProbList)

(putprop '|(DENOTES FACTS PROPOSITIONS)|
	 '|
    Propositions represent facts in the world that is begin modelled.
(CARPENTER JIM) states that fact that Jim is a Carpenter.  Terms, on the
other hand, represent objects.|
	 'RemedialMaterial)


(putprop '|(PROPOSITION QUANTIFIEDPROP)|
	 '(GP13 GP32)
	 'GenProbList)

(putprop '|(PROPOSITION QUANTIFIEDPROP)|
	 '|
    Propositions with with form (ALL var1 ... p1) or (EXIST var1 ... p1)
can be used in any place that a proposition is needed.  They can be nested
with in other quantifiers, or logical operators.|
	 'RemedialMaterial)


(putprop '|(PROPOSITION LOGICALPROP)|
	 '(GP21)
	 'GenProbList)

(putprop '|(PROPOSITION LOGICALPROP)|
	 '|
    Propositions with the form (LogicalOperator p1 ... pN) can be used in any
place that a propostion is needed.  It can be nested within another logical
operator or a quantifier.|
	 'RemedialMaterial)


(putprop '|(FIRSTARG PROPOSITION SYMBOL)|
	 '()
	 'GenProbList)

(putprop '|(FIRSTARG PROPOSITION SYMBOL)|
	 '| 
    The first argument of a proposition must always be a symbol.  That
symbol can either be a Relation, a Logical operator or a quantifier.|
	 'RemedialMaterial)


(putprop '|(FIRSTARG RELPROP RELATION)|
	 '(GP5)
	 'GenProbList)

(putprop '|(FIRSTARG RELPROP RELATION)|
	 '||
	 'RemedialMaterial)


(putprop '|(QUANTIFIER ALL)|
	 '()
	 'GenPropList)

(putprop '|(QUANTIFIER ALL)|
	 '|
ALL is a quantifier.  It is used to quantify variables in propositions
with the form (ALL var1 ... varN p).|
	 'RemedialMaterial)


(putprop '|(QUANTIFIER EXIST)|
	 '(GP12)
	 'GenPropList)

(putprop '|(QUANTIFIER EXIST)|
	 '|
    EXIST is a quantifier.  It is used to quantify variable in propositions
with the form (EXIST var1 ... varN p).|

	 'RemedialMaterial)


(Putprop '|(ARITY QUANTIFIER GEQ-2)|
	 '(GP6 GP7 GP8 GP31)
	 'GenProbList)

(Putprop '|(ARITY QUANTIFIER GEQ-2)|
	 '|
    A quantifier must be followed by at least one variable and a proposition.
	 |
	 'RemedialMaterial)


(putprop '|(MIDDLEARGS QUANTIFIER VARS)|	
	 '(GP35 GP8 GP9 GP10 GP11 GP29)
	 'GenProbList)

(putprop '|(MIDDLEARGS QUANTIFIER VARS)|	
	 '| 
   All the arguments following a quantifier, except the last must be symbols
which are treated as variables.  There must be at least one variable following
a quantifier.

Correct:	 (ALL X Y (FOLLOWS X Y))
	         (EXISTS P (STUMBLES P))
Incorrect:       (ALL (FROG X))|
	 'REMEDIALMATERIAL)


(putprop '|(LASTARG QUANTIFIER PROP)|
	 '(GP11 GP12 GP30 GP31 GP29)
	 'GenProblist)

(putprop '|(LASTARG QUANTIFIER PROP)|
	 '|
    The last argument of a quantifier must always be a proposition.

Correct:     (ALL X (FROG X))
Incorrect:   (ALL X (COLOR-OF X))
	     (EXISTS P R)|
	 'RemedialMaterial)


(putprop '|(ARITY NOT 1)|
	 '(GP26 GP27 GP19)
	 'GenProbList)

(putprop '|(ARITY NOT 1)|
	 '|
    NOT must have only one propositional argument.  No other number of
arguments is legal.

Correct:    (NOT (AND (FLOWER PANSY) (TREE FIR)))
Incorrect:  (NOT (FLOWER PANSY) (TREE FIR))|
	 'REMEDIALMATERIAL)


(putprop '|(ARITY IF 2)|
	 '(GP20)
	 'GenProbList)

(putprop '|(ARITY IF 2)|
	 '|
    IF must have two propositional arguments.  No other number of arguments
is correct.

Correct:    (IF (EASTS-MEAT $X) (CARNIVORE $X))
Incorrect:  (IF (LOVES SUZY BILL))|
	 'RemedialMaterial)


(putprop '|(ARGTYPE LOGICALOPERATOR PROP)|
	 '(GP14 GP21 GP22 GP23 GP24 GP16 GP15 GP17 GP26 GP27 GP13)
	 'GenProbList)

(putprop '|(ARGTYPE LOGICALOPERATOR PROP)|
	 '|
    All the arguments of logical operators must be propositions.  Since
nesting of propositions is allowed, these propositions can begin with
a relation, logical operator or quantifier.

	 (OR (FROG FRED) (SALAMANDER SUZY))
	 (IF (FROG FRED) (OR (SNAKE SAM) (LIZARD LOUIS)))
	 (NOT (All X Y (IF (AND (PRINCESS X) (FROG Y)) (KISSES X Y))))|
	 'RemedialMaterial)


(putprop '|(ARGTYPE RELATION TERM)|
	 '(GP34 GP3 GP17 GP27)
	 'GenProbList)

(putprop '|(ARGTYPE RELATION TERM)|
	 '|
    The arguments of all relations must be terms.   The terms can either be
functional terms or symbols.

	 (= 2 (+ 1 1))
	 (COLOR-OF TABLE1 RED)
	 (TALLERTHAN (SISTER-OF JOE) FRED)|
	 'RemedialMaterial)


(putprop '|(ARGTYPE FUNCTION TERM)|
	 '(GP33)
	 'GenProbList)

(putprop '|(ARGTYPE FUNCTION TERM)|
	 '|
    The argument type of all functions is term.  All the arguments must
be terms, either functional (complex) terms or symbols. 

	 (COLOR-OF TABLE)
	 (COLOR-OF (TABLE-OF MARY))  

	 (+ 2 3)
	 (+ (* 3 4) (- 5 2))|
	 'RemedialMaterial)


(putprop '|(FIRSTARG COMPLEXTERM FUNCTION)|
	 '()
	 'GenProbList)

(putprop '|(FIRSTARG COMPLEXTERM FUNCTION)|
	 '|
     The first argument of a functional term is a function.|
	 'RemedialMaterial)


(putprop '|(ARITY FUNCTION GEQ-1)|
	 '()
	 'GenProbList)

(putprop '|(ARITY FUNCTION GEQ-1)|
	 '|
    A function must always have at least one argument.  The number of
arguments will depend upon the function itself.

    Examples:
	 (COLOR-OF FRED)
	 (TAIL-OF FIDO)
	 (+ 2 3)|
	 'RemedialMaterial)


(putprop '|(ARITY AND ARBITRARY)|
	 '()
	 'GenProbList)

(putprop '|(ARITY AND ARBITRARY)|
	 '|
    AND can have any number of arguments.  Ususally there will be two
or more, but 0 or 1 are legal within the rules of MRS syntax.|
	 'RemedialMaterial)


(putprop '|(ARITY OR ARBITRARY)|
	 '()
	 'GenProbList)

(putprop '|(ARITY OR ARBITRARY)|
	 '|
    OR can have any number of arguments.  Usually there will be two
or more, but 0 or 1 are legal within the rules of MRS syntax.|
	 'RemedialMaterial)



