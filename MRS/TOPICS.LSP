
perm filename TOPICS.LSP[MRS,LSP] blob sn#702142 filedate 1983-03-18 generic text, type T, neo UTF8


;**********************************************************
;*                                                        *
;*                   TOPICS.LSP                           *
;*       Textual material for the MRS tutor               *
;*        (c) Copyright 1982, Jan Clayton                 *
;*                                                        *
;**********************************************************


(declare (*fexpr untrace)
	 (*Expr TutorIntro SyntaxIntro MRSSyntax Symbols Terms
		 propositions logicalOperators Quantification 
		 QuantificationOperators AccessingDB Tstash TLookup
		 TUNstash TAssert Ttruep Tunassert BackwardChaining
		 ForwardChaining BAckwardvsforward Inference PatternMatching))



(defun TutorIntro ()
  (AddToModel '(MaterialPresented TutorIntro))
  (princ '"
	               WELCOME to the MRS Tutor!!!

    This tutor is designed to introduce you to the syntax and basic
database accessing functions of MRS.

	 Type ? to get a list of commands.
	 CTRL-S to STOP text printout.
	 CTRL-Q to resume text printout.
"))


(defun SyntaxIntro ()
  (AddToModel '(MaterialPresented SyntaxIntro))
  (Princ '"
    Representation languages provide a way to store and retrieve facts
from a computer.   Since English is a grammatically and textually ambiguous
language, representation systems use a more formal language to describe the
world.  The way in which the words or symbols of a language are put together
to form phrases and sentences is termed the ``syntax'' or ``grammar'' of the
language.  
    The next several sections of this tutorial will introduce you to the
syntax of the MRS language.
 "))



(defun MRSSyntax ()
  (AddToModel '(MaterialPresented MRSSyntax))
  (princ '"
    MRS is a predicate calculus-like language.  It is based upon
combinations of ``symbols'' into ``terms'' and ``propositions''.  Facts are
stated as N-tuples or lists which describe a relationship between individuals.
The statement

	 (NEIGHBOR BERTRAM ARTHUR)

consists of three symbols.  Neighbor represents a relationship
between the second and third symbols which represent objects (in this case
people) in the world.  The proposition above states that Bertram and
Arthur are neighbors.
     "))


(defun Symbols ()
  (AddtoModel '(MaterialPresented Symbols))
  (Princ '"
    In general, symbols within MRS expressions can represent a number of
things, including objects, functions, relations, actions, etc.  More
specifically, there are three general categories of symbols.

1.  Object symbols name specific objects, concepts or individuals.
    Examples: STANFORD, KENNEDY, ELEPHANTS, BLUE (the color), JEALOUSY")

  (Princ '"

2.  Function symbols represent functions on the objects in the world.
    Examples:  PRESIDENT-OF, HEIGHT-OF, +, COLOR-OF

3.  Relation symbols represent relations between objects in the world.
    Examples: NEIGHBOR, >, TALLER-THAN, =
"))




(defun Terms ()
  (AddToModel '(MaterialPresented Terms))
  (Princ '"
    Sometimes we need to talk about an individual or object that doesn't
have a name (or for whose name we don't know).  For example, Fido's
tail does not have a particular name or symbol associated with it, but
we may need to refer to it.  To solve this problem, symbols can be combined
into more complex expressions to form functional ``terms''.  Examples:")

  (Princ '"

	   (TAIL-OF FIDO)
	   (PRESIDENT-OF STANFORD)
	   (+ 2 2)
	   (HEIGHT-OF (PRESIDENT-OF STANFORD))

Expressions of this type along with all symbols are called
terms.  More formally, any N-ary function symbol associated with N terms
(Function term1 ... termN) is a term.")

(princ '"
It is important to note that even though a functional term like 
(MOTHER-OF FRED) may be associated with an individual in the world
named Sally, the database will not automatically know that fact.
"))


(defun Propositions ()
  (AddToModel '(MaterialPresented Propositions))
  (Princ '"
   In general, a fact is stated within MRS in the form of a
proposition which consists of any N-ary relation symbol and N terms,
(relation term1 ... termN).  

MRS allows the use of functional terms in stating facts about the world.

           (= (MOTHER-OF FRED) SALLY)
           (> (* 2 3) (+ 2 3))
	   (= (NAME (DOG-OF BEATRICE)) NOODLES)
"))


(defun LogicalOperators ()
  (AddToModel '(MaterialPresented LogicalOperators))
  (Princ '"
    Quite often facts cannot be stated as a direct fact like (NEIGHBOR
BERTRAM ARTHUR).  For example, you may want to say that Sally is not a
neighbor of Bertram, or that if George fails the test he will be sad.  
Complex propositions such as negations, disjunctions, conjunctions and
contingencies can be expressed as non-atomic propositions using the logical
symbols NOT, OR, AND and IF respectively.")

  (princ '"

	   (NOT (NEIGHBOR SALLY GEORGE))
	   (IF (FAILED GEORGE TEST)
	       (SAD GEORGE))
 	   (OR (NEIGHBOR PALOALTO MENLOPARK)
	       (NEIGHBOR SANFRANCISCO LOSANGELES))
"))


(defun Quantification ()
  (AddToModel '(MaterialPresented Quantification))
  (Princ '"
    Up to now we have only used symbols that are constants.  Quite often,
however, one needs to refer to arbitrary or unknown individuals.  For example
you might want to say ``Somebody killed George's aunt'' or ``elephants are
grey'' To do this you need to use universally or existentially quantified
symbols")

  (princ '"

	   (EXIST P (KILLED (AUNT-OF GEORGE) P))
	   (ALL E (IF (ELEPHANT E) (= (COLOR-OF E) GREY)))

The proposition (All x1 ... xN (prop x1 ... xN)) states that the proposition
(prop x1 ... xN) is true for all possible values of x1 ... xN.  The proposition
(Exists x1 ... xN (prop x1 ... xN)) states that there is some x1 ... xN for
which (prop x1 ... xN) is true." )

   (Princ '"
    The All and Exist quantifiers can also be nested within each other
or can be used with non-atomic propositions.  For example, you can
state ``for all numbers there is another number that is greater in
value'' with the proposition

        (ALL X (EXIST Y (> Y X)))

Each quantifier can also take multiple variables as arguments.

        (ALL H R (IF (AND (HORSE H) (RABBIT R))
		     (CAN-OUTRUN H R)))            ")

   (Princ '"
Be careful of your use of nested quantifiers.  A change in their position in
respect to each other can change the meaning of the proposition drastically.  
For example the two propositions

        (EXIST X (ALL Y (LOVES X Y)))
        (ALL X (EXISTS Y (LOVES X Y)))

mean ``Every person has someone who loves them'' and ``There is a
person that is loved by everyone'' respectively.  " ))



(defun QuantificationOperators ()
  (AddToModel '(MaterialPresented QuantificationOperators))
  (Princ '"
    You may use a shorthand for specifying universal and leftmost
existential variables.  The prefix character $ denotes a universally
quantified variable, whereas ? will label an existential variable.  To
illustrate this shorthand notation the following examples show
equivalent statements.")

  (princ '"

	   (ALL E (IF (ELEPHANT E) (GREY E))) 
	   (IF (ELEPHANT $E) (GREY $E))   

	   (EXIST F (FROG F))
           (FROG ?F)

    Now that you know the language of MRS, you can learn how to
enter and retrieve facts from the data base. 
"))


(defun AccessingDB ()
  (AddToModel '(MaterialPresented AccessingDB))
  (Princ '"
    Now that we know how to describe facts, it is necessary to know
how to tell MRS that you want to enter or remove a fact from the
system.  MRS provides the users with several subroutines to access and
modify the data base.  "))



(defun TStash ()
  (AddToModel '(MaterialPresented TStash))
  (empty 'tutor)
  (Princ '"
    A Simple way of entering facts into a data base is with the $Stash
command.  ($Stash <p>) enters the proposition <p> directly into the
current data base.  (Below and in subsequent examples MRS is doing the
actual evaluation of the typed expressions.)  ")
    (PrtExpr '|($STASH '(NEIGHBOR BEATRICE BERTRAM))|)
    (PrtExpr '|($STASH '(NEIGHBOR BEATRICE SALLY))|))



(defun TLookup ()
  (AddToModel '(MaterialPresented TLookup))
  ;  First get the right facts into the system.
  ($stash '(NEIGHBOR BEATRICE BERTRAM))
  ($stash '(NEIGHBOR BEATRICE SALLY))

  (Princ '"
    ($Lookup <P>) checks to see whether the proposition <P> has been
entered in to the current data base.  $Lookup makes no inferences on
other propositions in the database, but ``looks up'' <p> to see if it
has been entered.  If <P> contains any existential variables, the")
  (Princ '"
valued returned from $Lookup will contain a list of variable bindings
that satisfy <P>.  If <P> is free of existential variables and <P> is
in the data base, the result will be ((T . T)).
    The following facts are in the data base:
        (NEIGHBOR BEATRICE BERTRAM)
        (NEIGHBOR BEATRICE SALLY) " )
    (PrtExpr '|($LOOKUP '(NEIGHBOR BEATRICE BERTRAM))|)
    (PrtExpr '|($LOOKUP '(NEIGHBOR BEATRICE $X))|)

    (Princ '" 
The command $Lookups is an extension of $Lookup.  Like $Lookup, it
looks to see whether <p> is in the data base.  The major difference
between the two is when existential variables are used, $Lookups
returns ALL the bindings for which <P> is true.  " )
    (PrtExpr '|($LOOKUPS '(NEIGHBOR BEATRICE BERTRAM))|)
    (PrtExpr '|($LOOKUPS '(NEIGHBOR BEATRICE $X))|)
    (PrtExpr '|($LOOKUPS '(NEIGHBOR $Y BERTRAM))|))



(defun TUnStash ()
  (AddToModel '(MaterialPresented TUnStash))
  ($stash '(NEIGHBOR BEATRICE BERTRAM))
  ($stash '(NEIGHBOR BEATRICE SALLY))
  
  (Princ '"
    Individual propositions in the data base can be removed with the $UnStash
command. 
  The following facts are in the data base:
        (NEIGHBOR BEATRICE BERTRAM)
        (NEIGHBOR BEATRICE SALLY)
")
  (PrtExpr '|($UNSTASH '(NEIGHBOR BEATRICE BERTRAM))|)
  (PrtExpr '|($LOOKUP '(NEIGHBOR BEATRICE BERTRAM))| )
  (PrtExpr '|($LOOKUPS '(NEIGHBOR BEATRICE $X))|))


(defun TAssert ()
  (AddToModel '(MaterialPresented TAssert))
  (empty 'tutor)
  (Princ '"
    Usually a user will not only want to have facts stored in the database,
but will also want to enable the system to do a certain amount of reasoning 
about those facts at a later time.  ($Assert <p>) enters a proposition and
performs all appropriate forward inference on that fact.  In the initial
start-up system of MRS $STASH and $ASSERT are equivalent.
")
    (PrtExpr '|($ASSERT '(APPLE FRED))| ) 
    (prtExpr '|($ASSERT '(IF (APPLE $X) (RED $X)))|))


(defun TTruep ()
  (AddToModel '(MaterialPresented TTruep))
  (princ '"
    ($Truep <p>) attempts to infer whether the proposition <p> is
true.  <p> does not need to be in the database verbatim for $Truep to
return ((T.T)) as was the case for $Lookup.  Instead it has the
ability to ``infer'' from other propositions that <p> is true.  If
existential variables are used in <p>, then $Truep will return a list
of bindings for $X that satisfy <p>.")

 (Princ '"

NOTE: In the current version of MACLISP MRS, existential and universal
variables must be reversed when using any of the commands that query
the database (LOOKUP, TRUEP, GETVAL...).  For example ``Are there any
frogs?'' translates into ($TRUEP '(FROG $X)) not ($TRUEP '(FROG ?X)).

    The following facts are in the database:
	 (APPLE FRED)
	 (IF (APPLE $X) (RED $X))
")
  ($Assert '(Apple Fred))
  ($Assert '(If (Apple $X) (Red $X)))

   (PrtExpr '|($TRUEP '(APPLE FRED))| )
   (PrtExpr '|($LOOKUP '(APPLE FRED))| )
   (PrtExpr '|($TRUEP '(RED FRED))| )
   (PrtExpr '|($LOOKUP '(RED FRED))|)
   (PrtExpr '|($TRUEP '(RED $X))| )

   (Princ '"
    From the example you can see that $Truep is a much more powerful command
than $Lookup.  In addition, there is a extension to $Truep called $Trueps
which returns all bindings for an existential variable in <p> that satisfy
<p>.
 ")

   (PrtExpr '|($ASSERT '(RED BARNEY))| )
   (PrtExpr '|($TRUEPS '(RED BARNEY))| )
   (PrtExpr '|($TRUEPS '(RED $W))| ))


(defun TUnAssert ()
  (AddToModel '(MaterialPresented TUnAssert))
  (princ '"
    $ASSERT also has a partner command, ($UNASSERT <p>), which not only removes
<p> from the data base, but also undoes any forward inferences that might have
been made when the proposition was originally entered. 
     Facts in the data base are:
	 (APPLE FRED)
	 (IF (APPLE $X) (RED $X))
	 (RED BARNEY)
" )
  ($Assert'(Apple Fred))
  ($Assert '(If (Apple $X) (Red $X)))
  ($Assert '(Red Barney))

  (PrtExpr '|($UNASSERT '(IF (APPLE $X) (RED $X)))| ) 
  (PrtExpr '|($TRUEP '(RED FRED))| )
  (PrtExpr '|($TRUEPS '(RED $WHO))|))


(defun Inference ()
  (AddToModel '(MaterialPresented Inference))
  (princ '"
    In past units, we have refered to inference a number of times without
explaining what is meant by the word.  Formally, inference refers to the
act of deriving a conclusion in logic by either induction or deduction.")

  (Princ '"
In MRS, conclusions are derived from facts and rules that are already in
the database.   There are a number of techniques that can be used to 
infer new facts; the two presented here are backward and forward chaining
since those are the two most heavily used inference techniques in rule-based
systems.")

  (Princ '"
It is important to state that inference techniques are independent
of any particular representation or methods of retrieving facts.  
Both of the chaining techniques mentioned above are used with many different
types of representations: frames, semantic nets, and production systems to 
name a few.")

)

;(defun PatternMatching ()
;  (AddToModel '(MaterialPresented PatternMatching)))
;


(defun BackwardChaining ()
  (AddToModel '(MaterialPresented BackwardChaining))
  (Princ '"
    As mentioned previously, if a fact is not found directly in the database,
$Truep will attempt to infer the fact from others that are in the database.
The type of inference used is backward chaining (goal-directed reasoning).

	   (IF (ELEPHANT $X) (GREY $X))
	   (ELEPHANT CLYDE)")
  (Princ '"

    If these two assertions are in the database, and we ask ``Is Clyde 
grey?'' [($TRUEP '(GREY CLYDE))], MRS will first look for the fact
(GREY CLYDE).  When its search is unsuccessful, MRS will look for implications
that might be able to conclude that Clyde is grey (ALL X (IF (...) (GREY X)).
In practice, MRS tries to match the right-hand side of all implications with
(GREY CLYDE).")
  (Princ '"

    In this particular instance, MRS finds the implication 
(IF (ELEPHANT $X) (GREY $X)) and then tries to prove that the left-hand
side of the implication (ELEPHANT CLYDE) is true.  Since that assertion is
in the database, Truep returns ((T . T)) which is the truth value.")
  (Princ '"


    Backward chaining does not necessarily stop after examining one level
of implications.  If (ELEPHANT CLYDE) had not been in the database, MRS
would have looked for implications that might be able to prove that Clyde
is an elephant.")
  (Princ '"


    Below is a more extensive example of backward chaining, where the
queries that $TRUEP makes in trying to prove a fact are traced.  The following
assertions are assumed in the database.

        (IF (AND (PLANT $P) (PURPLE $P)) (POISONOUS $P))
        (IF (MUSHROOM $M) (PLANT $M)) 
        (IF (TREE $T) (PLANT $T))
        (IF (VIOLET $X) (PURPLE $X))
        (VIOLET PHIL) 
        (MUSHROOM PHIL) ")

  ($Assert '(If (and (plant $p) (purple $p)) (poisonous $p)))
  ($Assert '(If (mushroom $m) (plant $m)) )
  ($Assert '(If (tree $t) (plant $t)))
  ($Assert '(If (violet $x) (purple $x)))
  ($Assert '(violet Phil) )
  ($Assert '(mushroom Phil))

  (Trace Truep)
  (PrtExpr '|($TRUEP '(POISONOUS PHIL))|)
  (Untrace truep))


(defun ForwardChaining ()
  (AddToModel '(MaterialPresented ForwardChaining))
  (Princ '"
    Forward chaining is another type of reasoning that MRS can perform.
As the name implies, forward chaining reasons in the opposite of backward
chaining.  Instead of being ``goal-directed'', forward chaining is
``data-directed'', that is initiated and driven by the addition of new facts.")
  (Princ '"

    If a new fact is asserted (when forward chaining is turned on), MRS will 
automatically try to prove as many facts as possible from this new assertion.
The forward chaining mechanism attempts to match the ``left side'' of each 
implication with the new fact, and if successful will assert the proposition  
of the right side.")
  (Princ '"


    Let's say that the following facts are in the data base:
                (IF (ELEPHANT $X) (GREY $X))
                (IF (ELEPHANT $X) (PACHYDERM $X))

    If we then assert (ELEPHANT CLYDE), MRS will automatically assert both
(GREY CLYDE) and (PACHYDERM CLYDE).")
  (Princ '"

Note:    If (IF (ELEPHANT $X) (VEGETARIAN $X)) is added later, the fact
(VEGETARIAN CLYDE) will not automatically be asserted!  In the case of 
asserting a new implication, MRS will not try to forward chain on the left
side of the new implication, just on the entire implication.  This is not 
an inherit insufficiency of forward chaining, but it is a reasoning 
limitation within the current MRS implementation.")
  (Princ '"

    Note also that forward chaining is not turned on in the initial MRS 
system.  To do so, give the MRS command ($ASSERT '(MYTOASSERT $X FS-ASSERT)).

The following trace of MRS forward chaining uses the assertions below.

           (IF (AND (PLANT $P) (PURPLE $P)) (POISONOUS $P))
           (IF (TOADSTOOL $X) (PLANT $X))
           (PURPLE PHIL)
           (PURPLE FRED) 
")
  ($Assert '(If (and (plant $p) (purple $p)) (poisonous $p)))
  ($Assert '(If (toadstool $x) (plant $x)))
  ($Assert '(purple phil))
  ($Assert '(purple fred))

  ($Assert '(MytoAssert $x Fs-Assert))
  (Trace Assert)
  (PrtExpr '|($ASSERT '(TOADSTOOL PHIL))|)
  (Untrace assert)
  ($Assert '(MytoAssert $x Stash)))



(defun BackwardvsForward ()
  (AddToModel '(MaterialPresented BackwardvsForward))
    (Princ '"
    From the previous sections on Forward and Backward chaining, it is hard
to see any advantage of one over the other.  From the examples
they appear to be almost exactly the same.  What are the differences?

    The most obvious difference is that forward chaining occurs when you are
building the database.  There is no need to query the database to get the
inference mechanism to work.  On the otherhand, backward chaining occurs
only when a query takes place.")
(Princ '"

    Although the above difference may not influence the choice between the
use of the two reasoning schemes, the following should.  The major
difference between the forward and backward chaining has to do with the
shape of the database that they search best.  If a database has many initial 
facts with implications that determine very few goal states (fan-in),")
  (Princ '"
it is best to use forward chaining.  However, if your database can have
a large number of possible conclusions with a small set of initial facts,
backward chaining is the inference method of choice.  Why?  Because in each
case it is best to limit the amount of time infering facts that will never
be used.")
  (Princ '"

   The following example should shed some light on the subject.

	   (IF (ELEPHANT $X) (PACHYDERM $X))
	   (IF (ELEPHANT $X) (BIG $X))
	   (IF (BIG $X) (HEAVY $X))
	   (IF (ELEPHANT $X) (STRONG $X))")
  (Princ '"

    If we assert the fact (ELEPHANT CLYDE) with the above assertions in the
database, a forward chainer would immediately assert four additional facts,
(PACHYDERM CLYDE), (BIG CLYDE), (HEAVY CLYDE) and (STRONG CLYDE).  If
(ELEPHANT EDWARD) is asserted, four additional facts will be put in the
database.  There are many possible conclusions for each new fact in this 
this database (it has the characteristic of fan-out).")
  (Princ '"

    A backward chainer in this situation would not asserted any new facts
when (ELEPHANT CLYDE) is added, but would create new facts only if a query
is made.  To find out (STRONG CLYDE) there is no need to know (HEAVY CLYDE),
etc., and the backward chainer will not try to prove these facts. ")
  (Princ '"

    It is evident from this example that backward chaining is best to use
with a database like the one above.


    The similarities between the two mechanisms are also important.  Both
forward and backward chaining use the same rule of inference, Modus Ponens.
This method of inference is also entirely separate from the method of search
used to find facts in the database.")
  (Princ '"

Caution!  There are possible problems that can arise using chaining.
One can construct a database that will reason in loops.  MRS, at this time,
is unable to stop circular reasoning, so beware of situations like the
following.")
  (Princ '"

For forward chaining:
        1)  (IF (AND (INTEGER $X) (INTEGER $Y)) (INTEGER (+ $X $Y)))
	2)  (IF (P A) (P B))
	    (IF (P B) (P C))
	    (IF (P C) (P A))

For backward chaining:
	1)  (IF (AND (INTEGER $X) (INTEGER (+ $Y $X))) (INTEGER $Y))
	2)  (IF (P A) (P B))
	    (IF (P B) (P C))
	    (IF (P C) (P A))

    All analogous situations will cause MRS to loop indefinitely. 
"))






;  Now put in all the special information that is needed for the tutor to
;  work.  Eventually nearly all of this information will be contained in
;  MRS assertions, but for now there is a combination of assertions and
;  property lists.

(putprop 'SyntaxIntro 'AccessingDB 'NextTopic)
    (putprop 'MRSSyntax 'Symbols 'NextTopic)
    (putprop 'Symbols 'Terms 'NextTopic)
    (putprop 'Terms 'Propositions 'NextTopic)
    (putprop 'Propositions 'LogicalOperators 'NextTopic)
    (putprop 'LogicalOperators 'SyntaxExercises1 'NextTopic)
    (putprop 'SyntaxExercises1 'Quantification 'NextTopic)
    (putprop 'Quantification 'QuantificationOperators 'NextTopic)
    (putprop 'QuantificationOperators 'SyntaxExercises2 'NextTopic)


(putprop 'AccessingDB 'Inference 'NextTopic)
    (putprop 'TStash 'TLookup 'NextTopic)
    (putprop 'TLookup 'TUnstash 'NextTopic)
    (putprop 'TUnstash 'TAssert 'NextTopic)
    (putprop 'TAssert 'TTruep 'NextTopic)
    (putprop 'TTruep 'TUnassert 'NextTopic)
    (putprop 'TUnAssert 'AccessingExercises 'NextTopic)

(putprop 'Inference 'NIL 'NextTopic)
;   (putprop 'PatternMatching 'BackwardChaining 'NextTopic)
    (putprop 'BackwardChaining 'ForwardChaining 'NextTopic)
    (putprop 'ForwardChaining 'BackwardvsForward 'NextTopic)
    (putprop 'BackwardvsForward 'InteractiveExercises 'nextTopic)

(putprop 'TutorIntro '"Introduction to the MRS Tutor" 'TableName)
    (putprop 'SyntaxIntro  '"What is a Representation Language?" 'TableName)
    (putprop 'MRSSyntax '"The Syntax of MRS" 'TableName)  
    (putprop 'Symbols '"Types of Symbols" 'TableName) 
    (putprop 'Terms '"Terms" 'TableName)
    (putprop 'Propositions '"Atomic Propositions" 'TableName)
    (putprop 'LogicalOperators '"Logical Proposistions" 'TableName)
    (putprop 'SyntaxExercises1 '"Exercises" 'TableName)
    (putprop 'Quantification '"Quantified Propositions" 'TableName)
    (putprop 'QuantificationOperators '"Free Variables in Proposistions" 
	     'TableName)
    (putprop 'SyntaxExercises2 '"Exercises" 'TableName)


(putprop 'AccessingDB '"Entering and Accessing Facts" 'TableName)
    (putprop 'TStash '"The Stash Command" 'TableName)
    (putprop 'TLookup '"Lookup" 'TableName)
    (putprop 'TUnstash '"Removing Facts" 'TableName)
    (putprop 'TAssert '"Entering Facts with Assert" 'TableName)
    (putprop 'TTruep '"Inference and Truep" 'TableName)
    (putprop 'TUnassert '"Unassert" 'TableName)
    (putprop 'AccessingExercises '"Exercises" 'TableName) 

(putprop 'Inference '"Using Inference" 'TableName)
    (putprop 'PatternMatching '"Pattern Matching" 'Tablename)
    (putprop 'BackwardChaining '"Truep and Backward Chaining" 'TableName)
    (putprop 'ForwardChaining '"Assert and Forward Chaining" 'TableName)
    (putprop 'BackwardvsForward '"Forward vs. Backward Chaining" 'TableName)
    (putprop 'InteractiveExercises '"Interacting with MRS" 'TableName)

(putprop 'TutorIntro '(SyntaxIntro AccessingDB Inference) 'Subtopics)
(putprop 'SyntaxIntro '(MRSSyntax Symbols Terms Propositions LogicalOperators 
			SyntaxExercises1 Quantification QuantificationOperators
			SyntaxExercises2)
	 'Subtopics) 
(putprop 'AccessingDB '(TStash TLookup TUnStash TAssert TTruep TUnassert 
			       AccessingExercises) 
	 'SubTopics) 

(putprop 'Inference '(BackwardChaining ForwardChaining 
                      BackwardvsForward InteractiveExercises)
	 'SubTopics)

(let ((theory 'tutorsetup))

  ($Assert '(TopicNum TutorIntro i))

;  ($Assert '(TopicNum SyntaxIntro #+maclisp /1 #+franz\1))
  ($Assert '(TopicNum SyntaxIntro 1))
  ($Assert '(TopicNum MRSSyntax 1-1))
  ($Assert '(TopicNum Symbols 1-2))
  ($Assert '(TopicNum Terms 1-3))
  ($Assert '(TopicNum Propositions 1-4))
  ($Assert '(TopicNum LogicalOperators 1-5))
  ($Assert '(TopicNum SyntaxExercises1 E-1))
  ($Assert '(TopicNum Quantification 1-6))
  ($Assert '(TopicNum QuantificationOperators 1-7))
  ($Assert '(TopicNum SyntaxExercises2 E-2))

;  ($Assert '(TopicNum AccessingDB #+maclisp /2 #+franz \2))
  ($Assert '(TopicNum AccessingDB 2))
  ($Assert '(TopicNum TStash 2-1))
  ($Assert '(TopicNum TLookup 2-2))
  ($Assert '(TopicNum TUnstash 2-3))
  ($Assert '(TopicNum TAssert 2-4))
  ($Assert '(TopicNum TTruep 2-5))
  ($Assert '(TopicNum TUnassert 2-6))
  ($Assert '(TopicNum AccessingExercises E-3))

;  ($assert '(TopicNum Inference #+maclisp /3 #+franz \3))
    ($assert '(TopicNum Inference 3))
;  ($Assert '(TopicNum patternMatching 3-1))
  ($Assert '(TopicNum BackwardChaining 3-1))
  ($Assert '(TopicNum ForwardChaining 3-2))
  ($Assert '(TopicNum BackwardvsForward 3-3))
  ($assert '(TopicNum InteractiveExercises E-4))

  
  ($assert '(SuperTopic TutorIntro NIL))		       
  ($assert '(SuperTopic SyntaxIntro TutorIntro))
  ($assert '(SuperTopic MRSSyntax SyntaxIntro))
  ($assert '(SuperTopic Symbols SyntaxIntro))
  ($assert '(SuperTopic Terms SyntaxIntro))	
  ($assert '(SuperTopic Propositions SyntaxIntro))
  ($assert '(SuperTopic LogicalOperators SyntaxIntro))
  ($assert '(SuperTopic Quantification SyntaxIntro))
  ($assert '(SuperTopic QuantificationOperators SyntaxIntro))

  ($assert '(SuperTopic AccessingDB TutorIntro))
  ($assert '(SuperTopic TStash AccessingDB))
  ($assert '(SuperTopic TLookup AccessingDB))
  ($assert '(SuperTopic TUnstash AccessingDB))
  ($assert '(SuperTopic TAssert AccessingDB))
  ($assert '(SuperTopic TTruep AccessingDB))
  ($assert '(SuperTopic TUnassert AccessingDB))

  ($assert '(SuperTopic Inference TutorIntro))
  ($Assert '(SuperTopic PatternMatching Inference))
  ($assert '(SuperTopic BackwardChaining Inference))
  ($assert '(SuperTopic ForwardChaining Inference))
  ($assert '(SuperTopic BackwardvsForward Inference))

  ($assert '(SuperTopic SyntaxExercises1 SyntaxIntro))
  ($assert '(SuperTopic SyntaxExercises2 SyntaxIntro))
  ($assert '(SuperTopic AccessingExercises AccessingDB))
  ($assert '(SuperTopic InteractiveExercises Inference))
)

