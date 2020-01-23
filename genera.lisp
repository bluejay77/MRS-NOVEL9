;;; -*- Mode: Common-LISP -*-
;;;
;;;GENERA.LSP    1.7 18-Mar-83 0134 000   1 ML E      18-Mar-83   
;;;	EXample Generator for MRS TUTOR
;;;
;;;perm filename GENERA.LSP[MRS,LSP] blob sn#702124 filedate
;;;	1983-03-18 generic  text, type C, neo UTF8
;;;
;;;COMMENT ⊗   VALID 00004 PAGES
;;;C REC  PAGE   DESCRIPTION
;;;C00001 00001
;;;C00002 00002	**********************************************************
;;;C00022 00003
;;;C00027 00004
;;;C00030 ENDMK
;;;C⊗;
;**********************************************************
;*                                                        *
;*                GENERATOR.LSP                           *
;*        Example Generator for the MRS tutor             *
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



;;; (declare (special genvars rel numvars genvarlist val arity dictionary
;;;		  pname theory))


;;; Faulty, needs to be corrected later on

#|

($Stash '(MRSSuperStructure Term))
($Stash '(MRSStructure ComplexTerm))
($Stash '(MRSSuperStructure Proposition))
($Stash '(MRSStructure RegProp))
($Stash '(MRSStructure LogicalProp))
($Stash '(MRSStructure QuantifiedProp))

($Stash '(LogicalOperator OR))	     
($Stash '(LogicalOperator AND))	       
($Stash '(LogicalOperator IF))
($Stash '(LogicalOperator NOT))

($Stash '(ArityType Or Arbitrary))           ; Orginally for the formalization
($Stash '(ArityType And Arbitrary))          ; of the MRS syntax.
($Stash '(ArityType Not Fixed))              
($Stash '(ArityType If Fixed))               
                                              

($Stash '(Arity If 2))
($Stash '(Arity Not 1))
; ($Stash '(Arity And NIL))
; ($Stash '(Arity Or NIL))

($Stash '(PlaceDescriptor FirstArg))         ; for the random part of the 
($Stash '(PlaceDescriptor MiddleArgs))       ; generator
($Stash '(PlaceDescriptor LastArg))
($Stash '(PlaceDescriptor Argtype))

($Stash '(SymbolType Relation))           ; Formalization for the random 
($Stash '(SymbolType Function))           ; part of the generator.
($Stash '(SymbolType LogicalOperator))
($Stash '(SymbolType Symbol))
($Stash '(SymbolType Quantifier))
($Stash '(symbolType Variable))


($Stash '(Simple Proposition RegProp))       ; A statement that says that they
($Stash '(Simple Term Symbol))               ; do not allow recursion of their
                                              ; own type.

($Stash '(CanImbedProp QuantifiedProp))
($Stash '(ImbedAt Quantifiedprop N))

($Stash '(CanImbedProp LogicalProp))
($Stash '(ImbedAt LogicalProp N))

($Stash '(Quantifier ALL))
($Stash '(Quantifier EXIST))

($Stash '(Variable X))                       ; A generator dictionary of vars.
($Stash '(Variable Y))
($Stash '(Variable Z))
($Stash '(Variable X2))
($Stash '(Variable X3))
($Stash '(Variable X4))
($Stash '(Variable X5))

($Stash '(Dictionary Dict1))
($Stash '(Dictionary Dict2))

;;  Below is a fairly detailed formalizm of MRS syntax in predicate calculus.
;;  Most of this information is used by the random part of the generator, but
;;  little of it is used by the part of the generator that parses the command
;;  language from the tutor.


;;  There are basically two types of terms,  symbols and Complex terms.
;;  There was no reason to define both SIMPLETERM and COMPLEXTERM, so
;;  symbols are considered here as "simple terms".

($Assert '(If (Term $t) (OR (Symbol $t) (ComplexTerm $t))))

($Assert '(Iff (ComplexTerm $t) 
	       (AND (FirstArg $t Function)
		    (Argtype $t Term))))
	 

;; This is basically an arity check that I am not using right now in my
;; formalization, but something like it should be put in.
;;($Assert '(If (ComplexTerm $t)
;;	     (= (NumArgs $t) (TakesNumArgs (FirstArg $t)))))



;;  I am splitting propositions into 3 groups for simplification
;;  The three groups are different enough to be treated separately.

($Assert '(If (Proposition $P) 
	      (OR (RegProp $P) (LogicalProp $P) (QuantifiedProp $P))))



;;  A regular proposition is much less complicated than the others.
;;  It consist of a RELATION and a specified number of arguments that are
;;  terms.  What is missing from this definition is a statement about the
;;  ARITY of the relation being used in the proposition.  The arity needs
;;  to match the number of arguments generated.   This information is 
;;  hardwired into the generator which know that it has to look for the
;;  arity of the relation before generating the arguments.


($Assert '(Iff (RegProp $P)
	       (AND (FirstArg $p Relation)
		    (ArgType $p Term))))



;;  Logical propositions always start with a Logical Operator.  All their 
;;  arguments are propositions, and some of them can only have a specific
;;  number of arguments.  Others can have an arbitrary number of arguments.

($Assert '(Iff (LogicalProp $P)
	       (AND (FirstArg $p LogicalOperator)
		    (Argtype $p Proposition)
		    (LegalNumArgs $p))))


;;  The number of arguments to generate for a logical operator with a fixed
;;  number is quite easy.  For the ones that take arbitrary numbers of 
;;  arguments one must randomly pick a reasonable number of arguments to
;;  generate.  Because of this problem, I have simplified things for the 
;;  MACLISP code and I consider that all logical operators and 
;;  relations have a fixed number of arguments.  I go ahead and look up
;;  the ARITY and then I check to see if it makes sense.  If the arity can not
;;  be found then I pick the arity randomly.


($Assert '(if (LegalNumArgs $x)
	     (OR (AND (ArityType $x Fixed)
		      (Arity $r $a)
		      (PropLength $x $a))
	         (ArityType $x Arbitrary))))



;;  Now for the Quantified propositions.  Try to capture the idea that the
;;  first argument is the quantifier, that the last must be a proposition
;;  and that everything in between are variables.  What is really missing
;;  here is the fact that there is an ARBITRARY number of MIDDLEARGS that
;;  must be found in the proposition.  This number of middles is wired into
;;  the generator.

($Assert '(Iff (QuantifiedProp $p)
	       (AND (FirstArg $p Quantifier)
		    (LastArg $p Proposition)
		    (MiddleArgs $p Variable))))



;;  **************************** Controlled Part **************************


;    As one looks through this code, there are a number of similaries
; that you will see between the random generator and the controlled generator.
; For the random generator I use a set of predefined rules about MRS syntax
; to randomly construct the structure based on those facts.  

; When controlling the generation, you may not want to choose some things, but
; definitely not everything randomly.  In addition, you may want to change
; the rules of MRS syntax to generate a counterexample.  For example,
; normally a logical proposition is defined as 
;       (AND (FirstArg $p LogicalOperator) (Argtype $p Proposition))
; Later down the like you may want to generate in incorrect example that
; uses the definition
;       (AND (FirstArg $p LogicalOperator) (Argtype $p TERM)).
; The controlled part of the generator must understand a language that let's
; you describe the definition that you want to use at a particular time.

    

 ; The generator takes a name of the outermost proposition and a list of 
 ; criterion that the example must meet, and generates an example.  Some
 ; of the generation is quite controlled, some of it is completely random.

 ; Special note:
 ;  One idea that I have is to randomly change the dictionary for generating
 ;  the example.  Right now DICT1.LSP is always used.  Later on I would like
 ;  to have 3 or 4 dictionaries to choose from.  The way to do it would be
 ;  to activate the dictionary in the GENERATOR and then deactivate it later.
 ;  How αfun¬!

(defun generator (CriterionList)
  
  (let ((theory 'generator) 
	(GenVarList) (GenVars '(P1 P2 P3 P4 P5))
	(symbol) (Variable) (LogicalOperator) (Quantifier) (Function) 
	(relation) (Dictionary (pickone ($Lookupvals '(Dictionary))))
	answer)

    ;  Assert the criterion so that they can be found easily using the
    ;  MRS indexing.

    (Do ((C CriterionList (cdr C)))
	((null C))
      ($Assert (car C)))

    (setq answer (CONSTRUCT (BuildDescription 'P1 1)))
    
    ;  We need to clean out the generator theory before we leave.

    (remprop 'symbol 'symlist)
    (remprop 'function 'symlist)
    (remprop 'relation 'symlist)

    (empty theory)
    answer))



(defun BuildDescription (PName Depth)
  (let ((Arity) (type))
    (setq GenVars (cons pName genvars))     ;;;  We need to know all the
                                            ;;;  names of the subparts.

    (Cond (($Truep `(IMBED ,PName))
	   (Imbedder PNAME Depth))

	  ((or (Setq type (symtypeP Pname))
	       (Setq type (StructureP Pname)))
	   (Expand PName type (1+ Depth)))

	   ;  If the first Argument is already defined in terms of ARGI
	   ;  then we don't have to worry about it.  If, however, FirstArg
	   ;  describes it, then we have to instantiate the argument and
	   ;  declare it.

	  (T (Cond ((Setq rel (PickOne (GetSyms 
					($Lookupval `(FirstArg ,pName)))))
		    ($Stash `(GArgi 0 ,PName ,rel)))
		   (T (Setq rel ($Lookupval `(Gargi 0 ,Pname)))))
	     
	     (Setq Arity (GetArity rel))
	     
	     (Cond ((setq type ($Lookupval `(ARGTYPE ,Pname)))
		    ; do the right thing for argtype;
		    (Do ((i 1 (1+ i)))
			((> i Arity))
		      
		      ($Stash `(GArgi ,i ,Pname ,(Expand PName type 
							  (1+ Depth))))))
		    
		   ((setq type ($Lookupval `(MIDDLEARGS ,Pname)))
		    ; do the right thing for quantified props
		    (setq NumVars (pickOne '(1 2)))
		    (Setq GenVarlist (cons NIL Genvarlist))


		    (do ((i 1 (1+ i)))
			((> i NumVars) 
			 ($Stash `(GArgi ,i ,Pname ,(Expand PName

				        ($Lookupval `(LastArg ,Pname))
					(1+ Depth)))))

		      ($Stash `(GArgi ,i ,Pname ,(Expand PName type 
							  (1+ Depth)))))
		    (Setq GenVarList (cdr GenVarList)))


		   ; Assume that the arguments are listed one by one.
		   (T (Do ((I 1 (1+ I)))
			  ((not (or (setq type ($Lookupval 
						`(ArgStruc ,I ,PName)))
				    (Setq val ($Lookupval `(GARgi ,I ,Pname))))))
			    
			(Cond (Type
			       ($Stash `(GArgi ,i ,PName ,(Expand PName type 
							  (1+ Depth)))))
			      ((and val (Member val Genvars))
			       ($UnStash `(GArgi ,i ,Pname ,Val))
			       ($Stash `(GArgi ,i ,PName 
						,(BuildDescription Val 
							     (1+ Depth)))))))))
	     PName))))


(defun Imbedder (PName Depth)
  ;  This function randomly imbeds a proposition that is specified in the
  ;  descriptive language in either a random quantified proposition or
  ;  a logical proposition.  This allows for imbedding without specifying
  ;  the outermost form of the example.

  (Let ((innerprop) (outer) (where) (NPname (intern (gensym 'Pname))) (reltype)
		    (argtype)(middleargs)(lastarg)
	(structure (PickOne ($Lookupvals '(CanImbedprop)))))

    ($UnStash `(IMBED ,pName))
    (Setq Where ($Lookupval `(ImbedAt ,Structure)))

    (setq reltype (subvar '$x ($truep `(if (,Structure $y)(FirstArg $y $x)))))
    (setq rel (PickOne (GetSyms reltype)))
    ($Stash `(GArgi 0 ,NPname ,Rel))

    (Setq Arity (getArity rel))
    (setq where ($Lookupval `(ImbedAt ,Structure)))
    (cond ((eq where 'N) (Setq where Arity)))

    (setq Argtype (subvar '$x ($Truep `(If (,Structure $y)(ArgType $y $x)))))

    (Cond ((null Argtype)
	   (setq MiddleArgs (subvar '$x ($Truep `(If (,Structure $y)
						     (MiddleArgs $y $x)))))
	   (setq LastArg (subvar '$x ($Truep `(If (,Structure $y)
						  (LastArg $y $x)))))))

    (do ((i 1 (1+ I)))
	((> i arity))

      (Cond ((Eq i where)
	     ($Stash `(GArgi ,I ,NPName ,Pname)))
	    
	    ((and LastArg (= i arity))
	     ($Stash `(Argstruc ,I ,NPname ,LastArg)))

	    (MiddleArgs
	     ($Stash `(ArgStruc ,I ,NPName ,MiddleArgs)))

	    (T ($Stash `(ArgStruc ,I ,NPname ,Argtype)))))

    (BuildDescription Npname Depth)
    NPName))



(defun GetArity (rel)
  ;  If the arity is for some reason undefined (the rel could have an arbitrary
  ;  arity), GetArity just picks between 2 and 3 and sends the number back.

  (let ((Theory dictionary) (Arity))

    (setq Arity ($Lookupval `(arity ,rel)))
    (Cond ((null arity) (setq Arity (pickone '(2 3)))))
    Arity))
    

;; ************************* The Random Part *********************************
						      
;;  If the structure is a super structure or a very simple one, pick one of 
;;  the possibilities and expand it.  Otherwise, get all the features of the 
;;  structure and send them on to a function that knows how to build it.
;;  Some of this may seem a little awkward, especially the destroying
;;  and recreating the propositions that tell what structure to make.
;;  But the hope was that this would lead to formalization of the whole
;;  generation process.  Ha, Ha!

;;  Warning!  In a number of places here you will see something that looks
;;  like (PICKONE (CDAR (GETVALS `(IF ,STRUCTURE)))).   This should more
;;  precisely be (SUBVARS '$X ($TRUEP `(IF ,STRUCTURE $X))), but this works
;;  and I am too lazy at this point to write subvars.  $GETVALS will not 
;;  work in these spots.


(defun EXPAND (PName Stype Level)
  (Let (NVar)

    (Cond (($Truep `(MRSSuperStructure ,SType))
	   (Cond ((>= level 4) ; do want to get too deep
		  (Expand PName (getval `(Simple ,stype)) level))

		 (T (Expand PName (car (PickOne 
			 (cdar (Getvals `(If (,stype $X)))))) level))))

	  ((eq SType 'Variable)
	   (Prog ()
	    VARLOOP
	     (setq NVar (PickOne (GetSyms 'Variable)))
	     (Cond ((Do ((VL GenvarList (cdr VL))
			 )
			((null Vl) NIL)
		      (Cond ((member NVAR (car VL)) (Return T))))
		    (GO Varloop))))

	   (Setq GenVarList (Cons (cons NVAR (car GenVarList)) 
				  (cdr GenVarList)))
	   (BuildSym PName Nvar))
		 
	  ((and (Eq SType 'symbol)
	    ; Let's see if there is some variable to used instead of picking
	    ; a symbol from out of the dictionary.
		(setq nVar (Do ((Vl GenVarList (cdr VL)))
			       ((Null VL) NIL)
			     (Cond ((car VL)
				    (Setq NVAR (PickOne (car vl)))
				    (Setq GenVarList (SPDelete NVAR Genvarlist))
				    (Return NVAR))))))
	   (BuildSym Pname NVAR))

	  (($Truep `(SymbolType ,Stype))
	   (setq Nvar (PickOne (GetSyms stype)))
	   (BuildSym PName nvar))

	  (T (Let ((NewPName (intern (GenSym 'Pname))) features)
	       (Setq Features (Getvals `(If (,Stype $x))))
	       (Mapcar '$Stash (ReplaceVars Features NewPname))
	       (BuildDescription NewPName Level))))))
  


;;  This will find the length of the list of possible substructures and will
;;  randomly choose between them and send it back to be built.

(defun PickOne (ChoiceList)
  (Let ((ChoiceNum (length ChoiceList)))
    (Nth (Random ChoiceNum) ChoiceList)))

(defun Replacevars (VL Pname)
  (Do ((L VL (cdr L))
       (ToReturn) (Item))
      ((Null L) ToReturn)
    (Setq ToReturn (nconc Toreturn (list
	       (Cond ((atom (setq item (car l)))
		      (Cond ((Eq '$ (car (explodec item)))
			     Pname)
			    (T Item)))

		     (T (replacevars item pname))))))))



(defun StructureP (Pname)
  (Do ((n (Append ($Lookupvals '(MRSSuperStructure))
		  ($Lookupvals '(MRSStructure)))
	  (cdr n)))
      ((null n) NIL)

    (Cond (($Lookup (list (car N) Pname)) (return (Car N))))))


(defun SymTypeP (Pname)
  (Do ((N ($Lookupvals '(SymbolType)) (cdr N)))
      ((Null n) NIL)

    (Cond (($Lookup (list (car N) Pname)) (return (Car N))))))

(defun BuildSym (Pname sym)
  (let ((NewName (intern (gensym 'P))))
    (setq genvars (cons Newname genvars))
    ($Stash `(SYM ,Newname ,sym))
    Newname))


(defun SPDelete (NVAR genvarlist)
  (do ((vl genvarlist (cdr vl))
       (found) (frontend))
      (found genvarlist)
    (Cond ((member nvar (car vl))
	 (Setq genvarlist (append frontend
				 (list (delete nvar (car vl)))
				 (Cdr vl)))
	 (setq found t))
	 (T (Setq frontend (append frontend (list (car vl))))))))



; Once we have the structure described in terms of GARGI then we can just
; easily go through and make it construct the example.


(defun Construct (PName)
  (Cond (($Lookupval `(SYM ,Pname)))
	(T (Do ((i 0 (1+ i))
		(Example) (Value))
	       ((Null (Setq Value ($Lookupval `(GArgi ,i ,Pname)))) Example)

	     (Setq Example (nconc Example (list
				  (cond ((Member value Genvars)
					 (Construct value))
					(T value)))))))))


(defun GetSyms (type)
  (Cond ((null type) '()) 
	((get type 'symlist))
	(T (let ((theory dictionary))
		 (putprop type ($lookupvals (list type))
			  'symlist)))))

|#

