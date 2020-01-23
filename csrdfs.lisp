
perm filename CSRDFS.LSP[MRS,LSP] blob sn#710788 filedate 1983-05-12 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00003 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002		Utility Functions and Macros from NWREP.TXT[AT,LGC]/4p
C00014 00003			  New Reasoning Data Structures
C00032 ENDMK
C⊗;
;	Utility Functions and Macros from NWREP.TXT[AT,LGC]/4p

(DECLARE (fasload struct fas dsk (mac lsp))
	 (mapex 't)
	 (setq defmacro-for-compiling nil)

	 (*lexpr CSR:UPDATE-R-GRAPH PUSH-REASONING-GRAPH PUSH-TASK-RECORD)

	 (special *ALL-BEL-LEVELS*  *ALL-R-RULE-EXPERTS-LIST*
		  *ALL-R-HEURISTIC-EXPERTS-LIST* R-AGENDA -CONTEXT-
		  -CONTEXT:GLOBAL- -ALLWORLDS- -NATURE- -REALWORLD-
		  *BL-NEG-INDEX* YHπ-FLAG -EM:LINEL-
		  REAS-SPECS MAX-EFFORT CURRENT-TOTAL-EFFORT )

	 (fixnum -EM:LINEL-)

	 (SETQ *WRITE-DO-LIST*
	       '(SPACES DISPLAY POSPRINC GO TAB BREAK ERROR SETQ
			COND DISPLAY-TRIAL-REPORT )
	       IBASE 10. BASE 10. ) )

(NCONC *WRITE-DO-LIST* '(COND DISPLAY-TRIAL-REPORT))

(SETQ *ALL-BEL-LEVELS*
      '(CERTAIN DOUBTLESS VERY-LIKELY FAIRLY-LIKELY SOMEWHAT-LIKELY
		LIKELY-AS-NOT SOMEWHAT-UNLIKELY FAIRLY-UNLIKELY
		VERY-UNLIKELY MOST-UNLIKELY NEG-CERTAIN )
      *BL-NEG-INDEX*
      (NCONC (MAPCAR #'CONS *ALL-BEL-LEVELS* (REVERSE *ALL-BEL-LEVELS*))
	     '((INDETERMINATE . INDETERMINATE)) ) )

(DECLARE

 (load '|nsublis.lsp|)  ;; NOTE : This file contains up-to-date
	;; copies of all *DEFUN definitions in both NWREP and DNET.

 (DEFSTRUCT (LT-QUANTIFIER (TYPE HUNK) (CONC-NAME LT-))
	    Q-DEPENDENCIES Q-DETERMINER QSORT-EXPR Q-SCOPE )

 (DEFSTRUCT (ROLELINK (TYPE TREE))
	    ROLEMARK ARGUMENT )

 (DEFSTRUCT (PFC-FORMULA (TYPE TREE))
	    PFC-CONCEPT ROLELINKS )
 ; PFC-FORMULA => (pred rlnk1 rlnk2 ... rlnkn) or (func rlnk1 rlnk2 ... rlnkn)
 ;		   or (connective rlnk1 rlnk2 ... rlnkn)

 (DEFMACRO HUNKQUANTP (LT-FORM)
    `(AND (HUNKP ,LT-FORM)
	  (EQ 'DETERMINER (GET (LT-Q-DETERMINER ,LT-FORM) 'CATEGORY)) ) )

 (DEFMACRO ANTECEDENT (LT-⊃-PROPO)
   `(ARGUMENT (ASSQ 'ANTECEDENT (ROLELINKS ,LT-⊃-PROPO))) )

 (DEFMACRO CONSEQUENT (LT-⊃-PROPO)
   `(ARGUMENT (ASSQ 'CONSEQUENT (ROLELINKS ,LT-⊃-PROPO))) )

(DEFMACRO UQ-KERNEL-LT-TYPE (LT-QUANTIFIERFORM)
 `(LT-TYPE (UQ-KERNEL ,LT-QUANTIFIERFORM)) )

 (DEFMACRO SUBSET (LIST PREDICATE)
   (SETQ PREDICATE (EVAL PREDICATE))
   `(MAPCAN #'(LAMBDA (MEMBER)
		(COND ((,PREDICATE MEMBER) (NCONS MEMBER))) )
	    ,LIST ) )

; Definition of SUBSET for LISP-Machine:
;  (DEFMACRO SUBSET (LIST PREDICATE)
;    `(REM-IF-NOT ,PREDICATE ,LIST) )

(DEFMACRO CONSP (EXPR)
   `(EQ (TYPEP ,EXPR) 'LIST) )

; TCONC adds an item onto the end of a list that is maintained via the
; cons-cell PTR.  The list itself is (CAR PTR), while (CDR PTR) is (LAST list),
; the last cons of the list.  To start such a list, PTR should be initialized
; to (NCONS NIL).  TCONC returns the updated PTR.  Thus, in order to
; "pass through" the item added, one may write (CADR (TCONC ... )).
(DEFUN TCONC (ADDITEM PTR)
       (OR (CONSP PTR) (BREAK |TCONC - PTR not a CONS-cell!|))
       (COND ((CDR PTR)
	      (RPLACD PTR (CDR (RPLACD (CDR PTR) (NCONS ADDITEM)))) )
	     (T (RPLACD PTR (CAR (RPLACA PTR (NCONS ADDITEM))))) ) )

(DEFUN NSUBLIS (A-LIST S-EXPR &aux SUBSTPAIR)
  (COND ((CONSP S-EXPR)
	   (COND ((CONSP (CAR S-EXPR)) (NSUBLIS A-LIST (CAR S-EXPR)))
		 ((SETQ SUBSTPAIR (ASSQ (CAR S-EXPR) A-LIST))
		    (RPLACA S-EXPR (CDR SUBSTPAIR)) ) )
	   (COND ((CONSP (CDR S-EXPR)) (NSUBLIS A-LIST (CDR S-EXPR)))
		 ((SETQ SUBSTPAIR (ASSQ (CDR S-EXPR) A-LIST))
		    (RPLACD S-EXPR (CDR SUBSTPAIR)) ) )
	   S-EXPR )
	((COND ((SETQ SUBSTPAIR (ASSQ S-EXPR A-LIST)) (CDR SUBSTPAIR))
	       (S-EXPR) )) ) )

(DEFMACRO SETF* (SETFORM VALUEFORM)
  (LIST 'SETF SETFORM (NSUBLIS `((-*- . ,SETFORM)) VALUEFORM)) )

(DEFMACRO SOME (LIST PREDICATE . &opt:STEP-FUNCTION)
  (SETF* PREDICATE (EVAL -*-))
  (COND (&opt:STEP-FUNCTION (SETF* &opt:STEP-FUNCTION (EVAL -*-))))
  `(DO ((LISTAIL ,LIST (,(COND (&opt:STEP-FUNCTION
				 (CAR &opt:STEP-FUNCTION) )
			       (T 'CDR) )
			  LISTAIL )))
       ((NULL LISTAIL) NIL)
       (COND ((,PREDICATE (CAR LISTAIL)) (RETURN LISTAIL))) ) )

(DEFMACRO ALL (LIST PREDICATE . &opt:STEP-FUNCTION)
  (SETF* PREDICATE (EVAL -*-))
  (COND (&opt:STEP-FUNCTION (SETF* &opt:STEP-FUNCTION (EVAL -*-))))
  `(DO ((LISTAIL ,LIST (,(COND (&opt:STEP-FUNCTION
				 (CAR &opt:STEP-FUNCTION) )
			       (T 'CDR) )
			  LISTAIL )))
       ((NULL LISTAIL) 'T)
       (COND ((NOT (,PREDICATE (CAR LISTAIL))) (RETURN NIL))) ) )

(DEFMACRO COPYLIST (LIST)
  `(APPEND ,LIST NIL) )

(DEFMACRO WRITE BODY
 `(PROGN
    ,@(MAPCAN #'(LAMBDA (X)
		  (COND ((EQ X 'T) (NCONS '(TERPRI)))
			((EQ X 'T*) (LIST '(TERPRI) '(SETQ CURRENTPOS 1)))
			((ATOM X) (NCONS `(PRINC ,X)))
			((CONSP X)
			   (COND ((MEMQ (CAR X) *WRITE-DO-LIST*)
				    (NCONS X) )
				 ((EQ '1* (CAR X))
				    (NCONS `(PRIN1 ,(CDR X))) )
				 ((EQ 'IF* (CAR X))
				    (NCONS `(LET ((VAL ,(CDR X)))
						 (COND (VAL (PRINC VAL))) )) )
				 (T (NCONS `(PRINC ,X))) ) ) ) )
	      BODY ) ) )

(DEFMACRO COPYLIST (LIST)
  `(APPEND ,LIST NIL) )

(DEFMACRO RASSQ (KEY A-LIST)
   `(DO ((A-TAIL ,A-LIST (CDR A-TAIL)))
	((NULL A-TAIL))
	(COND ((EQ (CDAR A-TAIL) ,KEY) (RETURN (CAR A-TAIL)))) ) )

; the 'Q' connotes "EQ" and "ASSQ"
(DEFMACRO A-Q-GET (A-LIST INDICATOR)
  `(CDR (ASSQ ,INDICATOR ,A-LIST)) )

(DEFMACRO ATC-GET (GENL-PLIST INDICATOR)
  `(LET ((GENL-PLIST ,GENL-PLIST))
	(COND ((AND YHπ-FLAG (π-YH-UNITP GENL-PLIST))
	         (π-GET GENL-PLIST ,INDICATOR) )
	      (T (GET GENL-PLIST ,INDICATOR)) ) ) )

(DEFMACRO (NRML-FORMULA defmacro-for-compiling 't) (LT-FORM)
  `(ATC-GET (NRML-ANL-YZE ,LT-FORM) 'LT-FORMULA) )

(DEFMACRO (NRML-ANL-YZE defmacro-for-compiling 't) (LT-FORM . AL-VARS-TAIL)
 `(LET ((LT-FORM ,LT-FORM))
       (COND ((ATOM LT-FORM) LT-FORM)
	     (T (LET ((AL-VARS ,(CAR AL-VARS-TAIL)))
		     (NORMALIZE-CMPD-CONCEPT
		            LT-FORM
			    (ANALYZE-CMPD-CONCEPT LT-FORM AL-VARS)
			    AL-VARS ) )) ) ) )

(DEFMACRO ISA-SUPERSORT-OF (SORT1 SORT2)
 `(LET ((SORT1 ,SORT1)
	(SORT2 ,SORT2) )
       (OR (EQ SORT1 SORT2) (SUPERSORT* SORT1 SORT2)) ) )

(DEFMACRO ISA-QUANT-TERM (LT-FORM)
 `(AND (CONSP ,LT-FORM)
       (EQ 'QUANT-TERM (CAR ,LT-FORM)) ) )

 (DEFMACRO UQ-KERNEL (LT-QUANTIFIERFORM)
  `(DO ((CURR-SUB-EXPR ,LT-QUANTIFIERFORM (LT-Q-SCOPE CURR-SUB-EXPR)))
       ((NOT (HUNK-UQUANTP CURR-SUB-EXPR))
	  CURR-SUB-EXPR ) ) )

 (DEFMACRO UQ-⊃-KERNEL (LT-QUANTIFIERFORM)
  `(DO ((CURR-SUB-EXPR ,LT-QUANTIFIERFORM (LT-Q-SCOPE CURR-SUB-EXPR)))
       ((NOT (HUNK-UQUANTP CURR-SUB-EXPR))
	  (CONSEQUENT CURR-SUB-EXPR) ) ) )

 )	;; end of DECLARE

; This is equivalent to the *DEFUN definition of (THE-OF:LT-QUANT . QSORT).
(DEFMACRO LT-QSORT (LT-QUANT)
  `(LET* ((QSORTEXPR (LT-QSORT-EXPR ,LT-QUANT))
	  (ATOMICQSORTEXPR
	    (CASEQ (LT-TYPE QSORTEXPR)
	       (ATOMICPROPO QSORTEXPR)
	       (CONJ-PROPO (ARGUMENT (CAR (ROLELINKS QSORTEXPR)))) ) ) )
	 (COND ((EQ (PFC-CONCEPT ATOMICQSORTEXPR) 'CONCEPT) 
		  (NORMALIZE-TERMSORTEXPR
		   (CONS '↑
			 (COND ((ARGUMENT (ASSQ 'OBJECT-CATEGORY*
						(ROLELINKS ATOMICQSORTEXPR) )))
			       (T (TERMSORT
				   (ARGUMENT
				    (ASSQ 'OBJECT
					  (ROLELINKS ATOMICQSORTEXPR) ) ) )) ) ) ) )
	       (T (PFC-CONCEPT ATOMICQSORTEXPR)) )) )
;		  New Reasoning Data Structures

;		        (Inspired in part by consideration of RPG's REASON.8)
;					     Original Version:    5 Nov  1982
;						 Last Revised:    6 Dec  1982

;  The proposed basic data structure for commonsense reasoning is a graph or
; network with complex propositional nodes (REASONING-PROPOSITION-NODEs), and
; complex labelled links (REASONING-CONSIDERATION-LINKs).  The entire reasoning
; network is partitioned into two subsets, the TARGET-CORPUS, bounded on its
; unanchored side by the TARGET-FRONTIER, and the KNOWLEDGE-CORPUS, bounded on
; its unanchored side by the KNOWLEDGE-FRONTIER.  Reasoning is essentially a
; knowledge-governed, bi-directional search for arguments both for and against
; the TARGET-PROPOS.  The search proceeds forward from the KNOWLEDGE-BASIS and
; backward from the TARGET-PROPOS, until the two frontiers meet and become
; sufficiently connected.

(DEFSTRUCT (REASONING-GRAPH (CONC-NAME R-GRAPH-))
	   (RB-CONTEXT ())		;; the reasoning background-context
	   (T-BASIS ())			;; the set of ultimate target-rp-nodes
	   (T-FRONTIER ())		;; target frontier
	   (T-DIRECTORY ())		;; target directory
	   (K-BASIS ())	 ;; knowledge basis - relevant premises previously known
	   (K-FRONTIER ())		;; knowledge frontier
	   (K-DIRECTORY ())		;; knowledge directory
	   (CONSID-LIST ()) )		;; a list of all considerations

(DEFSTRUCT (RG-DIRECTORY-ENTRY (CONC-NAME RG-DIR-ENTRY-))
	   P-UNIT CONTEXT RP-NODE )

; This defstruct is used (but not defined) by senten.def[at,lgc], and by
;  csrexp[at,lgc].
(DEFSTRUCT (BELIEF CONC-NAME)
	   (WT-CNTXT -REALWORLD-)     ;; A world-time-context, which determines
				      ;;  part of the content of the belief.
	   (TYPE ())		;; knowledge, hypothesis, assumption, etc.
	   (P-UNIT ())		;; A property-list with FORMULA and
				;;  F-DESCRIPTS indicators.
	   (EPISTATUS ()) )

(DEFSTRUCT (QUERY CONC-NAME)  ;; a belief-like construct for target propositions
	   (WT-CNTXT ())	;; A world-time-context, which determines
				;;  part of the content of the query.
	   (TYPE 'QUERY)
	   (P-UNIT ())		;; a property-list with FORMULA and
				;;  F-DESCRIPTS indicators.
	   (EPISTATUS (MAKE-EPISTATUS BEL-LEVEL 'INDETERMINATE
				      BEL-FIRMNESS () )) )
					;; soon 'INDETERMINATE

(declare (cond ((and (boundp 'csreas-dfc-flag) csreas-dfc-flag)
		  (setq defmacro-for-compiling 't) )))

(DEFMACRO BELIEF-FORMULA (BELIEF)
  `(GET (BELIEF-P-UNIT ,BELIEF) 'LT-FORMULA) )

(DEFMACRO RP-NODE-FORMULA (RP-NODE)
  `(BELIEF-FORMULA (RP-NODE-CONTENT ,RP-NODE)) )

(DEFMACRO QUERY-FORMULA (QUERY)
  `(GET (QUERY-P-UNIT ,QUERY) 'LT-FORMULA) )

(DEFMACRO BELIEF-DESCRIPTS (BELIEF)
  `(GET (BELIEF-P-UNIT ,BELIEF) 'F-DESCRIPTS) )

(DEFMACRO QUERY-DESCRIPTS (QUERY)
  `(GET (QUERY-P-UNIT ,QUERY) 'F-DESCRIPTS) )

(DEFMACRO BELIEF-BEL-LEVEL (BELIEF)
  `(EPIST-BEL-LEVEL (BELIEF-EPISTATUS ,BELIEF)) )

(DEFMACRO QUERY-BEL-LEVEL (QUERY)
  `(EPIST-BEL-LEVEL (QUERY-EPISTATUS ,QUERY)) )

(declare (setq defmacro-for-compiling ()))

; This defstruct is used (but not defined) by senten.def[at,lgc]
;  and by csrexp[at,lgc].
(DEFSTRUCT (EPISTATUS (CONC-NAME EPIST-))
	   (BF-GROUNDS ())	   ;; descriptions of the reasoning and learning
				   ;;  processes that underlie bel-firmness
	   (BEL-LEVEL ())	   ;; level of belief or commitment
	   (BL-GROUNDS ())	   ;; supporting considerations, etc.
	   (BEL-FIRMNESS ()) )	   ;; firmness of belief or commitment

(DEFMACRO COPY-EPISTATUS (X)
 `(MAKE-EPISTATUS BF-GROUNDS (EPIST-BF-GROUNDS ,X)
		  BEL-LEVEL (EPIST-BEL-LEVEL ,X)
		  BL-GROUNDS (EPIST-BL-GROUNDS ,X)
		  BEL-FIRMNESS (EPIST-BEL-FIRMNESS ,X) ) )

(DEFMACRO CSR:COPY-P-UNIT (P-UNIT)
 `(LET ((COPY (NCONS '*P-UNIT*)))
       (SETPLIST COPY (COPYLIST (PLIST ,P-UNIT)))
       COPY ) )

(DEFMACRO CSR:COPY-BLF∨QRY (B∨Q-VAR)
  `(MAKE-BELIEF WT-CNTXT (BELIEF-WT-CNTXT ,B∨Q-VAR)
		TYPE (BELIEF-TYPE ,B∨Q-VAR)
		P-UNIT (BELIEF-P-UNIT ,B∨Q-VAR)	;; all p-units are normalized
		EPISTATUS (COPY-EPISTATUS (BELIEF-EPISTATUS ,B∨Q-VAR)) ) )

(declare (cond ((and (boundp 'csreas-dfc-flag) csreas-dfc-flag)
		  (setq defmacro-for-compiling 't) )))

;  This macro assumes a call of the sort:
;  (csr:create-lt-blf∨qry belief
;			  formula '(canary tweety)
;			  bel-level 'doubtless
;			   ...  		;; more belief slots 'n' values
;			  wt-cntxt -real-world- )
;  , where a value for the slot FORMULA must be specified.
(DEFMACRO CSR:CREATE-LT-BLF∨QRY  ARGLIST
  (LET ((MAKEFN (CASEQ (CAR ARGLIST) (QUERY 'MAKE-QUERY) (T 'MAKE-BELIEF)))
	(LINFORMULA (GET ARGLIST 'FORMULA))
	(ARG-P-LIST (CONS '*P-LIST* (APPEND (NTHCDR 3. ARGLIST) NIL)))
	(EPIST-IV-LIST)
	(BEL-CXT-VAL) )
       (COND ((SETQ BEL-CXT-VAL (GET ARG-P-LIST 'WT-CNTXT))
		(REMPROP ARG-P-LIST 'WT-CNTXT) ))
       (SETQ EPIST-IV-LIST (CDR ARG-P-LIST))
       `(LET ((P-UNIT (NRML-ANL-YZE-LINFORMULA ,LINFORMULA)))
	     (,MAKEFN TYPE ',(CAR ARGLIST)
		      P-UNIT P-UNIT
		      WT-CNTXT ,(COND (BEL-CXT-VAL) (T '-REALWORLD-))
		      ,@(COND (EPIST-IV-LIST
			       `(EPISTATUS (MAKE-EPISTATUS ,@EPIST-IV-LIST)) )
			      (T NIL) ) ) ) ) )

(declare (setq defmacro-for-compiling ()))

(DEFSTRUCT (REASONING-TASK (CONC-NAME R-TASK-))
	   EFFORT PRIORITY DESCRIPTION R-EXPERT METHOD ARGUMENTS
	   (TRIAL-REPORT 'UNTRIED) )

(DEFSTRUCT (REASONING-PROPOSITION-NODE (CONC-NAME RP-NODE-))
	   (R-GRAPH ())
	   (TYPE ())		       ;; either 'TARGET or 'KNOWLEDGE
	   (CONTENT ())		       ;; a belief (knowledge) or query (target)
	   (RLVT-CONSIDS ())		;; ReLeVanT CONSIDerations
	   (PART-CONSIDS ())		;; CONSIDerations PARTicipated in
	   (NEGATION ())		;; the rp-node of the negation
	   (TRAV-LIST ()) )		;; for use by r-graph TRAVersal programs

;;;	   (INSTAN-STATUS ())		;; current INSTANtiation-STATUS,
;;;					;;  either 'SCHEMATIC or 'DETERMINATE
;;;	   (GOAL-RLVT-CONSIDS ())	;; these have at least one GOAL-node
;;;	   (GOAL-PART-CONSIDS ())	;; these have at least one GOAL-node

(DEFMACRO ISA-RP-NODE (RG-ITEM)
  `(MEMQ (CAR ,RG-ITEM) '(TARGET KNOWLEDGE)) )

;;; NOTE: for the time being at least, INSTAN-STATUS is obselete (1 Dec 82).
; Rules of INSTAN-STATUS:  rp-nodes are the primary carriers of this property,
;  and are DETERMINATE iff their content is.  A consid-link is DETERMINATE in
;  a secondary sense if its conclusion and all of its premises are DETERMINATE.
;  If all the prem-nodes of a consid-link are determinate, then its concl-node
;  should also be determinate.

; this is a base-defstruct to be INCLUDEd in more specific defstructs
(DEFSTRUCT (REASONING-CONSIDERATION-LINK (CONC-NAME CONSID-))
	   (R-GRAPH ())
	   (TYPE 'ORDINARY-CONSID)  ;; either ORDINARY-CONSID or NEGATION-CONSID
	   (RULE ())			;; the governing epistemic rule
	   (PREM-NODES ())		;; the premises
	   (CONCL-NODE ())		;; the conclusion
	   (INHER-REL-STRENGTH ())	;; inherent relative strength
	   (FORCE ())	       ;; prima-facie in-situ epistatus for conclusion
	   (GOAL-NODES ()) )   ;; prem- or concl-nodes sought, but not yet found

;;;	   (TRAV-LIST ())      ;; a slot for use by r-graph TRAVersal programs
;;;	   (SCHEMA-NODES ())   ;; a list of all SCHEMAtic prem- or concl-nodes
;;;	   (SUPP-STATUS 'INDETERMINATE) ;; current SUPPort status,
;;;			       ;; either SUPPORT, NON-SUPPORT, or INDETERMINATE

(DEFMACRO ISA-CONSID (RG-ITEM)
  `(MEMQ (CAR ,RG-ITEM) '(ORDINARY-CONSID NEGATION-CONSID)) )

(DEFSTRUCT (CONSIDERATION-FORCE (TYPE TREE) (CONC-NAME CNSD-FORCE-))
	   (INDICATOR 'IF-ALONE)
	   (VALUE ()) )	   ;; either a Prima-Facie BEL-LEVEL for a conclusion,

(declare (cond ((and (boundp 'csreas-dfc-flag) csreas-dfc-flag)
		  (setq defmacro-for-compiling 't) )))

(DEFMACRO CREATE-ADVICE-CONSID (CF-VALUE)
 `(MAKE-REASONING-CONSIDERATION-LINK
	RULE 'USER-ADVICE
	CONCL-NODE '***
	FORCE (MAKE-CONSIDERATION-FORCE VALUE ,CF-VALUE) ) )

(declare (setq defmacro-for-compiling ()))

(DEFMACRO CSR:COPY-CONSID-FORCE (F)
  `(MAKE-CONSIDERATION-FORCE
      INDICATOR (CNSD-FORCE-INDICATOR ,F)
      VALUE (CNSD-FORCE-VALUE ,F) ) )

(DEFSTRUCT (QMP-CONSID CONC-NAME (INCLUDE REASONING-CONSIDERATION-LINK
					  (RULE 'QUANTIFIED-MODUS-PONENS)
					  (INHER-REL-STRENGTH 'CERTAIN-AWPC) )))

;;;	   (Q-PREM-NODE ())		;; mnemonic for: Quantified premise
;;;	   (S-PREM-NODE ()) )		;; mnemonic for: Singular premise

(DEFSTRUCT (STAT-CONSID CONC-NAME (INCLUDE REASONING-CONSIDERATION-LINK
					  (RULE 'STATISTICAL-SYLLOGISM)
					  (INHER-REL-STRENGTH 'DOUBTLESS-AWPC) ))
	   (STAT-PREM-NODE ())		;; mnemonic for: STATistical premise
	   (S-PREM-NODE ()) )		;; mnemonic for: Singular premise

(DEFSTRUCT (NEG-CONSID CONC-NAME (INCLUDE REASONING-CONSIDERATION-LINK
					  (RULE 'NEGATION)
					  (INHER-REL-STRENGTH
					      'NEG-CERTAIN-AWPC ) ))
	   (N-PREM-NODE ()) )		;; mnemonic for: Negation premise

;Some testing and demonstration code
;(setq c0 (make-reasoning-consideration-link premises 'premises
;					    conclusion 'conclusion
;					    rule 'rule
;					    root 'root ))

;(typep c0)
;(car c0)
;(consid-type c0)
;(consid-rule c0)

;(setq c1 (make-qmp-consid premises 'premises
;			  conclusion 'conclusion
;			  root 'root
;			  q-prem 'q-prem
;			  s-prem 's-prem ))

;(typep c1)
;(car c1)
;(consid-type c1)
;(consid-rule c1)
;(qmp-consid-rule c1)	;; note: causes undefined-function error
;(qmp-consid-q-prem c1)
;(qmp-consid-s-prem c1)

(DEFSTRUCT (DN-CONSID CONC-NAME (INCLUDE REASONING-CONSIDERATION-LINK
					  (RULE 'DEDUCTIVE-NECESSARY) )))
; do we need to include or summarize intermediate conclusions and rules?
			;; CONSID-PREMISES contains the ultimate premises.
			;; CONSID-CONCLUSION contains the final conclusion.

(DEFSTRUCT (CINF-CONSID CONC-NAME (INCLUDE REASONING-CONSIDERATION-LINK
					  (RULE 'CAUSAL-INFLUENCE) ))
	   (INF-LAWS ())	; mnemonic for: LAW-of-causal-INFluence premiseS
	   (CC-PREMS ()) )	;; mnemonic for: Causal-Condition PREMises
			  ;; CONSID-CONCLUSION is a set of influence-conclusions

(DEFSTRUCT (CACT-CONSID CONC-NAME (INCLUDE REASONING-CONSIDERATION-LINK
					  (RULE 'CAUSAL-ACTION) ))
	   (AL-PREM ())		;; mnemonic for: causal-Action-Law PREMise
	   (I-PREMS ())		;; mnemonic for: Influence PREMiseS
	   (C-M-PREM ()) )	;; mnemonic for: Completeness Meta-PREMise

(DEFSTRUCT (CAUS-CONSID CONC-NAME (INCLUDE REASONING-CONSIDERATION-LINK
					  (RULE 'CAUSAL-CONSEQUENCE) ))
	   (INF-LAWS ())	; mnemonic for: LAW-of-causal-INFluence premiseS
	   (CC-PREMS ())	;; mnemonic for: Causal-Condition PREMises
; do we need to include or summarize intermediate conclusions and rules?
	   (ACT-LAW ())		;; mnemonic for: law of causal action
	   (C-PREM ()) )	;; mnemonic for: Completeness meta-PREMise

(DEFSTRUCT (REASONING-EXPERT (CONC-NAME R-EXPERT-))
	     TYPE		;; either RULE-EXPERT or HEURISTIC-EXPERT
	     R∨H-NAME		;; either <rule-name> or <heuristic-name>
	     DESCRIPTION
	     FORWARD-METHOD 
	     BACKWARD-METHOD
	     FM-PREDICATES
	     BM-PREDICATE ) ;; an applicability condition for BACKWARD-METHOD

