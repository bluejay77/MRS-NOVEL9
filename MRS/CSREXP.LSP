
perm filename CSREXP.LSP[MRS,LSP] blob sn#710801 filedate 1983-05-12 generic text, type T, neo UTF8

;   This File:  Commonsense Reasoning Experts and Associated Processes

(include "CSRDFS.LSP")

(DECLARE (special CONCL-LT-TYPE UQ-KERNEL-PATT S-PREM-LT-TYPE S-PREM-P-UNIT
		  SRT-PREM-LT-TYPE SRT-PREM-P-UNIT ANT-PREM-LT-TYPE
		  ANT-PREM-P-UNIT Q-KERNEL-PATT ) )
	 ; the above lambda-vars are used freely in predicates passed to
	 ;  context:pred-lookup.

;		This Page:  Matching Processes

(DEFMACRO HUNK-UQUANTP (LT-FORM)
   `(AND (HUNKP ,LT-FORM)
	 (EQ '∀ (LT-Q-DETERMINER ,LT-FORM)) ) )

(DEFMACRO LT-QUANT-TERM-SORT (QT-PAIR)
 `(LT-QSORT (CDR ,QT-PAIR)) )

(DEFMACRO LT-LITERAL-MATCH (DATUM PATT)
 `(LET ((D-TYPE (LT-TYPE ,DATUM))
	(P-TYPE (LT-TYPE ,PATT)) )
       (COND ((EQ D-TYPE P-TYPE)
		(CASEQ D-TYPE
		  (ATOMICPROPO (LT-SIMPLE-ATOMIC-MATCH ,DATUM ,PATT))
		  (NEGPROPO (LT-SIMPLE-ATOMIC-MATCH 
				(ARGUMENT (CAR (ROLELINKS ,DATUM)))
				(ARGUMENT (CAR (ROLELINKS ,PATT))) ))
		  (T NIL) ) )
	     (T NIL) ) ) )

(DEFUN AT-MATCH (DATUM PATT)
;(break at-match:test)
  (COND ((EQ DATUM PATT) T)
	((AND (ISA-QUANT-TERM PATT)
	      (ISA-SUPERSORT-OF (LT-QUANT-TERM-SORT PATT)
			     (TERMSORT DATUM) ) )
	   (LIST (CONS DATUM PATT)) )
	(T (LT-LITERAL-MATCH DATUM PATT)) ) )

; This fn assumes that (LT-TYPE DATUM) and (LT-TYPE PATT) are both ATOMICPROPO.
(DEFUN LT-SIMPLE-ATOMIC-MATCH (DATUM PATT &aux D-ITEM P-ITEM)
  (SETQ D-ITEM (PFC-CONCEPT DATUM)  P-ITEM (PFC-CONCEPT PATT))
  (COND ((OR (EQ D-ITEM P-ITEM)
	     (ISA-PATT-VARIABLE? P-ITEM) )
	   (DO ((D-RLTAIL (ROLELINKS DATUM) (CDR D-RLTAIL))
		(P-RLTAIL (ROLELINKS PATT) (CDR D-RLTAIL))
		(BINDINGS) )
	       ((OR (NULL D-RLTAIL) (NULL P-RLTAIL))
		  (COND ((AND (NULL D-RLTAIL) (NULL P-RLTAIL))
			   (OR BINDINGS T) )) )
	       (COND ((NOT (EQ (ROLEMARK (CAR D-RLTAIL))
			       (ROLEMARK (CAR P-RLTAIL)) ))
			(RETURN NIL) ))
	       (SETQ D-ITEM (ARGUMENT (CAR D-RLTAIL))
		     P-ITEM (ARGUMENT (CAR P-RLTAIL)) )
	       (COND ((OR (EQ D-ITEM P-ITEM)
			  (ISA-PATT-VARIABLE? P-ITEM) )
		        (PUSH (CONS D-ITEM P-ITEM) BINDINGS) )
		     ((AND (ISA-QUANT-TERM P-ITEM)
			   (ISA-SUPERSORT-OF (LT-QUANT-TERM-SORT P-ITEM)
					  (TERMSORT D-ITEM) ) )
		        (COND ((EQ '∀ (LT-Q-DETERMINER D-ITEM))
			         (PUSH (CONS D-ITEM P-ITEM) BINDINGS) )
			      (T (BREAK |LT-SIMP-A-M - quantifier punt!|)) ) )
		     (T (RETURN NIL)) ) ) )
	(T NIL) ) )

(DEFMACRO UQ-KERNEL-TYPE-CHECK (DESCRIPTION-A-LIST PROPO-LT-TYPE)
 `(LET ((LT-TYPE*UQ-KERNEL (A-Q-GET ,DESCRIPTION-A-LIST 'LT-TYPE*UQ-KERNEL)))
       (OR (EQ ,PROPO-LT-TYPE LT-TYPE*UQ-KERNEL)
	   (EQ 'QT-PAIR LT-TYPE*UQ-KERNEL) ) ) )

(DEFMACRO UQ-⊃-KERNEL-TYPE-CHECK (DESCRIPTION-A-LIST PROPO-LT-TYPE)
 `(LET ((LT-TYPE*UQ-⊃-KERNEL (A-Q-GET ,DESCRIPTION-A-LIST 'LT-TYPE*UQ-⊃-KERNEL)))
       (OR (EQ ,PROPO-LT-TYPE LT-TYPE*UQ-⊃-KERNEL)
	   (EQ 'QT-PAIR LT-TYPE*UQ-⊃-KERNEL) ) ) )

;		      Lists of Experts

(SETQ *ALL-R-RULE-EXPERTS-LIST* (LIST

(MAKE-REASONING-EXPERT
  TYPE 'RULE-EXPERT
  R∨H-NAME 'QUANTIFIED-MODUS-PONENS
  DESCRIPTION ()
  FORWARD-METHOD  ()
  BACKWARD-METHOD #'QUANT-MP-B-METHOD2
  FM-PREDICATES ()
  BM-PREDICATE #'QUANT-MP-BM-PREDICATE2 )

(MAKE-REASONING-EXPERT
  TYPE 'RULE-EXPERT
  R∨H-NAME 'STATISTICAL-SYLLOGISM
  DESCRIPTION ()
  FORWARD-METHOD  ()
  BACKWARD-METHOD #'STATIST-B-METHOD
  FM-PREDICATES ()
  BM-PREDICATE #'STATIST-BM-PREDICATE1 )

(MAKE-REASONING-EXPERT
  TYPE 'RULE-EXPERT
  R∨H-NAME 'SUBJUNCTIVE-CONDITIONALIZATION
  DESCRIPTION ()
  FORWARD-METHOD  ()
  BACKWARD-METHOD #'SBJCOND-B-METHOD
  FM-PREDICATES ()
  BM-PREDICATE #'SBJCOND-BM-PREDICATE1 )

(MAKE-REASONING-EXPERT
  TYPE 'RULE-EXPERT
  R∨H-NAME 'CONJUCTION
  DESCRIPTION ()
  FORWARD-METHOD  ()
  BACKWARD-METHOD #'CONJUNCT-B-METHOD
  FM-PREDICATES ()
  BM-PREDICATE #'CONJUNCT-BM-PREDICATE )

(MAKE-REASONING-EXPERT
   TYPE 'RULE-EXPERT
   R∨H-NAME 'CAUSAL-INFLUENCE
   DESCRIPTION ()
   FORWARD-METHOD  ()
   BACKWARD-METHOD ()
   FM-PREDICATES ()
   BM-PREDICATE () )	;; an applicability condition for BACKWARD-METHOD
;( MATCH-DESCRIPTIONS 
;   '((IL-PREM-DESCR . ()) ;; mnemonic for: Influence-Law Premise-DESCRiption
;     (CC-PREM-DESCR . ()) ;; mnemonic for: Causal-Condition Premise-DESCRiption
;     (CONCL-DESCR .	   ;; mnemonic for: CONCLusion-DESCRiption
;	(LAMBDA (CONCL) NIL) ) ) )

(MAKE-REASONING-EXPERT
   TYPE 'RULE-EXPERT
   R∨H-NAME 'CAUSAL-ACTION
   DESCRIPTION ()
   FORWARD-METHOD  ()
   BACKWARD-METHOD ()  ;; #'CAUSAL-ACTION-B-METHOD
   FM-PREDICATES ()
   BM-PREDICATE () ) ;; #'CAUSAL-ACTION-BM-PRED1
;( MATCH-DESCRIPTIONS 
;   '((AL-PREM-DESCR ())  ;; mnemonic for: causal Action-Law PREMise-DESCRiption
;     (I-PREMS-DESCR ())  ;; mnemonic for: Influence PREMiseS-DESCRiption
;     (C-M-PREM-DESCR ()) ;; mnemonic for: Completeness Meta-PREMise-DESCRiption
;     (CONCL-DESCR .	  ;; mnemonic for: CONCLusion-DESCRiption
;	(LAMBDA (CONCL) NIL) ) ) )

)) 	;; End of the rule-expert list

(SETQ *ALL-R-HEURISTIC-EXPERTS-LIST* (LIST

(MAKE-REASONING-EXPERT
	TYPE 'HEURISTIC-EXPERT
	R∨H-NAME 'NORMAL-EVENT-CHAIN
	DESCRIPTION ()
	FORWARD-METHOD  ()
	BACKWARD-METHOD ()
	FM-PREDICATES ()
	BM-PREDICATE () )
;	MATCH-DESCRIPTIONS 
;	 '((NORM-ADV-PATT ())   ;; mnemonic for: NORMality-ADVice PATTern
;	   (PREM1-PATT ())    ;; mnemonic: PATTern for 1st PREMise-link in chain
;	   (CONCL-PATT ()) )  ;; mnemonic for: CONCLusion-PATTern

)) 	;; End of the heuristic-expert list
;		Subjunctive Conditional Proof

(DEFUN SBJCOND-BM-PREDICATE1 (CONCL-EXPR)
  (EQ 'IF-WOULD-PROPO (LT-TYPE CONCL-EXPR)) )
   ;; We don't want to investigate the negation of CONCL-EXPR in a separate
   ;;  high-level task, since creation of a new r-graph is involved.  Instead
   ;;  we can arrange, at the level of new r-graph creation, to investigate
   ;;  the negation as well as the major contrary of CONCL-EXPR.

(DEFUN SBJCOND-B-METHOD (RP-TGT-NODE)
 (LET* ((R-GRAPH (RP-NODE-R-GRAPH RP-TGT-NODE))
	(CONCL-EXPR (QUERY-FORMULA (RP-NODE-CONTENT RP-TGT-NODE)))
	(ANTE-SITUATION (CONTEXT:SPROUT-CONTEXT -REALWORLD-))
	 ;; We'll need some sort of time-conditional visibility of -REALWORLD-
	 ;;  in ANTE-SITUATION -- using a filtering-predicate rather than
	 ;;  a deletion-list.  We need a subset-predicate arg to CONTEXT:SPROUT.
	(ANTE-P-UNIT (NRML-ANL-YZE (ANTECEDENT CONCL-EXPR)))
	(ANTE-SUPPOSITION (MAKE-BELIEF
			    TYPE 'SUPPOSITION
			    P-UNIT ANTE-P-UNIT
			    WT-CNTXT ANTE-SITUATION
			    EPISTATUS () ))
	(CONSE-QUERY (MAKE-QUERY
			P-UNIT (NRML-ANL-YZE (CONSEQUENT CONCL-EXPR))
			WT-CNTXT ANTE-SITUATION )) )
       (CONTEXT:ADD ANTE-SUPPOSITION ANTE-SITUATION)
       (MULTIPLE-VALUE-BIND
	      (CONCLUSIVE? CONCL MEM-BLF STOP-REAS EFFORT TASK-REC RGRAPH)
	      (CSR:INVESTIGATE-FROM-MEMORY
		CONSE-QUERY
		`((MAX-EFFORT . ,(- MAX-EFFORT CURRENT-TOTAL-EFFORT))
		   ;; the specvars MAX-EFFORT and CURRENT-TOTAL-EFFORT
		   ;;  are bound in the fn CSR:FIND-CONSIDERATIONS.
		  (CONCLUSIVENESS-LEVEL . ,(A-Q-GET REAS-SPECS
						    'CONCLUSIVENESS-LEVEL ))
		   ;; specvar REAS-SPECS is bound in fn CSR:FIND-CONSIDERATIONS.
		  (EXTRA-TARGETS . NIL)		;; sbj-negation-possibilities
		  (RECORD-BELIEF? . NO) ) )
	 (COND ((EQ 'SUFFICIENT CONCLUSIVE?)	;; we have a cnd-prf-conclusion
; code to wide to indent properly
(LET* ((PREM-NODES
	 (MAPCAN #'(LAMBDA (RP-NODE)
		     (COND  ((AND (NOT (EQ ANTE-P-UNIT
					   (BELIEF-P-UNIT
						(RP-NODE-CONTENT RP-NODE) ) ))
				  (SOME (RP-NODE-PART-CONSIDS RP-NODE)
					#'(LAMBDA (CNSD)
					    (NULL (CONSID-GOAL-NODES CNSD)) ) ) )
			       (NCONS (CSR:UPDATE-R-GRAPH
					   (RP-NODE-CONTENT RP-NODE)
					   R-GRAPH
					   'KNOWLEDGE 'BASIS )) )) )
		 (R-GRAPH-K-BASIS RGRAPH) ) )
       (NEW-CONSID (MAKE-REASONING-CONSIDERATION-LINK
		      R-GRAPH R-GRAPH
		      RULE 'SUBJUNCTIVE-CONDITIONALIZATION
		      PREM-NODES PREM-NODES
		      CONCL-NODE RP-TGT-NODE ))
       (CONCL-EPISTATUS (BELIEF-EPISTATUS (RP-NODE-CONTENT RP-TGT-NODE)) ) )
      (CSR:INSTALL-CONSID-LINK NEW-CONSID)
      ;; next, return stuff from the lower r-graph to the upper, putting the
      ;;  lower r-graph and task-record in a bl-grounds slot of an rp-node.
      (SETF* (EPIST-BL-GROUNDS CONCL-EPISTATUS)
	     (A-Q-PUTPROP -*- RGRAPH 'CONDITIONAL-PROOF-R-GRAPH) )
      (A-Q-PUTPROP (EPIST-BL-GROUNDS CONCL-EPISTATUS) TASK-REC
		   'CONDITIONAL-PROOF-TASK-RECORD )
      (A-Q-PUTPROP (EPIST-BL-GROUNDS CONCL-EPISTATUS)
		   `((STOP-REAS . ,STOP-REAS) (EFFORT . ,EFFORT))
		   'CONDITIONAL-PROOF-DATA )
      `((TRIAL-RESULT . SUCCESS)	;; returns a TRIAL-REPORT a-list.
	(NUMBER-OF-NEW-CONSIDS . 1) ) ) )
; re-indent to proper depth
	       (T	;; we have no conditional-proof-conclusion
		  `((TRIAL-RESULT . FAILURE)
		    (CONDITIONAL-PROOF-R-GRAPH . ,RGRAPH)
		    (CONDITIONAL-PROOF-TASK-RECORD . ,TASK-REC)
		    (CONDITIONAL-PROOF-DATA . ((STOP-REAS . ,STOP-REAS)
					       (EFFORT . ,EFFORT) )) ) ) ) ) ) )
;			Conjunction

(DEFUN CONJUNCT-BM-PREDICATE (CONCL-EXPR)
  (EQ 'CONJ-PROPO (LT-TYPE CONCL-EXPR)) )

(DEFUN CONJUNCT-B-METHOD (RP-TGT-NODE)
;(WRITE T "CONJ-CONCL: "
;	  (DISPLAY (QUERY-FORMULA (RP-NODE-CONTENT RP-TGT-NODE)) 13.) )
 (LET* ((CONCL-EXPR (QUERY-FORMULA (RP-NODE-CONTENT RP-TGT-NODE)))
	 ;; conclusion expression
	(R-GRAPH (RP-NODE-R-GRAPH RP-TGT-NODE))
; should we normalize and use the normal conjuncts?
	(CONJUNCT-NODES
	  (MAPCAR #'(LAMBDA (ROLINK)
		      () )
		  (ROLELINKS CONCL-EXPR) ) ) )
       '((TRIAL-RESULT . FAILURE)
	 (FAILURE-REASON . |Unfinished B-Method|) ) ) )
;		Quantified Modus Ponens  (New Version)

;;; This version cannot yet handle premises with no ⊃-kernel.
;;;  Thus, the use of UQ-KERNEL-TYPE-CHECK, etc. needs to be folded in properly.

(DEFUN QUANT-MP-BM-PREDICATE2 (CONCL-EXPR)
 ;(MEMQ (LT-QQU-TYPE CONCL-EXPR) '(ATOMICPROPO CONN-PROPO QUANTIFIERFORM))
 (MEMQ (LT-TYPE CONCL-EXPR) '(ATOMICPROPO NEGPROPO)) )
; This is somewhat overly narrow.  Ideally, this predicate would return
;  T iff CONCL-EXPR contains some quantifiable individual term.
;  (But what if it instantiates a universally quantified affairstate variable?)

(DECLARE (special CONCL-EXPR))

(DEFUN QUANT-MP-B-METHOD2 (RP-TGT-NODE)
;(WRITE T "QMP2-CONCL: "
;	  (DISPLAY (QUERY-FORMULA (RP-NODE-CONTENT RP-TGT-NODE)) 13.) )
 (LET* ((CONCL-EXPR (QUERY-FORMULA (RP-NODE-CONTENT RP-TGT-NODE)))
	 ;; conclusion expression
	(R-GRAPH (RP-NODE-R-GRAPH RP-TGT-NODE))
	(CONCL-LT-TYPE (LT-TYPE CONCL-EXPR))
	(NEW-CONSID-LINKS) )
       (MULTIPLE-VALUE-BIND (KF-Q-PREM-CANDS RC-Q-PREM-CANDS)
	;;  KF-: knowledge-frontier beliefs, RC-: reasoning-context beliefs
	;;  Both are lists of q-premise candidates.  Eventually, we'll need to
	;;   eliminate any possible duplications of beliefs in these two lists.
	  (CSR:KNOWLEDGE-LOOKUP-ALL
	    R-GRAPH
	    #'(LAMBDA (*DAL*) 
		(AND (EQ 'QUANTIFIERFORM (A-Q-GET *DAL* 'LT-TYPE))
		     (EQ '∀ (A-Q-GET *DAL* 'LT-Q-DETERMINER))
		     (UQ-⊃-KERNEL-TYPE-CHECK *DAL* CONCL-LT-TYPE) ) )
	    #'(LAMBDA (*UNIT*) 
		(AT-MATCH CONCL-EXPR (UQ-⊃-KERNEL (GET *UNIT* 'LT-FORMULA))) )
	    #'(LAMBDA (*EPS*) 
		(≥-BEL-LEVEL (EPIST-BEL-LEVEL *EPS*) 'VERY-LIKELY) ) )
;(cond (rc-q-prem-cands (break qmp:test)))
;(break qmp:test)
      ;; body of mult-val-bind of KF-Q-PREM-CANDS and RC-Q-PREM-CANDS.
      (MAPC #'(LAMBDA (Q-PREM-CAND-PAIR)	;; (<belief> . <bindings>)
		(LET* (((Q-PREM-CAND . Q-BINDINGS) Q-PREM-CAND-PAIR)
		       (Q-PREM-WFF (BELIEF-FORMULA Q-PREM-CAND))
		       (SRT-PREM-BLF∨QRY-LIST	;; the sortal premises
; code too wide to indent
(MAPCAR #'(LAMBDA (Q-BINDING)  ;;  (<d-item> . <qt-pair>)
	    (LET ((SRT-PREM-WFF		;; a sortal premise wff
		    (LT-SUBST Q-BINDINGS (LT-QSORT-EXPR (CDDR Q-BINDING))) ))
		 (COND ((AND (ISA-SIMPLE-SORT-PROPO SRT-PREM-WFF)
			     (OR (SORTALLY-CERTAIN? SRT-PREM-WFF)
				 (SORTALLY-NEG-CERTAIN? SRT-PREM-WFF) )))
		       (
     ;; code too wide to indent properly
     (LET ((SRT-PREM-LT-TYPE (LT-TYPE SRT-PREM-WFF))
	   (SRT-PREM-P-UNIT (NRML-ANL-YZE SRT-PREM-WFF)) )
	  (CSR:KNOWLEDGE-LOOKUP
	    R-GRAPH
	    #'(LAMBDA (*DAL*) 
		(EQ SRT-PREM-LT-TYPE (A-Q-GET *DAL* 'LT-TYPE)) )
	    #'(LAMBDA (*UNIT*) 
		(EQ *UNIT* SRT-PREM-P-UNIT) )
	    #'(LAMBDA (*EPS*) 
		(≥-BEL-LEVEL (EPIST-BEL-LEVEL *EPS*) 'VERY-LIKELY) ) ) ) )
    (T (MAKE-QUERY P-UNIT (NRML-ANL-YZE SRT-PREM-WFF)
		   WT-CNTXT (R-GRAPH-RB-CONTEXT R-GRAPH) )) ) ) )
Q-BINDINGS ) ) )
; end of computation of SRT-PREM-BLF∨QRY-LIST, each member of which
;  will be EQ either to SORTALLY-CERTAIN, or SORTALLY-NEG-CERTAIN,
;  or a known <mem-blf>, or a new <query>.
; code too wide to indent properly
; body of main LET* in MAPC lambda-fn mapping Q-PREM-CAND-PAIRs
(COND ((MEMQ 'SORTALLY-NEG-CERTAIN SRT-PREM-BLF∨QRY-LIST))
       ;; in the case above, quit and move to next Q-PREM-CAND-PAIR
      (T (LET* ((ANT-PREM-WFF   ;; the instantiated-antecedent premise
		  (LT-SUBST Q-BINDINGS (ANTECEDENT (UQ-KERNEL Q-PREM-WFF))) )
		(ANT-PREM-P-UNIT (NRML-ANL-YZE ANT-PREM-WFF))
		(ANT-PREM-LT-TYPE (LT-TYPE ANT-PREM-WFF))
		(ANT-PREM-BLF∨QRY
		  (COND
		    ((CSR:KNOWLEDGE-LOOKUP
		       R-GRAPH
		       #'(LAMBDA (*DAL*) 
			   (EQ ANT-PREM-LT-TYPE (A-Q-GET *DAL* 'LT-TYPE)) )
		       #'(LAMBDA (*UNIT*) 
			   (EQ *UNIT* ANT-PREM-P-UNIT) )
		       #'(LAMBDA (*EPS*) 
			   (≥-BEL-LEVEL (EPIST-BEL-LEVEL *EPS*) 'VERY-LIKELY) )))
		    (T (MAKE-QUERY P-UNIT ANT-PREM-P-UNIT
				   WT-CNTXT (R-GRAPH-RB-CONTEXT R-GRAPH) )) ) ))
		     ;; but what if only the *EPS*-test failed?
;(break qmp:test)
; code too wide to indent properly -- body of the previous LET*
(LET* ((Q-PREM-NODE
	(CSR:UPDATE-R-GRAPH Q-PREM-CAND R-GRAPH 'KNOWLEDGE 'BASIS) )
       (SRT-PREM-NODE-LIST
	(MAPCAN
    ; too wide to indent fully
    #'(LAMBDA (SRT-PREM-BLF∨QRY)
	(COND ((EQ 'SORTALLY-CERTAIN SRT-PREM-BLF∨QRY) NIL)
	      ((EQ 'KNOWLEDGE (BELIEF-TYPE SRT-PREM-BLF∨QRY))
		(NCONS
	         (CSR:UPDATE-R-GRAPH SRT-PREM-BLF∨QRY R-GRAPH 'KNOWLEDGE 'BASIS) ))
	      ((EQ 'QUERY (BELIEF-TYPE SRT-PREM-BLF∨QRY))
		(NCONS
	         (CSR:UPDATE-R-GRAPH SRT-PREM-BLF∨QRY R-GRAPH 'TARGET 'FRONTIER) ))
	      (T (BREAK |QUANT-MP-B-METHOD2 - bad SRT-PREM-BLF∨QRY|)) ) )
    SRT-PREM-BLF∨QRY-LIST ) )
       (ANT-PREM-NODE
	(COND ((EQ 'KNOWLEDGE (BELIEF-TYPE ANT-PREM-BLF∨QRY))
	        (CSR:UPDATE-R-GRAPH ANT-PREM-BLF∨QRY R-GRAPH 'KNOWLEDGE 'BASIS) )
	      ((EQ 'QUERY (BELIEF-TYPE ANT-PREM-BLF∨QRY))
	         (CSR:UPDATE-R-GRAPH ANT-PREM-BLF∨QRY R-GRAPH 'TARGET 'FRONTIER) )
	      (T (BREAK |QUANT-MP-B-METHOD2 - bad ANT-PREM-BLF∨QRY|)) ) )
       (GOAL-NODES
	(NCONC (MAPCAN #'(LAMBDA (SRT-PREM-NODE)
			   (COND ((AND SRT-PREM-NODE
				       (EQ 'TARGET (RP-NODE-TYPE SRT-PREM-NODE)) )
				    (NCONS SRT-PREM-NODE) )) )
		       SRT-PREM-NODE-LIST )
	       (COND ((EQ 'TARGET (RP-NODE-TYPE ANT-PREM-NODE))
		        (NCONS ANT-PREM-NODE) )) ) )
       (NEW-CONSID
	(MAKE-QMP-CONSID
	  ; the following are CONSID- slots INCLUDEd in QMP-CONSID
	  R-GRAPH R-GRAPH
	  PREM-NODES (CONS Q-PREM-NODE (APPEND SRT-PREM-NODE-LIST
					       (NCONS ANT-PREM-NODE) ))
	  CONCL-NODE RP-TGT-NODE
	  GOAL-NODES GOAL-NODES ) ) )
      (CSR:INSTALL-CONSID-LINK NEW-CONSID)
      (PUSH NEW-CONSID NEW-CONSID-LINKS) ) ) ) ) ) )
	      ;; 2nd arg to earlier MAPC
	     (NCONC KF-Q-PREM-CANDS RC-Q-PREM-CANDS) )
	      ;; eventually, we'll want to eliminate any duplications
	      ;;  in these two lists before NCONCing them.
       (COND (NEW-CONSID-LINKS		;; returns a TRIAL-REPORT a-list.
		`((TRIAL-RESULT . SUCCESS)
		  (NUMBER-OF-NEW-CONSIDS . ,(LENGTH NEW-CONSID-LINKS)) ) )
	     (T '((TRIAL-RESULT . FAILURE))) ) ) ) )

(DECLARE (unspecial CONCL-EXPR))
;		Statistical Syllogism  (Old Version)

(DEFUN STATIST-BM-PREDICATE1 (CONCL-EXPR)
  (OR (AND (EQ 'ATOMICPROPO (LT-TYPE CONCL-EXPR))
	   (= (LENGTH CONCL-EXPR) 2) )
      (AND (EQ 'NEGPROPO (LT-TYPE CONCL-EXPR))
	   (STATIST-BM-PREDICATE1 (ARGUMENT (CAR (ROLELINKS CONCL-EXPR)))) ) ) )
; This is just a temporary hack.  In general, this predicate should return
;  T iff CONCL-EXPR contains some quantifiable individual term.

(DEFUN STATIST-B-METHOD (RP-TGT-NODE)
 (LET* ((CONCL-EXPR (QUERY-FORMULA (RP-NODE-CONTENT RP-TGT-NODE)))
	 ;; conclusion expression
	(R-GRAPH (RP-NODE-R-GRAPH RP-TGT-NODE))
	(CONCL-LT-TYPE (LT-TYPE CONCL-EXPR))
	(CONCL-SUBJ
	   (CASEQ CONCL-LT-TYPE		;; this is just a temporary hack
	     (ATOMICPROPO
	       (ARGUMENT (CAR (ROLELINKS CONCL-EXPR))) )
	     (NEGPROPO
	       (ARGUMENT (CAR (ROLELINKS
			       (ARGUMENT (CAR (ROLELINKS CONCL-EXPR)))))))
	     (T 'PUNT NIL) ))
	(Q-KERNEL-PATT
	   (COND ((#.(ISA-OF:LT . PFC-FORMULA) CONCL-EXPR)
		    (SUBST '?X CONCL-SUBJ CONCL-EXPR) )
		 (T 'PUNT NIL) ) )
	 ;; In general, one Q-KERNEL-PATT can be obtained for each different way
	 ;;  of substituting '?X' for an individual term in CONCL-EXPR.  For
	 ;;  large exprs, there will be many such ways, and some heuristic
	 ;;  guidance will be needed to explore only the most promising of them.
	(NEW-CONSID-LINKS) )
       (MULTIPLE-VALUE-BIND (KF-STAT-PREM-CANDS RC-STAT-PREM-CANDS)
	;; 	 knowledge-frontier beliefs, reasoning-context beliefs
	;;  Both are lists of stat-premise candidates.  Eventually, we'll need to
	;;   eliminate any possible duplications of beliefs in these two lists.
	  (CSR:KNOWLEDGE-LOOKUP-ALL
	    R-GRAPH
	    #'(LAMBDA (*DAL*) 
		(AND (EQ 'QUANTIFIERFORM (A-Q-GET *DAL* 'LT-TYPE))
		     (EQ 'GREAT-MAJORITY (A-Q-GET *DAL* 'LT-Q-DETERMINER))
		     (EQ CONCL-LT-TYPE (A-Q-GET *DAL* 'LT-TYPE*UQ-KERNEL)) ) )
	    #'(LAMBDA (*UNIT*) 
		(AT-MATCH (UQ-KERNEL (GET *UNIT* 'LT-FORMULA)) Q-KERNEL-PATT) )
	    #'(LAMBDA (*EPS*) 
		(≥-BEL-LEVEL (EPIST-BEL-LEVEL *EPS*) 'VERY-LIKELY) ) )
      (MAPC #'(LAMBDA (STAT-PREM-CAND)	;; a belief
		(LET* ((STAT-PREM-WFF (BELIEF-FORMULA STAT-PREM-CAND))
		       (QSORT-EXPR (LT-QSORT-EXPR STAT-PREM-WFF))
		       (S-PREM-WFF
			  (SUBST CONCL-SUBJ STAT-PREM-WFF QSORT-EXPR) )
			   ;; recall that quantified terms are pointers to
			   ;;  the quantified expressions in which they occur.
		       (S-PREM-P-UNIT (NRML-ANL-YZE S-PREM-WFF))
		       (S-PREM-LT-TYPE (LT-TYPE S-PREM-WFF))
		       (S-PREM-BELIEF
	   ;; code too wide to indent properly
	  (CSR:KNOWLEDGE-LOOKUP
	    R-GRAPH
	    #'(LAMBDA (*DAL*) 
		(EQ S-PREM-LT-TYPE (A-Q-GET *DAL* 'LT-TYPE)) )
	    #'(LAMBDA (*UNIT*) 
		(EQ *UNIT* S-PREM-P-UNIT) )
	    #'(LAMBDA (*EPS*) 
		(≥-BEL-LEVEL (EPIST-BEL-LEVEL *EPS*) 'VERY-LIKELY) ) ) ) )
    ;; code too wide to indent properly
    (COND (S-PREM-BELIEF	;; complete success
	   (LET* ((STAT-PREM-NODE
		   (CSR:UPDATE-R-GRAPH STAT-PREM-CAND R-GRAPH 'KNOWLEDGE 'BASIS) )
		  (S-PREM-NODE
		   (CSR:UPDATE-R-GRAPH S-PREM-BELIEF R-GRAPH 'KNOWLEDGE 'BASIS) )
		  (NEW-CONSID
		   (MAKE-STAT-CONSID
		     STAT-PREM-NODE STAT-PREM-NODE
		     S-PREM-NODE S-PREM-NODE
		     ; the following are CONSID- slots INCLUDEd in STAT-CONSID
		     R-GRAPH R-GRAPH
		     PREM-NODES (LIST STAT-PREM-NODE S-PREM-NODE)
		     CONCL-NODE RP-TGT-NODE ) ) )
		 (CSR:INSTALL-CONSID-LINK NEW-CONSID)
		 (PUSH NEW-CONSID NEW-CONSID-LINKS) ) )
	  (T	    ;; partial success -- in this case we set up a GOAL-consid
	     (LET* ((STAT-PREM-NODE
		     (CSR:UPDATE-R-GRAPH STAT-PREM-CAND R-GRAPH 'KNOWLEDGE 'BASIS) )
		    (S-PREM-QUERY
		      (MAKE-QUERY
			 P-UNIT (NRML-ANL-YZE S-PREM-WFF)
			 WT-CNTXT (R-GRAPH-RB-CONTEXT R-GRAPH) ) )
		    (S-PREM-NODE
		     (CSR:UPDATE-R-GRAPH S-PREM-QUERY R-GRAPH 'TARGET 'FRONTIER))
		    (NEW-CONSID
		     (MAKE-STAT-CONSID
		       STAT-PREM-NODE STAT-PREM-NODE
		       S-PREM-NODE S-PREM-NODE
		       ; the following are CONSID- slots INCLUDEd in STAT-CONSID
		       R-GRAPH R-GRAPH
		       PREM-NODES (LIST STAT-PREM-NODE S-PREM-NODE)
		       CONCL-NODE RP-TGT-NODE
		       GOAL-NODES (NCONS S-PREM-NODE) ) ) )
		   (CSR:INSTALL-CONSID-LINK NEW-CONSID)
		   (PUSH NEW-CONSID NEW-CONSID-LINKS) ) ) ) ) )
	     (NCONC KF-STAT-PREM-CANDS RC-STAT-PREM-CANDS) )
	      ;; eventually, we'll want to eliminate any duplications
	      ;;  in these two lists before NCONCing them.
       (COND (NEW-CONSID-LINKS		;; returns a TRIAL-REPORT a-list.
		`((TRIAL-RESULT . SUCCESS)
		  (NUMBER-OF-NEW-CONSIDS . ,(LENGTH NEW-CONSID-LINKS)) ) )
	     (T '((TRIAL-RESULT . FAILURE))) ) ) ) )
;		Quantified Modus Ponens  (Old Version)

(DEFUN QUANT-MP-BM-PREDICATE1 (CONCL-EXPR)
 (OR (AND (EQ 'ATOMICPROPO (LT-TYPE CONCL-EXPR))
	  (= (LENGTH CONCL-EXPR) 2) )
     (AND (EQ 'NEGPROPO (LT-TYPE CONCL-EXPR))
	  (QUANT-MP-BM-PREDICATE1 (ARGUMENT (CAR (ROLELINKS CONCL-EXPR)))) ) ) )
; This is just a temporary hack.  In general, this predicate should return
;  T iff CONCL-EXPR contains some quantifiable individual term.

(DEFUN QUANT-MP-B-METHOD (RP-TGT-NODE)
 (LET* ((CONCL-EXPR (QUERY-FORMULA (RP-NODE-CONTENT RP-TGT-NODE)))
	 ;; conclusion expression
	(R-GRAPH (RP-NODE-R-GRAPH RP-TGT-NODE))
	(CONCL-LT-TYPE (LT-TYPE CONCL-EXPR))
	(CONCL-SUBJ
	   (CASEQ CONCL-LT-TYPE		;; this is just a temporary hack
	     (ATOMICPROPO
	       (ARGUMENT (CAR (ROLELINKS CONCL-EXPR))) )
	     (NEGPROPO
	       (ARGUMENT (CAR (ROLELINKS
			       (ARGUMENT (CAR (ROLELINKS CONCL-EXPR)))))))
	     (T 'PUNT NIL) ))
	(Q-KERNEL-PATT
	   (COND ((#.(ISA-OF:LT . PFC-FORMULA) CONCL-EXPR)
		    (SUBST '?X CONCL-SUBJ CONCL-EXPR) )
		 (T 'PUNT NIL) ) )
	 ;; In general, one Q-KERNEL-PATT can be obtained for each different way
	 ;;  of substituting '?X' for an individual term in CONCL-EXPR.  For
	 ;;  large exprs, there will be many such ways, and some heuristic
	 ;;  guidance will be needed to explore only the most promising of them.
	(NEW-CONSID-LINKS) )
       (MULTIPLE-VALUE-BIND (KF-Q-PREM-CANDS RC-Q-PREM-CANDS)
	;; 	 knowledge-frontier beliefs, reasoning-context beliefs
	;;  Both are lists of q-premise candidates.  Eventually, we'll need to
	;;   eliminate any possible duplications of beliefs in these two lists.
	  (CSR:KNOWLEDGE-LOOKUP-ALL
	    R-GRAPH
	    #'(LAMBDA (*DAL*) 
		(AND (EQ 'QUANTIFIERFORM (A-Q-GET *DAL* 'LT-TYPE))
		     (EQ '∀ (A-Q-GET *DAL* 'LT-Q-DETERMINER))
		     (EQ CONCL-LT-TYPE (A-Q-GET *DAL* 'LT-TYPE*UQ-KERNEL)) ) )
	    #'(LAMBDA (*UNIT*) 
		(AT-MATCH (UQ-KERNEL (GET *UNIT* 'LT-FORMULA)) Q-KERNEL-PATT) )
	    #'(LAMBDA (*EPS*) 
		(≥-BEL-LEVEL (EPIST-BEL-LEVEL *EPS*) 'VERY-LIKELY) ) )
;(cond (rc-q-prem-cands (break qmp:test)))
      (MAPC #'(LAMBDA (Q-PREM-CAND)	;; a belief
		(LET* ((Q-PREM-WFF (BELIEF-FORMULA Q-PREM-CAND))
		       (QSORT-EXPR (LT-QSORT-EXPR Q-PREM-WFF))
		       (S-PREM-WFF
			  (SUBST CONCL-SUBJ Q-PREM-WFF QSORT-EXPR) )
			   ;; recall that quantified terms are pointers to
			   ;;  the quantified expressions in which they occur.
		       (S-PREM-P-UNIT (NRML-ANL-YZE S-PREM-WFF))
		       (S-PREM-LT-TYPE (LT-TYPE S-PREM-WFF))
		       (S-PREM-BELIEF
	   ;; code too wide to indent properly
	  (CSR:KNOWLEDGE-LOOKUP
	    R-GRAPH
	    #'(LAMBDA (*DAL*) 
		(EQ S-PREM-LT-TYPE (A-Q-GET *DAL* 'LT-TYPE)) )
	    #'(LAMBDA (*UNIT*) 
		(EQ *UNIT* S-PREM-P-UNIT) )
	    #'(LAMBDA (*EPS*) 
		(≥-BEL-LEVEL (EPIST-BEL-LEVEL *EPS*) 'VERY-LIKELY) ) ) ) )
;(break qmp:test)
    ;; code too wide to indent properly
    (COND (S-PREM-BELIEF	;; complete success
	   (LET* ((Q-PREM-NODE
		   (CSR:UPDATE-R-GRAPH Q-PREM-CAND R-GRAPH 'KNOWLEDGE 'BASIS) )
		  (S-PREM-NODE
		   (CSR:UPDATE-R-GRAPH S-PREM-BELIEF R-GRAPH 'KNOWLEDGE 'BASIS) )
		  (NEW-CONSID
		   (MAKE-QMP-CONSID
		     ; the following are CONSID- slots INCLUDEd in QMP-CONSID
		     R-GRAPH R-GRAPH
		     PREM-NODES (LIST Q-PREM-NODE S-PREM-NODE)
		     CONCL-NODE RP-TGT-NODE ) ) )
		 (CSR:INSTALL-CONSID-LINK NEW-CONSID)
		 (PUSH NEW-CONSID NEW-CONSID-LINKS) ) )
	  (T	    ;; partial success -- in this case we set up a GOAL-consid
	     (LET* ((Q-PREM-NODE
		     (CSR:UPDATE-R-GRAPH Q-PREM-CAND R-GRAPH 'KNOWLEDGE 'BASIS) )
		    (S-PREM-QUERY
		      (MAKE-QUERY
			 P-UNIT (NRML-ANL-YZE S-PREM-WFF)
			 WT-CNTXT (R-GRAPH-RB-CONTEXT R-GRAPH) ) )
		    (S-PREM-NODE
		     (CSR:UPDATE-R-GRAPH S-PREM-QUERY R-GRAPH 'TARGET 'FRONTIER))
		    (NEW-CONSID
		     (MAKE-QMP-CONSID
		       ; the following are CONSID- slots INCLUDEd in QMP-CONSID
		       R-GRAPH R-GRAPH
		       PREM-NODES (LIST Q-PREM-NODE S-PREM-NODE)
		       CONCL-NODE RP-TGT-NODE
		       GOAL-NODES (NCONS S-PREM-NODE) ) ) )
		   (CSR:INSTALL-CONSID-LINK NEW-CONSID)
		   (PUSH NEW-CONSID NEW-CONSID-LINKS) ) ) ) ) )
	     (NCONC KF-Q-PREM-CANDS RC-Q-PREM-CANDS) )
	      ;; eventually, we'll want to eliminate any duplications
	      ;;  in these two lists before NCONCing them.
       (COND (NEW-CONSID-LINKS		;; returns a TRIAL-REPORT a-list.
		`((TRIAL-RESULT . SUCCESS)
		  (NUMBER-OF-NEW-CONSIDS . ,(LENGTH NEW-CONSID-LINKS)) ) )
	     (T '((TRIAL-RESULT . FAILURE))) ) ) ) )

