
perm filename LOSS.LSP[MRS,LSP] blob sn#656544 filedate 1982-04-26 generic text, type C, neo UTF8

COMMENT ⊗   VALID 00003 PAGES
C REC  PAGE   DESCRIPTION
C00001 00001
C00002 00002	(DECLARE (fasload struct fas dsk (mac lsp))
C00011 00003	(declare (gc))
C00041 ENDMK
C⊗;
(DECLARE (fasload struct fas dsk (mac lsp))
	 (mapex t)
	 (*lexpr λ-UNSUBST QV-QUASI-UNSUBST NORMALIZE-CMPD-CONCEPT
		 ANALYZE-CMPD-CONCEPT)
	 (load '|nsublis.lsp|)

	 (special QV-SUBSTLIST UQ-LIST CURRENTPOS SUBSTLISTPTR GENVARINDEX
		  GENVAR-RANGES BREAK-POINTS BREAK-BEFORE-POINTS *CONCEPTS*
		  ALPHABET REVERSE-ALPHABET ALPHA-NCONSES *NOPOINT
		  ↑-MATRIX-ANALYSIS-LIST JUNCT-ANALYSIS-LIST ANALYSIS-LIST
		  TERMCOPIES TCOPYPAIR *-ASCII AL-VARS RO-INDEX CURRENTNODE
		  CURRENT-NODE-PATH VERBOSITY XPDN-HELP-TABLE YHπ-FLAG )

	 (special |cc-op: |  |=>|  | .|  |  |  | ;|  V  C  | |  | - | ) )

(SETQ IBASE 10. BASE 10.)
(SETQ PRINLEVEL 3.)
(SETQ PRINLENGTH 60.)
(LINEL NIL 80)
(SETQ BREAK-POINTS '(|, |  | ∧ |  | ∨ |)	;; possibly also /:
      BREAK-BEFORE-POINTS '(|↑[|) )

(DEFSTRUCT (↑↓-TERM (TYPE TREE))
	   ↑↓-MARKER ↑↓-MATRIX )
(declare (gc))

(DEFSTRUCT (LT-QUANTIFIER (TYPE HUNK) (CONC-NAME LT-))
	   DEPENDENCIES DETERMINER QSORTEXPR SCOPE )

(declare (gc))
(DEFSTRUCT (ROLELINK (TYPE TREE))
	   ROLEMARK ARGUMENT )

(declare (gc))
(DEFSTRUCT (PFC-FORMULA (TYPE TREE))
	   PFC-CONCEPT ROLELINKS )
; PFC-FORMULA => (pred rlnk1 rlnk2 ... rlnkn) or (func rlnk1 rlnk2 ... rlnkn)
;		   or (connective rlnk1 rlnk2 ... rlnkn)

(declare (gc))
(DEFSTRUCT (ROLEXENTRY (TYPE TREE))
	   ROLENAME ROLEPHRASE )

(declare (gc))
(DEFSTRUCT (LT-λ-EXPR (TYPE TREE) (CONC-NAME LT-))
	   (λ-PREFIX (MAKE-LT-λ-PREFIX)) λ-SCOPE )

(declare (gc))
(DEFSTRUCT (LT-λ-PREFIX (TYPE TREE) (BUT-FIRST LT-λ-PREFIX) (CONC-NAME LT-))
	   (λ-MARK 'λ) PATHKEYLISTS )
; PATHKEYLISTS => ((<termsort> <pathkey> {<pathkey>} ... ) ... )

(declare (gc))
(DEFSTRUCT (PATHKEYLIST (TYPE TREE))
	   λ-TERMSORT PATHKEYS )

(declare (gc))
(DEFMACRO *DEFUN ((F-TYPE . F-NAME) ARGLIST . BODY)
  `(PROGN
      (PUTPROP (OR (GET ',F-NAME 'FUNCTIONS) 
		   (PUTPROP ',F-NAME (NCONS "*DEFUN-PLIST") 'FUNCTIONS))
	       ,(COND ((EQ (CAR BODY) '*SYN) `',(CADR BODY))
		      (T `'(LAMBDA ,ARGLIST ,@BODY)) )
	       ',F-TYPE )
      (LET ((OLDMACRO (GET ',F-TYPE 'MACRO))
	    (NEWMACRO '(LAMBDA (FORM)
			`(GET (GET ',(CDR FORM) 'FUNCTIONS) ',',F-TYPE) )) )
	   (COND ((AND OLDMACRO 
		       (NOT (EQUAL OLDMACRO NEWMACRO)) )
		  (TERPRI) (PRIJC "Macro ") (PRIN1 ',F-TYPE)
		  (PRINC " already defineD differently!")
		  (BREAK *DEFUN) )) )
      (DEFUN ,F-TYPE MACRO (FORM)
	 `(GET (GET ',(CDR FORM) 'FUNCTIONS) ',',F-TYPE) ) ) )

(declare (gc))
(*DEFUN (ISA , COREROLE) (ROLEMARK LT-FORM)
   (MEMQ ROLEMARK (GET (PFC-CONCEPT LT-FORM) 'COREROLES)) )

(declare (gc))
(*DEFUN (THE-FOR:ROLELINK . ROLEPHRASE) (ROLELINK LT-FORM)
   (CDR (ASSQ (ROLEMARK ROLELINK) (GET (PFC-CONCEPT LT-FORM) 'ROLEXICON))) )

(declare (gc))
(*DEFUN (THE-OF:LT-QUANT . QSORT) (LT-QUANT)
   (LET* ((QSORTEXPR (LT-QSORTEXPR LT-QUANT))
	  (ATOMICQSORTEXPR
	    (CASEQ (LT-TYPE QSORTEXPR)
	       (ATOMICPROPO QSORTEXPR)
	       (CONJ-PROPO (ARGUMENT (CAR (ROLELINKS QSORTEXPR)))) ) ) )
	 (COND ((EQ (PFC-CONCEPT ATOMICQSORTEXPR) 'CONCEPT) 
		  (NORMALIZE-TERMSORTEXPR
		   (CONS '↑ (TERMSORT
			     (ARGUMENT
			      (ASSQ 'OBJECT
				    (ROLELINKS ATOMICQSORTEXPR) ) ) )) ) )
	       (T (PFC-CONCEPT ATOMICQSORTEXPR)) )) )

(declare (gc))
(*DEFUN (THE-OF:LT-QUANT . DETERMINER) (LT-QUANT)
    *SYN CAR )
;   *SYN LT-DETERMINER )  This usage causes an "; AMPROPER USE OF MACRO - EVAL"
; error message; what LISP doesn't like here iS simply the fact that 
; LT-DETERMINER is a macro.

(declare (gc))
("DEFUN (THE-OF:LT-λ-PREFIX . PATHKEYLISPS) (λ-PREFIX)
    *SYN CDR )

(dec@1CeJ@!OFRR4∀PE	∃
+≤@!)⊃
[=u→(5#+β≥P@\A#M∨%)a!$R@!→([#Uβ≥)∪→∪$R4∀@@@Qπ1$dA→(5#+β≥Q∪
∪HR@R~(~∀QI∃GYCe∀@QOF$R~∀P)	
+8@Q)⊃∀[∨u1([#+¬≥(@\↓'π∨!∀R@A→P[#+β9)∪
∪∃$R~∀@@@Q
1$@f↓→([#Uβ≥)∪→∪$RR~∀~(QIKG1CeJ@!OFRR4∀PE	∃
+≤@!)⊃
[=u→∪9#+β≥P@\A	∃)%≠%≥β$RQ→∪≥E+β≥($~∀@@@QπβHA→∪≥E+β≥($@R~∀4∀QIK
YCeJQOFR$~∀PU⊃
+≤Q∪'α5∨u→P@\@⊂αj⊗bB⊂I↓"2"j~>Jh¬⊂hR∧∧αDhDαD≤yj5α∧JET4⎇)Q∩αD9ye≥α¬λ4
∩	JBl4z)RJJ¬	T,m∀¬∧≤~$∧e"Xiu∀j∀∧r@B	H∀l∀H∃∩JJ¬⊃PPh'4Bo≥↔∪R¬π@Bn\≡&[r¬dβg→<[.=|]-≥Y~8l≡≠|OE⊃"Nh↓¬98<M7Hαβ!'h≥→.-<{|NE:;Y
≤x=≠n'H→2.M→<HπN→<[.=|],≡≠{/D
|H
πKK;8.-y<OD¬H∂≥↑[<{n.8=
⎇/J#!'h↔K-\<Zy.'H→2.M→<@d≠|HmHλ
d_Y:-lh_(M9z=∧∞⎇8z∧∞~_=∧εG≠Gπ∃C"J¬(⊃1U)d
∩4h∀Hα¬Z⊂24E∀
⊂u(*u⊃Sj)*#"D∧
⊂3HD
⊂sijtλ∀jXTu⊃IzS*#!$λλλ∧∧λ
⊃*∀	bλ¬λp4H
:0TuλitS*%⊃"Hλ∧∧λλλ¬	tH

;30SiJλ
⊂hJH∀u(*u⊃Sj)**#!!(λλ¬λ3Qλ¬
v30Iy∀λ
λ81∀H
:0TuλitS*%⊃"B"%λ4(	kd
⊃q*Hr⊂4D¬⊂p1
$∀u0J:⊃StIU(*%∀
(
$¬(
#!!"J→\{_<LT
→xe∃#"J¬(⊃1U)d
∩4h∀H⊂J(02k((1StHU4∪r)j
(


R3Uλ~∪s*!QHλλ¬λ3Qλ¬
v30Iy∀λ∀
)3U⊂*Is*#!!*⊃4$∧wH
λx5⊂rλ~H∀∀I→U⊂5	y(*%⊃"B*	zH
⊃*∀	kvd¬⊃q5λ9⊂4H

R3Uλ~∪s(ε%*#"A∀λλλ¬λ4(	e{h
⊃hZ⊂r⊂*$∀∀R)j⊂5∪iTj*$¬(
(¬⊃"Nb%λ4(	e{h
⊂h~H
∪λ~uλ
λ[∀∪∪hH(∀∀I→P5∪iU***$¬(
(π7h≥≠mt≠=8m∧_{{N=;Yc!!"J→\{_<LT
→xe∃#"J¬(⊃1U)d
∩4h∀H⊂J(02k*	r3U¬∀
∀∀I→U⊂5	y*#"D∧λ
∪(Y4(∀
)3U⊂*Is(⊂J(02k*	r3U
5(
#!!"J→\{_<LT
→xe∃#"J¬(⊃1U)d
∩4h∀H∀Iy⊃3∩)ij(
	J1Sj)*#"D∧λ
⊂)hλ
⊂iyTtλ	J1Sj)*(
λZ(
⊃hZλ
⊂h~H∪∃¬XStS%∀	pp*H1stK∃(	tIy⊃30*)j*(¬⊃"C"E→8{≡Y(
|j*#!%
Q⊃(j3H
	~p+3hgS∩3D¬H∀5(→U∩1I_4J(¬	∩3QIzS*#!$λλ
λZ(
⊃hZλ
⊂h~H∪∩)hStS%∀	pp*H1stK∃(	qλZ⊃4S)→Q4J$¬#"C!%→→8mL<Y(¬xj*!QJ
QλXU3H¬	4p+)xNS∃∧¬H∀5(→U∩1I_4J(¬	∃1IzS*#!$λλ
λZ(
⊃hZλ
λee∃∩⊃%YqNS
E450)jλHλH5⊃4IY3Q4E∀∪∃(itS*$∧pp5λXstV%∀	q⊃*H4S2)h4J(¬⊃"C"@↓E→→8mL<Y(¬xj*!QJ⊃⊃(i00tIt⊃.Q	t
∀u
)3Qj!QHλ_¬λ3.Q(9s30)h∀h
λ[∀∪∪hH0h
:∀R3Hu*(
!QC"JL8{_.,(
→l5*#"Eλ⊃1S(_tShλWUP4D¬∃P4Ih31*!QHλ_¬λq⊂4D¬⊃3.J(01∪ii⊗+5H~Th	e¬∃P4Ih31*%∃(
#!!"J→\{_<LT
→xe∃#"JλH1U3Dλ.Tq*I∩3Q$¬∪U3((4J#!$λ
∪λZλ

λ9⊂4K)I4u
%⊃"Hλ∧∧λλλ¬
q5∀$¬SSt	y3Uλ
E#"H∧∧λλλ∧¬∀q5
∀⊂r⊂*%3∩4jD
⊂4
λ3Qλ¬	04⊂h→Hλie	⊂30HH(
∀∧λ
(
	I4uλ
∧⊃
*!QB""!∀λλλ∧∧λ	j$HDHJ!QB""!∀λλλ∧∧λ
⊃+
∪∪qλXh∪U)XQ4J$¬#"B!⊃(λλ∧∧λλ	eH∪
$¬*#"D∧λλλ∧∧
∀q*J(
SIz∪r3JD∪R3¬⊃"Hλ∧∧λλλ¬λ3.Q(9s30)h∀h⊂iλ4K3	~u
(¬∀
#"AQJ→→,=_<Y$¬→xj%⊃"J⊃λXS00j)h⊂sijtλ
λ[∀∀J!QHλλ¬⊃4(¬
⊗4⊃*∧⊃6

J(	iI4u
$¬#"C!%→→8mL<Y(¬xj*!QJ⊃⊃(i00tIt∪⊂3((⊂+3j
H
∪h)J#"D∧λ_
	X34(¬λp04D¬∪pRE∀	j∪λ→0Q⊂$↓
*(¬⊃"C"E→8{≡Y(
|j*#!%⊃⊃1IX0tSdλ1⊃⊂iyPh
λ_⊃∪∩*:λ⊂P*83∩4jH5∪s%⊃"Hλ∧
∀q*J(⊂H~q3∩*:⊂5∪iT
∪PiyPhλ_⊃∪∩*:λ⊂H~q3∩*:⊂5∪iU*(
!QC"JL8{_.,(
→l5*#"Eλ⊃1S(_tShλYQ⊂sihh
⊂(H∪∩4jD⊂P4hY∩4uλ~∪s*!QHλλ¬⊂ssHD
⊂H~q3∩*:⊂5∪iT
∪PiyPhλ(4q3	~u⊂5	y(⊂(H⊃∩4jE*#"A∀λ
∃∧¬∀q5
∀⊂P*83∩4jH5∪s$¬⊂1⊃	I4u
%∀
(
!QC"JL8{_.,(
→l5*#"Eλ⊃1S(_tShλYQ⊂1λD
⊂1λI5⊃3$λP4q)I4u⊂*Is*#!$λ_
λ9sQλ¬¬⊂P4hY∩4uλ~∪s(¬	PssH4⊂P*83∩4jH5∪s$¬∪Psijh⊂(H∩5⊃)U**#!!(
∃∧¬∀q5
∀⊂P*83∩4jH5∪s$¬∪Psijh⊂(H∩5⊃)U**(¬∀
#"AQJ→→,=_<Y$¬→xj%⊃"J⊃λXS00j)h∀P*:t(
	86(⊂%Y∩4u¬⊃"Hλ∧
⊃∪d¬
⊂+*H23λ¬H+3∩*:λ
⊂hJH⊂+*H23
%∃#"B%¬∪U3	D⊂+5λ→3
*!QB*⊂iyQλ
¬λ4(
λ8⊂4Hλ∃5⊂2)E(∩h[*(

(5∃4Id
⊂p*$⊂+5λ→3
*%∃(
(¬⊃"C"E→8{≡Y(
|j*#!%⊃⊃1IX0tSdλ1∩5λ⊃h
λ[∀∀@∧nY<⎇∧λSq⊗%⊃"Hλ¬∀∀ShyH
⊃(I5 ¬H6∀∀E∀Hβ!!(
	X4⊂p*$	j∪λ→0Q⊂$¬⊂s1¬∀_
	(ZP3∃(~⊃(	eHs1
%∀⊂Sq∃(
(¬⊃"C"E→8s≡Y(
|j*#!%⊃⊃1IX0tSdλ1∩5λIh
	N,<⎇λλ)q⊗*!QHλ_¬
∀SqidH¬	04⊂h~H	j	H30Qλ∀
⊂s(E(_
∧X5P3
X5⊃(∧u⊂s1¬∃(⊂ShK*(
$¬#"C!%→→8mL<Y(¬xj*!QJ⊃⊃(i00tIt⊂st→∩4u∧¬⊃∩4jE#"H∧
⊂4
λ3Qλ¬I∩4u∧	R3
$¬#"C!%→→8mL<Y(¬xj*!QJ⊃⊃(i00tIt⊂U5	H4uλ¬	∩4u¬⊃"Hλ¬∪PQ*hαi)bH∀!b)λ∀)"k⊃i)bP&$ij
TTP∀CEεE∀→2qv0\2P∀3XTTFE
 ∧EFMACRO REPEAT (NUMBERFORM)
  `(DO ((TALLY ,NUMBER (1- TALLY)))
       ((ZEROP TALLY))
       ,FORM ) )

(declare (gc))
(DEFMACRO SETF* (SETFORM VALUEFORM)
  (LIST 'SETF SETFORM (NSUBLIS `((-*- . ,SETFORM	) VALUEFORM)) )

(declare (gc))
(@EFMACRO SOME (LIST PREDICATE . &opt:STEP-FUNCTION)
  (SETF* PREDICATE (ETAL -*-))
  (COND (&opt:STEP-FUNCTION (SETF* &opt:STEP-FUNCTION (EVAL -*-))))
  `(DO ((LISTAIL ,LIST (,(COND (&opt:STEP-FUNCTION
∩∩$∩@Qπ¬$@M←Ahu')∃ [
+9π)∪∨8R@B~(∩∩∩@@@@@Q(@O
	$R@$~∀∩∩$@A	∪M)β∪_RRR~(@@@@@@PQ9+→_A1∪')β%_RA≥%_R~∀@@@@@Qπ∨9λ@PP1!%	%πβ)
Qπβ$↓→∪')¬∪_BRQ%)U%≤A→%')β∪0RRR@$@R~∀4∀QIK
YCeJQOFR$~∀Q	∃
≠βπI≡A'	'(@!!%	%πβ)
↓→∪'($~∀@@!')(A!%⊃∪πβ)∀@Q-¬_@ZT4RR~∀A@Q≠¬!πβ≤FNQ→¬≠¬	αQ≠≠	$R~(∩@@@@@@Q
∨≥λ@ PY!%∃	∪πβQ
A≠5¬$RQ≥π∨9&A≠5¬$R$R@B~(∩@@@1→∪'(R@R~(~∀QI∃GYCe∀@QOF$R~∀Q⊃
+≤↓∂∨∨λ5≥%-∃%'
@!→∪'($~∀@@Qπ∨≥⊂@PQ∨H@Q≥+1_A→∪M(R@Q9+→_@!π	$A1∪'(R$RA→∪M(R~∀$@PQ∨H@Q≥+1_@Qπ⊃	$A→%'(RRQ≥+→0@Qπ	⊃	$A→%'(RR$~∀∩@@@@Q1(@P!%≠4@QπβH@Q→βM(A→∪M(RRR$~∀∩∩@Q%!1βπα@!→β'(↓→∪'($@QπβHA→∪'PRR~∀$∩@@QI!→βπ∧A→∪'PA%≠∃~R@RR~∀∩Q(@QA%∨∞@!)%β∪1$A!=∪≥)HA→β⊃$R~(∩∩@@!')"↓)%β∪1$@Q
	$A→%'(R~(∩∩∪!=∪≥)H@Qπ	HA)%β%→$R4∀∩∩∪1β	H@Qπ	HA!∨∪9)$RR~∀∩@@@@↓%!(@!%!→βNTER TRAILER)
		  (COND ((CDR LEADER) (SETQTRAILER POINTER
					    POINTER LEADER
					    LEADER (CDR LEADER) )
				      (GO RPT) ))
		  (RPLACD (CDR LIST) LEADER)
		  (RPLACD LIST POINTER)
		  (SETQ TRAILER (CAR LEADER))
		  (RPLACA LEADER (CAR LIST))
		  (RETURN (RPLACA LIST TRAILER)) ) ) ) )

(declare (gc))
(DEFUN NSUBLIS (A-LIST S-EXPR &aux SUBSTPAIR)
  (COND ((CONSP S-EXPR)
	   (COND ((CONSP (CAR S-EXPR	) (NSUBLIS A-LIST (CAR S-EXPR	))
		 ((SETQ SUBSTPAIR (ASSQ(CAR S-EXPR) A-LIST))
		    (RPLACA S-EXPR (CDR SUBSTPAIR)) ) )
	   (COND ((CONSP (CDR S-EXPR)) (NSUBLIS A-LIST (CDR S-EXPR)))
		 ((SETQ SUBSTPAIR (ASSQ (CDR S-EXPR) A-LIST))
		    (RPLACD S-EXPR (CDR SUBSTPAIR)) ) )
	   S-EXPR )
	((COND ((SETQ SUBSTPAIR (ASSQ S-EXPR A-LIST)) (CDR SUBSTPAIR))
	       (S-EXPR) )) ) )

(declare (gc))
(DEFMACRO HUNKQUANTP (LT-FORM)
   `(AND (HUNKP ,LT-FORM) (#.(ISA-OF:LT . QUANTIFIER) ,LT-FORM)) )

(declare (gc))
(DEFUN QNSUBLIS (A-LIST S-EXPR)
  (COND ((CONSP S-EXPR)
	  (COND ((CONSP (CAR S-EXPR)) (QNSUBLIS A-LIST (CAR S-EXPR))))
	  (COND ((OR (CONSP (CDR S-EXPR)) (HUNKQUANTP (CDR S-EXPR)))
		  (QNSUBLIS A-LIST (CDR S-EXPR)) )
		((ATOM (CDR S-EXPR))
		  (LET ((SUBSTPAIR (ASSQ  ⊃π⊃$A&[∃1!$R↓α[→∪M(RRR4∀∩∩@@@@@Qπ∨≥⊂@Q'+	')!β%$@Q%A→βπλ↓&[1A$@Qπ⊃$A'+	')!β%$RRR$@R@RR~∀∩A&[a!$@R4∀∩PQ!+≥↔#Uβ≥) ↓&[1A$R~∀$@@Q#9'+¬→%&Aα[1∪'(@!→([#M∨%)a!$A&51!$αI$4(J↓↓"Fu~V
2M→α¬6dJNQ↓DbQ6N≤zB∃α~j⊗bB⊂I$4(J↓αM6-BBI↓Hh(%!D
R>5¬→6⊗b¬⊃%αMl*bBIHh(%""↓"
J,
-↓
rNV
dJM↓5π+;K↔≡{∨;'V+⊃βSOβ∃β?2αM6⊗EαI9	JI↓%↓Hh(4)F#↔∂3∂∪∃↓#>→%$4RB∩⊗~l
∞J=∧~>:∩≤
JBV≤A↓"B∀*∩⊗b¬⊃αBV≤B⊗bB⊂αNRε≤Z⊗bB⊂H4)↓αBN⊗R
αBVND*bBIαB:NV∀b&MβαA!5)j↓9↓2¬∩⊗∩⊗EαI%%¬αVN"-BBI%Hh)↓↓E~⊗RE¬~Rε∞\*bBIαB:NV∀b&MβαA!5)j↓9↓2¬∩⊗∩⊗EαI%%¬~Rε∞\*bBIJH4)↓ε↓"∞>t!↓!2¬∩⊗∩⊗EαI↓"≤
I↓"¬*N!↓eαVN"-BBI↓e~Rε∞\*bBIJI$4(J↓"Q↓eαVN"-BBI%αI↓$4Ph)#∪.≠3πK*↓#∨
JH4)"$*~6ε≥∩=α↑∀JR∃α∀z∩d4R↓β↓"¬∩>≡9αb↓"6
α∞εIα→≥"2j
∩¬αBa$4PH%↓↓α↓↓↓↓αB∞>:"↓!"⊗
αa↓≡"I↓≥"$*JBJJI$4(HH%↓↓α↓↓↓!D
R>5¬A%β↓EαJ&:~↓2a%Hh($$J↓↓↓↓α↓!"εt!↓"∞|rNAαBH4($HH%↓↓α↓"6⊗m	↓"∞
⊃αa%α9"NB~⊗Mα$JNB2
IαB>≥αJ&:_h($$HH$%↓α↓↓α≡zαRε	∧∩J⊗εZα⊗JJ⎇⊃↓%%αH4($HI↓↓↓α↓↓↓↓¬A↓$4PH$%↓α↓↓↓↓BBε:⊃αB∞>:≥↓αa$hP$$$J↓↓↓↓D*E↓≥R↓"∞ε∩αa%%αH4($HI↓↓↓α↓↓↓↓ε↓"BJLr
↓1D~∩IαBI%↓$hP$$%α↓↓↓↓αBQβ↓EαJ&9
↓2a%J↓%↓$hP$%↓α↓α
>%I↓%↓J↓$4(hQ#∪↔≡cπK∃αC∨
%Hh)"∩,2V9α≥αε∞⊗~↓"9$hQ↓↓"$y↓!"$
22e∧q↓!EjαRε2eI%%$hQ↓↓↓α↓↓!"T*J>A¬"ε22JIαQ$hQ↓↓↓α↓↓"B∀J:
↓:y↓%↓J↓$4(hQ#∪↔≡cπK∃αC∨
%Hh)"∩,26ε∞∀yα:>∀jε2&T)6∞>tr⊗∞RM2∃↓"≤z:9$hQ↓↓βαB∞εN-	↓2∞|r84)α↓↓↓↓αA ⊃↓2αε:⊃J↓≤⊃$hQ↓↓↓α↓↓! zα>I%α8⎇$4R↓↓↓↓α↓! ∃↓Qα:>"I↓≤∃Hh)↓↓α↓↓↓""↓"↑JM"∃αQα⊃mβWw∪↔∂?>s'k↔"β∂?;v+∂S'6)i↓	αb∞>:ph($%D∩J⊗εZα:>Jl
2&j*j∞>:t*∞R&4)%↓%J↓%↓$hP4)#&+∂3π⊗)↓#∨~I$4)D"⊗~Vrα2&:%JB∃↓Db&NBLrBVR4zJ5$hQ↓↓"≤z:⊃↓BB:V2bα2&N∧J:BV$2>J5J↓"
J,
-↓
dJ:Rf∧)↓5βw+31βNsCWQε3?K5
⊃%%hP%!"
">5αdJNB&uαVR~⎇∩5%↓=~&6Bd*R⊗JjI4(JA":>"↓"∞>u~Aα2M~B&:¬*R~>∀i%$4PI↓↓↓E:J&R*αQ↓	ZβW;↔Gβ↔∂S.!β';π+Qβ≠␈∪5i↓∩α2&N∧J:BV$2>J4hP$%↓αB
J⊗Yα2&u"fB∃J↓%↓$hP%!"
">5↓D~εIαdJNB&uαVR~⎇∩5%%h(%↓α↓"∞>t!↓!"l*6E↓D:⊗Q↓D~εIαdJNB&uαVR~⎇∩5%↓<~εR⊗<zJe$hP$$%:BεRR∀J
VR*α∞>Vu!6N>∃!α∞ε$*≡>JJαN>J"I↓$4PH%↓↓α↓≡εR|j&∞B∀zB=↓Hh($%αA"⊗EαB≡⊗QαB∞εI∧b&NBLrBVR4zJ5%α:∞εR,:>JeJ↓≡~Vt~R&>rI4(HI↓↓↓α:→6R-∩5↓$hP$%↓BB⊗E↓D:⊗Q↓D:⊗R∞D
I↓"≤
Iα2M~B&:¬*R~>∀i%↓EJ↓≡∞ε$*≡>JJH4($J↓↓↓↓α↓≡Nfu"ε∞RL→66ε∀Z⊗I↓Hh($%α↓↓↓≡p	6R⊗∀i↓$4PH%↓!D*E↓"<*Q↓"tzJ6εdJj∃6≤z::⊗≥"&Z∃αB∞εI∧b&NBLrBVR4zJ5%J↓≡∞ε$*≡>JJH4($J↓↓↓↓α↓≡∞>tr⊗∞RM2∃↓¬h($%α↓↓↓≡≤z::B∀zB=↓Hh($%αBQ↓"=∩&R∃¬!↓	mπ+;K↔≡{∨;'V+⊃β'wβWQβ6{K5iα⊃α2&≥α&:B-"~>Jhh($$J↓↓↓"∃∩⊗ε-∧b&*RMα∃%↓JI↓%↓Hh(%""↓"∞>t!↓!"-	↓"≡-!↓!
rBR"∃lz→j2LrFVεu!↓9α$*R⊗JlJ:⊗IJ↓"∞ε∩α2&N∧J:BV$2>J5JH4($HI↓↓↓<~εR⊗<zJe↓Hh($%α↓↓↓↓α:∩⊗R-∩6&:-⊃↓%↓=
Vε:%αJ>BzH4($J↓!"⊗
↓"≡⊗"↓!
9E""∃6|1j2&u
Vε:"↓9α∩-"⊗J6Lr⊗I%αB∞εI∧b&NBLrBVR4zJ5%Hh($$J↓↓↓≡≤
R⊗≡⎇∩e↓$hP$%↓α↓↓↓↓<bε6
$	6∩⊗$*J6&t*I↓%α8!6⊗EαI$4PH%↓""↓"↑JM"∃αQα⊃mβWw∪↔∂?>s'k↔"β';C/!β≠?⊗ii↓	∧b&NBLrBVR4zJ44PH$%↓α↓"
J,
-α2LrRfB*I↓%%αI%↓%αH4(4RC∪↔∂fK∃↓F;
%$hQ"α⊗5*9α⊗t~>∩∃lb&:~⎇∩6V2
↓"~>∀i↓≠π/AαFYm~V
N$b&NQHh)↓↓αB2⊗QαA"⊗:≤z∩⊗⊃lb&*~⎇∩6V2
jM↓",r∞>∩*j2&:4zJ6Vd	6Mα4zJ5αtJ1%%Hh(%"rNV
dJMαF2jNF
≥"2&N"α⊗*∞|"⊗⊃6dJ:
>∀jV2¬m→%↓¬αH4(4RC∪↔∂fK∃↓F;
%$hP4)"$*~6ε≥∩=αN-"VBF,
:RMαBFVεu"2&N α:⊗↑l
RJ&BH4)↓αβ↓"∩z↓!"F,
:RεLa↓2F,
:R2M~Q↓"≤"IαF,
:RεLa%%$hP%!"u*21↓D~∩Iα
*ε:RJ1%$hP%↓"¬*N!↓D~>:MαB2Q6≤~>B∃αB∞εI¬
Vε:$
&1¬J↓"∞ε∩αFVεu"ε&1JIαFYm~V
N$b&NQHh(%↓[YαFYm~V
N$b&NQR↓!!s6K'π⊗c∃y↓r↓sGW∞sS'≠N+Iy%αq99$hP%↓"≤*R→↓DbQ6N≤zB∃↓D~εIα
*ε:RJ1%%αb:⊗↑l
RJ&BI↓$4PI"BV≤A↓"∞|rM↓"e!6N∞⎇α∃↓α≤
IαF,
:RεLa%%↓D~εIα
*ε:RJ1%%¬
Y6N,∩NR2M~Q$4PI"N⊗$1↓"2"jN∞>∧)↓"∞
⊃αFVrRε&bI%↓"≤
∩Iα
*ε:RJ1%%αI↓$4Ph)"∩,26ε∞∀yα>J$*I6B
"".⊗M→↓"B
"".⊗Lb&NQHh)↓βαBN>J"↓2Bε$B.⊗fdJNQ↓~:ε2BD
2⊗N≥↓%↓$hP4)#&+∂3π⊗)↓#∨~I$4(hQmα⊗t~>∩∃lb&:~⎇∩6V2
jMβW≡+MβSF)β[π⊗Kπ3*αFY6≥*
NRdJNQβ7∪↔↔3Jp4)"$*~V9∧*:∞>$)62&t2>J6,b¬6MαB~>JjαVE6dJNQ$hQ↓↓↓α↓↓↓mZαVE6dJNQβO→βWO.!βS=π∪↔∂?⊗!βGW∞sS'≠N+Iβ∪/β↔;∪.s∂'↔_h)↓↓D~εN⊗
↓"2&u"fB∃∧2>J5Hh)↓↓α↓↓!"
">6&≥αJ>Bzα→6R-∩5$4PI"RJrN~>∀i6J>d*2&:]→α~>∀i$4)α↓↓↓↓α↓↓"6
α
↓
:B2ε6∀"¬↓"∀b:-%h($%α↓"N⊗$1)↓"
∩≡V6,rQαJdr-%↓D*:∞>$)62&t2>J6,b¬6Mαi)5α-	62&≥!%%↓Hh(%↓α↓↓↓↓E∩>"⊗dJ:.M∧2>J5JH4(%E~⊗R→αBJ>2,b&:.~α~>JhI↓">∀"⊗I6∀z2⊗2Lr.Mα4zJ5%Hh(&~⎇∩5↓$hQ↓↓↓α↓"∞>trBJ>∧x4)↓α↓↓↓↓α↓"JBd
∞¬α4zJ5↓Dr>J6b&j∃l~>::,~R&Z*↓"∞ε⊂α~>JhI%$4PI"RJrN~>∀i6J>d*2&:]→α~>∀i$4)α↓↓↓↓α↓↓"6
α
↓
:B2ε6∀"¬↓"∀b:-%h($%α↓"N⊗$1)↓"
∩≡V6,rQαJdr-%↓D*:∞>$)62&t26J6,b¬6Mαi)5α-	62&≥!%%↓Hh(%↓α↓↓↓↓E∩>"⊗dJ:.M∧2>J5J↓$4(L2>J5αH4)↓α↓↓↓"
*ε:R¬∩>B<hQ↓↓↓α↓↓↓↓Db⊗Q↓BBF@,→jDdM:@λ¬	04⊂h→H
⊃JYPp
$Sdε ENCODE-QUANT)  BUTLAST FORM)))
	       NEWHβ)I∪0@Q∃_
∞≡$)62→hdm∀ZYD
m4¬∧
$¬∧d
:@∧4⎇)P
%∀∃4 ⊗S$ij)) !
	     (SETUPQUANTS QUANDLIST NEW@≠βQ%∪0R4∀∩@@@@Qπ¬$A#+¬≥)→∪M(R@RR~∧@@@@QM∪≠!→∃)%~4∀∪
∨I~R~∀@@@@!<α[)∃%~~∀∩Q%!1βπλA→∨%~@!ββ	$αα~>Jh¬∩Hh!∃¬≤-He"αE`∃Tl
J)∃B∧iz$jJ¬λTd≤xHRld→hd⎇∀ZYD
m4¬RRj
Z∩ld~:BJHQ!∀4⎇)PαHh$∧ααα¬↓αl-

 hR∧∧ααα∧∧αE∃	H∀≤"λiu∀j¬λT`(qq⊃%Y⊂3QIZS53λ∃4h
λ81∀@λitS ∀H*hVf∩ij∀TCE∧T&Ph⊂⊃SJ& fa⊃ P∀+⊂i&$iU df∀CE∧DP
&"j⊂
∀(%bVd$ijβE∧DDH∀&`eQVh j∩%blf∩ij⊂(⊂j$%bViFE∧BDDDPλ⊂∀'i⊃ i⊗h⊂j$%bViFE∧BDDDPλ⊂⊂⊂∀∪`h!`T⊂⊃SdSh&'b⊃FE∧DBDDDPλ⊂⊂⊂∀∀k⊗hjPidVjS)ja)UεE∧DBDDDPλ⊂⊂⊂⊂λ⊂⊂⊂⊂
!`i⊂∃αARLISTAIL)
							   (LT-λ-SCOPE FORM) ) ) ) ) ))
		      (ADJUST-λ-TERMSORT PKEYLIST FORM)
		      (SETF (CAR VARLISTAIL) PKEYLIST) ) )
	     (CDAR FORM) )
	(SETF* (LT-PATHKEYLISTS FORM) (ORDER-PATHKEYLISTS -*-))
	(λ-UNSUBST FORM NIL) )
     (T (BREAK "ENCODE-LINFORMULA-S - unrecognized formula type.")) ) )

