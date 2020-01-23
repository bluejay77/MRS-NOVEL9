;;;DICT1.LSP    0.3 18-Mar-83 0053 000   1 ML PUPFTP 18-Mar-83   
;;;	Just an exercise with relations between people - only interest
;;;	is that the arity is declared.
;;;
;;;perm filename DICT1.LSP[MRS,LSP] blob sn#702119 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;  This is a dictionary of people, relationships between people and 
;  occupations (primarily).  Associated with each relation is an arity.
;  The arity can have a numeric value from 1 - inf.  Or it can be called
;  ARBITRARY.

(let ((Theory 'dict1))
  ($Stash '(Relation TallerThan))
  ($Stash '(Arity TallerThan 2))

  ($Stash '(Relation Eats-with))
  ($Stash '(Arity Eats-with 2))

  ($Stash '(Relation Loves))
  ($Stash '(Arity Loves 2))

  ($Stash '(Relation Hates))
  ($Stash '(Arity Hates 2))

  ($Stash '(Relation Lives-with))
  ($Stash '(Arity Lives-with 2))

  ($Stash '(Relation Baker))
  ($Stash '(Arity Baker 1))

  ($Stash '(Relation Carpenter))
  ($Stash '(Arity Carpenter 1))

  ($Stash '(Relation Is-Married))
  ($Stash '(Arity Is-Married 1))

  ($Stash '(Function Brother-of))
  ($Stash '(Arity Brother-of 1))

  ($Stash '(Function Sister-of))
  ($Stash '(Arity Sister-of 1))
  
  ($Stash '(Function Parent-of))
  ($Stash '(Arity Parent-of 1))
  
  ($Stash '(Function Neighbor-of))
  ($Stash '(Arity Neighbor-of 1))
  
  ($Stash '(Function Friend-of))
  ($Stash '(Arity Friend-of 1))		      
  
  ($Stash '(Symbol Bill))
  ($Stash '(Symbol Joe))
  ($Stash '(Symbol Sally))
  ($Stash '(Symbol Linda))
  ($Stash '(Symbol George))
  ($Stash '(Symbol suzy))
  ($Stash '(Symbol Zelda))
  ($Stash '(Symbol Virginia))
  ($stash '(Symbol Alfred))
  ($Stash '(Symbol Richard))
)


;  This is a dictionary of the blocks world.

(let ((Theory 'dict2))
  ($Stash '(Relation OnTopOf))
  ($Stash '(Arity OnTopOf 2))

  ($Stash '(Relation InFrontOf))
  ($Stash '(Arity InFrontOf 2))

  ($Stash '(Relation Behind))
  ($Stash '(Arity Behind 2))

  ($Stash '(Relation InStack))
  ($Stash '(Arity InStack 3))

  ($Stash '(Relation Green))
  ($Stash '(Arity Green 1))

  ($Stash '(Relation Blue))
  ($Stash '(Arity Blue 1))

  ($Stash '(Relation Red))
  ($Stash '(Arity Red 1))

  ($Stash '(Relation Pyramid))
  ($Stash '(Arity Pyramid 1))

  ($stash '(Relation cube))
  ($Stash '(Arity Cube 1))

  ($Stash '(Relation Small))
  ($Stash '(Arity Small 1))

  ($Stash '(Relation Large))
  ($Stash '(Arity Large 1))

  ($Stash '(Function Block-supported-by))
  ($Stash '(Arity Block-supported-by 1))

  ($Stash '(Function Copy-of))
  ($Stash '(Arity Copy-of 1))
  
  ($Stash '(Function block-beneath))
  ($Stash '(Arity Block-Beneath 1))

  ($Stash '(Function Block-next-to))
  ($Stash '(Arity Block-next-to 1))

  ($Stash '(Function Neighbor-of))
  ($Stash '(Arity Neighbor-of 1))

  ($Stash '(Symbol Block1))
  ($Stash '(Symbol Block2))
  ($Stash '(Symbol Block3))
  ($Stash '(Symbol Block4))
  ($Stash '(Symbol Block5))
  ($Stash '(Symbol Block6))
)

