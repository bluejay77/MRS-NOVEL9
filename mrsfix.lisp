;;; -*- Mode: Common-LISP -*-
;;;
;;;MRSFIX.LSP    0.3 18-Mar-83 0056 000   1 ML PUPFTP 18-Mar-83   
;;;	FIXES **** are these for new things or old things ****
;;;
;;;perm filename MRSFIX.LSP[MRS,LSP] blob sn#702129 filedate
;;;	1983-03-18 generic text, type T, neo UTF8 
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-10, on Mother's Day
;;;
;;; ------------------------------------------------------------



;;; This has to be done with care.
;;;

#|

($stash '(tostash (prop ^better $x $y) 'stash-better))
($stash '(tostash (prop ^if $p (prop ^better $x $y)) 'stash-better))
($stash '(tolookup (prop ^objconst $x) 'tl-lookup))
($stash '(tolookup (prop ^funconst $x) 'tl-lookup))
($stash '(tolookup (prop ^relconst $x) 'tl-lookup))
($stash '(tolookup (prop ^variable $x) 'tl-lookup))
($stash '(tolookup (prop ^rel $p $f) 'perceive-rel))
($stash '(tolookup (prop ^arg $p $x) 'perceive-arg))
($stash '(tolookup (prop ^val $p $y) 'perceive-val))
($stash '(tolookup (prop ^argn $n $p $y) 'perceive-argn))
($stash '(toperceive (prop ^rel $p $f) 'perceive-rel))
($stash '(toperceive (prop ^arg $p $x) 'perceive-arg))
($stash '(toperceive (prop ^val $p $y) 'perceive-val))
($stash '(toperceive (prop ^argn $n $p $y) 'perceive-argn))
($stash '(tolookup (prop ^intheory $p $t) 'perceive-intheory))
($stash '(toperceive (prop ^intheory $p $t) 'perceive-intheory))
($stash '(tostash (prop ^indb $p) 'achieve-indb))
($stash '(toachieve (prop ^indb $p) 'achieve-indb))
($stash '(tolookup (prop ^indb $p) 'perceive-indb))
($stash '(toperceive (prop ^indb $p) 'perceive-indb))
($stash '(tostash (prop ^includes $c $d) 'ach'ieve-includes))
($stash '(tounstash (prop ^includes $c $d) 'unstash-includes))
($stash '(toachieve (prop ^includes $c $d) 'achieve-includes))
($stash '(tolookup (prop ^includes $c $d) 'dl-lookup))
($stash '(toperceive (prop ^includes $c $d) 'dl-lookup))
($stash '(tolookupval (prop ^includes $t) 'dl-lookupval))
($stash '(tolookup (prop ^number $x) 'perceive-symbol))
($stash '(tolookup (prop ^symbol $x) 'perceive-symbol))
($stash '(toperceive (prop ^symbol $x) 'perceive-symbol))
($stash '(tostash (prop ^value $x $y) 'achieve-value))
($stash '(toachieve (prop ^value $x $y) 'achieve-value))
($stash '(tolookup (prop ^value $x $y) 'perceive-value))
($stash '(toperceive (prop ^value $x $y) 'perceive-value))
($stash '(tostash (prop ^property $x $y $z) 'achieve-property))
($stash '(toachieve (prop ^property $x $y $z) 'achieve-property))
($stash '(tolookup (prop ^property $x $y $z) 'perceive-property))
($stash '(toperceive (prop ^property $x $y $z) 'perceive-property))
($stash '(tolookup (prop ^islist $x) 'perceive-islist))
($stash '(toperceive (prop ^islist $x) 'perceive-islist))
($stash '(tolookup (prop ^length $x $y) 'perceive-length))
($stash '(toperceive (prop ^length $x $y) 'perceive-length))
($stash '(tolookup (prop ^member $x $y) 'perceive-member))
($stash '(toperceive (prop ^member $x $y) 'perceive-member))
($stash '(totruep (prop ^= $x $y) 'truep-=))
($stash '(totruep (prop ^not $p) 'truep-not))
($stash '(toassert (prop ^and *p) 'assert-and))
($stash '(tounassert (prop ^and *p) 'unassert-and))
($stash '(totruep (prop ^and *p) 'truep-and))
($stash '(totruep (prop ^or *p) 'truep-or))
($stash '(tostash (prop ^lisp $x $s) 'pl-stash))
($stash '(togetval (prop ^+ $x $y) 'getval-arith))
($stash '(togetval (prop ^- $x) 'getval-arith))
($stash '(togetval (prop ^- $x $y) 'getval-arith))
($stash '(togetval (prop ^* $x $y) 'getval-arith))
($stash '(togetval (prop ^/ $x $y) 'getval-arith))
($stash '(totruep (prop ^> $x $y) 'truep-arith))
($stash '(totruep (prop ^< $x $y) 'truep-arith))

|#

