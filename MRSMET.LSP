
perm filename MRSMET.LSP[MRS,LSP] blob sn#612318 filedate 1981-09-30 generic text, type T, neo UTF8

;;;               -*-Mode:LISP; Package:MACSYMA -*-                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.  See MRG.                 ;;;
;;;            (c) Copyright 1981  Michael R. Genesereth                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

($assert '(MyToStash (indb $p) stash-indb))
($assert '(MyToUnStash (indb $p) unstash-indb))
($assert '(MyToLookup (indb $p) lookup-indb))
($assert '(MyToLookups (indb $p) lookups-indb))

(defun stash-indb (p) (stash (cadr p)))
(defun unstash-indb (p) (unstash (cadr p)))
(defun lookup-indb (p) (lookup (cadr p)))
(defun lookups-indb (p) (lookups (cadr p)))


($defunit myworld
	  (MyToLookupval (MyWorld $p) dl-lookupval)
	  (MyToLookupvals (MyWorld $p) dl-lookupvals))


($defunit subworld
	  (MyToStash (subworld $c $d) stash-subworld)
	  (MyToUnstash (subworld $p $c) unstash-subworld)
	  (MyToLookup (subworld $p $c) dl-lookup)
	  (MyToLookupval (subworld $p) dl-lookupval)
	  (MyToLookupvals (subworld $p) dl-lookupvals))

(defun stash-subworld (p) (ut-subworld (cadr p) (caddr p)))
(defun unstash-subworld (p) (ut-unsubworld (cadr p) (caddr p)))


($defunit contents
	  (MyToLookupval (contents $c) dl-lookupval)
	  (MyToLookupvals (contents $c) dl-lookupvals))

