
perm filename MRSSUB.LSP[MRS,LSP] blob sn#612039 filedate 1981-09-30 generic text, type T, neo UTF8


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

