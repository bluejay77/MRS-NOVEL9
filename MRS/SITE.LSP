
perm filename SITE.LSP[MRS,LSP] blob sn#702136 filedate 1983-03-18 generic text, type T, neo UTF8

;;; It is reccommended that a directory be set up to contain all library
;;; files that MRS needs.  On Diablo, this is /hpp/mrsx.  The following
;;; form adds this directory to the load-search-path.
#+franz(setq mrs-lib-directory '/hpp/mrsx)

#+franz(eval `(sstatus load-search-path ,(append (status load-search-path)
					  (ncons mrs-lib-directory))))

;;; This should be set up to point to a file which contains two fixnums.
;;; The first number is the version number of the MRS, and the second
;;; number is the number of modifications that have been made.  Every time
;;; an MRS is made, the mod number will be incremented.  To bootstrap the
;;; system, the file should contain "3 1".
#+franz(setq mrs-version-file (concat mrs-lib-directory '/version))

;;; This should point to the file mrs.demo.
#+franz(setq mrs-demo-file (concat mrs-lib-directory '/d74.demo))

;;; This should be set up to point to describe.dat
#+franz(setq describe-dat-file (concat mrs-lib-directory '/describe.dat))

