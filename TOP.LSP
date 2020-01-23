;;;TOP.LSP    0.3 18-Mar-83 0058 000   1 ML PUPFTP 18-Mar-83   
;;;	For Common LISP.  AJY 2015-03-14
;;;
;;;perm filename TOP.LSP[MRS,LSP] blob sn#702141 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;; mrs-version-file should be set up to point to a file which contains two
;;; fixnums. The first number is the version number of the MRS, and the second
;;; number is the number of modifications that have been made.  Every time
;;; an MRS is made, the mod number will be incremented.  To bootstrap the
;;; system, the file should contain 4 1".


(defvar mrsversion '(4 1))

(defvar mrs-version-file "MRS.VERSION")

(defun mrs-top-level ()
       (cond ((symbol-plist 'user-top-level)
	      (prog NIL
		 ;; (home rcfile)
		 (princ "MRS Version ")
		 (princ mrsversion)
		 (princ " in ")
		 ;;(princ (status version)) (terpri) (terpri)
		 (setf rcfile (parse-command-line))
		 ;(setf home (getenv '|HOME|))
		 (cond (rcfile (or (and (probef rcfile)
					(load rcfile))
				   (and (probef (concat home "/" rcfile))
					(load (concat home "/" rcfile)))
				   (and (probef ".mrsrc")
					(load ".mrsrc"))
				   (and (probef (concat home "/" ".mrsrc"))
					(load (concat home "/" ".mrsrc"))))))
		 (setq user-top-level nil)
		 (setplist 'user-top-level nil)
		 (remob 'mrs-top-level)))))


(defun parse-command-line ()
  (prog NIL
     (rcfile args arg file)
     (setf progrm (stripdirectory (argv 0)))
     (setf rcfile (concat "." progrm "rc"))
     ;; (setf rcfile (concat ".mrsrc")) ;always look for .mrsrc
     (setf args (1- (argv -1))
	   arg 0)
     loop
     (cond ((= arg args) (return rcfile))
	   (t (setq arg (1+ arg))))
     (cond ((eq (argv arg) '-f)
	    (setq rcfile nil))
	   ((eq (argv arg) '-r)
	    (load (argv (setq arg (1+ arg)))))
	   (t (princ (argv arg))
	      (princ " unknown switch, ignored.")
	      (terpri)))
     (go loop)))


(defun stripdirectory (file)
  (let ((temp (explodec file)))
    (do ((proc (memq '/ temp) (memq '/ temp)))
	((null proc) (implode temp))
      (setq temp (cdr proc)))))


(defun read-version ()
  (let* ((file (infile mrs-version-file))
	 (ver (read file)) (mod (read file)))
    (close file)
    (setq file (outfile mrs-version-file))
    (princ ver file)
    (princ " " file)
    (princ (1+ mod) file)
    (terpri file)
    (close file)
    (setq ver (implode (nconc (explodec ver) (ncons '|.|) (explodec mod))))
    (setq mrsversion ver)))


(defun read-version ()
       (let* ((file (open mrs-version-file 'in))
	      (ver (read file)) (mod (read file)))
	 (close file)
	 (setq file (open mrs-version-file 'out))
	 (princ ver file)
	 (princ " " file)
	 (princ (1+ mod) file)
	 (terpri file)
	 (close file)
	 (setq ver (implode (nconc (explodec ver) (ncons '|.|) (explodec mod))))
	 (setq mrsversion ver)))


(setplist 'user-top-level '(t))

(setf user-top-level 'mrs-top-level)

;;; (sstatus feature mrs)

;(read-version)

