;;;TEST.LSP    0.2 18-Mar-83 0058 000   1 ML PUPFTP 18-Mar-83   
;;;	TEST a new MRS - **** retrieve test files from score *****
;;;
;;;
;;;perm filename TEST.LSP[MRS,LSP] blob sn#702138 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;;; This function reads forms from the file whose name is in
;;; and prints errors on the terminal.  When done,
;;; the function prints the total number of errors and exits.

(defun test (in)
  (setq in (open in '(in ascii block)))
  (do ((test) (expect) (actual) (n 0))
      ((nil))
    (cond ((eq 'stop (setq test (read in 'stop))) (close in) (return n))
	  ((eq 'stop (setq expect (read in 'stop)))
	   (close in) (princ '|Early end of file.|) (terpri) (return n))
	    ((not (setq actual (errset (eval test))))
	     (setq n (1+ n))
	     (princ test) (terpri)
	     (princ '|Error.|) (terpri))
	    ((or (eq '* expect) (equal expect (setq actual (car actual)))))
	    (t (setq n (1+ n))
	       (terpri) (princ in) (princ '| file |) 
	       (terpri) (princ test)
	       (terpri) (princ actual) (princ '| not |) (princ expect)))))

#+maclisp
(defun $testmrs ()
  (+ (test '|mrs:bc.tst|)
     (test '|mrs:bcsb.tst|)
     (test '|mrs:resolution.tst|)
     (test '|mrs:fc.tst|)
     (test '|mrs:fckb.tst|)
     (test '|mrs:fcsb.tst|)
     (test '|mrs:d74.tst|)))

#+franz 
(defun $testmrs ()
  (+ (test '|bc.tst|)
     (test '|bcsb.tst|)
     (test '|resolution.tst|)
     (test '|fc.tst|)
     (test '|fckb.tst|)
     (test '|fcsb.tst|)
     (test '|d74.tst|)))

#+lispm 
(defun $testmrs ()
  (+ (test '|>mrsx>bc.tst|)
     (test '|>mrsx>bcsb.tst|)
     (test '|>mrsx>resolution.tst|)
     (test '|>mrsx>fc.tst|)
     (test '|>mrsx>fckb.tst|)
     (test '|>mrsx>fcsb.tst|)
     (test '|>mrsx>d74.tst|)))

