;;; -*- Mode: Common-LISP -*-
;;;
;;; A simple DEFSYSTEM facility for the Common LISP MRS.
;;;
;;; AJY 



(defun defsystem (sysname
		  &rest filelist)
  (setf (symbol-plist sysname) NIL)
  (setf (get sysname :defsystem) '())
  (setf (get sysname :defsystem)
	filelist))


(defun load-system (sysname)
  (let ((filelist (get sysname :defsystem)))
    (loop for filename in filelist
	 do
	 (load filename))))


(defsystem 'MRS
    "MACROS.LSP" ; the macros package needed for the software
    "COMPATIBILITY.LSP"
    "PLIST.LSP"  ; the fundamental access functions for PLISTs
    "COMMON.LSP" ; property lists etc etc
    "PROPRE.LSP" ; activate, include, and pr- functions  BUG, (SETF SETQ)
    ;"TOP.LSP"    ; the MRS top level BUG, commented out
    ;"SITE.LSP"   ; the site specific material
    "MRS.LSP"    ; the main functionality of the MRS system
    "SETS.LSP"   ; truep-setof in file
    "CNF.LSP"    ; conjunctive and disjunctive normal form
    ;"INTERF.LSP" ; the user interface
    ;"TIMER.LSP"  ; the TIMER facility
    ;"MATCH.LSP"  ; the matcher
    ;"KB.LSP"     ; the knowledge base stuff
    ;"BC.LSP"     ; backward chaining
    ;"BCKB.LSP"   ; backward chaining knowledge base
    ;"BCSB.LSP"   ; backward chaining stuff base
    ;"FC.LSP"     ; forward chaning
    ;"FCKB.LSP"   ; forward chaining knowledge base
    ;"FCSB.LSP"   ; forward chaining stuff base
    ;"DEMONS.LSP" ; demons for forward chaining
    ;"NEXT.LSP"   ; scheduler for meta level
    ;"META.LSP"   ; the meta level
    ;"RESOLU.LSP" ; the resolution functions
    ;"OUTPUT.LSP" ; the natural language interface
    ;"ARITHM.LSP" ; arith funcs, such as truep for arithmetic
    ;"ASK.LSP"    ; query user on truth of a proposition
    ;"MRSFIX.LSP" ; fixes for various points
    ;"DEMO.LSP"   ; the MRS demo package, will run DEMO.MRS
    ;"TUTOR.LSP"  ; the MRS tutor package
    ;"CONCEP.LSP" ; text for TUTOR
    ;"GENERA.LSP" ; generator for the MRS tutor
    ;"EXERCI.LSP" ; Problem information for TUTOR.LSP
    ;"SYNTAX.LSP" ; TUTOR - checks syntax of stud input, such as arity
    ;"TOPICS.LSP" ; topics for MRS tutor
    ;"TEST.LSP"   ; TEST the new MRS retrieve test files
    ;"DICT1.LSP"  ; exercise with relations between people
    ;"DESCRI.LSP" ; directory of places where (DESCRIBE...) occurs
  )


(defun loadm ()
  (load-system 'MRS))


