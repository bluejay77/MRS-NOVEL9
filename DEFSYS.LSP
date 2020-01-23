;;; -*- Mode: Common-LISP -*-
;;;
;;; ------------------------------------------------------------
;;;
;;; Author: Dr Antti J Ylikoski 2019-08-15
;;;
;;; ------------------------------------------------------------
;;;
;;; The DEFSYSTEM definition for the Common LISP MRS.
;;;
;;; AJY 2014-07-29 -- 
;;;
;;; Reports on the MRS system:
;;;
;;; There are 2 tech reports -- STAN-CS-85-1080 is titled "The
;;; Complete Guide to MRS" (Jun 1985).  But that may be much the same
;;; material as KSL-85408 by (the) Winston, Mitchell and Buchanan
;;; (1985).  Both should still be available from Stanford.
;;;

;;; A simple DEFSYSTEM facility for the Common LISP MRS.



(defun defsystem (sysname
		  &key
		    :name nam
		    :author auth
		    :version ver
		    :maintainer maint
		    :license lic
		    :description desc
		    :long-description long-desc
		    :filelist filelist)
  (setf (symbol-plist sysname) NIL)
  (setf (get sysname :name) nam)
  (setf (get sysname :author) auth)
  (setf (get sysname :version) ver)
  (setf (get sysname :maintainer) maint)
  (setf (get sysname :license) lic)
  (setf (get sysname :description) desc)
  (setf (get sysname :long-sescription) long-desc)
  (setf (get sysname :defsystem)
	filelist))


(defun load-system (sysname)
  (let ((filelist (get sysname :defsystem)))
    (loop for filename in filelist
	 do
	 (load filename))))



(defsystem 'MRS
    :name "MRS"
    :author "Michael R. Genesereth, Antti J Ylikoski ylikoskia@yahoo.com"
    :version "5.1"
    :maintainer "Antti J Ylikoski ylikoskia@yahoo.com"
    :license "(C) Michael R. Genesereth, by the immaterial rights laws"
    :description "The Common LISP port of the MRS"
    :long-description "The Metalevel Reasoning System, the CL port"
    :filelist '(
      "macros.lisp" ; the macros package needed for the software
      "compatibility.lisp" ; MACLISP -- Common LISP compat points
      ;; First the basic functionalities:
      "common.lisp" ; property lists etc etc
      "propre.lisp" ; activate, include, and pr- functions
      "match.lisp"  ; the matcher
      "plist.lisp"  ; the fundamental access functions for PLISTs
      "mrs.lisp"    ; the main functionality of the MRS system
      "top.lisp"    ; the MRS top level
     ;; The Backward Chaining facilities:
      "bc.lisp"     ; backward chaining
      "bckb.lisp"   ; backward chaining knowledge base
      "bcsb.lisp"   ; backward chaining stuff base
     ;; The Forward Chaining facilities:
      "fc.lisp"     ; forward chaning
      "fckb.lisp"   ; forward chaining knowledge base
      "fcsb.lisp"   ; forward chaining stuff base
      "demons.lisp" ; demons for forward chaining
     ;; Miscellaneous machinery
      "sets.lisp"   ; truep-setof in file
      "arithm.lisp" ; arith funcs, such as truep for arithmetic
     ;; CNF and DNF facilities
      "cnf.lisp"    ; Conjunctive and Disjunctive Normal Form
     ;; The venerable resolution principle machinery
      "resolu.lisp" ; the resolution functions
     ;; MRS style knowledge base functions:
      "kb.lisp"     ; the knowledge base stuff
     ;; The meta level facilities
      "meta.lisp"   ; the meta level
      "next.lisp"   ; scheduler for meta level
     ;; Miscellaneous
      "timer.lisp"  ; the TIMER facility
     ;; The nontrivial user interface facilities
      "interf.lisp" ; the user interface
     ;; Misc functionality
      "mrsfix.lisp" ; fixes for various points
      "ask.lisp"    ; query user on truth of a proposition
      "output.lisp" ; the natural language interface
      "demo.lisp"   ; the MRS demo package, will run DEMO.MRS
     ;; The MRS tutor:
      "tutor.lisp"  ; the MRS tutor package
      "syntax.lisp" ; The MRS tutor - checks syntax of stud input
      "concep.lisp" ; text for TUTOR
      "topics.lisp" ; topics for MRS tutor
      "genera.lisp" ; generator for the MRS tutor
      "exerci.lisp" ; Problem information for TUTOR
      "test.lisp"   ; TEST the new MRS retrieve test files
      ;; "dict1.lisp"  ; exercise with relations between people
      ;; "descri.lisp" ; dir of places where (DESCRIBE...) occurs
     )
    )



(defun loadm ()
  (load-system 'MRS))



