;;; -*- Mode: Common-LISP -*-
;;;
;;; ------------------------------------------------------------
;;;
;;; Author: Dr Antti J Ylikoski 2015-05-02
;;;
;;; ------------------------------------------------------------
;;;
;;; The ASDF DEFSYSTEM definition for the Common LISP MRS.
;;;
;;; AJY 2014-07-29 -- 2015-05-02.
;;;
;;; Superseded a self written DEFSYSTEM with the standard ASDF.
;;; AJY 2015-05-02.
;;;
;;;
;;; Reports on the MRS system:
;;;
;;; There are 2 tech reports -- STAN-CS-85-1080 is titled "The
;;; Complete Guide to MRS" (Jun 1985).  But that may be much the same
;;; material as KSL-85408 by (the) Winston, Mitchell and Buchanan
;;; (1985).  Both should still be available from Stanford.
;;;






(defsystem 'MRS
    :name "MRS"
    :author "Michael R. Genesereth, Antti J Ylikoski ylikoskia@yahoo.com"
    :version "5.1"
    :maintainer "Antti J Ylikoski ylikoskia@yahoo.com"
    :license "(C) Michael R. Genesereth, modifs by AJY are freeware"
    :description "The Common LISP port of the MRS"
    :long-description "The Metalevel Reasoning System, the CL port"
    :components ; A simple linear list of source files
    '((:file "macros1") ; the macros package needed for the software
     ; (:file "compatibility") ; MACLISP -- Common LISP compat points
     ;; First the basic functionalities:
     ; (:file "common") ; property lists etc etc
     ; (:file "propre") ; activate, include, and pr- functions
     ; (:file "match")  ; the matcher
     ; (:file "plist")  ; the fundamental access functions for PLISTs
     ; (:file "mrs")    ; the main functionality of the MRS system
     ; (:file "top")    ; the MRS top level
     ;; The Backward Chaining facilities:
     ; (:file "bc")     ; backward chaining
     ; (:file "bckb")   ; backward chaining knowledge base
     ; (:file "bcsb")   ; backward chaining stuff base
     ;; The Forward Chaining facilities:
     ; (:file "fc")     ; forward chaning
     ; (:file "fckb")   ; forward chaining knowledge base
     ; (:file "fcsb")   ; forward chaining stuff base
     ; (:file "demons") ; demons for forward chaining
     ;; Miscellaneous machinery
     ; (:file "sets")   ; truep-setof in file
     ; (:file "arithm") ; arith funcs, such as truep for arithmetic
     ;; CNF and DNF facilities
     ; (:file "cnf")    ; Conjunctive and Disjunctive Normal Form
     ;; The venerable resolution principle machinery
     ; (:file "resolu") ; the resolution functions
     ;; MRS style knowledge base functions:
     ; (:file "kb")     ; the knowledge base stuff
     ;; The meta level facilities
     ; (:file "meta")   ; the meta level
     ; (:file "next")   ; scheduler for meta level
     ;; Miscellaneous
     ; (:file "timer")  ; the TIMER facility
     ;; The nontrivial user interface facilities
     ; (:file "interf") ; the user interface
     ;; Misc functionality
     ; (:file "mrsfix") ; fixes for various points
     ; (:file "ask")    ; query user on truth of a proposition
     ; (:file "output") ; the natural language interface
     ; (:file "demo")   ; the MRS demo package, will run DEMO.MRS
     ;; The MRS tutor:
     ; (:file "tutor")  ; the MRS tutor package
     ; (:file "syntax") ; The MRS tutor - checks syntax of stud input
     ; (:file "concep") ; text for TUTOR
     ; (:file "topics") ; topics for MRS tutor
     ; (:file "genera") ; generator for the MRS tutor
     ; (:file "exerci") ; Problem information for TUTOR
     ; (:file "test")   ; TEST the new MRS retrieve test files
     ;; (:file "dict1")  ; exercise with relations between people
     ;; (:file "descri") ; dir of places where (DESCRIBE...) occurs
     )
    )


(defun loadm ()
  (asdf:operate 'asdf:load-op 'mrs))

