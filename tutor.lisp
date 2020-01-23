;;; -*- Mode: Common-LISP --
;;;
;;;TUTOR.LSP    1.7 18-Mar-83 0059 000   1 ML PUPFTP 18-Mar-83   
;;;	code for MRS tutor
;;;
;;;perm filename TUTOR.LSP[MRS,LSP] blob sn#702143 filedate 1983-03-18
;;;	generic text, type T, neo UTF8 
;;;
;**********************************************************
;*                                                        *
;*                  TUTOR.LSP                             *
;*      The main functions for the MRS tutor              *
;*        (c) Copyright 1982, Jan Clayton                 *
;*                                                        *
;**********************************************************
;;;
;;; ------------------------------------------------------------
;;;
;;; Dr Antti J Ylikoski 2015-05-11
;;;
;;; ------------------------------------------------------------
;;;



#|
(declare (special activetheories helpmessage exercisehelp theory
		  currenttopic supertopics nexttopic commentfile))
|#


(defun $Tutor ()
  ;  This is the top level of the MRS tutor.  It can be called with the
  ;  command $TUTOR from the MACLISP version of MRS.  First the introduction
  ;  to the tutor is call which gives an initial explanation of the commands.
  ;  Then the user is asked what he wants to do.  
  ;  A CRLF will cause the tutor to go to the next subject.
  ;  Review <section number> displays that particular section
  ;  <section number> makes that section the current section and the
  ;       session will be continued from there is the user hits CRLF.
  ;  List <section number>  gives a table of contents of all things below
  ;       that section.  The default is the whole table.
  ;  Quit - exits the program

  (Let ((theory 'tutor) 
	(oldactivetheories activetheories)
	(topic)
	(CurrentTopic 'tutorintro))

   ; We need to save all the assertions from the user's work perviously in the
   ; mrs session.  We also want to activate the tutorsetup which is the set
   ; of assertions about the tutor itself, which are made when the file is 
   ; loaded originally.

    (mapcar #'deactivate oldactivetheories)
    (activate 'tutorsetup)

    (TutorIntro)(terpri)
    (Princ '"Tutor> ")   
    (readline t)              ; This is supposed to get rid of the first crlf
    (Do ((Command (readcom NIL) (readcom NIL))
	 (SuperTopics) (FromHere) (LongCommand))
	((eq (Car (Setq LongCommand (explode (car Command)))) 'Q))
      (setq Topic
	    (cond ((eq (car command) NIL) (NextTopic))
		  ((eq (car command) '?) (Princ HelpMessage) NIL)
		  ((eq (car Longcommand) 'C) (MakeComments) NIL)
		  ((eq (car Longcommand) 'L)
		   (Cond ((null (cadr command)) (setq fromhere 'TutorIntro))
			 (T (Setq fromhere (GetTopicName (cadr command)))))
		   (ListTopics fromhere 3))

		  ((eq (car Longcommand) 'R)
		   (Review (cadr command)))
		  ((Setq fromHere (GetTopicName (car command)))
		   (StartAt FromHere))
		  
		  (T (princ '"    Unknown command, please try again.  Type ? for help.") NIL)))
      (cond (topic 
	     (Princ (getVal `(TopicNum ,topic)))
	     (princ '"  ")
	     (Princ (get topic 'TableName))
	     (terpri)
	     (eval (list topic))))
      (terpri)
      (princ '"Tutor> "))

    (deactivate 'tutorsetup)
    (mapcar 'activate oldactivetheories)
    'Bye))


(defun GetTopicName (num)
  ;  This little function knows how to get the TopicName given the topic
  ;  number.  It probably should be made into a macro when the thing gets 
  ;  compiled.
  (cdar (lookup `(topicNum $X ,(datum NUM)))))


(defun readcom (FLG)
  ;  This is a slightly souped up version of the original READCOM.  If I
  ;  know that the response should be a list structure then I pass things
  ;  ComCheck which makes sure that parens match, so that READLIST does not
  ;  blow the tutor out of the water.  It is also possible to have READCOM
  ;  always check for a possible paren problem, but it will waste alot of time.

  (Let ((newval (delete #\| (explodec (list (readline T))))))
    (Cond (FLG (readlist (comcheck newval)))
	  (T (readlist Newval)))))

(defun Comcheck (ComList)
   (Do ((CharL ComList (cdr CHarL))
	(Finished) (FixedCom) (NumLevels 0))
       (Finished (Reverse FixedCom))

     (Cond ((Eq (car CharL) '|(| ) (Setq numlevels (1+ NumLevels)))
	   ((Eq (car CharL) '|)| ) (Setq numlevels (1- NumLevels))))

     (Cond ((> Numlevels -1) (Setq FixedCom (Cons (car CharL) FixedCom)))
           (T (Princ '"    Dropping extra characters in answer. ") (Terpri)
	      (Setq Finished 'T)))

     (Cond ((null (cdr CharL)) (Setq Finished 'T)
	   		       (Cond ((> NumLevels 0)
				      (Princ '"    Adding missing parentheses.")
				      (terpri)
				      (Do ((I 1 (1+ I)))
					  ((> I NumLevels))
					(Setq FixedCom (Cons '|)| 
							     FixedCom)))))))))



(defun ListTopics (Topic NumSpaces)
  ;  Takes a topic and a number of spaces to go in front of the topic.
  ;  This is a recursive function that prints out a tree which is the 
  ;  table of contents of the tutor.
  ;    The current topic is marked with ==>, and then rest are just nicely
  ;  indented with spaces.

  (Let ((spaces Numspaces))
    (Cond ((Eq topic currentTopic) (Princ '"==>")
				   (setq spaces (- numspaces 3))))
    (Do ((i 1 (+ i 1)))
	((> i spaces))
      (Princ '" "))
    (princ (LookupVal `(TopicNum ,topic)))
    (princ '"  ")
    (princ (GET topic 'TableName))
    (terpri)

    ;  Now make sure that we do the same thing for the subtopics.
    (Do ((subtopic (GET topic 'Subtopics) (cdr subtopic)))
	((null subtopic))
            (listtopics (car subtopic) (+ numspaces 3)))))


(defun MakeComments ()
  (Princ '"
    Type out your comment(s) or question(s), and finish with ESC<cr>.")
  (terpri)
  (Let ((Cfile (OPEN commentfile 'APPEND)))
    (terpri cfile) (terpri cfile)
    (Princ (status userid) cfile)     (terpri cfile)
    (princ (status date) cfile)   (Princ '"    " cfile)
    (princ (status daytime) cfile) (terpri cfile)

    ;  This is the part that needs to be changed

    (Do ((Input (tyi) (tyi)))
	((= input 27.) (close cfile))
      (tyo input cfile))))



(defun review (place)
  ;  This function returns the name of the function that the user wants
  ;  to review.  We need to add a few things here to guard against a
  ;  bad response by the user.  Now we assume that he knows what he is doing.

  (cond ((Null place) 
	 (princ '"Topic to review: ")
	 (setq place (read T))))
  (cdar (Lookup `(TopicNum $X ,place))))



(defun StartAt (topic)
  ;  This changes the current topic and the session will continue from that
  ;  point.  It also makes sure that the supertopic list is maintained 
  ;  properly, so that the program can jump up a few levels.

  (setq currenttopic topic)
  (setq supertopics nil)
  (Do ((super (GETval `(supertopic ,currenttopic))
	      (GetVal `(supertopic ,super))))
      ((Null super))
    (Setq SuperTopics (Append supertopics (list super))))
  topic)


(defun NextTopic ()
  ;  This returns the name of the topic that should come next.  If there
  ;  are any sub topics then return the first one.  Otherwise return the
  ;  next topic of the current topic.  If there is no next topic then 
  ;  go up one level to get a next topic at the upper level.

  (let (sublist nexttopic)
    (cond ((setq sublist (get currenttopic 'subtopics))
	   #+maclisp (push currenttopic supertopics)
	   #+franz (setq supertopics (cons currenttopic supertopics))
	   (setq currenttopic (car sublist)))
	  ((do ((ThisTopic CurrentTopic #+maclisp (Pop SuperTopics)
			                #+franz (prog1 (car supertopics)
						  (setq supertopics
							(cdr supertopics)))
					))
	      ((Null ThisTopic))
	    (Cond ((Setq currentTopic (Get ThisTopic 'NextTopic))
		   (return CurrentTopic)))))
	  (T (Princ '"No more topics") NIL))))



(defun PrtExpr (Expr)
   (terpri)					
    (princ '"USER: ")
    (princ expr)
    (terpri)
    (Princ '"MRS:  ")
    (princ (eval (readlist (explodec expr))))
    (terpri))


;  Now special procedures for the exercises.

(defun PresentExercises (EList Directions Prompt ExType GiveDirections)
  (Let ((theory 'exercises))

    (Cond (GiveDirections
	   (Princ Directions) (Terpri)))

    (do ((NExList EList (cdr NExList))
	 (Quitflg))
	((or Quitflg
	     (Null NExList)) 'Done)
      (setq quitflg (presentproblem (car NExList) directions prompt Extype)))

    ))


(defun PresentProblem (problemname directions prompt ExType)
  ;  The first thing to be done is to show the problem to the person, and
  ;  then ask for an answer

  (AskQuestion ProblemName Extype)
  (princ prompt)
  (Do ((resp (readcom 'T) (readcom 'T))
       longcom)
      ((eq (car (setq LongCom (explode (car resp)))) 'Q) 'T)
    
    (Cond ((null (car resp)) (return nil))

          ((eq (car resp) '?) 
	   (princ directions) (terpri)
	   (Princ (car ExerciseHelp))
	   (cond ((cdr ExerciseHelp)
		  (mapc (function (lambda (text)
					  (terpri)
					  (Princ text)))
			(cdr exerciseHelp)))))
	
	  ((eq (car longcom) 'C) (MakeComments))

	  ((eq (car longcom) 'A) 
	   (cond ((eq (cadr longcom) 'N)
		  (GiveAnswer ProblemName))
				     
		 ((eq (cadr Longcom) 'S)	       
		  (AskQuestion ProblemName Extype))		  
		 
		 (T (Princ '"    Ambiguous command.") (terpri))))

	  ((eq (car longcom) 'L)
	   (cond ((cadr resp)
		  (ListTopics (GetTopicName (cadr resp)) 3))
		 (T (listTopics 'TutorIntro 3))))
	  
	  ((eq (car longcom) 'R)
	   (Eval (list (Review (cadr resp)))))
     
	  ((eq (car longcom) 'W)
	   (GiveExplanation ProblemName))
	  
	  ((CheckAnswer (car resp) ProblemName Extype)
	   (return NIL))
	  
	  (T (Princ '"    Try again or type ? for options.")))
    (terpri)
    (Princ '"Answer: " )))

(Defun AskQuestion (ProblemName ExType)
  (terpri)
  (Cond ((eq Extype 'Gen)
	 (princ (generator (eval problemname))))
	(T (princ (eval problemname))))
  (terpri))

	   
(Defun GiveExplanation (ProblemName)
  (princ (Get ProblemName 'Explanation)))

(Defun GiveAnswer (problemName)
  (princ '"    The answer expected was: ")
  (princ (Get ProblemName 'Answer))
  (Terpri))




(Defun CheckAnswer (StudentAnswer ProblemName ExType)
  (Let ((CorrectAnswer (Get problemName 'Answer)))
    (cond ((Eq Extype 'Int)
	   (Cond ((SpecialCheck Studentanswer ProblemName CorrectAnswer)
		  (Princ (Eval StudentAnswer))
		  (Terpri) 
		  (MarkConcepts ProblemName 'OKAY)
		  T)
		 (T (ReportMistake ProblemName))))

	  ((Equal StudentAnswer CorrectAnswer)
	   (MarkConcepts problemName 'OKAY)
	   ($Assert `(OKAY , ProblemName NIL)))
	  (T (princ '"    Incorrect answer.") (terpri)
	     (ReportMistake ProblemName))
	  )))



(defun ReportMistake (ProblemName)
    (MarkConcepts ProblemName 'MisConception)
    (AddToModel `(MissedProblem ,ProblemName))
    nil)


(Defun SpecialCheck (StudentAnswer ProblemName CorrectAnswer)
 (cond ((Atom (cadr StudentAnswer))
	(Princ '"    Your answer is not in the correct format.
    ``(<MRS-command> '<proposition>)''") (Terpri))
       
       (T
	(Let ((Clist (get ProblemName 'PossibleCommands))
	      (Command (car StudentAnswer))
	      (Qu (caadr StudentAnswer))
	      (Prop (cadadr StudentAnswer))
	      (New))

	  (Cond ((not (member command clist))
		 (Princ '"    Incorrect MRS command for this problem")
		 (Terpri) NIL)
		((not (eq qu 'QUOTE))
		 (Princ '"    QUOTE missing before proposition")
		 (Terpri) NIL)
		((not (CheckSyntax prop)) NIL) 
		(T (setq new (semant Prop))
		   (cond ((samep new CorrectAnswer))
			 (T (Princ '"    Your answer does not agree with mine.") 
			    (terpri) NIL))))))))
       

(defun CorrectMisconceptions (directions prompt probtype)
  ; get the misconceptions
  ; get the OKAYS
  ; do the comparisons and determine what to test

  ; for each concept....test....present remedial material if needed and retest
  (activate 'studentmodel)
  (let ((BadList (SortConcepts ($Trueps `(Misconception $a $b))))
	(GoodList (SortConcepts ($trueps `(Okay $a $b))))
	)

    ;(Setq BadList (ConceptElimination BadList GoodList))

    (Do ((CL BadList (cdr CL))
	 (concept) (problemlist) (WrongAgain))
	((null CL))

      (setq concept (caar CL))
      (setq problemlist (cdar cl))

      (MoveMarkedConcept (car cl))

      ;Let's just retest the problems that were missed for now.  At this point
      ;they will all be generator problems, so we don't have to worry about
      ;anything weird.

      (PresentExercises problemlist directions prompt Probtype NIL)

      (cond ((setq wrongagain ($Getvals `(Misconception ,concept)))
	     (PresentRemedialMaterial concept)
	     (moveMarkedConcept (car CL))
	     (PresentExercises wrongagain directions prompt Probtype NIL)
	     
	     ;There may still be somethings left, for more mistakes, and they
	     ;should be handled in some way.  Maybe just a note to the student
	     ;that says, I can't seem to help you try a humam might be good.

	     (Cond ((MoveMarkedConcept 
		     (cons Concept ($Getvals `(Misconception ,concept))))
		    (Princ '"
    I can't seem to help you with your problem with this concept.  Please ask
    someone to help you.") (terpri)))
	     ))))
  (deactivate 'studentmodel))

	     
(defun MoveMarkedConcept (Clist)
  (let ((Concept (car Clist))
	(Probs (cdr Clist)))
    (Do ((p probs (cdr P))
	 )
	((null p))
      (moveAssertion `(misconception ,concept ,(car p))
		     'StudentModel 'OldStudentFacts))
    PROBS))


(setq helpmessage '"
    Commands available:
      Comment               - To make a comment about the tutor
      List [Topic number]   - For a list of topics and section numbers
      Review <topic number> - To briefly review that topic alone
      <Topic Number>        - To skip to that topic and continue from there
      <carriage return>     - To advance to the next appropriate topic
      Quit                  - To exit from the tutor
      ?                     - For this message
")

;;; This is too long for franz
(setq exerciseHelp '("
    Options available are:
      <answer>              - Give an answer to the problem
      ASkagain              - To see the problem again
      ANswer                - To see the answer to the problem
      Why?                  - For an explanation of the problem"
"      List [Topic number]   - To get a one level list of topics (and numbers)
      Review <topic number> - To review a topic
      <carriage return>     - Proceed to the next problem
      Quit                  - To stop the exercises and go on to next topic"
"
      CTRL-S                - To stop textual output
      CTRL-Q                - To resume textual output"))

;; ************************** Student Model Routines ************************

(defun AddToModel (fact)
  (Let ((Theory 'StudentModel))
    ($assert fact)))


(defun MarkConcepts (problemname type)
  (let ((Theory 'studentModel)
	(ConceptList ($Getvals `(Concept ,Problemname))))
    (mapc '(Lambda (Concept)
	    ($Assert (list type concept ProblemName))) ConceptList)))


(defun MoveAssertion (Fact Model1 Model2)
  (Let ((theory model1))
    ($Unassert Fact))
  (Let ((theory Model2))
    ($Assert Fact)))


(defun SortConcepts (CList)
  (Do ((Cl Clist (cdr CL))
       (NCList))
      ((Null CL) NCList)
    (Let ((A (subvar '$A (car CL)))
	  (B (subvar '$B (car CL))))

      (Do ((N NCList (cdr N)))
	  ((null N) (setq NCLIST (nconc NCList (list (List A B)))))
	(Cond ((Eq A (caar N))
	       (return (nconc (car n) (list B)) )))))))



(defun PresentRemedialMaterial (concept)
  ;  This should probably become more elaborate especially when it
  ;  comes to generating good examples that have the proper context
  ;  to tell the student about his mistakes.

  (Princ (get concept 'RemedialMaterial))
  (Terpri))


(defun ProblemSelection (Badlist Goodlist)
  ; We need to map down all the concepts in BADLIST and figure out
  ; what problems we should test the student with.  There are a number
  ; of possible petagogical strategies to take.  I have just chosen some
  ; that sound reasonable to me.

  ; If the student has always answered questions that test a given concept
  ; incorrectly, then we should definitely test for that concept fully.  This
  ; will probably consist of retesting the specific problems that we missed
  ; as well as adding a few harder or more complicated problems.

  ; If the student has answered a particular problem both correctly and
  ; incorrectly then just try harder ones to make sure that he does 
  ; finally understand his error.

  ; If the student answers problems that test a concept correctly and 
  ; incorrectly, then find out which set is larger.  If he usually gets
  ; it right, it just might be the case that you cannont test a concept
  ; that he gets wrong without testing the one that he understands.  
  ; We want to avoid testing the student on concepts that he understands.

  (Do ((BL Badlist (cdr BL))
       (BProbs) (GProbs) (Concept) (NewBL) (maxnumbprobs 2))
      ((null BL) NewBL)

    (Setq concept (caar BL))
    (setq BProbs (cdar BL))
    
    (setq Gprobs
	  (Do ((GL GoodList (cdr GL)))
	      ((null GL) NIL)

	    (Cond ((eq (caar GL) Concept)
		   (Return (cdar GL))))))

    (Cond ; He always missed the problems that tested that concept
          ((null GProbs)
	   (Setq NewBL (nconc NewBL 
		  (list (append (car BL)
				(GetHarderProbs 
				 Concept
				 (max (map 
				       '(lambda (x) ($Getval 
						     (list 'Difficulty X))) 
					 BProbs))
				 (- maxnumbprobs (length BPROBS))
				 ))))))

	  ((< (length GProbs)(length BProbs))
	   (Setq NewBL (nconc NewBL 
		  (list (append (list (caar BL))
				(GetHarderProbs 
				 Concept
				 (max (map 
				       '(lambda (x) ($Getval 
						     (list 'Difficulty X))) 
					 BProbs)) 
				 (- maxnumbprobs (length BPROBS))
				 ))))))

	  ((equal GPROBS BPROBS)
	   (Setq NewBL (nconc NewBL 
		  (list (append (list (Caar BL))
				(GetHarderProbs 
				 Concept
				 (max (map 
				       '(lambda (x) ($Getval 
						     (list 'Difficulty X)))
				       BProbs)) 
				 (- maxnumbprobs (length BPROBS))
				 )))))))))


(defun GetHarderProbs (Concept ProbDifficulty NumProbsWanted)
  (let ((AllProbs (Get Concept 'Problist)))
    
    (DO ((ap AllProbs (cdr AP))
	 (MoreDif) (HowMany 0))
	((or (null AP) (>= HowMany NumProbsWanted)) MoreDif)
      
      (cond ((> ($getval `(Difficulty ,(car AP))) ProbDifficulty)
	     (setq MoreDif (nconc MoreDif (car AP)))
	     (setq HowMany (1+ Howmany)))))))

