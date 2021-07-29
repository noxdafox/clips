(clear)                   ; Optimized Rete Evaluator Issue

(defrule rule-1
   (factoid ?x ?y&:(and ?x ?y)) ; FactPNGetVar3
   =>)

(defrule rule-2
   (factoid ?x ?y)
   (test (and ?x ?y)) ; FactJNGetVar3
   =>)

(defrule rule-3
   (factoid $? ?x ?y&:(and ?x ?y) $?) ; FactPNGetVar1
   =>)

(defrule rule-4
   (factoid $? ?x ?y $?)
   (test (and ?x ?y)) ; FactJNGetVar1
   =>)
(defglobal ?*z* = FALSE)

(defrule rule-5
   (factoid ? ?)
   (test (and ?*z* ?*z*))
   =>)
(assert (factoid FALSE FALSE))
(agenda)
(clear)

(deftemplate factoid
   (slot s1)
   (slot s2))

(defrule rule-1
   (factoid (s1 ?x) (s2 ?y&:(and ?x ?y))) ; FactPNGetVar2
   =>)

(defrule rule-2
   (factoid (s1 ?x) (s2 ?y))
   (test (and ?x ?y)) ; FactJNGetVar2
   =>)
(assert (factoid (s1 FALSE) (s2 FALSE)))
(agenda)
(clear)

(defclass OBJOID1
   (is-a USER)
   (slot s1)
   (slot s2))

(defclass OBJOID2
   (is-a USER)
   (multislot ms1))

(defrule rule-1
   (object 
      (is-a OBJOID1)
      (s1 ?x)
      (s2 ?y&:(and ?x ?y))) ; ObjectGetVarPNFunction1
   =>)

(defrule rule-2
   (object 
      (is-a OBJOID1)
      (s1 ?x)
      (s2 ?y))
   (test (and ?x ?y)) ; ObjectGetVarJNFunction1
   =>)

(defrule rule-3
   (object 
      (is-a OBJOID2)
      (ms1 $? ?x ?y&:(and ?x ?y))) ; ObjectGetVarPNFunction2
   =>)
   
(defrule rule-4
   (object 
      (is-a OBJOID2)
      (ms1 $? ?x ?y))
   (test (and ?x ?y)) ; ObjectGetVarJNFunction2
   =>)
(make-instance o1 of OBJOID1 (s1 FALSE) (s2 FALSE))
(make-instance o2 of OBJOID2 (ms1 FALSE FALSE))
(agenda)
(clear)
