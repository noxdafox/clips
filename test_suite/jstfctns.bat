(set-rule-justifications 3)
(set-rule-justifications none 3)
(set-rule-justifications off)
(get-rule-justifications 3)
(justification-names)
(justification-texts)
(justification-names 2 2)
(justification-texts 2 2)
(show-justifications)
(show-justifications 2)
(show-justifications 2.1)
(show-justifications [a])
(show-justifications 2 2)
(justification-names 2)
(justification-texts 2)
(justification-names 2.1)
(justification-texts 2.2)
(justification-names [a])
(justification-texts [a])

(deftemplate output
   (slot value))
   
(deftemplate person
   (slot first-name)
   (slot last-name))
   
(deffacts start
   (person (first-name Gary) (last-name Riley)))
   
(defrule rule-1
   (declare (justification "Because " ?name " said so"))
   (person (first-name ?name))
   =>
   (assert (output (value 1))))
   
(defrule rule-2
   (person (first-name ?name))
   =>
   (assert (output (value 2))))
(set-rule-justifications none) ;; Fact None
(get-rule-justifications)
(reset)
(run)
(justification-names 2)
(justification-texts 2)
(justification-names 3)
(justification-texts 3)
(show-justifications 2)
(show-justifications 3)
(set-rule-justifications text) ;; Fact Text
(get-rule-justifications)
(reset)
(run)
(justification-names 2)
(justification-texts 2)
(justification-names 3)
(justification-texts 3)
(show-justifications 2)
(show-justifications 3)
(set-rule-justifications name) ;; Fact Name
(get-rule-justifications)
(reset)
(run)
(duplicate 2 (value 3))
(duplicate 3 (value 4))
(modify 5 (value 6))
(justification-names 2)
(justification-texts 2)
(justification-names 3)
(justification-texts 3)
(show-justifications 2)
(show-justifications 3)
(justification-names 4)
(justification-texts 4)
(justification-names 6)
(justification-texts 6)
(show-justifications 4)
(show-justifications 6)
(clear)
(set-rule-justifications none) ;; Instance None
(get-rule-justifications)

(defclass OUTPUT
   (is-a USER)
   (slot value))
   
(defclass PERSON
   (is-a USER)
   (slot first-name)
   (slot last-name))
   
(definstances start
   (of PERSON (first-name Gary) (last-name Riley)))
   
(defrule rule-1
   (declare (justification "Because " ?name " said so"))
   (object (is-a PERSON) (first-name ?name))
   =>
   (make-instance [j1] of OUTPUT (value 1))))
   
(defrule rule-2
   (object (is-a PERSON) (first-name ?name))
   =>
   (make-instance [j2] of OUTPUT (value 2))))
(reset)
(run)
(justification-names [j1])
(justification-texts [j1])
(justification-names [j2])
(justification-texts [j2])
(show-justifications [j1])
(show-justifications [j2])
(set-rule-justifications text) ;; Instance Text
(get-rule-justifications)
(reset)
(run)
(justification-names [j1])
(justification-texts [j1])
(justification-names [j2])
(justification-texts [j2])
(show-justifications [j1])
(show-justifications [j2])
(set-rule-justifications name) ;; Instance Name
(get-rule-justifications)
(reset)
(run)
(duplicate-instance [j1] to [j3] (value 3))
(duplicate-instance [j2] to [j4] (value 4))
(modify-instance [j4] (value 6))
(justification-names [j1])
(justification-texts [j1])
(justification-names [j2])
(justification-texts [j2])
(show-justifications [j1])
(show-justifications [j2])
(justification-names [j3])
(justification-texts [j3])
(justification-names [j4])
(justification-texts [j4])
(show-justifications [j3])
(show-justifications [j4])
(set-rule-justifications none)
(get-rule-justifications)
