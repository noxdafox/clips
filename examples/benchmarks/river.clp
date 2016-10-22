;;; Yet another river crossing puzzle

;;; The raft can carry no more than 2 passengers.
;;; Only the Adults (mother, father and officer) can operate the raft.
;;; The father can not be in the presence of the daughters without the mother.
;;; The mother can not be in the presence of the sons without the father.
;;; The thief can not be alone with any of the family without the officer.
;;; While onshore, the cat must be with either the father or the daughters
;;; While onshore, The dog must be with either the mother or the sons
;;; The officer can not move the cat or the dog

;;; 18515 rules shoud be fired

;;; There are two solutions.
;;;
;;; Solution found: 
;;; 
;;;    officer and thief move to shore 2.
;;;    officer moves alone to shore 1.
;;;    officer and son move to shore 2.
;;;    officer and thief move to shore 1.
;;;    father and son move to shore 2.
;;;    father moves alone to shore 1.
;;;    father and dog move to shore 2.
;;;    father moves alone to shore 1.
;;;    father and mother move to shore 2.
;;;    mother moves alone to shore 1.
;;;    officer and thief move to shore 2.
;;;    father moves alone to shore 1.
;;;    father and mother move to shore 2.
;;;    mother moves alone to shore 1.
;;;    mother and cat move to shore 2.
;;;    mother moves alone to shore 1.
;;;    mother and daughter move to shore 2.
;;;    officer and thief move to shore 1.
;;;    officer and daughter move to shore 2.
;;;    officer moves alone to shore 1.
;;;    officer and thief move to shore 2.
;;; 
;;; 
;;; Solution found: 
;;; 
;;;    officer and thief move to shore 2.
;;;    officer moves alone to shore 1.
;;;    officer and daughter move to shore 2.
;;;    officer and thief move to shore 1.
;;;    mother and daughter move to shore 2.
;;;    mother moves alone to shore 1.
;;;    mother and cat move to shore 2.
;;;    mother moves alone to shore 1.
;;;    father and mother move to shore 2.
;;;    father moves alone to shore 1.
;;;    officer and thief move to shore 2.
;;;    mother moves alone to shore 1.
;;;    father and mother move to shore 2.
;;;    father moves alone to shore 1.
;;;    father and dog move to shore 2.
;;;    father moves alone to shore 1.
;;;    father and son move to shore 2.
;;;    officer and thief move to shore 1.
;;;    officer and son move to shore 2.
;;;    officer moves alone to shore 1.
;;;    officer and thief move to shore 2.

;;;***********
;;;* GLOBALS *
;;;***********

;;; Set ?*output* to t to see see the solution printed when the program is run.
;;; Set ?*output* to nil to disable printing when benchmarking.

(defglobal ?*output* = t)

;;;*************
;;;* TEMPLATES *
;;;*************

(deftemplate actor
   (slot role)
   (slot location)
   (slot parent)
   (slot search-depth)
   (slot id)
   (slot viewpoint)
   (slot move))

(deftemplate solution
   (slot viewpoint)
   (slot moves)) 
   
(deftemplate viewpoint
   (slot id)
   (slot parent)
   (slot processed))
   
(deftemplate attempt
   (slot viewpoint)
   (slot actor)
   (slot with)
   (slot priority))

(deftemplate viewpoint-count
   (slot next-id))
   
(deftemplate shore
   (slot id)
   (slot opposite)
   (slot name)
   (slot opposite-name))
   
(deftemplate remove-viewpoint
   (slot id))
   
;;;*****************
;;;* INITIAL STATE *
;;;*****************

(defrule initial-state
   =>
   (assert (viewpoint-count (next-id 1)))
   (assert (viewpoint (id 0) (parent none) (processed no)))
   (assert (actor (role raft) (location S1) (search-depth 1) (id raft) (viewpoint 0)))
   (assert (actor (role father) (location S1) (search-depth 1) (id father) (viewpoint 0)))
   (assert (actor (role mother) (location S1) (search-depth 1) (id mother) (viewpoint 0)))
   (assert (actor (role son) (location S1) (search-depth 1) (id son-1) (viewpoint 0)))
   (assert (actor (role son) (location S1) (search-depth 1) (id son-2) (viewpoint 0)))
   (assert (actor (role daughter) (location S1) (search-depth 1) (id daughter-1) (viewpoint 0)))
   (assert (actor (role daughter) (location S1) (search-depth 1) (id daughter-2) (viewpoint 0)))
   (assert (actor (role officer) (location S1) (search-depth 1) (id officer) (viewpoint 0)))
   (assert (actor (role thief) (location S1) (search-depth 1) (id thief) (viewpoint 0)))
   (assert (actor (role cat) (location S1) (search-depth 1) (id cat) (viewpoint 0)))
   (assert (actor (role dog) (location S1) (search-depth 1) (id dog) (viewpoint 0)))
   (assert (shore (id S1) (opposite S2) (name "shore 1") (opposite-name "shore 2")))
   (assert (shore (id S2) (opposite S1) (name "shore 2") (opposite-name "shore 1"))))

;;;*****************
;;;* ATTEMPT MOVES *
;;;*****************

(defrule initiate-moves
   ?f <- (viewpoint (id ?viewpoint) (processed no))
   (not (viewpoint (id ?viewpoint2&:(< ?viewpoint2 ?viewpoint)) (processed no)))
   (not (attempt))
   =>
   (modify ?f (processed yes))
   (assert (attempt (viewpoint ?viewpoint) (actor officer) (with father) (priority 280)))
   (assert (attempt (viewpoint ?viewpoint) (actor officer) (with mother) (priority 270)))
   (assert (attempt (viewpoint ?viewpoint) (actor father) (with mother) (priority 260)))
   (assert (attempt (viewpoint ?viewpoint) (actor father) (with alone) (priority 250)))
   (assert (attempt (viewpoint ?viewpoint) (actor mother) (with alone) (priority 240))) 
   (assert (attempt (viewpoint ?viewpoint) (actor mother) (with daughter) (priority 230)))
   (assert (attempt (viewpoint ?viewpoint) (actor mother) (with cat) (priority 210)))
   (assert (attempt (viewpoint ?viewpoint) (actor father) (with cat) (priority 200)))
   (assert (attempt (viewpoint ?viewpoint) (actor mother) (with dog) (priority 190)))
   (assert (attempt (viewpoint ?viewpoint) (actor father) (with dog) (priority 180)))
   (assert (attempt (viewpoint ?viewpoint) (actor father) (with son) (priority 170)))
   (assert (attempt (viewpoint ?viewpoint) (actor officer) (with son) (priority 140)))
   (assert (attempt (viewpoint ?viewpoint) (actor officer) (with daughter) (priority 120)))
   (assert (attempt (viewpoint ?viewpoint) (actor officer) (with alone) (priority 110)))
   (assert (attempt (viewpoint ?viewpoint) (actor officer) (with thief) (priority 100))))

;;;***********************
;;;* GENERATE PATH RULES *
;;;***********************

(defrule inherit-role
   (declare (salience 300))
   ?node <- (actor (role ?role) (location ?l) (id ?id) (viewpoint ?parent) (search-depth ?num))
   (actor (parent ?parent) (viewpoint ?viewpoint) (move ?move))
   (not (actor (role ?role) (id ?id) (parent ?parent) (viewpoint ?viewpoint)))
   =>
   (assert (actor (role ?role) 
                  (location ?l) 
                  (parent ?parent)
                  (search-depth (+ 1 ?num))  
                  (id ?id)
                  (viewpoint ?viewpoint)  
                  (move ?move))))
                  
;;;************************
;;;* PRINT SOLUTION RULES *
;;;************************

(defrule solution-found
   (declare (salience 100))
   (actor (id father) (search-depth ?sd1) (viewpoint ?vp1) (location S2) (parent ?parent) (move ?move))
   (actor (id mother) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id son-1) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id son-2) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id daughter-1) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id daughter-2) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id thief) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id officer) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id cat) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   (actor (id dog) (search-depth ?sd1) (viewpoint ?vp1) (location S2))
   =>
   (assert (solution (viewpoint ?parent) 
                     (moves (format nil "   %s%n" ?move)))))

(defrule progress-solution
   (declare (salience 100))
   ?f <- (solution (viewpoint ?viewpoint) (moves ?moves))
   (actor (viewpoint ?viewpoint&~0) (parent ?parent) (move ?move))
   =>
   (modify ?f (viewpoint ?parent) 
              (moves (format nil "   %s%n%s" ?move ?moves))))

(defrule print-solution
   (declare (salience 100))
   ?f <- (solution (viewpoint 0) (moves ?moves))
   =>
   (format ?*output* "%n%nSolution found: %n%n")
   (format ?*output* "%s" ?moves))

;;;**************
;;;* MOVE RULES *
;;;**************

(defrule move-alone
   ?f <- (attempt (viewpoint ?parent) (actor ?role) (with alone) (priority ?p1))
   (not (attempt (viewpoint ?parent) (priority ?p2&:(> ?p2 ?p1))))
   (actor (role ?role) (viewpoint ?parent) (search-depth ?num) (location ?is) (id ?id))
   (actor (role raft) (viewpoint ?parent) (search-depth ?num) (location ?is))
   (shore (id ?is) (opposite ?ns) (opposite-name ?nns))
   ?vc <- (viewpoint-count (next-id ?new-vp))
   =>
   (retract ?f)
   (assert (viewpoint (id ?new-vp) (parent ?parent) (processed no)))
   (modify ?vc (next-id (+ ?new-vp 1)))
   (bind ?move (str-cat ?role " moves alone to " ?nns "."))
   (assert (actor (role raft)
                  (location ?ns) 
                  (parent ?parent) 
                  (search-depth (+ 1 ?num)) 
                  (id raft)
                  (viewpoint ?new-vp) 
                  (move ?move)))
   (assert (actor (role ?role) 
                  (location ?ns) 
                  (parent ?parent)
                  (search-depth (+ 1 ?num))  
                  (id ?id)
                  (viewpoint ?new-vp) 
                  (move ?move))))

(defrule can-move-together
   ?f <- (attempt (viewpoint ?parent) (actor ?role) (with ?with&~alone) (priority ?p1))
   (not (attempt (viewpoint ?parent) (priority ?p2&:(> ?p2 ?p1))))
   (actor (role ?role) (viewpoint ?parent) (search-depth ?num) (location ?is) (id ?id1))
   (actor (role ?with) (viewpoint ?parent) (search-depth ?num) (location ?is) (id ?id2))
   (actor (role raft) (viewpoint ?parent) (search-depth ?num) (location ?is))
   (shore (id ?is) (opposite ?ns) (opposite-name ?nns))
   ?vc <- (viewpoint-count (next-id ?new-vp))
   =>
   (retract ?f)
   (assert (viewpoint (id ?new-vp) (parent ?parent) (processed no)))
   (modify ?vc (next-id (+ ?new-vp 1)))
   (bind ?move (str-cat ?role " and " ?with " move to " ?nns "."))
   
   (assert (actor (role raft)
                  (location ?ns) 
                  (parent ?parent) 
                  (search-depth (+ 1 ?num)) 
                  (id raft)
                  (viewpoint ?new-vp) 
                  (move ?move)))
   
   (assert (actor (role ?role) 
                  (location ?ns) 
                  (parent ?parent)
                  (search-depth (+ 1 ?num))  
                  (id ?id1)
                  (viewpoint ?new-vp) 
                  (move ?move))) 
   
   (assert (actor (role ?with) 
                  (location ?ns) 
                  (parent ?parent)
                  (search-depth (+ 1 ?num))  
                  (id ?id2)
                  (viewpoint ?new-vp) 
                  (move ?move))))

(defrule can't-move-together
   ?f <- (attempt (viewpoint ?parent) (actor ?role) (with ?with&~alone) (priority ?p1))
   (not (attempt (viewpoint ?parent) (priority ?p2&:(> ?p2 ?p1))))
   (actor (role ?role) (viewpoint ?parent) (search-depth ?num) (location ?is))
   (not (actor (role ?with) (viewpoint ?parent) (search-depth ?num) (location ?is)))
   =>
   (retract ?f))

(defrule can't-use-raft
   ?f <- (attempt (viewpoint ?parent) (actor ?role) (priority ?p1))
   (not (attempt (viewpoint ?parent) (priority ?p2&:(> ?p2 ?p1))))
   (actor (role ?role) (viewpoint ?parent) (search-depth ?num) (location ?is))
   (actor (role raft) (viewpoint ?parent) (search-depth ?num) (location ~?is))
   =>
   (retract ?f))
        
;;;*************************
;;;* CONSTRAINT VIOLATIONS *
;;;*************************

(defrule mother-with-sons-and-no-father
   (declare (salience 200))
   (actor (role mother) (viewpoint ?viewpoint) (location ?shore))
   (actor (role father) (viewpoint ?viewpoint) (location ~?shore))
   (actor (role son) (viewpoint ?viewpoint) (location ?shore))
   (not (remove-viewpoint (id ?viewpoint)))
   =>
   (assert (remove-viewpoint (id ?viewpoint))))

(defrule father-with-daughters-and-no-mother
   (declare (salience 200))
   (actor (role father) (viewpoint ?viewpoint) (location ?shore))
   (actor (role mother) (viewpoint ?viewpoint) (location ~?shore))
   (actor (role daughter) (viewpoint ?viewpoint) (location ?shore))
   (not (remove-viewpoint (id ?viewpoint)))
   =>
   (assert (remove-viewpoint (id ?viewpoint))))

(defrule thief-with-family-and-no-officer
   (declare (salience 200))
   (actor (role thief) (viewpoint ?viewpoint) (location ?shore))
   (actor (role officer) (viewpoint ?viewpoint) (location ~?shore))
   (actor (role father|mother|son|daughter) (viewpoint ?viewpoint) (location ?shore))
   (not (remove-viewpoint (id ?viewpoint)))
   =>
   (assert (remove-viewpoint (id ?viewpoint))))

(defrule cat-without-father-or-daughter
   (declare (salience 200))
   (actor (role cat) (viewpoint ?viewpoint) (location ?shore))
   (not (actor (role father | daughter) (viewpoint ?viewpoint) (location ?shore)))
   (not (remove-viewpoint (id ?viewpoint)))
   =>
   (assert (remove-viewpoint (id ?viewpoint))))

(defrule dog-without-mother-or-son
   (declare (salience 200))
   (actor (role dog) (viewpoint ?viewpoint) (location ?shore))
   (not (actor (role mother | son) (viewpoint ?viewpoint) (location ?shore))) 
   (not (remove-viewpoint (id ?viewpoint)))
   =>
   (assert (remove-viewpoint (id ?viewpoint))))

(defrule quicker-path
   (declare (salience 200))
   
   (actor (role father) (search-depth ?sd1) (viewpoint ?vp1) (location ?s1))
   (actor (role mother) (search-depth ?sd1) (viewpoint ?vp1) (location ?s2))
   (actor (role son) (id ?sid11) (search-depth ?sd1) (viewpoint ?vp1) (location ?s3))
   (actor (role son) (id ~?sid11) (search-depth ?sd1) (viewpoint ?vp1) (location ?s4))
   (actor (role daughter) (id ?did11) (search-depth ?sd1) (viewpoint ?vp1) (location ?s5))
   (actor (role daughter) (id ~?did11) (search-depth ?sd1) (viewpoint ?vp1) (location ?s6))
   (actor (role thief) (search-depth ?sd1) (viewpoint ?vp1) (location ?s7))
   (actor (role officer) (search-depth ?sd1) (viewpoint ?vp1) (location ?s8))
   (actor (role raft) (search-depth ?sd1) (viewpoint ?vp1) (location ?s9))
   (actor (role cat) (search-depth ?sd1) (viewpoint ?vp1) (location ?s10))
   (actor (role dog) (search-depth ?sd1) (viewpoint ?vp1) (location ?s11))

   (actor (role father) (search-depth ?sd2&:(< ?sd1 ?sd2)) (viewpoint ?vp2) (location ?s1))
   (actor (role mother) (search-depth ?sd2) (viewpoint ?vp2) (location ?s2))
   (actor (role son) (id ?sid12) (search-depth ?sd2) (viewpoint ?vp2) (location ?s3))
   (actor (role son) (id ~?sid12) (search-depth ?sd2) (viewpoint ?vp2) (location ?s4))
   (actor (role daughter) (id ?did12) (search-depth ?sd2) (viewpoint ?vp2) (location ?s5))
   (actor (role daughter) (id ~?did12) (search-depth ?sd2) (viewpoint ?vp2) (location ?s6))
   (actor (role thief) (search-depth ?sd2) (viewpoint ?vp2) (location ?s7))
   (actor (role officer) (search-depth ?sd2) (viewpoint ?vp2) (location ?s8))
   (actor (role raft) (search-depth ?sd2) (viewpoint ?vp2) (location ?s9))
   (actor (role cat) (search-depth ?sd2) (viewpoint ?vp2) (location ?s10))
   (actor (role dog) (search-depth ?sd2) (viewpoint ?vp2) (location ?s11))

   (not (remove-viewpoint (id ?vp2)))

   =>

   (assert (remove-viewpoint (id ?vp2))))

;;;*********************
;;;* VIEWPOINT CLEANUP *
;;;*********************

(defrule remove-viewpoint
   (declare (salience 400))
   ?f <- (remove-viewpoint (id ?viewpoint))
   ?v <- (viewpoint (id ?viewpoint))
   (not (actor (viewpoint ?viewpoint)))
   (not (attempt (viewpoint ?viewpoint)))
   =>
   (retract ?v)
   (retract ?f))

(defrule remove-viewpoint-actor
   (declare (salience 400))
   (remove-viewpoint (id ?viewpoint))
   ?f <- (actor (viewpoint ?viewpoint))
   =>
   (retract ?f))

(defrule remove-viewpoint-attempt
   (declare (salience 400))
   (remove-viewpoint (id ?viewpoint))
   ?f <- (attempt (viewpoint ?viewpoint))
   =>
   (retract ?f))
