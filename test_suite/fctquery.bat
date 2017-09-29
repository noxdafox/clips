(reset)
(do-for-all-facts ((?i MAN)) TRUE
   (+ (eval "(gensym)") 2))
(do-for-all-facts ((?mb MALE) (?wg FEMALE))
   TRUE
   (printout t ?mb:name " " ?wg:name crlf))
(do-for-all-facts ((?mb BOY MAN) (?wg GIRL WOMAN))
   TRUE
   (progn (printout t ?mb:name ) (printout t " " ?wg:name crlf)))
(do-for-all-facts ((?f1 FEMALE) (?f2 FEMALE))
   TRUE
   (printout t ?f1:name " " ?f2:name crlf))
(do-for-all-facts ((?mb MALE) (?wg FEMALE))
   (= (fact-slot-value ?mb age) (fact-slot-value ?wg age))
   (printout t ?mb:name " " ?wg:name crlf))
(do-for-all-facts ((?mb MALE) (?wg FEMALE))
   (= ?mb:age ?wg:age)
   (printout t ?mb:name " " ?wg:name crlf))
(count-facts WOMAN)
(count-facts-2 WOMAN)
(count-facts BOY)
(count-facts-2 BOY)
(any-factp ((?man MAN)) (> ?man:age 30))
(find-fact ((?m MAN) (?w WOMAN)) (= ?m:age ?w:age))
(find-all-facts ((?m MAN) (?w WOMAN)) (= ?m:age ?w:age))
(do-for-fact ((?p1 PERSON) (?p2 PERSON) (?p3 PERSON))
  (and (= ?p1:age ?p2:age ?p3:age)
       (neq ?p1 ?p2)
       (neq ?p1 ?p3)
       (neq ?p2 ?p3))
  (printout t ?p1:name " " ?p2:name " " ?p3:name crlf))
(do-for-all-facts ((?p1 PERSON) (?p2 PERSON) (?p3 PERSON))
  (and (= ?p1:age ?p2:age ?p3:age)
       (> (str-compare ?p1:name ?p2:name) 0)
       (> (str-compare ?p2:name ?p3:name) 0))
  (printout t ?p1:name " " ?p2:name " " ?p3:name crlf))
(do-for-all-facts ((?p1 PERSON) (?p2 PERSON) (?p3 PERSON))
  (= ?p1:age ?p2:age ?p3:age)
  (printout t ?p1:name " " ?p2:name " " ?p3:name crlf))
(watch facts)
(delayed-do-for-all-facts ((?b1 BOY))
   (not (any-factp ((?b2 BOY))
           (> ?b2:age (fact-slot-value ?b1 age))))
   (retract ?b1))
(unwatch facts)
(reset)
(watch facts)
(do-for-all-facts ((?b1 BOY))
   (not (any-factp ((?b2 BOY))
           (> ?b2:age (fact-slot-value ?b1 age))))
   (retract ?b1))
(unwatch facts)
(reset)
(do-for-all-facts ((?b BOY)) TRUE
  (do-for-all-facts ((?g GIRL)) TRUE
    (printout t ?b:name " " ?g:name crlf)))
(assert (A (name [a])))
(do-for-all-facts ((?a A)) TRUE
  (printout t ?a crlf))
(any-factp)
(any-factp TRUE)
(any-factp ())
(any-factp (?B OBJECT) TRUE)
(any-factp ((?b A)))
(any-factp ((?b BOGUS)) TRUE)
(any-factp ((?b 34)) TRUE)
(any-factp ((?b (mv-append BOGUS USER))) TRUE)
(any-factp ((?b (mv-append USER 34))) TRUE)
(any-factp ((?b A)) (progn (bind ?b 1) TRUE))
(find-fact)
(find-fact TRUE)
(find-fact ())
(find-fact (?B OBJECT) TRUE)
(find-fact ((?b A)))
(find-fact ((?b BOGUS)) TRUE)
(find-fact ((?b 34)) TRUE)
(find-fact ((?b (mv-append BOGUS USER))) TRUE)
(find-fact ((?b (mv-append USER 34))) TRUE)
(find-fact ((?b A)) (progn (bind ?b 1) TRUE))
(find-all-facts)
(find-all-facts TRUE)
(find-all-facts ())
(find-all-facts (?B OBJECT) TRUE)
(find-all-facts ((?b A)))
(find-all-facts ((?b BOGUS)) TRUE)
(find-all-facts ((?b 34)) TRUE)
(find-all-facts ((?b (mv-append BOGUS USER))) TRUE)
(find-all-facts ((?b (mv-append USER 34))) TRUE)
(find-all-facts ((?b A)) (progn (bind ?b 1) TRUE))
(do-for-fact)
(do-for-fact TRUE)
(do-for-fact ())
(do-for-fact (?B OBJECT) TRUE)
(do-for-fact ((?b A)))
(do-for-fact ((?b BOGUS)) TRUE)
(do-for-fact ((?b A)) (progn (bind ?b 1) TRUE))
(do-for-fact ((?b 34)) TRUE abc)
(do-for-fact ((?b (mv-append BOGUS USER))) TRUE abc)
(do-for-fact ((?b (mv-append USER 34))) TRUE abc)
(do-for-fact ((?b MALE)) TRUE (bind ?b 1))
(do-for-all-facts)
(do-for-all-facts TRUE)
(do-for-all-facts ())
(do-for-all-facts (?B OBJECT) TRUE)
(do-for-all-facts ((?b A)))
(do-for-all-facts ((?b BOGUS)) TRUE)
(do-for-all-facts ((?b A)) (progn (bind ?b 1) TRUE))
(do-for-all-facts ((?b 34)) TRUE abc)
(do-for-all-facts ((?b (mv-append BOGUS USER))) TRUE abc)
(do-for-all-facts ((?b (mv-append USER 34))) TRUE abc)
(do-for-all-facts ((?b MALE)) TRUE (bind ?b 1))
(delayed-do-for-all-facts)
(delayed-do-for-all-facts TRUE)
(delayed-do-for-all-facts ())
(delayed-do-for-all-facts (?B OBJECT) TRUE)
(delayed-do-for-all-facts ((?b A)))
(delayed-do-for-all-facts ((?b BOGUS)) TRUE)
(delayed-do-for-all-facts ((?b A)) (progn (bind ?b 1) TRUE))
(delayed-do-for-all-facts ((?b 34)) TRUE abc)
(delayed-do-for-all-facts ((?b (mv-append BOGUS USER))) TRUE abc)
(delayed-do-for-all-facts ((?b (mv-append USER 34))) TRUE abc)
(delayed-do-for-all-facts ((?b MALE)) TRUE (bind ?b 1))
(assert (V (name [v])))
(assert (W (name [w])))
(assert (X (name [x])))
(assert (Y (name [y])))
(assert (Z (name [z]))))
(do-for-all-facts ((?ins1 ?*list* X (mv-append Y Z)) 
                       (?ins2 X (mv-append Y Z) ?*list*))
  TRUE
  (printout t ?ins1:name " " ?ins2:name crlf))
(while TRUE do
   (delayed-do-for-all-facts ((?b initial-fact)) TRUE 
     (progn (printout t DELAYED ?b crlf) (break)))
   (do-for-all-facts ((?b initial-fact)) TRUE 
     (progn (printout t DO-FOR-ALL ?b crlf) (break)))
   (break))
(deffunction quoxnar ()
   (do-for-all-facts ((?b initial-fact)) TRUE 
     (progn (printout t INS-RETURN ?b crlf) (return))))
(quoxnar)
(clear)
(deftemplate point (slot x) (slot y))
(deffacts points (point (x 1) (y 2)) (point (x 3) (y 4)))
(reset)
(do-for-all-facts ((?f point)) TRUE (retract ?f) (+ ?f:x ?f:y))
(reset)
(do-for-all-facts ((?f point)) TRUE (+ ?f:x ?f:z))
(do-for-all-facts ((?f point)) TRUE (+ ?f:x ?f:78))
(clear)
(deftemplate a (slot x))
(deftemplate b (slot y))
(deftemplate c (slot z))

(deffacts init
   (a (x 1)) 
   (a (x 2)) 
   (b (y 3)) 
   (b (y 4)) 
   (c (z 5)) 
   (c (z 6))
   (c (z 7)))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?a))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?b))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?c))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?a ?b))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?b ?c))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?a ?c))
(reset)

(do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?a ?b ?c))
(reset)

(delayed-do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf))
(reset)

(delayed-do-for-all-facts ((?a a) (?b b) (?c c)) TRUE 
  (printout t (if (fact-existp ?a) then ?a:x else "?") " " 
              (if (fact-existp ?b) then ?b:y else "?") " " 
              (if (fact-existp ?c) then ?c:z else "?") crlf)
  (retract ?a ?b ?c))
(reset)

(find-all-facts ((?a a) (?b b) (?c c)) 
  (progn (retract ?a ?b ?c) FALSE))
(facts)
(reset)

(find-fact ((?a a) (?b b) (?c c)) 
  (progn (retract ?a ?b ?c) FALSE))
(facts)
(reset)

(find-all-facts ((?a a) (?b b) (?c c)) 
  (progn (if (eq ?b:y 3) then (retract ?b)) FALSE))
(facts)
(reset)

(find-fact ((?a a) (?b b) (?c c)) 
  (progn (if (eq ?b:y 3) then (retract ?b)) FALSE))
(facts)
(reset)

(find-all-facts ((?a a) (?b b) (?c c)) 
  (progn (if (eq ?b:y 3) then (retract ?b)) 
         (if (eq ?c:z 5) then (retract ?c))
         (if (eq ?a:x 2) then TRUE else FALSE)))
(facts)
(reset)

(find-fact ((?a a) (?b b) (?c c)) 
  (progn (if (eq ?b:y 3) then (retract ?b)) 
         (if (eq ?c:z 5) then (retract ?c))
         (if (eq ?a:x 2) then TRUE else FALSE)))
(facts)
(clear)
(watch facts)
(deftemplate foo (multislot list))
(assert (foo (list 1 2)))

(do-for-fact ((?f foo)) TRUE
   (retract ?f)
   (bind ?x ?f:list)
   (assert (foo (list ?x 3))))
(assert (foo (list 3 4)) (foo (list 5 6)))   

(do-for-all-facts ((?f foo)) TRUE
   (retract ?f)
   (bind ?x ?f:list)
   (assert (foo (list ?x 3)))) 
(unwatch facts)
(clear)
