(clear) ; Modify rebind
(deftemplate point (slot x) (slot y))
(deftemplate person (slot name) (slot age))

(deffacts start
   (point (x 2) (y 3))
   (person (name "John Smith") (age 53)))

(defrule munge
   ?p <- (point)
   =>
   (modify ?p (x 4))
   (bind ?p (nth$ 1 (find-fact ((?f person)) TRUE)))
   (modify ?p (age 54)))
(reset)
(run)
(facts)
(clear) ; Modify rebind invalid slot
(deftemplate point (slot x) (slot y))
(deftemplate person (slot name) (slot age))

(deffacts start
   (point (x 2) (y 3))
   (person (name "John Smith") (age 53)))

(defrule munge
   ?p <- (point)
   =>
   (modify ?p (x 4))
   (bind ?p (nth$ 1 (find-fact ((?f person)) TRUE)))
   (modify ?p (y 5)))
(reset)
(run)
(facts)
(clear) ; ?var:slot rebind
(deftemplate point (slot x) (slot y))
(deftemplate person (slot name) (slot age))

(deffacts start
   (point (x 2) (y 3))
   (person (name "John Smith") (age 53)))

(defrule munge
   ?p <- (point)
   =>
   (println "x = " ?p:x)
   (bind ?p (nth$ 1 (find-fact ((?f person)) TRUE)))
   (println "age = " ?p:age))
(reset)
(run)
(clear) ; Invalid ?var:slot
(deftemplate point (slot x) (slot y))

(deffacts start
   (point (x 2) (y 3)))

(defrule munge
   ?p <- (point)
   =>
   (println "(" ?p:x "," ?p:y "," ?p:z ")"))

(defrule munge
   ?p <- (point)
   =>
   (retract ?p)
   (+ ?p:x ?p:y))
(reset)
(run)
(clear)
(deffunction x (?x) (println ?x:foo))
(x 3)
(clear)
