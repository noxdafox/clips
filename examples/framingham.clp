;;; Attribute/Value deftemplate

(deftemplate av
  (slot attribute)
  (slot value))

;;; Table related deftemplates

(deftemplate te
   (slot t)
   (multislot x)
   (multislot y)
   (multislot z)
   (multislot p)
   (multislot r))
   
(deftemplate table
   (slot name)
   (multislot attributes))

;;; Scoring related constructs

(deftemplate banner
  (multislot text))
  
(deftemplate score
   (multislot contributions)
   (slot value))
   
(deffacts score
   (score (value 0)))
 
(deftemplate result
   (slot table)
   (slot text (default "Your result is %s%n")))
             
(deftemplate contribution
   (slot table)
   (slot value))

;;; Banner rule

(defrule print-banner
   (declare (salience 100))
   (banner (text $?text))
   =>
   (progn$ (?t ?text)
      (printout t ?t crlf)))

;;; Question related constructs

(deftemplate question
   (slot attribute)
   (slot priority (default 0))
   (slot text)
   (slot type)
   (multislot possibles)
   (multislot range))

(defmethod ask-question ((?question STRING) (?allowed-values MULTIFIELD))
   (printout t ?question " " ?allowed-values " ")
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question " " ?allowed-values " ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)

(defmethod ask-question ((?question STRING) (?lower INTEGER) (?upper INTEGER))
   (printout t ?question " (" ?lower " - " ?upper") ")
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (or (not (integerp ?answer))
              (< ?answer ?lower)
              (> ?answer ?upper)) do
      (printout t ?question " (" ?lower " - " ?upper") ")
      (bind ?answer (read)))
   ?answer)

(defrule question-symbol
   (question (attribute ?attribute)
             (priority ?p)
             (text ?text)
             (type symbol)
             (possibles $?possibles))
   (not (av (attribute ?attribute)))
   (not (and (question (attribute ?attribute2&~?attribute)
                       (priority ?p2&:(> ?p2 ?p)))
             (not (av (attribute ?attribute2)))))
   =>
   (bind ?value (ask-question ?text ?possibles))
   (assert (av (attribute ?attribute)
               (value ?value))))
               
(defrule question-integer
   (question (attribute ?attribute)
             (priority ?p)
             (text ?text)
             (type integer)
             (range ?lower ?upper))
   (not (av (attribute ?attribute)))
   (not (and (question (attribute ?attribute2&~?attribute)
                       (priority ?p2&:(> ?p2 ?p)))
             (not (av (attribute ?attribute2)))))
   =>
   (bind ?value (ask-question ?text ?lower ?upper))
   (assert (av (attribute ?attribute)
               (value ?value))))
   
(defmethod check ((?v SYMBOL INTEGER FLOAT) (?e SYMBOL INTEGER FLOAT))
   (eq ?v ?e))

(defmethod check ((?v INTEGER FLOAT) 
                  (?lower INTEGER FLOAT)
                  (?op SYMBOL (eq ?op -))
                  (?upper INTEGER FLOAT))
   (and (>= ?v ?lower) (<= ?v ?upper)))

(defmethod check ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op >))
                  (?lower INTEGER FLOAT))
   (> ?v ?lower))

(defmethod check ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op <))
                  (?upper INTEGER FLOAT))
   (< ?v ?upper))
                     
(defrule apply-two-entry-table
   (table (name ?table-name)
          (attributes ?a1 ?a2))
   (av (attribute ?a1) (value ?v1))
   (av (attribute ?a2) (value ?v2))
   (te (t ?table-name) (x $?x) (y $?y) (p ?p))
   (test (and (check ?v1 (expand$ ?x))
              (check ?v2 (expand$ ?y))))
   =>
   (assert (contribution (table ?table-name)
                         (value ?p))))

(defrule apply-three-entry-table
   (table (name ?table-name)
          (attributes ?a1 ?a2 ?a3))
   (av (attribute ?a1) (value ?v1))
   (av (attribute ?a2) (value ?v2))
   (av (attribute ?a3) (value ?v3))
   (te (t ?table-name) (x $?x) (y $?y) (z $?z) (p ?p))
   (test (and (check ?v1 (expand$ ?x))
              (check ?v2 (expand$ ?y))
              (check ?v3 (expand$ ?z))))
   =>
   (assert (contribution (table ?table-name)
                         (value ?p))))
                         
(defrule apply-one-entry-result
   (declare (salience  -10))
   (result (table ?table-name)
           (text ?text))
   (table (name ?table-name)
          (attributes ?a1))
   (av (attribute ?a1) (value ?v1))
   (score (value ?v2))
   (te (t ?table-name) (x $?x) (p $?p) (r ?r))
   (test (and (check ?v1 (expand$ ?x))
              (check ?v2 (expand$ ?p))))
   =>
   (format t ?text ?r))

(defrule add-contribution
   ?f <- (score (contributions $?c)
                (value ?total))
   (contribution (table ?name&:(not (member$ ?name ?c)))
                 (value ?p))
   =>
   (modify ?f (contributions ?c ?name) (value (+ ?total ?p))))

;;; *******************************
;;; Framingham specific information
;;; *******************************
      
(deffacts banner
   (banner (text "The Framingham Risk Score estimates your 10-year risk for coronary heart disease."
                 "Your answers to a series of questions will determine your risk."
                 "")))
 
(deffacts result
   (result (table risk)
           (text "%nYour 10-year cardiovascular risk is %s%n")))
   
(deffacts questions
   (question (attribute age) 
             (priority 10)
             (text "How old are you?")
             (type integer) 
             (range 0 122))
   (question (attribute gender) 
             (text "What is your gender?")
             (type symbol) 
             (possibles male female))
   (question (attribute smokes) 
             (text "Do you smoke?")
             (type symbol) 
             (possibles yes no))
   (question (attribute bp) 
             (text "What is your systolic blood pressure?")
             (type integer) 
             (range 0 300))
   (question (attribute bp-treated) 
             (text "Is your blood pressure being treated?")
             (type symbol) 
             (possibles yes no))
   (question (attribute cholesterol) 
             (text "What is your total cholesterol?")
             (type integer) 
             (range 0 999))
   (question (attribute hdl) 
             (text "What is your HDL?")
             (type integer) 
             (range 0 199)))

(defrule invalid-age
   (declare (salience 10))
   (av (attribute age) 
       (value ?v&:(or (< ?v 20) (> ?v 79))))
   =>
   (printout t "Your age must be in the range 20 to 79." crlf)
   (halt))
    
(deffacts tables
   (table (name risk)
          (attributes gender))
   (table (name bp)
          (attributes gender bp bp-treated))
   (table (name hdl)
          (attributes gender hdl))
   (table (name chol)
          (attributes gender cholesterol age))
   (table (name smoke)
          (attributes gender smokes age))
   (table (name age)
          (attributes gender age)))

(deffacts age-table          
   (te (t age) (x male) (y 20 - 34) (p -9))
   (te (t age) (x male) (y 35 - 39) (p -4))
   (te (t age) (x male) (y 40 - 44) (p  0))
   (te (t age) (x male) (y 45 - 49) (p  3))
   (te (t age) (x male) (y 50 - 54) (p  6))
   (te (t age) (x male) (y 55 - 59) (p  8))
   (te (t age) (x male) (y 60 - 64) (p 10))
   (te (t age) (x male) (y 65 - 69) (p 11))
   (te (t age) (x male) (y 70 - 74) (p 12))
   (te (t age) (x male) (y 75 - 79) (p 13))
   
   (te (t age) (x female) (y 20 - 34) (p -7))
   (te (t age) (x female) (y 35 - 39) (p -3))
   (te (t age) (x female) (y 40 - 44) (p  0))
   (te (t age) (x female) (y 45 - 49) (p  3))
   (te (t age) (x female) (y 50 - 54) (p  6))
   (te (t age) (x female) (y 55 - 59) (p  8))
   (te (t age) (x female) (y 60 - 64) (p 10))
   (te (t age) (x female) (y 65 - 69) (p 12))
   (te (t age) (x female) (y 70 - 74) (p 14))
   (te (t age) (x female) (y 75 - 79) (p 16)))

(deffacts cholesterol-table          
   (te (t chol) (x male) (y 0 - 159) (z 20 - 39) (p 0))
   (te (t chol) (x male) (y 0 - 159) (z 40 - 49) (p 0))
   (te (t chol) (x male) (y 0 - 159) (z 50 - 59) (p 0))
   (te (t chol) (x male) (y 0 - 159) (z 60 - 69) (p 0))
   (te (t chol) (x male) (y 0 - 159) (z 70 - 79) (p 0))
   
   (te (t chol) (x male) (y 160 - 199) (z 20 - 39) (p 4))
   (te (t chol) (x male) (y 160 - 199) (z 40 - 49) (p 3))
   (te (t chol) (x male) (y 160 - 199) (z 50 - 59) (p 2))
   (te (t chol) (x male) (y 160 - 199) (z 60 - 69) (p 1))
   (te (t chol) (x male) (y 160 - 199) (z 70 - 79) (p 0))
   
   (te (t chol) (x male) (y 200 - 239) (z 20 - 39) (p 7))
   (te (t chol) (x male) (y 200 - 239) (z 40 - 49) (p 5))
   (te (t chol) (x male) (y 200 - 239) (z 50 - 59) (p 3))
   (te (t chol) (x male) (y 200 - 239) (z 60 - 69) (p 1))
   (te (t chol) (x male) (y 200 - 239) (z 70 - 79) (p 0))
   
   (te (t chol) (x male) (y 240 - 279) (z 20 - 39) (p 9))
   (te (t chol) (x male) (y 240 - 279) (z 40 - 49) (p 6))
   (te (t chol) (x male) (y 240 - 279) (z 50 - 59) (p 4))
   (te (t chol) (x male) (y 240 - 279) (z 60 - 69) (p 2))
   (te (t chol) (x male) (y 240 - 279) (z 70 - 79) (p 1))
   
   (te (t chol) (x male) (y > 279) (z 20 - 39) (p 11))
   (te (t chol) (x male) (y > 279) (z 40 - 49) (p 8))
   (te (t chol) (x male) (y > 279) (z 50 - 59) (p 5))
   (te (t chol) (x male) (y > 279) (z 60 - 69) (p 3))
   (te (t chol) (x male) (y > 279) (z 70 - 79) (p 1))      
   
   (te (t chol) (x female) (y < 160) (z 20 - 39) (p 0))
   (te (t chol) (x female) (y < 160) (z 40 - 49) (p 0))
   (te (t chol) (x female) (y < 160) (z 50 - 59) (p 0))
   (te (t chol) (x female) (y < 160) (z 60 - 69) (p 0))
   (te (t chol) (x female) (y < 160) (z 70 - 79) (p 0))
   
   (te (t chol) (x female) (y 160 - 199) (z 20 - 39) (p 4))
   (te (t chol) (x female) (y 160 - 199) (z 40 - 49) (p 3))
   (te (t chol) (x female) (y 160 - 199) (z 50 - 59) (p 2))
   (te (t chol) (x female) (y 160 - 199) (z 60 - 69) (p 1))
   (te (t chol) (x female) (y 160 - 199) (z 70 - 79) (p 1))
   
   (te (t chol) (x female) (y 200 - 239) (z 20 - 39) (p 8))
   (te (t chol) (x female) (y 200 - 239) (z 40 - 49) (p 6))
   (te (t chol) (x female) (y 200 - 239) (z 50 - 59) (p 4))
   (te (t chol) (x female) (y 200 - 239) (z 60 - 69) (p 2))
   (te (t chol) (x female) (y 200 - 239) (z 70 - 79) (p 1))
   
   (te (t chol) (x female) (y 240 - 279) (z 20 - 39) (p 11))
   (te (t chol) (x female) (y 240 - 279) (z 40 - 49) (p 8))
   (te (t chol) (x female) (y 240 - 279) (z 50 - 59) (p 5))
   (te (t chol) (x female) (y 240 - 279) (z 60 - 69) (p 3))
   (te (t chol) (x female) (y 240 - 279) (z 70 - 79) (p 2))
   
   (te (t chol) (x female) (y > 279) (z 20 - 39) (p 13))
   (te (t chol) (x female) (y > 279) (z 40 - 49) (p 10))
   (te (t chol) (x female) (y > 279) (z 50 - 59) (p 7))
   (te (t chol) (x female) (y > 279) (z 60 - 69) (p 4))
   (te (t chol) (x female) (y > 279) (z 70 - 79) (p 2)))

(deffacts smoking-table          
   (te (t smoke) (x male) (y no) (z 20 - 39) (p 0))
   (te (t smoke) (x male) (y no) (z 40 - 49) (p 0))
   (te (t smoke) (x male) (y no) (z 50 - 59) (p 0))
   (te (t smoke) (x male) (y no) (z 60 - 69) (p 0))
   (te (t smoke) (x male) (y no) (z 70 - 79) (p 0))
   
   (te (t smoke) (x male) (y yes) (z 20 - 39) (p 8))
   (te (t smoke) (x male) (y yes) (z 40 - 49) (p 5))
   (te (t smoke) (x male) (y yes) (z 50 - 59) (p 3))
   (te (t smoke) (x male) (y yes) (z 60 - 69) (p 1))
   (te (t smoke) (x male) (y yes) (z 70 - 79) (p 1))
   
   (te (t smoke) (x female) (y no) (z 20 - 39) (p 0))
   (te (t smoke) (x female) (y no) (z 40 - 49) (p 0))
   (te (t smoke) (x female) (y no) (z 50 - 59) (p 0))
   (te (t smoke) (x female) (y no) (z 60 - 69) (p 0))
   (te (t smoke) (x female) (y no) (z 70 - 79) (p 0))
   
   (te (t smoke) (x female) (y yes) (z 20 - 39) (p 9))
   (te (t smoke) (x female) (y yes) (z 40 - 49) (p 7))
   (te (t smoke) (x female) (y yes) (z 50 - 59) (p 4))
   (te (t smoke) (x female) (y yes) (z 60 - 69) (p 2))
   (te (t smoke) (x female) (y yes) (z 70 - 79) (p 1)))
   
(deffacts hdl-table          
   (te (t hdl) (x male) (y > 59)  (p -1))
   (te (t hdl) (x male) (y 50 - 59) (p 0))
   (te (t hdl) (x male) (y 40 - 49) (p 1))
   (te (t hdl) (x male) (y  0 - 40) (p 2))

   (te (t hdl) (x female) (y > 59)  (p -1))
   (te (t hdl) (x female) (y 50 - 59) (p 0))
   (te (t hdl) (x female) (y 40 - 49) (p 1))
   (te (t hdl) (x female) (y  0 - 40) (p 2)))
      
(deffacts bp-table          
   (te (t bp) (x male) (y   < 120) (z no) (p 0))
   (te (t bp) (x male) (y   < 120) (z yes)   (p 0))

   (te (t bp) (x male) (y 120 - 129) (z no) (p 0))
   (te (t bp) (x male) (y 120 - 129) (z yes)   (p 1))

   (te (t bp) (x male) (y 130 - 139) (z no) (p 1))
   (te (t bp) (x male) (y 130 - 139) (z yes)   (p 2))

   (te (t bp) (x male) (y 140 - 159) (z no) (p 1))
   (te (t bp) (x male) (y 140 - 159) (z yes)   (p 2))

   (te (t bp) (x male) (y > 159) (z no) (p 2))
   (te (t bp) (x male) (y > 159) (z yes)   (p 3))

   (te (t bp) (x female) (y   0 - 120) (z no) (p 0))
   (te (t bp) (x female) (y   0 - 120) (z yes)   (p 0))

   (te (t bp) (x female) (y 120 - 129) (z no) (p 1))
   (te (t bp) (x female) (y 120 - 129) (z yes)   (p 3))

   (te (t bp) (x female) (y 130 - 139) (z no) (p 2))
   (te (t bp) (x female) (y 130 - 139) (z yes)   (p 4))

   (te (t bp) (x female) (y 140 - 159) (z no) (p 3))
   (te (t bp) (x female) (y 140 - 159) (z yes)   (p 5))

   (te (t bp) (x female) (y > 159) (z no) (p 4))
   (te (t bp) (x female) (y > 159) (z yes)   (p 6)))

(deffacts 10-year-risk
   (te (t risk) (x male) (p < 0)   (r "< 1%"))
   (te (t risk) (x male) (p 1 - 4) (r "1%"))   
   (te (t risk) (x male) (p 5 - 6) (r "2%"))   
   (te (t risk) (x male) (p 7)     (r "3%"))   
   (te (t risk) (x male) (p 8)     (r "4%"))
   (te (t risk) (x male) (p 9)     (r "5%"))
   (te (t risk) (x male) (p 10)    (r "6%"))
   (te (t risk) (x male) (p 11)    (r "8%"))
   (te (t risk) (x male) (p 12)    (r "10%"))
   (te (t risk) (x male) (p 13)    (r "12%"))
   (te (t risk) (x male) (p 14)    (r "16%"))
   (te (t risk) (x male) (p 15)    (r "20%"))
   (te (t risk) (x male) (p 16)    (r "25%"))
   (te (t risk) (x male) (p > 16)  (r "> 30%"))
     
   (te (t risk) (x female) (p < 9)     (r "< 1%"))
   (te (t risk) (x female) (p 9 - 12)  (r "1%"))   
   (te (t risk) (x female) (p 13 - 14) (r "2%"))   
   (te (t risk) (x female) (p 15)      (r "3%"))   
   (te (t risk) (x female) (p 16)      (r "4%"))
   (te (t risk) (x female) (p 17)      (r "5%"))
   (te (t risk) (x female) (p 18)      (r "6%"))
   (te (t risk) (x female) (p 19)      (r "8%"))
   (te (t risk) (x female) (p 20)      (r "11%"))
   (te (t risk) (x female) (p 21)      (r "14%"))
   (te (t risk) (x female) (p 22)      (r "17%"))
   (te (t risk) (x female) (p 23)      (r "22%"))
   (te (t risk) (x female) (p 24)      (r "27%"))
   (te (t risk) (x female) (p > 24)    (r "> 30%")))
