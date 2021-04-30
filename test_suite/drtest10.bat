(clear)                   ; Memory Leak #1
(progn (release-mem) TRUE)
(mem-used)
(defclass SOURCE (is-a USER))

(deffunction foo()
   (do-for-all-instances ((?x SOURCE)) TRUE
      (bind ?y 0)
      (bogus)))
(clear)                   ; Memory Leak #2
(progn (release-mem) TRUE)
(mem-used)
(defclass SOURCE (is-a USER))

(deffunction foo()
   (do-for-all-instances ((?x SOURCE)) (progn (bind ?y 3) (bogus) TRUE)
      (+ 3 4)))
(clear)                   ; Memory Leak #3
(progn (release-mem) TRUE)
(mem-used)
(deftemplate SOURCE)

(deffunction foo()
   (do-for-all-facts ((?x SOURCE)) TRUE
      (bind ?y 0)
      (bogus)))
(clear)                   ; Memory Leak #41
(progn (release-mem) TRUE)
(mem-used)
(deftemplate SOURCE)

(deffunction foo()
   (do-for-all-facts ((?x SOURCE)) (progn (bind ?y 3) (bogus) TRUE)
      (+ 3 4)))
(clear)                   ; Memory Leak #5
(progn (release-mem) TRUE)
(mem-used)

(defclass FOO (is-a USER)
   (slot value1))

(deffunction foo ()
   (make-instance of FOO
      (value1 (bogus))))
(clear)                   ; Memory Leak #6
(progn (release-mem) TRUE)
(mem-used)

(deftemplate FOO
   (slot value1 (type SYMBOL)))

(defrule foo
   (FOO (value1 ?x))
   =>
   (+ ?x 1)
   (printout t ?x))
(clear)
(progn (release-mem) TRUE)
(mem-used)
(clear)

(deftemplate nar 
   (slot bc))

(defrule migrant 
   (test (eq 1 1))
   (nar (bc ?bc))
   =>
   (printout t ?bc crlf))

(deffacts stuff
   (nar  (bc "US")))
(reset)
(run)
(clear)                   ; SourceForge Bug #12
(defclass Test (is-a USER) (multislot Contents))
(make-instance of Test (Contents a b c d e f g h))

(defrule BrokenPatternMatchBehavior-Object
   (object (is-a Test) 
           (Contents $?first ?second ?third $?fourth ?fifth))
   =>
   (printout t ?first " " ?second " " ?third " " ?fourth " " ?fifth crlf))
(run)
(clear) ;; CLIPSESG Bug

(defclass A (is-a USER)
  (role concrete)
  (slot foo)
  (slot bar))
(make-instance a of A)
(watch all)
(modify-instance a (foo 0))
(unwatch all)
(clear) ;; CLIPSESG Bug

(defclass A
  (is-a USER)
  (role concrete)
  (slot one (type STRING))
  (slot two (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE)))

(definstances TEST (a1 of A) (a2 of A) (a3 of A))

(defrule rule1
  ?obj <- (object (is-a A) (name [a1]))
  =>
  (message-modify-instance ?obj (one "a") (two FALSE))
  (send ?obj print))

(defrule rule2
  ?obj <- (object (is-a A) (name [a2]))
  =>
  (message-modify-instance ?obj (two FALSE) (one "a"))
  (send ?obj print))

(defrule rule3
  ?obj <- (object (is-a A) (name [a3]))
  =>
  (modify-instance ?obj (two FALSE) (one "a"))
  (send ?obj print))
(reset)
(run)
(clear) ;; CLIPSESG Crash Bug

(defrule bug
   (dummy)
   (foo ?x)
   (not (and (huge ?x)
             (not (and (test (eq ?x 1))
                       (bar ?x)))))
   =>)
(reset)
(assert (bar 1))
(assert (huge 1))
(clear) ; SourceForge Bug

(defclass FOO
   (is-a USER)
   (slot _foo (default ?NONE)))
(make-instance foo1 of FOO)
(make-instance foo2 of FOO (_foo))
(clear) ; SourceForge Assert/Clear Bug
(defrule foo (a ?x&:(progn (clear) TRUE)) =>)
(assert (a 1))
(send [initial-object] delete)
(assert (a 2))
(clear)
(assert (a (clear)))
(clear)
(deffacts FOO (foo bar) (foo (clear)))
(reset)
(clear)
(assert-string "(a (clear))")
(clear)
(deftemplate foo (multislot x))
(assert (foo (x (clear) 1)))
(facts)
(modify 1 (x (clear) 2))
(facts)
(clear) ; SourceForge Bug

(defmodule FOO
   (export ?ALL))
   
(defclass FOO::DUMMY 
   (is-a USER)
   (slot foo))
   
(defmodule BAR
   (import FOO ?ALL))

(defclass BAR::BAR 
   (is-a USER)
   (slot bar (allowed-classes DUMMY)))
(set-dynamic-constraint-checking TRUE)

(make-instance b of BAR
   (bar (make-instance f of DUMMY)))
(set-dynamic-constraint-checking FALSE)   
(clear) ; CLIPSESG Bug

(deffunction generate (?a ?c)
   (str-cat ?a ?c))

(deffunction gm1 ()
   (progn$ (?ctype (create$ aaa))
      (generate 2 ?ctype)))

(deffunction gm2 ()
   (bind ?ctype aaa)
   (generate 2 ?ctype))
(gm1)
(gm2)
   
(clear) ; Dangling constructs

(progn
   (clear)
   (build "(defrule foo (count) =>)")
   (assert (count)))
(deftemplate blah (slot x))

(progn 
   (clear)
   (assert (blah (x 1))))
   
(defclass BLAH (is-a USER) (slot x))

(progn
   (clear)
   (make-instance of BLAH (x 1)))
(deffunction yabbo () (printout t "Hello, world!" crlf))

(progn (clear)
       (yabbo))

(defmethod blah ((?x STRING))
   (printout t ?x crlf))

(progn (clear)
       (blah "Hello, world!"))
(clear) ; Sourceforge bug
(funcall str-cat)

(deffunction bar1 (?func)
   (bind $?a (create$))
   (funcall ?func (expand$ $?a)))
(bar1 "str-cat")
(clear)

(defclass BOO (is-a USER)
   (multislot foo (cardinality -1 0)))
   
(defclass BOO (is-a USER)
   (multislot foo (cardinality 0 -3)))
(clear) ; Continuous operation issue
(defglobal ?*num* = 37)
(defglobal ?*val* = FALSE)

(deffunction get-number ()
   (bind ?*num* (+ ?*num* 1)))

(deffunction muck ()
   (bind ?*val* (create$ (get-number) (get-number))))

(deffacts startup
   (muck-around))
   
(defrule muck-around
   ?f0 <- (muck-around)
   =>
   (retract ?f0) 
   (muck)
   (assert (muck-around)))
(reset)
(run 1)
?*val*
(clear) ; SourceForge Crash Bug

(deftemplate table
   (slot table-id (type INTEGER)))

(deftemplate modeler-instance
   (slot class (type SYMBOL) (default ?NONE))
   (slot id (type SYMBOL) (default ?NONE)))

(deftemplate table-modeler-binding
   (slot modeler (type SYMBOL))
   (slot table-id))

(deffacts start
   (table (table-id 100002))
   (table (table-id 100003))
   (modeler-instance (class TIME-PROFILER) (id gen4)) 
   (table-modeler-binding (modeler gen4) (table-id 100003)) 
   (modeler-instance (class TIME-PROFILER) (id gen6))
   (table-modeler-binding (modeler gen6) (table-id 100002)))

(defrule mark   
   (modeler-instance (id ?m1))
   (modeler-instance (id ?m2&~?m1))
   (not (and (table-modeler-binding (modeler ?m1) (table-id ?t1))
             (table-modeler-binding (modeler ?m2) (table-id ?t2&~?t1))
             (table (table-id ?t1))
             (table (table-id ?t2))))
   (not (and
             (table-modeler-binding (modeler ?m2) (table-id ?t3))
             (table-modeler-binding (modeler ?m1) (table-id ?t4&~?t3))
             (table (table-id ?t4))))
   =>)

(defrule remove 
   =>)
(reset)
(matches mark)
(retract 2)
(matches mark)
(retract 3)
(matches mark)
(clear)
(clear) ; DR #882
(watch activations)

(defrule if 
   (not (and (not (and (A) (B)))
             (C)))
   (not (and (SAD ?v)
             (SAD ?v)))
   =>)
(assert (SAD 2))
(clear)

(defrule if 
    (and  
        (exists 
            (SAD T ?tx1 T01 ?t01)
            (SAD T ?tx1 T02 ?t02)
            (or  
                (test (not (not (str-index  "ABCD" ?t01)))) 
                (test (not (not (str-index  "ABCD" ?t02)))))) 
        (exists 
            (SAD G ?gx1 G02N ?g02n)
            (and  
                (test (eq (str-index  "9900" ?g02n) 1)) 
                (exists 
                    (SAD T ?tx2 T08 ?t08)
                    (SAD G ?gx1 G01 ?g01)
                    (or  
                        (test (<= ?t08 0)) 
                        (test (= ?t08 ?g01)))))))
   =>)
(assert (SAD G 2 G01 2))
(assert (SAD G 2 G02N "99009000"))
(assert (SAD T 3 T01 "ABCD XYX"))
(assert (SAD T 3 T02 "XYZ CDE"))
(assert (SAD T 3 T08 2))
(unwatch activations)
(clear) ; Matches issue
(defmodule MAIN (export ?ALL))
(deffacts start (a) (b) (c))
(defmodule A (import MAIN ?ALL))
(defrule A::foo (a) =>)
(defmodule B (import MAIN ?ALL))
(defrule B::foo (b) =>)
(defmodule C (import MAIN ?ALL))
(defrule C::foo (c) =>)
(reset)
(matches A::foo)
(matches B::foo)
(matches C::foo)
(set-current-module MAIN)
(matches A::foo)
(clear) ; SourceForge Bug

(defrule bug 
   (A)
   (B ?cot)     
   (not (and (X)  
             (C ?cot)))
   (not (and (D ?cot) 
             (not (Z))))
   =>)
(watch activations)
(assert (B R))
(assert (B C))
(assert (D C))
(assert (A)))
(agenda)
(unwatch activations)
(clear) ; SourceForge Bug
(deftemplate C (slot x))
(deftemplate D (slot x))

(defrule if ""
    (not 
         (and 
              (not 
                   (not 
                        (and (not (and (W) 
                                       (X)))
                             (not (and (Y) 
                                       (Z)))
                        )
                   )  
              )
              (C (x ?ix_t))
              (D (x ?ix_t))
         ) 
    )
   =>)
(assert (C (x 1)))
(clear) ; Memory leak
(deftemplate TAG (multislot attributes)))
(assert (TAG))
(modify 1 (attributes (create$)))
(clear) ; Load Crash

(defrule bug
   (X ?a)
   (Y ?b)
   (not (and (not (and (A ?a)      
                       (B ?b)))    
             (test (eq ?a ?b))))  
   (Z)
   =>)
(assert (Z)) 
(agenda)
(assert (X 1))
(agenda)
(assert (Y 2))
(agenda)
(assert (X 2))
(agenda)
(assert (Y 1))
(agenda)
(assert (A 1))
(agenda)
(assert (B 2))
(agenda)
(assert (A 2))
(agenda)
(assert (B 1))
(agenda)
(clear) ; Load Crash

(defrule bug
    (Surname ?surname_1)
    (PersonSurname ?PersonSurname_1)
    (exists 
        (or  
            (and  
                (exists 
                    (Surname ?Surname_2)
                    (LVAR two ?two)
                    (test (eq ?Surname_2 ?two))) 
                (test (eq ?surname_1 ?PersonSurname_1))) 
            (and  
                (exists 
                    (Surname ?Surname_3)
                    (LVAR three ?three)
                    (test (eq ?Surname_3 ?three))))))
=>)
(clear) ; DR0882

(defrule foo
   (logical (test (> 4 3))
            (a))
   =>
   (assert (b)))
(watch facts)
(assert (a))
(run)
(retract 1)
(unwatch facts) 
(clear) ; CLIPSESG Bug
(watch activations)
(defclass A (is-a USER))

(defrule crash
  (not (object (is-a A)))
  (object (is-a A))
  =>)
(make-instance test1 of A)
(unmake-instance [test1])
(run)
(clear)
(deftemplate A)

(defrule crash
  (not (A))
  (A)
  =>)
(assert (A))
(retract 1)
(unwatch activations)
(clear) ; CLIPSESG Bug
(watch activations)

(defclass A
  (is-a USER)
  (slot a))

(defrule test
  (not (object (is-a A) (a 1)))
  (object (is-a A) (a 1))
  =>)
(make-instance [a1] of A (a 1))
(modify-instance [a1] (a 2))
(agenda)
(unwatch activations)
(clear) ; SourceForge Ticket #14
(watch facts)
(deftemplate foo (multislot x))
(deffacts start (foo (x 1 2)) (foo (x a)))
(reset)

(do-for-fact ((?f foo)) TRUE
  (retract ?f)
  (bind ?x ?f:x)
  (assert (foo (x $?x 3))))
(reset)
(do-for-all-facts ((?f foo)) TRUE
  (retract ?f)
  (printout t ?f " " ?f:x crlf))
  
(unwatch facts)
(clear)  
(watch instances)
(watch slots)
(defclass FOO (is-a USER) (multislot x))

(definstances start
   ([f1] of FOO (x 1 2))
   ([f2] of FOO (x a)))
(reset)

(do-for-instance ((?f FOO)) TRUE
  (send ?f delete)
  (bind ?x ?f:x)
  (make-instance [f3] of FOO (x $?x 3)))
(reset)

(do-for-all-instances ((?f FOO)) TRUE
  (send ?f delete)
  (printout t ?f " " ?f:x crlf))
(unwatch all)
(clear) ; Indentation depth overflow
(defrule foo
   =>
   (if (eq 3 3)
      then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then (if (eq 3 3) then
      (if (eq 3 3) then 3)))))))))))))))))))))))))))))))))))))))))))
(clear) ; Crash bug

(deftemplate foobar
  (slot foo)
  (multislot bar))
(assert (foobar)))
(modify 1 (dummy TRUE))
(clear) ; Logical CE Issue
(deftemplate A (slot val))
(deftemplate B)
(deftemplate C)
(deftemplate D)

(deffacts infinite_setup
  (A (val 1)))

(defrule infinite_rule
  (logical
    (A (val ?val))
    (not (and (B) (C)))
    (test (eq ?val 1)))
  (not (D))
  =>
  (assert (D)))

(reset)
(watch rules)
(watch facts)
(watch activations)
(run 1)
(unwatch all)
(clear)
(deftemplate A (slot val))
(deftemplate B)
(deftemplate C)
(deftemplate D)

(deffacts infinite_setup
  (A (val 1)))

(defrule infinite_rule
  (logical
    (A (val ?val))
    (not (and (B) (C)))
    (test (eq ?val 1))
    (not (D)))
  =>
  (assert (D)))

(reset)
(watch rules)
(watch facts)
(watch activations)
(run 1)
(unwatch all)
(clear) ; Funcall module specifier #1

(defmodule M
  (export deffunction ?ALL))
(deffunction foo (?x) (printout t ?x " in M" crlf))
(defmodule MAIN)
(deffunction foo (?x) (printout t ?x " in MAIN" crlf))
(funcall foo bar)
(funcall M::foo baz)
(funcall MAIN::foo baz)
(clear) ; Funcall module specifier #2

(defmodule M
  (export deffunction ?ALL))
(deffunction foo (?x) (funcall ?x))
(deffunction bar () (printout t "bar in M" crlf))
(defmodule MAIN (export deffunction ?ALL))
(deffunction bar () (printout t "bar in MAIN" crlf))
(M::foo bar)
(M::foo MAIN::bar)
(M::foo M::bar)
(clear) ; Funcall module specifier #3

(defmodule M
  (export deffunction ?ALL))
(deffunction foo (?x) (funcall ?x))
(deffunction bar () (printout t "bar in M" crlf))
(defmodule MAIN)
(deffunction bar () (printout t "bar in MAIN" crlf))
(M::foo bar)
(M::foo MAIN::bar)
(M::foo M::bar)
(clear) ; Foreach bug
(foreach ?a (create$ a b c)
            (foreach ?b (create$ x y) (printout t ?a " " ?b crlf)))
(progn$ (?a (create$ a b c)) 
            (progn$ (?b (create$ x y)) (printout t ?a " " ?b crlf)))
(clear)
(defglobal ?*crash* = (create$ 1 2 3 etc))
(bind ?*crash* (rest$ ?*crash*))
(clear) ; SourceForge Ticket #49
(::)
(clear) ; SourceForge Ticket #54 Extraneous Module Specifier
(defmodule EXTRANEOUS::COMPUTE)
(deftemplate MAIN::EXTRANEOUS::point)
(deffacts MAIN::EXTRANEOUS::points)
(defrule MAIN::EXTRANEOUS::find-point =>)
(defclass MAIN::EXTRANEOUS::POINT (is-a USER))
(defclass MAIN::POINT (is-a USER))
(defmessage-handler MAIN::EXTRANEOUS::POINT add-points ())
(definstances MAIN::EXTRANEOUS::points)
(deffunction MAIN::EXTRANEOUS::add-point (?x ?y))
(defgeneric MAIN::EXTRANEOUS::add-point)
(defmethod MAIN::EXTRANEOUS::add-point ((?x FLOAT) (?y FLOAT)))
(clear) ; bsave-instances external-address issue

(defclass EXPERIMENT 
   (is-a USER)
   (slot fa (type FACT-ADDRESS))
   (slot ia (type INSTANCE-ADDRESS))
   (slot ea (type EXTERNAL-ADDRESS)))
   
(make-instance e1 of EXPERIMENT
   (fa (assert (b)))
   (ia (instance-address(make-instance e2 of EXPERIMENT))))
(bsave-instances "Temp//experiment.bins")
(reset)
(bload-instances "Temp//experiment.bins")
(send [e1] print)
(send [e2] print)
(clear) ; Local variables cannot be accessed bug

(deftemplate hello
  (slot self (type FACT-ADDRESS)))
  
(deffacts hellos
   (hello))
(clear) ; SourceForge Ticket #56

(deftemplate maze
   (multislot open-list)
   (slot goal))

(defrule test-1
   (maze (open-list)
         (goal ?g&nil))
   =>)

(defrule test-2
   (maze (open-list) 
         (goal ?g&:(eq ?g nil)))
   =>)

(defrule test-3
   (maze (open-list) 
         (goal ~nil))
   =>)
(clear) ; SourceForge Ticket #58

(defclass FOO (is-a USER)
  (slot ins (type INSTANCE-ADDRESS)))
(make-instance [foo] of FOO)
(timetag (send [foo] get-ins))

(deftemplate foo
  (slot fct (type FACT-ADDRESS)))
(assert (foo))   
(timetag (fact-slot-value 1 fct))
(clear) ; 2 variable comparison from right memory

(deftemplate manifest
   (slot origin)
   (slot destination)) 

(deftemplate shipzone
    (slot zonename)
    (multislot cities)
    (slot city1)
    (slot city2))

(deffacts fact-data
    (manifest (origin CHI) (destination WAS)) 
    (manifest (origin HOU) (destination ATL)) 
    (shipzone (cities CHI WAS BOS) (city1 CHI) (city2 WAS))
    (shipzone (cities BOS HOU ATL)))

(defclass MANIFEST
   (is-a USER)
   (slot origin)
   (slot destination)) 

(defclass SHIPZONE
   (is-a USER)
   (slot zonename)
   (multislot cities)
   (slot city1)
   (slot city2))

(definstances instance-data
    (m1 of MANIFEST (origin CHI) (destination WAS)) 
    (m2 of MANIFEST (origin HOU) (destination ATL)) 
    (s1 of SHIPZONE (cities CHI WAS BOS) (city1 CHI) (city2 WAS))
    (s2 of SHIPZONE (cities BOS HOU ATL)))

(defrule city-group-f1
    (manifest (origin ?x1) (destination ?x2))
    (shipzone (city1 ?x3&?x1|?x2) (city2 ~?x3&?x1|?x2))
    =>)

(defrule city-group-f2
    (manifest (origin ?x1) (destination ?x2))
    (shipzone (cities $? ?x3&?x1|?x2 ~?x3&?x1|?x2))
    =>)

(defrule city-group-f3
    (manifest (origin ?x1) (destination ?x2))
    (shipzone (cities $? ?x3&?x1|?x2 ~?x3&?x1|?x2 $?))
    =>)

(defrule city-group-f4
    (manifest (origin ?x1) (destination ?x2))
    (shipzone (cities $? ?x3&?x1|?x2 $?) (city1 ~?x3&?x1|?x2))
    =>)

(defrule city-group-i1
    (object (is-a MANIFEST) (origin ?x1) (destination ?x2))
    (object (is-a SHIPZONE) (city1 ?x3&?x1|?x2) (city2 ~?x3&?x1|?x2))
    =>)
    
(defrule city-group-i2 
    (object (is-a MANIFEST) (origin ?x1) (destination ?x2))
    (object (is-a SHIPZONE) (cities $? ?x3&?x1|?x2 ~?x3&?x1|?x2))
    =>)

(defrule city-group-i3
    (object (is-a MANIFEST) (origin ?x1) (destination ?x2))
    (object (is-a SHIPZONE) (cities $? ?x3&?x1|?x2 ~?x3&?x1|?x2 $?))
    =>)
    
(defrule city-group-i4 
    (object (is-a MANIFEST) (origin ?x1) (destination ?x2))
    (object (is-a SHIPZONE) (cities $? ?x3&?x1|?x2 $?) (city1 ~?x3&?x1|?x2))
    =>)
(reset)
(agenda)
(clear) ; Error line count issue
(load line_error_crlf.clp)
(clear)
(load line_error_lf.clp)
(clear) ; CLIPSESG https://groups.google.com/forum/#!topic/CLIPSESG/j4fmacgmAZY

(deftemplate example
   (slot value
      (type SYMBOL)
      (allowed-symbols FALSE TRUE)))

(defrule attempt-to-construct-example
   ?f <- (line ?line)
   =>
   (retract ?f)
   (assert (example (value (eq ?line "")))))
(clear) ; CLIPSESG https://groups.google.com/forum/#!topic/CLIPSESG/YaNnNF1xg5w

(defclass OAV
   (is-a USER)
   (slot object)
   (slot attribute)
   (multislot values))

(make-instance oav1 of OAV
   (object Fred)
   (attribute hobbies)
   (values reading hiking chess))
(bsave-instances "Temp//drins.bin")
(reset)
(bload-instances "Temp//drins.bin")
(send [oav1] print)
(clear) ; Object pattern matching underflow

(deftemplate range
   (slot start)
   (slot finish))
             
(deftemplate sequence
   (multislot elements))
(defclass container
   (is-a USER)
   (multislot contents))

(deffacts defranges
   (range (start 2) (finish 11))
   (range (start 12) (finish 19)))
          
(deffacts defsequences
   (sequence (elements 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))

(definstances containers
   (of container 
      (contents 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
   (of container
      (contents)))

(defrule eliminate-entire-range
   (range (start ?start) 
          (finish ?finish))
   (sequence (elements ?start $? ?finish))
   (object (is-a container)
           (contents $?before ?start $? ?finish $?after)
           (name ?name))
   =>
   (modify-instance ?name
      (contents $?before 
      $?after)))
(reset)
(run)
(clear) ; SourceForge Ticket #61
(defclass FOO (is-a USER))
(deftemplate FOO)

(deffunction test1()
  (bind ?ins (make-instance of FOO))
  (delayed-do-for-all-instances ((?f FOO)) TRUE
    (str-cat "abc" "def")))

(deffunction test2()
  (assert(FOO))
  (delayed-do-for-all-facts ((?f FOO)) TRUE
    (str-cat "uvw" "xyz")))

(deffunction test3()
  (bind ?ins (make-instance of FOO))
  (do-for-all-instances ((?f FOO)) TRUE
    (str-cat "abc" "def")))

(deffunction test4()
  (assert(FOO))
  (do-for-all-facts ((?f FOO)) TRUE
    (str-cat "uvw" "xyz")))
(test1)
(test2)
(test3)
(test4)
(clear) ; SourceForge Ticket #64

(deftemplate adrs
   (slot ia (type INSTANCE-ADDRESS))
   (slot fa (type FACT-ADDRESS))
   (slot ea (type EXTERNAL-ADDRESS)))

(defclass ADRS (is-a USER)
   (slot ia (type INSTANCE-ADDRESS))
   (slot fa (type FACT-ADDRESS))
   (slot ea (type EXTERNAL-ADDRESS)))

(deffacts start
   (adrs))
   
(definstances start
   ([adrs] of ADRS))
(bsave "Temp//ea1.bin")
(set-dynamic-constraint-checking TRUE)
(bsave "Temp//ea2.bin")
(clear)
(bload "Temp//ea1.bin")
(assert (adrs))
(make-instance [adrs] of ADRS)
(ppfact 1)
(send [adrs] print)
(reset)
(ppfact 1)
(send [adrs] print)
(clear)
(bload "Temp//ea2.bin")
(assert (adrs))
(make-instance [adrs] of ADRS)
(ppfact 1)
(send [adrs] print)
(reset)
(ppfact 1)
(send [adrs] print)
(set-dynamic-constraint-checking FALSE)
(clear) ; SourceForge Ticket #65
(ppdefclass undefined nil)
(ppdeftemplate undefined nil)
(ppdefrule undefined nil)
(ppdeffacts undefined nil)
(ppdefinstances undefined nil)
(ppdeffunction undefined nil)
(ppdefgeneric undefined nil)
(ppdefglobal undefined nil)
(clear)
(defglobal ?*x* = (create$ 1 2 3 4))
(defglobal ?*y* = (subseq$ ?*x* 2 3))
(defglobal ?*z* = (subseq$ (create$ 1 2 3 4) 2 3))
?*x*
?*y*
?*z*
(clear)
