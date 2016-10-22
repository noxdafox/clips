;;; The Waltz Algorithm implemented using CLIPS
;;; The original source for the OPS5 benchmark suite is available
;;; at http://www.cs.utexas.edu/ftp/pub/ops5-benchmark-suite/

;;; The copyright notice from the original code:
 
;;; Copyright C 1991 Satoshi Nishiyama and the University of Texas at Austin
;;; anyone may copy and use this program the terms listed in the copyright
;;; notice in and covering the Communications of the ACM, the program is 
;;; is not warranted to do anything

;;; Changes made for the CLIPS version
;;;
;;;   Reformatted code
;;;   Converted C code to deffunctions
;;;   Uses salience to obtain a number of rule firings consistent
;;;     with other rule engines running this benchmark 
 
;;; ##########################################################################
;;; Deftemplates  
;;;
;;; Lines have the lable line followed by the 2 points defining the line.  
;;;
;;; Edges are like lines accept that they can be labeled, permanently labeled,
;;; and plotted.  
;;;
;;; Junctions are defined by 4 points.  The basepoint is where the 3 (2) lines 
;;; intersect.  The points p1, p2, p3 are the other endpoints of the lines at 
;;; this junction
;;; ##########################################################################
 
(deftemplate stage 
   (slot value))
   
(deftemplate line 
   (slot p1) 
   (slot p2))
   
(deftemplate edge
   (slot p1)
   (slot p2)
   (slot joined)
   (slot label)
   (slot plotted))
   
(deftemplate junction 
   (slot p1)
   (slot p2)
   (slot p3)
   (slot base_point)
   (slot type))

;;; #################################################################
;;; Deffunctions  
;;; 
;;; In the OPS5 version of this program, the following deffunctions 
;;; were implemented in C code that needed to be linked with the OPS5 
;;; interpreter. Using deffunctions allows the benchmark to be run  
;;; without having to recompile and relink CLIPS with the C code used 
;;; in the original benchmark. The performance impact of using 
;;; deffunctions for this benchmark rather than C code is minimal.
;;; #################################################################

(defglobal ?*MOD-NUM* = 100)

;;; *****
;;; atan2
;;; *****

(deffunction atan2 (?y ?x)
   (if (> ?x 0) 
      then
      (return (atan (/ ?y ?x))))
         
   (if (< ?x 0) 
      then
      (return (- (atan (/ ?y ?x)) (pi))))
      
   (if (> ?y 0) then
      then
      (return (pi)))

   (if (< ?y 0)
      then
      (return (- 0 (pi))))
      
   (return undefined))
    
;;; *****
;;; get-y
;;; *****

(deffunction get-y (?val)
   (mod ?val ?*MOD-NUM*))
   
;;; *****
;;; get-x
;;; *****

(deffunction get-x (?val)
   (integer (/ ?val ?*MOD-NUM*)))
   
;;; *********
;;; get-angle
;;; *********

(deffunction get-angle (?p1 ?p2)
   (bind ?delta-x (- (get-x ?p2) (get-x ?p1)))
   (bind ?delta-y (- (get-y ?p2) (get-y ?p1)))
   (if (= ?delta-x 0)
      then
       (if (> ?delta-y 0)
		  then (return (/ (pi) 2))
		  else (if (< ?delta-y 0) then (return (/ (pi) -2))))
      else
      (if (= ?delta-y 0)
         then
         (if (> ?delta-x 0)
            then (return 0.0)
		    else (if (< ?delta-x 0) then return (pi)))
         else
         (return (atan2 ?delta-y ?delta-x)))))
   
;;; ***************
;;; inscribed-angle
;;; ***************

(deffunction inscribed-angle (?basepoint ?p1 ?p2)
	
	;; Get the angle between line #1 and the origin and the angle
	;; between line #2 and the origin, and then subtract these values. 

   (bind ?angle1 (get-angle ?basepoint ?p1))
   (bind ?angle2 (get-angle ?basepoint ?p2))
   (bind ?temp (- ?angle1 ?angle2))
      
   (if (< ?temp 0) 
      then (bind ?temp (- 0 ?temp)))
   
   ;; We always want the smaller of the two angles inscribed, so 
   ;; if the answer is greater than 180 degrees, calculate the 
   ;; smaller angle and return it. 

   (if (> ?temp (pi))
      then
      (bind ?temp (- (* 2 (pi)) ?temp)))
      
   (if (< ?temp 0)
      then (return (- 0 ?temp)))
      
   (return ?temp))

;;; ***************
;;; make-3-junction
;;; ***************

(deffunction make-3-junction (?basepoint ?p1 ?p2 ?p3)
   (bind ?angle12 (inscribed-angle ?basepoint ?p1 ?p2))
   (bind ?angle13 (inscribed-angle ?basepoint ?p1 ?p3))
   (bind ?angle23 (inscribed-angle ?basepoint ?p2 ?p3))
   
   (bind ?sum1213 (+ ?angle12 ?angle13))
   (bind ?sum1223 (+ ?angle12 ?angle23))
   (bind ?sum1323 (+ ?angle13 ?angle23))
   
   (if (< ?sum1213 ?sum1223)
      then
      (if (< ?sum1213 ?sum1323)
         then
         (bind ?sum ?sum1213)
         (bind ?shaft ?p1)
         (bind ?barb1 ?p2)
         (bind ?barb2 ?p3)
         else
         (bind ?sum ?sum1323)
         (bind ?shaft ?p3)
         (bind ?barb1 ?p1)
         (bind ?barb2 ?p2))
      else
      (if (< ?sum1223 ?sum1323)
         then
         (bind ?sum ?sum1223)
         (bind ?shaft ?p2)
         (bind ?barb1 ?p1)
         (bind ?barb2 ?p3)
         else
         (bind ?sum ?sum1323)
         (bind ?shaft ?p3)
         (bind ?barb1 ?p1)
         (bind ?barb2 ?p2)))
         
   (bind ?delta (- ?sum (pi)))
   (if (< ?delta 0)
      then (bind ?delta (- 0 ?delta)))
      
   (if (< ?delta 0.001)
      then (bind ?type tee)
      else
      (if (> ?sum (pi))
         then (bind ?type fork)
         else (bind ?type arrow)))
 
   (assert (junction (p1 (integer ?barb1))
                     (p2 (integer ?shaft))
                     (p3 (integer ?barb2))
                     (base_point (integer ?basepoint))
                     (type ?type))))

;;; ########
;;; Defrules  
;;; ########

;;; *******************************************************
;;; begin: Our starting production. It checks to see if the 
;;;   start flag is in WM, and if it is, it deletes it
;;; *******************************************************

(defrule begin
	?f1 <- (stage (value start))
	=>
	(assert (line (p1 0122) (p2 0107)))
	(assert (line (p1 0107) (p2 2207)))
	(assert (line (p1 2207) (p2 3204)))
	(assert (line (p1 3204) (p2 6404)))
	(assert (line (p1 2216) (p2 2207)))
	(assert (line (p1 3213) (p2 3204)))
	(assert (line (p1 2216) (p2 3213)))
	(assert (line (p1 0107) (p2 2601)))
	(assert (line (p1 2601) (p2 7401)))
	(assert (line (p1 6404) (p2 7401)))
	(assert (line (p1 3213) (p2 6413)))
	(assert (line (p1 6413) (p2 6404)))
	(assert (line (p1 7416) (p2 7401)))
	(assert (line (p1 5216) (p2 6413)))
	(assert (line (p1 2216) (p2 5216)))
	(assert (line (p1 0122) (p2 5222)))
	(assert (line (p1 5222) (p2 7416)))
	(assert (line (p1 5222) (p2 5216)))
	(modify ?f1 (value duplicate)))
 
;;; **********************************************************************
;;; reverse_edges: If the duplicate flag is set, and there is still a line
;;;   in WM, delete the line and add two edges. One edge runs from p1 to
;;;   p2 and the other runs from p2 to p1. We then plot the edge.
;;; **********************************************************************

(defrule reverse_edges
	(stage (value duplicate))
	?f2 <- (line (p1 ?p1) (p2 ?p2))
	=>
	(assert (edge (p1 ?p1) (p2 ?p2) (joined false)))
    (assert (edge (p1 ?p2) (p2 ?p1) (joined false)))
	(retract ?f2))
 
;;; **************************************************************************
;;; done_reversing: If the duplicating flag is set, and there are no more 
;;;   lines, then remove the duplicating flag and set the make junctions flag.
;;; **************************************************************************

(defrule done_reversing
	(declare (salience -10))
	?f1 <- (stage (value duplicate))
	(not (line))
	=>
	(modify ?f1 (value detect_junctions)))
 
;;; *****************************************************************************
;;; make-3_junction: If three edges meet at a point and none of them have already 
;;;   been joined in a junction, then make the corresponding type of junction and 
;;;   label the edges joined.  This production calls make-3_junction to determine
;;;   what type of junction it is based on the angles inscribed by the 
;;;   intersecting edges.
;;; *****************************************************************************

(defrule make-3_junction
	(declare (salience 10))
	(stage (value detect_junctions))
	?f2 <- (edge (p1 ?base_point) (p2 ?p1) (joined false))
	?f3 <- (edge (p1 ?base_point) (p2 ?p2&~?p1) (joined false))
	?f4 <- (edge (p1 ?base_point) (p2 ?p3&~?p1&~?p2) (joined false))
	=>
	(make-3-junction ?base_point ?p1 ?p2 ?p3)
	(modify ?f2 (joined true))
	(modify ?f3 (joined true))
	(modify ?f4 (joined true)))
 
;;; ******************************************************
;;; make_L: If two, and only two, edges meet that have not 
;;;   already been joined, then the junction is an "L".
;;; ******************************************************

(defrule make_L
	(stage (value detect_junctions))
	?f2 <- (edge (p1 ?base_point) (p2 ?p2) (joined false))
	?f3 <- (edge (p1 ?base_point) (p2 ?p3&~?p2) (joined false))
	(not (edge (p1 ?base_point) (p2 ~?p2&~?p3)))
	=>
	(assert (junction (type L)
               		  (base_point ?base_point)
		              (p1 ?p2)
		              (p2 ?p3)))
	(modify ?f2 (joined true))
	(modify ?f3 (joined true)))
 
;;; ******************************************************************
;;; done_detecting: If the detect junctions flag is set, and there are 
;;;   no more un_joined edges, set the find_initial_boundary flag.
;;; ******************************************************************

(defrule done_detecting
	(declare (salience -10))
	?f1 <- (stage (value detect_junctions))
	(not (edge (joined false)))
	=>
	(modify ?f1 (value find_initial_boundary)))
 
;;; ****************************************************
;;; initial_boundary_junction_L: If the initial boundary  
;;;   junction is an L, then we know it's labelling
;;; ****************************************************

(defrule initial_boundary_junction_L
	?f1 <- (stage (value find_initial_boundary))
    (junction (type L) 
              (base_point ?base_point) 
              (p1 ?p1)
              (p2 ?p2))
	?f3 <- (edge (p1 ?base_point) (p2 ?p1))
	?f4 <- (edge (p1 ?base_point) (p2 ?p2))
    (not (junction (base_point ?bp&:(> ?bp ?base_point))))
	=>
    (modify ?f3 (label B))
	(modify ?f4 (label B))
	(modify ?f1 (value find_second_boundary)))
 
;;; ***************************************************
;;; initial_boundary_junction_arrow: Ditto for an arrow
;;; ***************************************************

(defrule initial_boundary_junction_arrow
	?f1 <- (stage (value find_initial_boundary))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	?f3 <- (edge (p1 ?bp) (p2 ?p1))
	?f4 <- (edge (p1 ?bp) (p2 ?p2))
	?f5 <- (edge (p1 ?bp) (p2 ?p3))
	(not (junction (base_point ?base_point&:(> ?base_point ?bp))))
	=>
    (modify ?f3 (label B))
	(modify ?f4 (label +))
	(modify ?f5 (label B))
	(modify ?f1 (value find_second_boundary)))
 
;;; ***********************************************************************
;;; second_boundary_junction_L: If we have already found the first boundary 
;;;   point, then find the second boundary point, and label it.
;;; ***********************************************************************
 
(defrule second_boundary_junction_L
	?f1 <- (stage (value find_second_boundary))
    (junction (type L) (base_point ?base_point) (p1 ?p1) (p2 ?p2))
	?f3 <- (edge (p1 ?base_point) (p2 ?p1))
	?f4 <- (edge (p1 ?base_point) (p2 ?p2))
    (not (junction (base_point ?bp&:(< ?bp ?base_point))))
	=>
    (modify ?f3 (label B))
    (modify ?f4 (label B))
    (modify ?f1 (value labeling)))

;;; ******************************
;;; second_boundary_junction_arrow
;;; ******************************

(defrule second_boundary_junction_arrow
	?f1 <- (stage (value find_second_boundary))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	?f3 <- (edge (p1 ?bp) (p2 ?p1))
	?f4 <- (edge (p1 ?bp) (p2 ?p2))
	?f5 <- (edge (p1 ?bp) (p2 ?p3))
	(not (junction (base_point ?base_point&:(< ?base_point ?bp))))
	=>
    (modify ?f3 (label B))
	(modify ?f4 (label +))
	(modify ?f5 (label B))
	(modify ?f1 (value labeling)))
  
;;; **********************************************************************
;;; match_edge: If we have an edge whose label we already know definitely,
;;;   then label the corresponding edge in the other direction
;;; **********************************************************************

(defrule match_edge
	(stage (value labeling))
	?f2 <- (edge (p1 ?p1) (p2 ?p2) (label ?label& + | - | B))
	?f3 <- (edge (p1 ?p2) (p2 ?p1) (label nil))
	=>
	(modify ?f2 (plotted t))
	(modify ?f3 (label ?label) (plotted t)))
	
;;; The following productions propogate the possible labellings of the edges
;;; based on the labellings of edges incident on adjacent junctions.  Since
;;; from the initial boundary productions, we have determined the labellings of
;;; of atleast two junctions, this propogation will label all of the junctions
;;; with the possible labellings.  The search space is pruned due to filtering,
;;; i.e. - only label a junction in the ways physically possible based on the
;;; labellings of adjacent junctions.
 
;;; *******
;;; label_L
;;; *******
 
(defrule label_L
	(stage (value labeling))
	(junction (type L) (base_point ?p1))
	(edge (p1 ?p1) (p2 ?p2) (label + | -))
	?f4 <- (edge (p1 ?p1) (p2 ~?p2) (label nil))
	=>
	(modify ?f4 (label B)))
 
;;; ***********
;;; label_tee_A
;;; ***********

(defrule label_tee_A
	(declare (salience 5))
	(stage (value labeling))
	(junction (type tee) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	?f3 <- (edge (p1 ?bp) (p2 ?p1) (label nil))
	?f4 <- (edge (p1 ?bp) (p2 ?p3))
	=>
    (modify ?f3 (label B))
	(modify ?f4 (label B)))
 
;;; ***********
;;; label_tee_B
;;; ***********

(defrule label_tee_B
	(stage (value labeling))
	(junction (type tee) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	?f3 <- (edge (p1 ?bp) (p2 ?p1))
	?f4 <- (edge (p1 ?bp) (p2 ?p3) (label nil))
	=>
    (modify ?f3 (label B))
	(modify ?f4 (label B)))
 
;;; ************
;;; label_fork-1
;;; ************

(defrule label_fork-1
	(stage (value labeling))
	(junction (type fork) (base_point ?bp))
	(edge (p1 ?bp) (p2 ?p1) (label +))
	?f4 <- (edge (p1 ?bp) (p2 ?p2&~?p1) (label nil))
	?f5 <- (edge (p1 ?bp) (p2 ~?p2&~?p1))
	=>
	(modify ?f4 (label +))
	(modify ?f5 (label +)))
 
;;; ************
;;; label_fork-2
;;; ************

(defrule label_fork-2
	(stage (value labeling))
	(junction (type fork) (base_point ?bp))
	(edge (p1 ?bp) (p2 ?p1) (label B))
	(edge (p1 ?bp) (p2 ?p2&~?p1) (label -))
	?f5 <- (edge (p1 ?bp) (p2 ~?p2&~?p1) (label nil))
	=>
	(modify ?f5 (label B)))
 
;;; ************
;;; label_fork-3
;;; ************

(defrule label_fork-3
	(stage (value labeling))
	(junction (type fork) (base_point ?bp))
	(edge (p1 ?bp) (p2 ?p1) (label B))
	(edge (p1 ?bp) (p2 ?p2&~?p1) (label B))
	?f5 <- (edge (p1 ?bp) (p2 ~?p2&~?p1) (label nil))
	=>
	(modify ?f5 (label -)))
 
;;; ************
;;; label_fork-4
;;; ************

(defrule label_fork-4
	(stage (value labeling))
	(junction (type fork) (base_point ?bp))
	(edge (p1 ?bp) (p2 ?p1) (label -))
	(edge (p1 ?bp) (p2 ?p2&~?p1) (label -))
	?f5 <- (edge (p1 ?bp) (p2 ~?p2&~?p1) (label nil))
	=>
	(modify ?f5 (label -)))
 
;;; **************
;;; label_arrow-1A
;;; **************

(defrule label_arrow-1A
	(declare (salience 5))
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p1) (label ?label& B | - ))
	?f4 <- (edge (p1 ?bp) (p2 ?p2) (label nil))
	?f5 <- (edge (p1 ?bp) (p2 ?p3))
	=>
	(modify ?f4 (label +))
	(modify ?f5 (label ?label)))
 
;;; **************
;;; label_arrow-1B
;;; **************

(defrule label_arrow-1B
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p1) (label ?label& B | - ))
	?f4 <- (edge (p1 ?bp) (p2 ?p2))
	?f5 <- (edge (p1 ?bp) (p2 ?p3) (label nil))
	=>
	(modify ?f4 (label +))
	(modify ?f5 (label ?label)))
 
;;; **************
;;; label_arrow-2A
;;; **************

(defrule label_arrow-2A
	(declare (salience 5))
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p3) (label ?label& B | - ))
	?f4 <- (edge (p1 ?bp) (p2 ?p2) (label nil))
	?f5 <- (edge (p1 ?bp) (p2 ?p1))
	=>
	(modify ?f4 (label +))
	(modify ?f5 (label ?label)))

;;; **************
;;; label_arrow-2B
;;; **************

(defrule label_arrow-2B
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p3) (label ?label& B | - ))
	?f4 <- (edge (p1 ?bp) (p2 ?p2))
	?f5 <- (edge (p1 ?bp) (p2 ?p1) (label nil))
	=>
	(modify ?f4 (label +))
	(modify ?f5 (label ?label)))
 
;;; **************
;;; label_arrow-3A
;;; **************

(defrule label_arrow-3A
	(declare (salience 5))
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p1) (label +))
	?f4 <- (edge (p1 ?bp) (p2 ?p2) (label nil))
	?f5 <- (edge (p1 ?bp) (p2 ?p3))
	=>
	(modify ?f4 (label -))
	(modify ?f5 (label +)))
 
;;; **************
;;; label_arrow-3B
;;; **************

(defrule label_arrow-3B
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p1) (label +))
	?f4 <- (edge (p1 ?bp) (p2 ?p2))
	?f5 <- (edge (p1 ?bp) (p2 ?p3) (label nil))
	=>
	(modify ?f4 (label -))
	(modify ?f5 (label +)))
 
;;; **************
;;; label_arrow-4A
;;; **************
 
(defrule label_arrow-4A
	(declare (salience 5))
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p3) (label +))
	?f4 <- (edge (p1 ?bp) (p2 ?p2) (label nil))
	?f5 <- (edge (p1 ?bp) (p2 ?p1))
	=>
	(modify ?f4 (label -))
	(modify ?f5 (label +)))
 
;;; **************
;;; label_arrow-4B
;;; **************

(defrule label_arrow-4B
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p3) (label +))
	?f4 <- (edge (p1 ?bp) (p2 ?p2))
	?f5 <- (edge (p1 ?bp) (p2 ?p1) (label nil))
	=>
	(modify ?f4 (label -))
	(modify ?f5 (label +)))
 
;;; **************
;;; label_arrow-5A
;;; **************

(defrule label_arrow-5A
	(declare (salience 5))
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p2) (label -))
	?f4 <- (edge (p1 ?bp) (p2 ?p1))
	?f5 <- (edge (p1 ?bp) (p2 ?p3) (label nil))
	=>
	(modify ?f4 (label +))
	(modify ?f5 (label +)))
 
;;; **************
;;; label_arrow-5B
;;; **************

(defrule label_arrow-5B
	(stage (value labeling))
	(junction (type arrow) (base_point ?bp) (p1 ?p1) (p2 ?p2) (p3 ?p3))
	(edge (p1 ?bp) (p2 ?p2) (label -))
	?f4 <- (edge (p1 ?bp) (p2 ?p1) (label nil))
	?f5 <- (edge (p1 ?bp) (p2 ?p3))
	=>
	(modify ?f4 (label +))
	(modify ?f5 (label +)))
  
;;; *******************************************************************************
;;; done_labeling: The conflict resolution mechanism will only execute a production 
;;;   if no productions that are more complicated are satisfied.  This production 
;;;   is simple, so all of the above dictionary productions will fire before this
;;;   change of state production
;;; *******************************************************************************

(defrule done_labeling
	(declare (salience -10))
	?f1 <- (stage (value labeling))
	=>
	(modify ?f1 (value plot_remaining_edges)))
 
;;; **************************************************
;;; plot_remaining: At this point, some labellings may  
;;;   have not been plotted, so plot them
;;; **************************************************

(defrule plot_remaining
	(stage (value plot_remaining_edges))
	?f2 <- (edge (plotted nil) (label ?label&~nil) (p1 ?p1) (p2 ?p2))
	=>
	(modify ?f2 (plotted t)))
 
;;; ********************************************************************************
;;; plot_boundaries: If we have been un able to label an edge, assume that it is a
;;;   boundary. This is a total Kludge, but what the hell. (if we assume only 
;;;   valid drawings will be given for labeling, this assumption generally is true!)
;;; ********************************************************************************

(defrule plot_boundaries
	(stage (value plot_remaining_edges))
	?f2 <- (edge (plotted nil) (label nil) (p1 ?p1) (p2 ?p2))
	=>
	(modify ?f2 (plotted t)))
 
;;; ****************************************************************************
;;; done_plotting: If there is no more work to do, then we are done and flag it.
;;; ****************************************************************************

(defrule done_plotting
	(declare (salience -10))
	?f1 <- (stage (value plot_remaining_edges))
	(not (edge (plotted nil)))
	=>
	(modify ?f1 (value done)))
 