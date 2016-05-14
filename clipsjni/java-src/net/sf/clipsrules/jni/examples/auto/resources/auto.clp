
;;;======================================================
;;;   Automotive Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a car.
;;;
;;;     CLIPS Version 6.3 Example
;;;
;;;     For use with the Auto Demo Example
;;;======================================================

;;;*****************
;;;* Configuration *
;;;*****************
   
(defglobal ?*target* = gui) ; console, cgi, or gui

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate MAIN::text-for-id
   (slot id)
   (slot text))

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (multislot display-answers)
   (slot state (default middle)))
   
;;;***************************
;;;* DEFFUNCTION DEFINITIONS *
;;;***************************

(deffunction MAIN::find-text-for-id (?id)
   ;; Search for the text-for-id fact
   ;; with the same id as ?id
   (bind ?fact
      (find-fact ((?f text-for-id))
                  (eq ?f:id ?id)))
   (if ?fact
      then
      (fact-slot-value (nth$ 1 ?fact) text)
      else
      ?id))
      
(deffunction MAIN::translate-av (?values)
   ;; Create the return value
   (bind ?result (create$))
   ;; Iterate over each of the allowed-values
   (progn$ (?v ?values)
      ;; Find the associated text-for-id fact
      (bind ?nv
         (find-text-for-id ?v))
      ;; Add the text to the return value
      (bind ?result (create$ ?result ?nv)))
   ;; Return the return value
   ?result)

(deffunction MAIN::replace-spaces (?str)
   (bind ?len (str-length ?str))
   (bind ?i (str-index " " ?str))
   (while (neq ?i FALSE)
      (bind ?str (str-cat (sub-string 1 (- ?i 1) ?str) "-" (sub-string (+ ?i 1) ?len ?str)))
      (bind ?i (str-index " " ?str)))
   ?str)

(deffunction MAIN::sym-cat-multifield (?values)
   (bind ?rv (create$))
   (progn$ (?v ?values)
      (bind ?rv (create$ ?rv (sym-cat (replace-spaces ?v)))))
   ?rv)

(deffunction MAIN::multifield-to-delimited-string (?mv ?delimiter)
   (bind ?rv "")
   (bind ?first TRUE)
   (progn$ (?v ?mv)
      (if ?first
         then
         (bind ?first FALSE)
         (bind ?rv (str-cat ?v))
         else
         (bind ?rv (str-cat ?rv ?delimiter ?v))))
   ?rv)

;;;*****************
;;;* STATE METHODS *
;;;*****************
      
;;; Console target
   
(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target console))
                         (?display LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (printout t ?display crlf)
   (str-assert (str-cat "(" ?relation-asserted " " yes ")")))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target console))
                         (?question LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE) ; default
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (bind ?display-answers (sym-cat-multifield ?display-answers))
   (format t "%s " ?question)
   (printout t ?display-answers " ")
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?display-answers)) do
      (format t "%s " ?question)
      (printout t ?display-answers " ")
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   (bind ?pos (member$ ?answer ?display-answers))
   (bind ?answer (nth$ ?pos ?valid-answers))
   (str-assert (str-cat "(" ?relation-asserted " " ?answer ")")))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target console))
                         (?display LEXEME))
   (assert (conclusion))
   (printout t ?display crlf)
   (halt))

;;; CGI target

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target cgi))
                         (?display LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (printout t "state=greeting" crlf)
   (printout t "display=" ?display crlf)
   (printout t "variable=greeting" crlf)
   (printout t "validAnswers=yes" crlf)
   (printout t "displayAnswers=yes" crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target cgi))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (printout t "state=interview" crlf)
   (printout t "display=" ?message crlf)  
   (printout t "variable=" ?relation-asserted crlf)
   (printout t "validAnswers=" (multifield-to-delimited-string ?valid-answers ":") crlf)
   (printout t "displayAnswers=" (multifield-to-delimited-string ?display-answers ":") crlf) 
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target cgi))
                         (?display LEXEME))
   (printout t "state=conclusion" crlf)
   (printout t "display=" ?display crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

;;; GUI target (iOS and JNI)

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted greeting)
                     (state ?state)
                     (valid-answers yes)
                     (display-answers yes)))
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (response ?response)
                     (valid-answers ?valid-answers)
                     (display-answers ?display-answers)))
   (halt))
 
(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target gui))
                         (?display LEXEME))
   (assert (UI-state (display ?display)
                     (state ?state)
                     (valid-answers)
                     (display-answers)))
   (halt))

;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""
  (not (greeting yes))
  =>
  (handle-state greeting
                ?*target*
                (find-text-for-id WelcomeMessage)
                greeting
                (create$)))
  
;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-engine-state ""

   (greeting yes)
   (not (engine-starts ?))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id StartQuestion)
                 engine-starts
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
   
(defrule determine-runs-normally ""

   (engine-starts yes)
   (not (runs-normally ?))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id RunQuestion)
                 runs-normally
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-rotation-state ""

   (engine-starts no)
   (not (engine-rotates ?))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id RotateQuestion)
                 engine-rotates
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
   
(defrule determine-sluggishness ""

   (runs-normally no)
   (not (engine-sluggish ?))

   =>
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id SluggishQuestion)
                 engine-sluggish
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
   
(defrule determine-misfiring ""

   (runs-normally no)
   (not (engine-misfires ?))

   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id MisfireQuestion)
                 engine-misfires
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-knocking ""

   (runs-normally no)
   (not (engine-knocks ?))

   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id KnockQuestion)
                 engine-knocks
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-low-output ""

   (runs-normally no)
   (not (engine-output-low ?))

   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id OutputQuestion)
                 engine-output-low
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-gas-level ""

   (engine-starts no)
   (engine-rotates yes)
   (not (tank-has-gas ?))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id GasQuestion)
                 tank-has-gas
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-battery-state ""
  
   (engine-rotates no)
   (not (battery-has-charge ?))

   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id BatteryQuestion)
                 battery-has-charge
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-point-surface-state ""

   (or (and (engine-starts no)  
            (engine-rotates yes))
       (engine-output-low yes))
   (not (point-surface-state ?))
   
   =>

   (bind ?answers (create$ normal burned contaminated))
   (handle-state interview
                 ?*target*
                 (find-text-for-id PointsQuestion)
                 point-surface-state
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-conductivity-test ""
   
   (engine-starts no)  
   (engine-rotates no)
   (battery-has-charge yes)
   (not (conductivity-test-positive ?))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CoilQuestion)
                 conductivity-test-positive
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule normal-engine-state-conclusions ""
   (declare (salience 10))
   (runs-normally yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id NoRepair)))
 
(defrule engine-sluggish ""
   (declare (salience 10))
   (engine-sluggish yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id FuelLineRepair)))

(defrule engine-misfires ""
   (declare (salience 10))
   (engine-misfires yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id PointGapRepair)))

(defrule engine-knocks ""
   (declare (salience 10))
   (engine-knocks yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id AdjustTimingRepair)))

(defrule tank-out-of-gas ""
   (declare (salience 10))
   (tank-has-gas no)
   =>
   (handle-state conclusion ?*target* (find-text-for-id AddGasRepair)))
   
(defrule battery-dead ""
   (declare (salience 10))
   (battery-has-charge no)
   =>
   (handle-state conclusion ?*target* (find-text-for-id ReplaceBatteryRepair)))

(defrule point-surface-state-burned ""
   (declare (salience 10))
   (point-surface-state burned)
   =>
   (handle-state conclusion ?*target* (find-text-for-id ReplacePointsRepair)))

(defrule point-surface-state-contaminated ""
   (declare (salience 10))
   (point-surface-state contaminated)
   =>
   (handle-state conclusion ?*target* (find-text-for-id CleanPointsRepair)))

(defrule conductivity-test-positive-yes ""
   (declare (salience 10))
   (conductivity-test-positive yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id LeadWireRepair)))
                     
(defrule conductivity-test-positive-no ""
   (declare (salience 10))
   (conductivity-test-positive no)
   =>
   (handle-state conclusion ?*target* (find-text-for-id CoilRepair)))
                     
(defrule no-repairs ""
   (declare (salience -10))
   (not (conclusion))
   =>
   (handle-state conclusion ?*target* (find-text-for-id MechanicRepair)))
  
