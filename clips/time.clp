;; forward declarations

; public functions
(deffunction after (?event1 ?event2 $?params))
(deffunction before (?event1 ?event2 $?params))

; internal functions
(deffunction compare-instances (?cmp ?event1 ?event2 ?min ?max))
(deffunction compare-facts (?cmp ?event1 ?event2 ?min ?max))
(deffunction compare (?cmp ?start1 ?end1 ?start2 ?end2 ?min ?max))
(deffunction interval-to-float (?string))
(deffunction convert-unit (?number ?unit))

(deffunction after
  ;; The after function matches the temporal distance between
  ;; the two given events returning TRUE if the first event starts after
  ;; the end of the second one.
  ;;
  ;; An optional interval consisting of two values (min, max) can be specified.
  ;;
  ;; Example:
  ;;    (after ?ev1 ?ev2 2m 5m)
  ;;
  ;; Will be TRUE if:
  ;;    (> (- ?event1:timestamp (+ ?event2:timestamp ?event2:duration)) 120)
  ;;    and
  ;;    (< (- ?event1:timestamp (+ ?event2:timestamp ?event2:duration)) 300)
  ;;

  (?event1 ?event2 $?params)

  (bind ?min nil)
  (bind ?max nil)

  ; interval wildcard parameter
  (if (> (length$ ?params) 0)
   then
     (bind ?min (interval-to-float (nth$ 1 ?params)))
     (if (> (length$ ?params) 1)
      then
        (bind ?max (interval-to-float (nth$ 2 ?params)))))

  (if (and (instancep ?event1) (instancep ?event2))
   then
     (compare-instances after ?event1 ?event2 ?min ?max)
   else
     (compare-facts after ?event1 ?event2 ?min ?max)))

(deffunction before
  ;; The before function matches the temporal distance between
  ;; the two given events returning TRUE if the first event ends before
  ;; the start of the second one.
  ;;
  ;; An optional interval consisting of two values (min, max) can be specified.
  ;;
  ;; Example:
  ;;    (before ?ev1 ?ev2 2m 5m)
  ;;
  ;; Will be TRUE if:
  ;;    (> (- ?event2:timestamp (+ ?event1:timestamp ?event1:duration)) 120)
  ;;    and
  ;;    (< (- ?event2:timestamp (+ ?event1:timestamp ?event1:duration)) 300)
  ;;

  (?event1 ?event2 $?params)

  (bind ?min nil)
  (bind ?max nil)

  ; interval wildcard parameter
  (if (> (length$ ?params) 0)
   then
     (bind ?min (interval-to-float (nth$ 1 ?params)))
     (if (> (length$ ?params) 1)
      then
        (bind ?max (interval-to-float (nth$ 2 ?params)))))

  (if (and (instancep ?event1) (instancep ?event2))
   then
     (compare-instances before ?event1 ?event2 ?min ?max)
   else
     (compare-facts before ?event1 ?event2 ?min ?max)))

(deffunction compare-instances
  (?cmp ?event1 ?event2 ?min ?max)

  (if (and (slot-existp (class ?event1) timestamp inherit)
           (slot-existp (class ?event2) timestamp inherit))
   then
     (bind ?start1 (send ?event1 (sym-cat get- timestamp)))
     (bind ?start2 (send ?event2 (sym-cat get- timestamp)))

     ; consider optional event duration fields
     (if (slot-existp (class ?event1) duration inherit)
      then
        (bind ?end1 (+ ?start1 (interval-to-float
                               (send ?event1 (sym-cat get- duration)))))
      else
        (bind ?end1 ?start1))
     (if (slot-existp (class ?event2) duration inherit)
      then
        (bind ?end2 (+ ?start2 (interval-to-float
                               (send ?event2 (sym-cat get- duration)))))
      else
        (bind ?end2 ?start2))

     (compare ?cmp ?start1 ?end1 ?start2 ?end2 ?min ?max)
   else
     ERROR))

(deffunction compare-facts
  (?cmp ?event1 ?event2 ?min ?max)

  (if (and (member$ timestamp (fact-slot-names ?event1))
           (member$ timestamp (fact-slot-names ?event2)))
   then
     (bind ?start1 (fact-slot-value ?event1 timestamp))
     (bind ?start2 (fact-slot-value ?event2 timestamp))

     ; consider optional event duration fields
     (if (member$ duration (fact-slot-names ?event1))
      then
        (bind ?end1 (+ ?start1 (interval-to-float
                               (fact-slot-value ?event1 duration))))
      else
        (bind ?end1 ?start1))
     (if (member$ duration (fact-slot-names ?event2))
      then
        (bind ?end2 (+ ?start2 (interval-to-float
                               (fact-slot-value ?event2 duration))))
      else
        (bind ?end2 ?start2))

     (compare ?cmp ?start1 ?end1 ?start2 ?end2 ?min ?max)
   else
     ERROR))

(deffunction compare
  (?cmp ?start1 ?end1 ?start2 ?end2 ?min ?max)

  (switch ?cmp
    (case after then (bind ?delta (- ?start1 ?end2)))
    (case before then (bind ?delta (- ?start2 ?end1))))

  (if (neq ?min nil)
   then
     (if (neq ?max nil)
      then
        (and (>= ?delta ?min) (<= ?delta ?max))
      else
        (>= ?delta ?min))
   else
     (> ?delta 0)))

(deffunction interval-to-float
  (?string)

  (if (integerp ?string)
   then
     (return (float ?string)))

  (bind ?unit nil)
  (bind ?number 0)
  (bind ?result 0)
  (bind ?counter 0)
  (bind ?length (str-length ?string))

  (while (> ?length 0)
    (switch (sub-string ?length ?length ?string)
      (case "0" then (bind ?char 0))
      (case "1" then (bind ?char 1))
      (case "2" then (bind ?char 2))
      (case "3" then (bind ?char 3))
      (case "4" then (bind ?char 4))
      (case "5" then (bind ?char 5))
      (case "6" then (bind ?char 6))
      (case "7" then (bind ?char 7))
      (case "8" then (bind ?char 8))
      (case "9" then (bind ?char 9))
      (case "h" then (bind ?char hours))
      (case "m" then (bind ?char minutes))
      (case "s" then (bind ?char seconds))
      (default (return nil)))

    (switch ?char
      (case hours then (bind ?result (+ ?result (convert-unit ?number ?unit)))
                       (bind ?unit hours)
                       (bind ?counter 0)
                       (bind ?number 0))
      (case minutes then (bind ?result (+ ?result (convert-unit ?number ?unit)))
                         (bind ?unit minutes)
                         (bind ?counter 0)
                         (bind ?number 0))
      (case seconds then (bind ?result (+ ?result (convert-unit ?number ?unit)))
                         (bind ?unit seconds)
                         (bind ?counter 0)
                         (bind ?number 0))
      (default
        (bind ?number (+ (integer (* ?char (** 10 ?counter))) ?number))
        (bind ?counter (+ 1 ?counter))))

    (bind ?length (- ?length 1)))

  (bind ?result (+ ?result (convert-unit ?number ?unit)))

  (float ?result))

(deffunction convert-unit
  (?number ?unit)

  (switch ?unit
    (case hours then (* ?number 3600))
    (case minutes then (* ?number 60))
    (default ?number)))
