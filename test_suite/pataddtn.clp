;;;************************************************************
;;; PATTERN ADDITION CHECKING
;;;
;;; This file tests to see if the addition of patterns to
;;; rules works correctly. The pattern (initial-fact) is
;;; added to and/not CEs thats first CE is a test CE or
;;; to and CEs thats first CE is a not CE.
;;;
;;; To test perform a (clear), (load ...), and (agenda)
;;; The agenda command should show of list of 24 activations,
;;; all beginning with the prefix should-fire-...
;;;************************************************************

(defrule should-fire-1
  (declare (salience 1))
  =>)

(defrule should-not-fire-1a
  (not (and (factoid)))
  =>)

(defrule should-not-fire-1b
  (not (factoid))
  =>)

; Single test CE

(defrule should-fire-2a
  (declare (salience 2))
  (test (> 5 3))
  =>)

(defrule should-fire-2b
  (declare (salience 3))
  (factoid)
  (test (> 5 3))
  =>)

(defrule should-not-fire-2a
  (test (< 5 3))
  =>)

(defrule should-not-fire-2b
  (factoid)
  (test (< 5 3))
  =>)

; Single test CE within and CE

(defrule should-fire-3a
  (declare (salience 4))
  (and (test (> 5 3)))
  =>)

(defrule should-fire-3b
  (declare (salience 5))
  (and (factoid)
       (test (> 5 3)))
  =>)

(defrule should-not-fire-3a
  (and (test (< 5 3)))
  =>)

(defrule should-not-fire-3b
  (and (factoid)
       (test (< 5 3)))
  =>)

; Single test CE within and CE within not CE

(defrule should-fire-4a
  (declare (salience 6))
  (not (and (test (< 5 3))))
  =>)
  
(defrule should-fire-4b
  (declare (salience 7))
  (not (test (< 5 3)))
  =>)
  
(defrule should-fire-4c
  (declare (salience 8))
  (not (and (factoid) 
            (test (< 5 3))))
  =>)

(defrule should-fire-4d
  (declare (salience 9))
  (factoid)
  (not (and (factoid) 
            (test (< 5 3))))
  =>)
  
(defrule should-fire-4e
  (declare (salience 10))
  (not (and (test (< 5 3)) 
            (factoid)))
  =>)
  
(defrule should-not-fire-4a
  (not (and (test (> 5 3))))
  =>)

(defrule should-not-fire-4b
  (not (and (factoid) 
            (test (> 5 3))))
  =>)
  
(defrule should-not-fire-4c
  (factoid)
  (not (and (factoid) 
            (test (> 5 3))))
  =>)

(defrule should-not-fire-4d
  (not (test (> 5 3)))
  =>)
  
(defrule should-not-fire-4e
  (not (and (test (> 5 3)) 
            (factoid)))
  =>)
  
; Single test CE within two not CEs
  
(defrule should-fire-5a
  (declare (salience 11))
  (exists (test (< 3 5)))
  =>)

(defrule should-fire-5b
  (declare (salience 12))
  (not (not (test (< 3 5))))
  =>)

(defrule should-fire-5c
  (declare (salience 13))
  (factoid)
  (exists (test (< 3 5)))
  =>)

(defrule should-fire-5d
  (declare (salience 14))
  (factoid)
  (not (not (test (< 3 5))))
  =>)
  
(defrule should-fire-5e
  (declare (salience 15))
  (factoid)
  (exists (and (factoid) (test (< 3 5))))
  =>)

(defrule should-fire-5f
  (declare (salience 16))
  (factoid)
  (not (not (and (factoid) (test (< 3 5)))))
  =>)

(defrule should-not-fire-5a
  (exists (test (> 3 5)))
  =>)

(defrule should-not-fire-5b
  (not (not (test (> 3 5))))
  =>)

(defrule should-not-fire-5c
  (factoid)
  (exists (test (> 3 5)))
  =>)

(defrule should-not-fire-5d
  (factoid)
  (not (not (test (> 3 5))))
  =>)
  
(defrule should-not-fire-5e
  (factoid)
  (exists (and (factoid) (test (> 3 5))))
  =>)

(defrule should-not-fire-5f
  (factoid)
  (not (not (and (factoid) (test (> 3 5)))))
  =>)

; Forall CE

(defrule should-fire-6a
  (declare (salience 17))
  (forall (factoid) 
          (factoid))
  =>)

(defrule should-fire-6b
  (declare (salience 18))
  (forall (factoid) 
          (test (> 5 3)))
  =>)

(defrule should-fire-6c
  (declare (salience 19))
  (forall (test (> 5 3)) 
          (factoid))
  =>)

(defrule should-fire-6d
  (declare (salience 20))
  (forall (test (> 5 3)) 
          (test (> 5 3)))
  =>)

(defrule should-fire-6e
  (declare (salience 21))
  (forall (test (< 5 3)) 
          (factoid))
  =>)

(defrule should-fire-6f
  (declare (salience 22))
  (forall (test (< 5 3)) 
          (test (> 5 3)))
  =>)

(defrule should-fire-6g
  (declare (salience 23))
  (forall (test (< 5 3)) 
          (test (< 5 3)))
  =>)

(defrule should-not-fire-6a
  (forall (factoid) 
          (test (< 5 3)))
  =>)

(defrule should-not-fire-6b
  (forall (test (> 5 3)) 
          (test (< 5 3)))
  =>)

