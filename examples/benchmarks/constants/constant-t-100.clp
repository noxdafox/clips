(deftemplate data
   (slot index)
   (slot value))

(deffacts start
   (data (index 1000000) (value 100000)))

(defrule loop
   (declare (salience 1))
   ?f <- (data (index ?i&~0))
   =>
   (modify ?f (index (- ?i 1)) (value (- ?i 1))))

(defrule rule-1
   (data (value ?x))
   (test (eq ?x 1))
   =>)

(defrule rule-2
   (data (value ?x))
   (test (eq ?x 2))
   =>)

(defrule rule-3
   (data (value ?x))
   (test (eq ?x 3))
   =>)

(defrule rule-4
   (data (value ?x))
   (test (eq ?x 4))
   =>)

(defrule rule-5
   (data (value ?x))
   (test (eq ?x 5))
   =>)

(defrule rule-6
   (data (value ?x))
   (test (eq ?x 6))
   =>)

(defrule rule-7
   (data (value ?x))
   (test (eq ?x 7))
   =>)

(defrule rule-8
   (data (value ?x))
   (test (eq ?x 8))
   =>)

(defrule rule-9
   (data (value ?x))
   (test (eq ?x 9))
   =>)

(defrule rule-10
   (data (value ?x))
   (test (eq ?x 10))
   =>)

(defrule rule-11
   (data (value ?x))
   (test (eq ?x 11))
   =>)

(defrule rule-12
   (data (value ?x))
   (test (eq ?x 12))
   =>)

(defrule rule-13
   (data (value ?x))
   (test (eq ?x 13))
   =>)

(defrule rule-14
   (data (value ?x))
   (test (eq ?x 14))
   =>)

(defrule rule-15
   (data (value ?x))
   (test (eq ?x 15))
   =>)

(defrule rule-16
   (data (value ?x))
   (test (eq ?x 16))
   =>)

(defrule rule-17
   (data (value ?x))
   (test (eq ?x 17))
   =>)

(defrule rule-18
   (data (value ?x))
   (test (eq ?x 18))
   =>)

(defrule rule-19
   (data (value ?x))
   (test (eq ?x 19))
   =>)

(defrule rule-20
   (data (value ?x))
   (test (eq ?x 20))
   =>)

(defrule rule-21
   (data (value ?x))
   (test (eq ?x 21))
   =>)

(defrule rule-22
   (data (value ?x))
   (test (eq ?x 22))
   =>)

(defrule rule-23
   (data (value ?x))
   (test (eq ?x 23))
   =>)

(defrule rule-24
   (data (value ?x))
   (test (eq ?x 24))
   =>)

(defrule rule-25
   (data (value ?x))
   (test (eq ?x 25))
   =>)

(defrule rule-26
   (data (value ?x))
   (test (eq ?x 26))
   =>)

(defrule rule-27
   (data (value ?x))
   (test (eq ?x 27))
   =>)

(defrule rule-28
   (data (value ?x))
   (test (eq ?x 28))
   =>)

(defrule rule-29
   (data (value ?x))
   (test (eq ?x 29))
   =>)

(defrule rule-30
   (data (value ?x))
   (test (eq ?x 30))
   =>)

(defrule rule-31
   (data (value ?x))
   (test (eq ?x 31))
   =>)

(defrule rule-32
   (data (value ?x))
   (test (eq ?x 32))
   =>)

(defrule rule-33
   (data (value ?x))
   (test (eq ?x 33))
   =>)

(defrule rule-34
   (data (value ?x))
   (test (eq ?x 34))
   =>)

(defrule rule-35
   (data (value ?x))
   (test (eq ?x 35))
   =>)

(defrule rule-36
   (data (value ?x))
   (test (eq ?x 36))
   =>)

(defrule rule-37
   (data (value ?x))
   (test (eq ?x 37))
   =>)

(defrule rule-38
   (data (value ?x))
   (test (eq ?x 38))
   =>)

(defrule rule-39
   (data (value ?x))
   (test (eq ?x 39))
   =>)

(defrule rule-40
   (data (value ?x))
   (test (eq ?x 40))
   =>)

(defrule rule-41
   (data (value ?x))
   (test (eq ?x 41))
   =>)

(defrule rule-42
   (data (value ?x))
   (test (eq ?x 42))
   =>)

(defrule rule-43
   (data (value ?x))
   (test (eq ?x 43))
   =>)

(defrule rule-44
   (data (value ?x))
   (test (eq ?x 44))
   =>)

(defrule rule-45
   (data (value ?x))
   (test (eq ?x 45))
   =>)

(defrule rule-46
   (data (value ?x))
   (test (eq ?x 46))
   =>)

(defrule rule-47
   (data (value ?x))
   (test (eq ?x 47))
   =>)

(defrule rule-48
   (data (value ?x))
   (test (eq ?x 48))
   =>)

(defrule rule-49
   (data (value ?x))
   (test (eq ?x 49))
   =>)

(defrule rule-50
   (data (value ?x))
   (test (eq ?x 50))
   =>)

(defrule rule-51
   (data (value ?x))
   (test (eq ?x 51))
   =>)

(defrule rule-52
   (data (value ?x))
   (test (eq ?x 52))
   =>)

(defrule rule-53
   (data (value ?x))
   (test (eq ?x 53))
   =>)

(defrule rule-54
   (data (value ?x))
   (test (eq ?x 54))
   =>)

(defrule rule-55
   (data (value ?x))
   (test (eq ?x 55))
   =>)

(defrule rule-56
   (data (value ?x))
   (test (eq ?x 56))
   =>)

(defrule rule-57
   (data (value ?x))
   (test (eq ?x 57))
   =>)

(defrule rule-58
   (data (value ?x))
   (test (eq ?x 58))
   =>)

(defrule rule-59
   (data (value ?x))
   (test (eq ?x 59))
   =>)

(defrule rule-60
   (data (value ?x))
   (test (eq ?x 60))
   =>)

(defrule rule-61
   (data (value ?x))
   (test (eq ?x 61))
   =>)

(defrule rule-62
   (data (value ?x))
   (test (eq ?x 62))
   =>)

(defrule rule-63
   (data (value ?x))
   (test (eq ?x 63))
   =>)

(defrule rule-64
   (data (value ?x))
   (test (eq ?x 64))
   =>)

(defrule rule-65
   (data (value ?x))
   (test (eq ?x 65))
   =>)

(defrule rule-66
   (data (value ?x))
   (test (eq ?x 66))
   =>)

(defrule rule-67
   (data (value ?x))
   (test (eq ?x 67))
   =>)

(defrule rule-68
   (data (value ?x))
   (test (eq ?x 68))
   =>)

(defrule rule-69
   (data (value ?x))
   (test (eq ?x 69))
   =>)

(defrule rule-70
   (data (value ?x))
   (test (eq ?x 70))
   =>)

(defrule rule-71
   (data (value ?x))
   (test (eq ?x 71))
   =>)

(defrule rule-72
   (data (value ?x))
   (test (eq ?x 72))
   =>)

(defrule rule-73
   (data (value ?x))
   (test (eq ?x 73))
   =>)

(defrule rule-74
   (data (value ?x))
   (test (eq ?x 74))
   =>)

(defrule rule-75
   (data (value ?x))
   (test (eq ?x 75))
   =>)

(defrule rule-76
   (data (value ?x))
   (test (eq ?x 76))
   =>)

(defrule rule-77
   (data (value ?x))
   (test (eq ?x 77))
   =>)

(defrule rule-78
   (data (value ?x))
   (test (eq ?x 78))
   =>)

(defrule rule-79
   (data (value ?x))
   (test (eq ?x 79))
   =>)

(defrule rule-80
   (data (value ?x))
   (test (eq ?x 80))
   =>)

(defrule rule-81
   (data (value ?x))
   (test (eq ?x 81))
   =>)

(defrule rule-82
   (data (value ?x))
   (test (eq ?x 82))
   =>)

(defrule rule-83
   (data (value ?x))
   (test (eq ?x 83))
   =>)

(defrule rule-84
   (data (value ?x))
   (test (eq ?x 84))
   =>)

(defrule rule-85
   (data (value ?x))
   (test (eq ?x 85))
   =>)

(defrule rule-86
   (data (value ?x))
   (test (eq ?x 86))
   =>)

(defrule rule-87
   (data (value ?x))
   (test (eq ?x 87))
   =>)

(defrule rule-88
   (data (value ?x))
   (test (eq ?x 88))
   =>)

(defrule rule-89
   (data (value ?x))
   (test (eq ?x 89))
   =>)

(defrule rule-90
   (data (value ?x))
   (test (eq ?x 90))
   =>)

(defrule rule-91
   (data (value ?x))
   (test (eq ?x 91))
   =>)

(defrule rule-92
   (data (value ?x))
   (test (eq ?x 92))
   =>)

(defrule rule-93
   (data (value ?x))
   (test (eq ?x 93))
   =>)

(defrule rule-94
   (data (value ?x))
   (test (eq ?x 94))
   =>)

(defrule rule-95
   (data (value ?x))
   (test (eq ?x 95))
   =>)

(defrule rule-96
   (data (value ?x))
   (test (eq ?x 96))
   =>)

(defrule rule-97
   (data (value ?x))
   (test (eq ?x 97))
   =>)

(defrule rule-98
   (data (value ?x))
   (test (eq ?x 98))
   =>)

(defrule rule-99
   (data (value ?x))
   (test (eq ?x 99))
   =>)

(defrule rule-100
   (data (value ?x))
   (test (eq ?x 100))
   =>)




