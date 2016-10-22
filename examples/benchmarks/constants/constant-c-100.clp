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
   (data (value 1))
   =>)

(defrule rule-2
   (data (value 2))
   =>)

(defrule rule-3
   (data (value 3))
   =>)

(defrule rule-4
   (data (value 4))
   =>)

(defrule rule-5
   (data (value 5))
   =>)

(defrule rule-6
   (data (value 6))
   =>)

(defrule rule-7
   (data (value 7))
   =>)

(defrule rule-8
   (data (value 8))
   =>)

(defrule rule-9
   (data (value 9))
   =>)

(defrule rule-10
   (data (value 10))
   =>)

(defrule rule-11
   (data (value 11))
   =>)

(defrule rule-12
   (data (value 12))
   =>)

(defrule rule-13
   (data (value 13))
   =>)

(defrule rule-14
   (data (value 14))
   =>)

(defrule rule-15
   (data (value 15))
   =>)

(defrule rule-16
   (data (value 16))
   =>)

(defrule rule-17
   (data (value 17))
   =>)

(defrule rule-18
   (data (value 18))
   =>)

(defrule rule-19
   (data (value 19))
   =>)

(defrule rule-20
   (data (value 20))
   =>)

(defrule rule-21
   (data (value 21))
   =>)

(defrule rule-22
   (data (value 22))
   =>)

(defrule rule-23
   (data (value 23))
   =>)

(defrule rule-24
   (data (value 24))
   =>)

(defrule rule-25
   (data (value 25))
   =>)

(defrule rule-26
   (data (value 26))
   =>)

(defrule rule-27
   (data (value 27))
   =>)

(defrule rule-28
   (data (value 28))
   =>)

(defrule rule-29
   (data (value 29))
   =>)

(defrule rule-30
   (data (value 30))
   =>)

(defrule rule-31
   (data (value 31))
   =>)

(defrule rule-32
   (data (value 32))
   =>)

(defrule rule-33
   (data (value 33))
   =>)

(defrule rule-34
   (data (value 34))
   =>)

(defrule rule-35
   (data (value 35))
   =>)

(defrule rule-36
   (data (value 36))
   =>)

(defrule rule-37
   (data (value 37))
   =>)

(defrule rule-38
   (data (value 38))
   =>)

(defrule rule-39
   (data (value 39))
   =>)

(defrule rule-40
   (data (value 40))
   =>)

(defrule rule-41
   (data (value 41))
   =>)

(defrule rule-42
   (data (value 42))
   =>)

(defrule rule-43
   (data (value 43))
   =>)

(defrule rule-44
   (data (value 44))
   =>)

(defrule rule-45
   (data (value 45))
   =>)

(defrule rule-46
   (data (value 46))
   =>)

(defrule rule-47
   (data (value 47))
   =>)

(defrule rule-48
   (data (value 48))
   =>)

(defrule rule-49
   (data (value 49))
   =>)

(defrule rule-50
   (data (value 50))
   =>)

(defrule rule-51
   (data (value 51))
   =>)

(defrule rule-52
   (data (value 52))
   =>)

(defrule rule-53
   (data (value 53))
   =>)

(defrule rule-54
   (data (value 54))
   =>)

(defrule rule-55
   (data (value 55))
   =>)

(defrule rule-56
   (data (value 56))
   =>)

(defrule rule-57
   (data (value 57))
   =>)

(defrule rule-58
   (data (value 58))
   =>)

(defrule rule-59
   (data (value 59))
   =>)

(defrule rule-60
   (data (value 60))
   =>)

(defrule rule-61
   (data (value 61))
   =>)

(defrule rule-62
   (data (value 62))
   =>)

(defrule rule-63
   (data (value 63))
   =>)

(defrule rule-64
   (data (value 64))
   =>)

(defrule rule-65
   (data (value 65))
   =>)

(defrule rule-66
   (data (value 66))
   =>)

(defrule rule-67
   (data (value 67))
   =>)

(defrule rule-68
   (data (value 68))
   =>)

(defrule rule-69
   (data (value 69))
   =>)

(defrule rule-70
   (data (value 70))
   =>)

(defrule rule-71
   (data (value 71))
   =>)

(defrule rule-72
   (data (value 72))
   =>)

(defrule rule-73
   (data (value 73))
   =>)

(defrule rule-74
   (data (value 74))
   =>)

(defrule rule-75
   (data (value 75))
   =>)

(defrule rule-76
   (data (value 76))
   =>)

(defrule rule-77
   (data (value 77))
   =>)

(defrule rule-78
   (data (value 78))
   =>)

(defrule rule-79
   (data (value 79))
   =>)

(defrule rule-80
   (data (value 80))
   =>)

(defrule rule-81
   (data (value 81))
   =>)

(defrule rule-82
   (data (value 82))
   =>)

(defrule rule-83
   (data (value 83))
   =>)

(defrule rule-84
   (data (value 84))
   =>)

(defrule rule-85
   (data (value 85))
   =>)

(defrule rule-86
   (data (value 86))
   =>)

(defrule rule-87
   (data (value 87))
   =>)

(defrule rule-88
   (data (value 88))
   =>)

(defrule rule-89
   (data (value 89))
   =>)

(defrule rule-90
   (data (value 90))
   =>)

(defrule rule-91
   (data (value 91))
   =>)

(defrule rule-92
   (data (value 92))
   =>)

(defrule rule-93
   (data (value 93))
   =>)

(defrule rule-94
   (data (value 94))
   =>)

(defrule rule-95
   (data (value 95))
   =>)

(defrule rule-96
   (data (value 96))
   =>)

(defrule rule-97
   (data (value 97))
   =>)

(defrule rule-98
   (data (value 98))
   =>)

(defrule rule-99
   (data (value 99))
   =>)

(defrule rule-100
   (data (value 100))
   =>)


