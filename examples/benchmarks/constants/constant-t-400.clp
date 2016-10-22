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

(defrule rule-101
   (data (value ?x))
   (test (eq ?x 101))
   =>)

(defrule rule-102
   (data (value ?x))
   (test (eq ?x 102))
   =>)

(defrule rule-103
   (data (value ?x))
   (test (eq ?x 103))
   =>)

(defrule rule-104
   (data (value ?x))
   (test (eq ?x 104))
   =>)

(defrule rule-105
   (data (value ?x))
   (test (eq ?x 105))
   =>)

(defrule rule-106
   (data (value ?x))
   (test (eq ?x 106))
   =>)

(defrule rule-107
   (data (value ?x))
   (test (eq ?x 107))
   =>)

(defrule rule-108
   (data (value ?x))
   (test (eq ?x 108))
   =>)

(defrule rule-109
   (data (value ?x))
   (test (eq ?x 109))
   =>)

(defrule rule-110
   (data (value ?x))
   (test (eq ?x 110))
   =>)

(defrule rule-111
   (data (value ?x))
   (test (eq ?x 111))
   =>)

(defrule rule-112
   (data (value ?x))
   (test (eq ?x 112))
   =>)

(defrule rule-113
   (data (value ?x))
   (test (eq ?x 113))
   =>)

(defrule rule-114
   (data (value ?x))
   (test (eq ?x 114))
   =>)

(defrule rule-115
   (data (value ?x))
   (test (eq ?x 115))
   =>)

(defrule rule-116
   (data (value ?x))
   (test (eq ?x 116))
   =>)

(defrule rule-117
   (data (value ?x))
   (test (eq ?x 117))
   =>)

(defrule rule-118
   (data (value ?x))
   (test (eq ?x 118))
   =>)

(defrule rule-119
   (data (value ?x))
   (test (eq ?x 119))
   =>)

(defrule rule-120
   (data (value ?x))
   (test (eq ?x 120))
   =>)

(defrule rule-121
   (data (value ?x))
   (test (eq ?x 121))
   =>)

(defrule rule-122
   (data (value ?x))
   (test (eq ?x 122))
   =>)

(defrule rule-123
   (data (value ?x))
   (test (eq ?x 123))
   =>)

(defrule rule-124
   (data (value ?x))
   (test (eq ?x 124))
   =>)

(defrule rule-125
   (data (value ?x))
   (test (eq ?x 125))
   =>)

(defrule rule-126
   (data (value ?x))
   (test (eq ?x 126))
   =>)

(defrule rule-127
   (data (value ?x))
   (test (eq ?x 127))
   =>)

(defrule rule-128
   (data (value ?x))
   (test (eq ?x 128))
   =>)

(defrule rule-129
   (data (value ?x))
   (test (eq ?x 129))
   =>)

(defrule rule-130
   (data (value ?x))
   (test (eq ?x 130))
   =>)

(defrule rule-131
   (data (value ?x))
   (test (eq ?x 131))
   =>)

(defrule rule-132
   (data (value ?x))
   (test (eq ?x 132))
   =>)

(defrule rule-133
   (data (value ?x))
   (test (eq ?x 133))
   =>)

(defrule rule-134
   (data (value ?x))
   (test (eq ?x 134))
   =>)

(defrule rule-135
   (data (value ?x))
   (test (eq ?x 135))
   =>)

(defrule rule-136
   (data (value ?x))
   (test (eq ?x 136))
   =>)

(defrule rule-137
   (data (value ?x))
   (test (eq ?x 137))
   =>)

(defrule rule-138
   (data (value ?x))
   (test (eq ?x 138))
   =>)

(defrule rule-139
   (data (value ?x))
   (test (eq ?x 139))
   =>)

(defrule rule-140
   (data (value ?x))
   (test (eq ?x 140))
   =>)

(defrule rule-141
   (data (value ?x))
   (test (eq ?x 141))
   =>)

(defrule rule-142
   (data (value ?x))
   (test (eq ?x 142))
   =>)

(defrule rule-143
   (data (value ?x))
   (test (eq ?x 143))
   =>)

(defrule rule-144
   (data (value ?x))
   (test (eq ?x 144))
   =>)

(defrule rule-145
   (data (value ?x))
   (test (eq ?x 145))
   =>)

(defrule rule-146
   (data (value ?x))
   (test (eq ?x 146))
   =>)

(defrule rule-147
   (data (value ?x))
   (test (eq ?x 147))
   =>)

(defrule rule-148
   (data (value ?x))
   (test (eq ?x 148))
   =>)

(defrule rule-149
   (data (value ?x))
   (test (eq ?x 149))
   =>)

(defrule rule-150
   (data (value ?x))
   (test (eq ?x 150))
   =>)

(defrule rule-151
   (data (value ?x))
   (test (eq ?x 151))
   =>)

(defrule rule-152
   (data (value ?x))
   (test (eq ?x 152))
   =>)

(defrule rule-153
   (data (value ?x))
   (test (eq ?x 153))
   =>)

(defrule rule-154
   (data (value ?x))
   (test (eq ?x 154))
   =>)

(defrule rule-155
   (data (value ?x))
   (test (eq ?x 155))
   =>)

(defrule rule-156
   (data (value ?x))
   (test (eq ?x 156))
   =>)

(defrule rule-157
   (data (value ?x))
   (test (eq ?x 157))
   =>)

(defrule rule-158
   (data (value ?x))
   (test (eq ?x 158))
   =>)

(defrule rule-159
   (data (value ?x))
   (test (eq ?x 159))
   =>)

(defrule rule-160
   (data (value ?x))
   (test (eq ?x 160))
   =>)

(defrule rule-161
   (data (value ?x))
   (test (eq ?x 161))
   =>)

(defrule rule-162
   (data (value ?x))
   (test (eq ?x 162))
   =>)

(defrule rule-163
   (data (value ?x))
   (test (eq ?x 163))
   =>)

(defrule rule-164
   (data (value ?x))
   (test (eq ?x 164))
   =>)

(defrule rule-165
   (data (value ?x))
   (test (eq ?x 165))
   =>)

(defrule rule-166
   (data (value ?x))
   (test (eq ?x 166))
   =>)

(defrule rule-167
   (data (value ?x))
   (test (eq ?x 167))
   =>)

(defrule rule-168
   (data (value ?x))
   (test (eq ?x 168))
   =>)

(defrule rule-169
   (data (value ?x))
   (test (eq ?x 169))
   =>)

(defrule rule-170
   (data (value ?x))
   (test (eq ?x 170))
   =>)

(defrule rule-171
   (data (value ?x))
   (test (eq ?x 171))
   =>)

(defrule rule-172
   (data (value ?x))
   (test (eq ?x 172))
   =>)

(defrule rule-173
   (data (value ?x))
   (test (eq ?x 173))
   =>)

(defrule rule-174
   (data (value ?x))
   (test (eq ?x 174))
   =>)

(defrule rule-175
   (data (value ?x))
   (test (eq ?x 175))
   =>)

(defrule rule-176
   (data (value ?x))
   (test (eq ?x 176))
   =>)

(defrule rule-177
   (data (value ?x))
   (test (eq ?x 177))
   =>)

(defrule rule-178
   (data (value ?x))
   (test (eq ?x 178))
   =>)

(defrule rule-179
   (data (value ?x))
   (test (eq ?x 179))
   =>)

(defrule rule-180
   (data (value ?x))
   (test (eq ?x 180))
   =>)

(defrule rule-181
   (data (value ?x))
   (test (eq ?x 181))
   =>)

(defrule rule-182
   (data (value ?x))
   (test (eq ?x 182))
   =>)

(defrule rule-183
   (data (value ?x))
   (test (eq ?x 183))
   =>)

(defrule rule-184
   (data (value ?x))
   (test (eq ?x 184))
   =>)

(defrule rule-185
   (data (value ?x))
   (test (eq ?x 185))
   =>)

(defrule rule-186
   (data (value ?x))
   (test (eq ?x 186))
   =>)

(defrule rule-187
   (data (value ?x))
   (test (eq ?x 187))
   =>)

(defrule rule-188
   (data (value ?x))
   (test (eq ?x 188))
   =>)

(defrule rule-189
   (data (value ?x))
   (test (eq ?x 189))
   =>)

(defrule rule-190
   (data (value ?x))
   (test (eq ?x 190))
   =>)

(defrule rule-191
   (data (value ?x))
   (test (eq ?x 191))
   =>)

(defrule rule-192
   (data (value ?x))
   (test (eq ?x 192))
   =>)

(defrule rule-193
   (data (value ?x))
   (test (eq ?x 193))
   =>)

(defrule rule-194
   (data (value ?x))
   (test (eq ?x 194))
   =>)

(defrule rule-195
   (data (value ?x))
   (test (eq ?x 195))
   =>)

(defrule rule-196
   (data (value ?x))
   (test (eq ?x 196))
   =>)

(defrule rule-197
   (data (value ?x))
   (test (eq ?x 197))
   =>)

(defrule rule-198
   (data (value ?x))
   (test (eq ?x 198))
   =>)

(defrule rule-199
   (data (value ?x))
   (test (eq ?x 199))
   =>)

(defrule rule-200
   (data (value ?x))
   (test (eq ?x 200))
   =>)

(defrule rule-201
   (data (value ?x))
   (test (eq ?x 201))
   =>)

(defrule rule-202
   (data (value ?x))
   (test (eq ?x 202))
   =>)

(defrule rule-203
   (data (value ?x))
   (test (eq ?x 203))
   =>)

(defrule rule-204
   (data (value ?x))
   (test (eq ?x 204))
   =>)

(defrule rule-205
   (data (value ?x))
   (test (eq ?x 205))
   =>)

(defrule rule-206
   (data (value ?x))
   (test (eq ?x 206))
   =>)

(defrule rule-207
   (data (value ?x))
   (test (eq ?x 207))
   =>)

(defrule rule-208
   (data (value ?x))
   (test (eq ?x 208))
   =>)

(defrule rule-209
   (data (value ?x))
   (test (eq ?x 209))
   =>)

(defrule rule-210
   (data (value ?x))
   (test (eq ?x 210))
   =>)

(defrule rule-211
   (data (value ?x))
   (test (eq ?x 211))
   =>)

(defrule rule-212
   (data (value ?x))
   (test (eq ?x 212))
   =>)

(defrule rule-213
   (data (value ?x))
   (test (eq ?x 213))
   =>)

(defrule rule-214
   (data (value ?x))
   (test (eq ?x 214))
   =>)

(defrule rule-215
   (data (value ?x))
   (test (eq ?x 215))
   =>)

(defrule rule-216
   (data (value ?x))
   (test (eq ?x 216))
   =>)

(defrule rule-217
   (data (value ?x))
   (test (eq ?x 217))
   =>)

(defrule rule-218
   (data (value ?x))
   (test (eq ?x 218))
   =>)

(defrule rule-219
   (data (value ?x))
   (test (eq ?x 219))
   =>)

(defrule rule-220
   (data (value ?x))
   (test (eq ?x 220))
   =>)

(defrule rule-221
   (data (value ?x))
   (test (eq ?x 221))
   =>)

(defrule rule-222
   (data (value ?x))
   (test (eq ?x 222))
   =>)

(defrule rule-223
   (data (value ?x))
   (test (eq ?x 223))
   =>)

(defrule rule-224
   (data (value ?x))
   (test (eq ?x 224))
   =>)

(defrule rule-225
   (data (value ?x))
   (test (eq ?x 225))
   =>)

(defrule rule-226
   (data (value ?x))
   (test (eq ?x 226))
   =>)

(defrule rule-227
   (data (value ?x))
   (test (eq ?x 227))
   =>)

(defrule rule-228
   (data (value ?x))
   (test (eq ?x 228))
   =>)

(defrule rule-229
   (data (value ?x))
   (test (eq ?x 229))
   =>)

(defrule rule-230
   (data (value ?x))
   (test (eq ?x 230))
   =>)

(defrule rule-231
   (data (value ?x))
   (test (eq ?x 231))
   =>)

(defrule rule-232
   (data (value ?x))
   (test (eq ?x 232))
   =>)

(defrule rule-233
   (data (value ?x))
   (test (eq ?x 233))
   =>)

(defrule rule-234
   (data (value ?x))
   (test (eq ?x 234))
   =>)

(defrule rule-235
   (data (value ?x))
   (test (eq ?x 235))
   =>)

(defrule rule-236
   (data (value ?x))
   (test (eq ?x 236))
   =>)

(defrule rule-237
   (data (value ?x))
   (test (eq ?x 237))
   =>)

(defrule rule-238
   (data (value ?x))
   (test (eq ?x 238))
   =>)

(defrule rule-239
   (data (value ?x))
   (test (eq ?x 239))
   =>)

(defrule rule-240
   (data (value ?x))
   (test (eq ?x 240))
   =>)

(defrule rule-241
   (data (value ?x))
   (test (eq ?x 241))
   =>)

(defrule rule-242
   (data (value ?x))
   (test (eq ?x 242))
   =>)

(defrule rule-243
   (data (value ?x))
   (test (eq ?x 243))
   =>)

(defrule rule-244
   (data (value ?x))
   (test (eq ?x 244))
   =>)

(defrule rule-245
   (data (value ?x))
   (test (eq ?x 245))
   =>)

(defrule rule-246
   (data (value ?x))
   (test (eq ?x 246))
   =>)

(defrule rule-247
   (data (value ?x))
   (test (eq ?x 247))
   =>)

(defrule rule-248
   (data (value ?x))
   (test (eq ?x 248))
   =>)

(defrule rule-249
   (data (value ?x))
   (test (eq ?x 249))
   =>)

(defrule rule-250
   (data (value ?x))
   (test (eq ?x 250))
   =>)

(defrule rule-251
   (data (value ?x))
   (test (eq ?x 251))
   =>)

(defrule rule-252
   (data (value ?x))
   (test (eq ?x 252))
   =>)

(defrule rule-253
   (data (value ?x))
   (test (eq ?x 253))
   =>)

(defrule rule-254
   (data (value ?x))
   (test (eq ?x 254))
   =>)

(defrule rule-255
   (data (value ?x))
   (test (eq ?x 255))
   =>)

(defrule rule-256
   (data (value ?x))
   (test (eq ?x 256))
   =>)

(defrule rule-257
   (data (value ?x))
   (test (eq ?x 257))
   =>)

(defrule rule-258
   (data (value ?x))
   (test (eq ?x 258))
   =>)

(defrule rule-259
   (data (value ?x))
   (test (eq ?x 259))
   =>)

(defrule rule-260
   (data (value ?x))
   (test (eq ?x 260))
   =>)

(defrule rule-261
   (data (value ?x))
   (test (eq ?x 261))
   =>)

(defrule rule-262
   (data (value ?x))
   (test (eq ?x 262))
   =>)

(defrule rule-263
   (data (value ?x))
   (test (eq ?x 263))
   =>)

(defrule rule-264
   (data (value ?x))
   (test (eq ?x 264))
   =>)

(defrule rule-265
   (data (value ?x))
   (test (eq ?x 265))
   =>)

(defrule rule-266
   (data (value ?x))
   (test (eq ?x 266))
   =>)

(defrule rule-267
   (data (value ?x))
   (test (eq ?x 267))
   =>)

(defrule rule-268
   (data (value ?x))
   (test (eq ?x 268))
   =>)

(defrule rule-269
   (data (value ?x))
   (test (eq ?x 269))
   =>)

(defrule rule-270
   (data (value ?x))
   (test (eq ?x 270))
   =>)

(defrule rule-271
   (data (value ?x))
   (test (eq ?x 271))
   =>)

(defrule rule-272
   (data (value ?x))
   (test (eq ?x 272))
   =>)

(defrule rule-273
   (data (value ?x))
   (test (eq ?x 273))
   =>)

(defrule rule-274
   (data (value ?x))
   (test (eq ?x 274))
   =>)

(defrule rule-275
   (data (value ?x))
   (test (eq ?x 275))
   =>)

(defrule rule-276
   (data (value ?x))
   (test (eq ?x 276))
   =>)

(defrule rule-277
   (data (value ?x))
   (test (eq ?x 277))
   =>)

(defrule rule-278
   (data (value ?x))
   (test (eq ?x 278))
   =>)

(defrule rule-279
   (data (value ?x))
   (test (eq ?x 279))
   =>)

(defrule rule-280
   (data (value ?x))
   (test (eq ?x 280))
   =>)

(defrule rule-281
   (data (value ?x))
   (test (eq ?x 281))
   =>)

(defrule rule-282
   (data (value ?x))
   (test (eq ?x 282))
   =>)

(defrule rule-283
   (data (value ?x))
   (test (eq ?x 283))
   =>)

(defrule rule-284
   (data (value ?x))
   (test (eq ?x 284))
   =>)

(defrule rule-285
   (data (value ?x))
   (test (eq ?x 285))
   =>)

(defrule rule-286
   (data (value ?x))
   (test (eq ?x 286))
   =>)

(defrule rule-287
   (data (value ?x))
   (test (eq ?x 287))
   =>)

(defrule rule-288
   (data (value ?x))
   (test (eq ?x 288))
   =>)

(defrule rule-289
   (data (value ?x))
   (test (eq ?x 289))
   =>)

(defrule rule-290
   (data (value ?x))
   (test (eq ?x 290))
   =>)

(defrule rule-291
   (data (value ?x))
   (test (eq ?x 291))
   =>)

(defrule rule-292
   (data (value ?x))
   (test (eq ?x 292))
   =>)

(defrule rule-293
   (data (value ?x))
   (test (eq ?x 293))
   =>)

(defrule rule-294
   (data (value ?x))
   (test (eq ?x 294))
   =>)

(defrule rule-295
   (data (value ?x))
   (test (eq ?x 295))
   =>)

(defrule rule-296
   (data (value ?x))
   (test (eq ?x 296))
   =>)

(defrule rule-297
   (data (value ?x))
   (test (eq ?x 297))
   =>)

(defrule rule-298
   (data (value ?x))
   (test (eq ?x 298))
   =>)

(defrule rule-299
   (data (value ?x))
   (test (eq ?x 299))
   =>)

(defrule rule-300
   (data (value ?x))
   (test (eq ?x 300))
   =>)

(defrule rule-301
   (data (value ?x))
   (test (eq ?x 301))
   =>)

(defrule rule-302
   (data (value ?x))
   (test (eq ?x 302))
   =>)

(defrule rule-303
   (data (value ?x))
   (test (eq ?x 303))
   =>)

(defrule rule-304
   (data (value ?x))
   (test (eq ?x 304))
   =>)

(defrule rule-305
   (data (value ?x))
   (test (eq ?x 305))
   =>)

(defrule rule-306
   (data (value ?x))
   (test (eq ?x 306))
   =>)

(defrule rule-307
   (data (value ?x))
   (test (eq ?x 307))
   =>)

(defrule rule-308
   (data (value ?x))
   (test (eq ?x 308))
   =>)

(defrule rule-309
   (data (value ?x))
   (test (eq ?x 309))
   =>)

(defrule rule-310
   (data (value ?x))
   (test (eq ?x 310))
   =>)

(defrule rule-311
   (data (value ?x))
   (test (eq ?x 311))
   =>)

(defrule rule-312
   (data (value ?x))
   (test (eq ?x 312))
   =>)

(defrule rule-313
   (data (value ?x))
   (test (eq ?x 313))
   =>)

(defrule rule-314
   (data (value ?x))
   (test (eq ?x 314))
   =>)

(defrule rule-315
   (data (value ?x))
   (test (eq ?x 315))
   =>)

(defrule rule-316
   (data (value ?x))
   (test (eq ?x 316))
   =>)

(defrule rule-317
   (data (value ?x))
   (test (eq ?x 317))
   =>)

(defrule rule-318
   (data (value ?x))
   (test (eq ?x 318))
   =>)

(defrule rule-319
   (data (value ?x))
   (test (eq ?x 319))
   =>)

(defrule rule-320
   (data (value ?x))
   (test (eq ?x 320))
   =>)

(defrule rule-321
   (data (value ?x))
   (test (eq ?x 321))
   =>)

(defrule rule-322
   (data (value ?x))
   (test (eq ?x 322))
   =>)

(defrule rule-323
   (data (value ?x))
   (test (eq ?x 323))
   =>)

(defrule rule-324
   (data (value ?x))
   (test (eq ?x 324))
   =>)

(defrule rule-325
   (data (value ?x))
   (test (eq ?x 325))
   =>)

(defrule rule-326
   (data (value ?x))
   (test (eq ?x 326))
   =>)

(defrule rule-327
   (data (value ?x))
   (test (eq ?x 327))
   =>)

(defrule rule-328
   (data (value ?x))
   (test (eq ?x 328))
   =>)

(defrule rule-329
   (data (value ?x))
   (test (eq ?x 329))
   =>)

(defrule rule-330
   (data (value ?x))
   (test (eq ?x 330))
   =>)

(defrule rule-331
   (data (value ?x))
   (test (eq ?x 331))
   =>)

(defrule rule-332
   (data (value ?x))
   (test (eq ?x 332))
   =>)

(defrule rule-333
   (data (value ?x))
   (test (eq ?x 333))
   =>)

(defrule rule-334
   (data (value ?x))
   (test (eq ?x 334))
   =>)

(defrule rule-335
   (data (value ?x))
   (test (eq ?x 335))
   =>)

(defrule rule-336
   (data (value ?x))
   (test (eq ?x 336))
   =>)

(defrule rule-337
   (data (value ?x))
   (test (eq ?x 337))
   =>)

(defrule rule-338
   (data (value ?x))
   (test (eq ?x 338))
   =>)

(defrule rule-339
   (data (value ?x))
   (test (eq ?x 339))
   =>)

(defrule rule-340
   (data (value ?x))
   (test (eq ?x 340))
   =>)

(defrule rule-341
   (data (value ?x))
   (test (eq ?x 341))
   =>)

(defrule rule-342
   (data (value ?x))
   (test (eq ?x 342))
   =>)

(defrule rule-343
   (data (value ?x))
   (test (eq ?x 343))
   =>)

(defrule rule-344
   (data (value ?x))
   (test (eq ?x 344))
   =>)

(defrule rule-345
   (data (value ?x))
   (test (eq ?x 345))
   =>)

(defrule rule-346
   (data (value ?x))
   (test (eq ?x 346))
   =>)

(defrule rule-347
   (data (value ?x))
   (test (eq ?x 347))
   =>)

(defrule rule-348
   (data (value ?x))
   (test (eq ?x 348))
   =>)

(defrule rule-349
   (data (value ?x))
   (test (eq ?x 349))
   =>)

(defrule rule-350
   (data (value ?x))
   (test (eq ?x 350))
   =>)

(defrule rule-351
   (data (value ?x))
   (test (eq ?x 351))
   =>)

(defrule rule-352
   (data (value ?x))
   (test (eq ?x 352))
   =>)

(defrule rule-353
   (data (value ?x))
   (test (eq ?x 353))
   =>)

(defrule rule-354
   (data (value ?x))
   (test (eq ?x 354))
   =>)

(defrule rule-355
   (data (value ?x))
   (test (eq ?x 355))
   =>)

(defrule rule-356
   (data (value ?x))
   (test (eq ?x 356))
   =>)

(defrule rule-357
   (data (value ?x))
   (test (eq ?x 357))
   =>)

(defrule rule-358
   (data (value ?x))
   (test (eq ?x 358))
   =>)

(defrule rule-359
   (data (value ?x))
   (test (eq ?x 359))
   =>)

(defrule rule-360
   (data (value ?x))
   (test (eq ?x 360))
   =>)

(defrule rule-361
   (data (value ?x))
   (test (eq ?x 361))
   =>)

(defrule rule-362
   (data (value ?x))
   (test (eq ?x 362))
   =>)

(defrule rule-363
   (data (value ?x))
   (test (eq ?x 363))
   =>)

(defrule rule-364
   (data (value ?x))
   (test (eq ?x 364))
   =>)

(defrule rule-365
   (data (value ?x))
   (test (eq ?x 365))
   =>)

(defrule rule-366
   (data (value ?x))
   (test (eq ?x 366))
   =>)

(defrule rule-367
   (data (value ?x))
   (test (eq ?x 367))
   =>)

(defrule rule-368
   (data (value ?x))
   (test (eq ?x 368))
   =>)

(defrule rule-369
   (data (value ?x))
   (test (eq ?x 369))
   =>)

(defrule rule-370
   (data (value ?x))
   (test (eq ?x 370))
   =>)

(defrule rule-371
   (data (value ?x))
   (test (eq ?x 371))
   =>)

(defrule rule-372
   (data (value ?x))
   (test (eq ?x 372))
   =>)

(defrule rule-373
   (data (value ?x))
   (test (eq ?x 373))
   =>)

(defrule rule-374
   (data (value ?x))
   (test (eq ?x 374))
   =>)

(defrule rule-375
   (data (value ?x))
   (test (eq ?x 375))
   =>)

(defrule rule-376
   (data (value ?x))
   (test (eq ?x 376))
   =>)

(defrule rule-377
   (data (value ?x))
   (test (eq ?x 377))
   =>)

(defrule rule-378
   (data (value ?x))
   (test (eq ?x 378))
   =>)

(defrule rule-379
   (data (value ?x))
   (test (eq ?x 379))
   =>)

(defrule rule-380
   (data (value ?x))
   (test (eq ?x 380))
   =>)

(defrule rule-381
   (data (value ?x))
   (test (eq ?x 381))
   =>)

(defrule rule-382
   (data (value ?x))
   (test (eq ?x 382))
   =>)

(defrule rule-383
   (data (value ?x))
   (test (eq ?x 383))
   =>)

(defrule rule-384
   (data (value ?x))
   (test (eq ?x 384))
   =>)

(defrule rule-385
   (data (value ?x))
   (test (eq ?x 385))
   =>)

(defrule rule-386
   (data (value ?x))
   (test (eq ?x 386))
   =>)

(defrule rule-387
   (data (value ?x))
   (test (eq ?x 387))
   =>)

(defrule rule-388
   (data (value ?x))
   (test (eq ?x 388))
   =>)

(defrule rule-389
   (data (value ?x))
   (test (eq ?x 389))
   =>)

(defrule rule-390
   (data (value ?x))
   (test (eq ?x 390))
   =>)

(defrule rule-391
   (data (value ?x))
   (test (eq ?x 391))
   =>)

(defrule rule-392
   (data (value ?x))
   (test (eq ?x 392))
   =>)

(defrule rule-393
   (data (value ?x))
   (test (eq ?x 393))
   =>)

(defrule rule-394
   (data (value ?x))
   (test (eq ?x 394))
   =>)

(defrule rule-395
   (data (value ?x))
   (test (eq ?x 395))
   =>)

(defrule rule-396
   (data (value ?x))
   (test (eq ?x 396))
   =>)

(defrule rule-397
   (data (value ?x))
   (test (eq ?x 397))
   =>)

(defrule rule-398
   (data (value ?x))
   (test (eq ?x 398))
   =>)

(defrule rule-399
   (data (value ?x))
   (test (eq ?x 399))
   =>)

(defrule rule-400
   (data (value ?x))
   (test (eq ?x 400))
   =>)



