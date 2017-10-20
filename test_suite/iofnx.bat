(clear)                            
(open)                             ; 10.5.2.1
(open "blah1.dat")                 ; 10.5.2.1
(open "blah2.dat" blah2 "r" 10)    ; 10.5.2.1
(open 10 blah3 "r")                ; 10.5.2.1
(open [blah2.dat] blah4 "r")       ; 10.5.2.1
(open "blah4.dat" (create$) "r")   ; 10.5.2.1
(open blah5.dat blah5 r)           ; 10.5.2.1
(open blah6.dat blah6 "x")         ; 10.5.2.1
(open blah7.dat blah7 8)           ; 10.5.2.1
(open blah8.dat blah8 "r")         ; 10.5.2.1
(close blah6 89)                   ; 10.5.2.2
(close (create$))                  ; 10.5.2.2
(close [blah8])                    ; 10.5.2.2
(printout)                         ; 10.5.2.3
(printout (create$))               ; 10.5.2.3
(read (create$))                   ; 10.5.2.4
(read bogus)                       ; 10.5.2.4
(read stdin stdout)                ; 10.5.2.4
(read)                             ; 10.5.2.4 - 7
7
(read t)                           ; 10.5.2.4 - abc
abc
(read stdin)                       ; 10.5.2.4 - xyz
xyz abc
(read)                             ; 10.5.2.4 - a


    a    
(open "Temp/iofnx1.tmp" mydata "w")     ; 10.5.2.4
(printout mydata "red green")      ; 10.5.2.4
(close mydata)                     ; 10.5.2.4
(open "Temp/iofnx1.tmp" mydata)    ; 10.5.2.4
(read mydata)                      ; 10.5.2.4
(read mydata)                      ; 10.5.2.4
(read mydata)                      ; 10.5.2.4
(close mydata)                     ; 10.5.2.4
(readline (create$))               ; 10.5.2.5
(readline bogus)                   ; 10.5.2.5
(readline stdin stdout)            ; 10.5.2.5
(readline)                         ; 10.5.2.5 - "7"
7
(readline t)                       ; 10.5.2.5 - "abc"
abc
(readline stdin)                   ; 10.5.2.5 - "xyz abc"
xyz abc
(readline)                         ; 10.5.2.5 - ""

(open "Temp/iofnx1.tmp" 7.8923)    ; 10.5.2.5
(readline 7.8923)                  ; 10.5.2.5
(readline 7.8923)                  ; 10.5.2.5
(close 7.8923)                     ; 10.5.2.5
(format)                           ; 10.5.2.6
(format t)                         ; 10.5.2.6
(format (create$))                 ; 10.5.2.6
(format t "%f%%%n")                ; 10.5.2.6
(format nil "Integer: |%d|" 12)    ; 10.5.2.6
(format t "Integer: |%4d|" 12)     ; 10.5.2.6
(format nil "Integer: |%-04d|" 12)
(format t "Float:   |%f|" 12.01)   ; 10.5.2.6
(format nil "Float:   |%7.2f| "12.01)
(format t "Test:    |%e|" 12.01)   ; 10.5.2.6
(format nil "Test:    |%7.2e|" 12.01)
(format t "General: |%g|" 1234567890)
(format t "Hexadecimal: |%x|" 12)  ; 10.5.2.6
(format t "Octal:   |%o|" 12)      ; 10.5.2.6
(format nil "Symbols: |%s| |%s|" value-a1 capacity)
(format nil "the %% x %d x %s x %f y %%" 4 ab 3.5)
(format nil "%d" abc)              ; 10.5.2.6
(format nil "%d" 9.8)              ; 10.5.2.6
(format t "%f" 40)                 ; 10.5.2.6
(format nil "%f" abc)              ; 10.5.2.6
(format nil "%g" (create$))        ; 10.5.2.6
(format t "%o" 9.8)                ; 10.5.2.6
(remove "Temp/iofun.dat")          ; Open function modes
(open "Temp/iofun.dat" temp "bogus")
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/iofun.dat" temp "r")
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/iofun.dat" temp "w")
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/iofun.dat" temp "a")
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/iofun.dat" temp "rb")
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/iofun.dat" temp "wb")
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/iofun.dat" temp "ab")
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/iofun.dat" temp "w")
(printout temp red crlf)
(close temp)
(open "Temp/iofun.dat" temp "r")
(read temp)
(read temp)
(close temp)
(open "Temp/iofun.dat" temp "a")
(read temp)
(printout temp green crlf)
(close temp)
(open "Temp/iofun.dat" temp "r")
(read temp)
(read temp)
(read temp)
(close temp)
(remove "Temp/iofun.dat") 
(open "Temp/read.dat" temp "w") ; Return symbols for tokens that are not primitive values
(printout temp "(foo ?foo \"?foo\" \")\")" crlf)
(close temp)
(open "Temp/read.dat" temp "r")
(read temp)
(read temp)
(read temp)
(read temp)
(read temp)
(read temp)
(close temp)
(remove "Temp/read.dat") 
(put-char) ; put-char/get-char
(put-char temp)
(open "Temp/read.dat" temp "w") 
(put-char temp abc)
(put-char temp 72 34)
(put-char temp 72)
(put-char temp 101)
(put-char temp 108)
(put-char temp 112)
(put-char temp 111)
(put-char temp 10)
(close temp)
(open "Temp/read.dat" temp "r")
(read temp)
(close temp)
(open "Temp/read.dat" temp "r")
(readline temp)
(close temp)
(open "Temp/read.dat" temp "r")
(get-char temp)
(get-char temp)
(get-char temp)
(get-char temp)
(get-char temp)
(get-char temp)
(get-char temp)
(close temp)
(unget-char temp)
(open "Temp/read.dat" temp "r")
(get-char temp)
(get-char temp)
(unget-char temp 101)
(get-char temp)
(get-char temp)
(unget-char temp 108)
(unget-char temp 101)
(get-char temp)
(get-char temp)
(get-char temp)
(get-char temp)
(unget-char temp -1)
(get-char temp)
(get-char temp)
(close temp)
(remove "Temp/read.dat")
(while (not (member$ (get-char) (create$ 10 13))) do)
ab cd ef
(open "Temp/plus.dat" temp "w") ; seek, tell, rewind, flush args
(seek)
(seek temp)
(seek temp 0)
(seek temp 0 seek-cur)
(seek temp 0 seek-set)
(seek temp 0 seek-end)
(seek temp 0 bogus)
(seek temp 0 seek-set extra)
(seek bogus 0 seek-set)
(tell)
(tell temp extra)
(tell bogus)
(rewind)
(rewind temp extra)
(rewind bogus)
(flush temp extra)
(flush temp)
(flush)
(flush bogus)
(close temp)
(open "Temp/plus.dat" temp "w") ; rewind
(printout temp "Jack Smith" crlf)
(printout temp "John Jones" crlf)
(close temp)
(open "Temp/plus.dat" temp "r") 
(readline temp)
(readline temp)
(readline temp)
(printout temp "Sally Walker" crlf)
(rewind temp)
(readline temp)
(readline temp)
(readline temp)
(close temp)
(open "Temp/plus.dat" temp "r+") ; r+ mode
(readline temp)
(readline temp)
(readline temp)
(printout temp "Sally Walker" crlf)
(rewind temp)
(readline temp)
(readline temp)
(readline temp)
(readline temp)
(close temp)
(open "Temp/seek.dat" temp "w+") ; w+, seek, tell
(printout temp "Jack Smith" crlf)
(progn (bind ?pos (tell temp)) TRUE)
(printout temp "John Jones" crlf)
(printout temp "Sally Walker" crlf)
(seek temp ?pos seek-set)
(readline temp)
(readline temp)
(readline temp)
(seek temp 0 seek-set)
(readline temp)
(close temp)
(clear)
