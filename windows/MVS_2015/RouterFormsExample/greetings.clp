(defrule greeting
   =>
   (printout t "What's your name? ")
   (bind ?response (read))
   (printout t "Hello " ?response "." crlf))


