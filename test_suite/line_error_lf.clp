(deftemplate point
   (slot x)
   (slot y))
   
(deffacts points
   (point (x 1) (y 2))
   (point (x 3) (y 4) (z 5)))
