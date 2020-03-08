(clear-all)

(defun run-it ()
  (run 10))

(defun single-bind (val)
  (1+ val))

(defun multi-bind (chunk)
  (values (chunk-slot-value-fct chunk 'slot1)
          (chunk-slot-value-fct chunk 'slot2)))
          

(define-model bind-test
    
    (sgp :trace-detail high)
  (sgp-fct (list :v *out-file*))
  
  (chunk-type goal slot1 slot2)
  
  (p p1
     ?goal>
     buffer empty
     ==>
     +goal>
     isa goal
     slot1 free
     slot2 busy)
  
  (p p2
     =goal>
     isa goal
     !bind! =val (single-bind 3)
     !mv-bind! (=val1 =val2) (multi-bind =goal)
     ==>
     +goal>
     isa chunk
     !output! (the answers are =val =val1 =val2)
     )
  )
     
     
