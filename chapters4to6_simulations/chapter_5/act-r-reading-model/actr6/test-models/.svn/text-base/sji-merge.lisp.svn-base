(clear-all)

(defun run-it ()
  (run 10))

(define-model sji-merging
    
    (sgp :trace-detail high :esc t :mas 5.0 :act t :do-not-harvest retrieval :dcnn nil)
  (sgp-fct (list :v *out-file*))
  (chunk-type source)
  (chunk-type target slot)
  (chunk-type goal state source)
  
  (add-dm (a isa source))
  
  (p p1
     ?goal>
     buffer empty
     ==>
     +goal>
     isa goal
     state start
     +retrieval>
     isa source)
  
  (p p2
     =goal>
     isa goal
     state start
     =retrieval>
     isa source
     ==>
     =goal>
     source =retrieval
     state next
     +imaginal>
     isa target
     slot =retrieval)
  
  (p p3
     =goal>
     isa goal
     state next
     =imaginal>
     isa target
     ==>
     =goal>
     state retrieve-it
     )
  (p p4
     =goal>
     isa goal
     state retrieve-it
     ==>
     =goal>
     state done
     +retrieval>
     isa target
     ))
     
     