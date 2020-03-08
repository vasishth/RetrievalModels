
(defun run-it ()
  (reset)
  (run 8.1)
  (sgp :record-ticks nil)
  (run 10))

(clear-all)

(define-model test-temporal

(sgp :trace-detail high)
(sgp-fct (list :v *out-file*))

(sgp :seed (50 1))

(chunk-type test slot1 slot2)

(define-chunks (running isa chunk) (waiting isa chunk) (done isa chunk) (goal isa test))

(goal-focus goal)

(p start-timer
   =goal>
     isa test
     slot1 nil
==>
   +temporal>
     isa time
   =goal>
     slot1 running)


(p wait
   =goal>
     isa test
     slot1 running
==>
   =goal>
      slot1 done)

(spp wait :at 8.0)


(p read
   =goal>
     isa test
     slot1 done
   =temporal>
     isa time
     ticks =ticks
==>
   !output! =ticks
   +temporal>
     isa time
   =goal>
     slot1 waiting
     slot2 =ticks
)
   

(p wait2
   =goal>
     isa test
     slot1 waiting
     slot2 =ticks
   =temporal>
     isa time
     ticks =ticks
==>
   +temporal>
     isa clear
   -goal>)

)
