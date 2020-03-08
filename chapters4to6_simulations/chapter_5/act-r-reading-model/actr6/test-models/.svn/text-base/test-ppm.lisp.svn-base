(clear-all)

;;; Call run-test to set the test model run

(defun run-test ()
  (setf *out-file* t)
  (run-it))

(defun run-it ()
  (reset)
  (sgp :ppm nil)
  (model-output "~%~30,,,'*<~>~%Without ppm (default symbolic matching):~%~30,,,'*<~>~%")
  (whynot)
  (run 10)
  (reset)
  (sgp :ppm 1)
  (model-output "~%~30,,,'*<~>~%With ppm enabled:~%~30,,,'*<~>~%")
  (whynot)
  (run 10)
  (reset)
  (sgp :ppm 1 :ppm-hook show-ppm-values)
  (model-output "~%~30,,,'*<~>~%With ppm enabled and using the hook to show values as it goes:~%~30,,,'*<~>~%")
  (run 10))

(defun show-ppm-values (p mismatches)
  (model-output "ppm hook called with values:~%~S ~S" p mismatches)
  (fifth (car mismatches)))

(define-model test-ppm
    
    (sgp :esc t :do-not-harvest retrieval :do-not-harvest imaginal)
  (sgp-fct (list :v *out-file* :cmdt *out-file*))
  (chunk-type goal state slot)
  
  (add-dm (state1 isa chunk)
          (state2 isa chunk)
          (state3 isa chunk)
          (state4 isa chunk)
          (stateA isa chunk)
          (stateB isa chunk)
          (stateC isa chunk)
          (stateD isa chunk)
          (stateE isa chunk))
  
  (set-similarities (state1 stateA -.1)
                    (state2 stateA -.2)
                    (state3 stateA -.3)
                    (state4 stateA -.4)
                    (state1 stateB -.2)
                    (state2 stateB -.1)
                    (state3 stateB -.2)
                    (state4 stateB -.3)
                    (state1 stateC -.3)
                    (state2 stateC -.2)
                    (state3 stateC -.1)
                    (state4 stateC -.2)
                    (state1 stateD -.4)
                    (state2 stateD -.3)
                    (state3 stateD -.2)
                    (state4 stateD -.1))
  
  (define-chunks (g isa goal state stateA)
    (r isa goal slot state state state2)
    (i isa goal slot state state state4)
    (state isa chunk))
  
  (set-buffer-chunk 'goal 'g)
  (set-buffer-chunk 'imaginal 'i)
  (set-buffer-chunk 'retrieval 'r)
  
  
  (p constant-slot-test
     =goal>
       isa goal
       state state1
     ==>
     =goal>
       state stateB)
  
   (p variable-slot-test
     =retrieval>
       isa goal
       state =var
     =goal>
       isa goal
       state =var
     ==>
     =goal>
       state stateC)
  
  (p* variablized-slot-constant-val
     =goal>
       isa goal
       =slot state3
     =imaginal>
       isa goal
       slot =slot
     ==>
     =goal>
       state stateD)
  
  (p* variablized-slot-and-val
     =goal>
       isa goal
       =slot =val
     =imaginal>
       isa goal
       slot =slot
       state =val
     ==>
     =goal>
       state stateE)
)

