(clear-all)

(defun run-it ()
  (let ((*error-output* *out-file*))
    (reset)
    (run 10)))

(define-model test-bad-productions
    (sgp-fct (list :v *out-file* :cmdt *out-file*))
  
  (chunk-type test slot1 slot2)
  
  (goal-focus-fct (car (define-chunks (g1 isa test))))
  
  (p bad-order
     =goal>
     isa test
     slot1 nil
     slot2 nil
     ==>
     !eval! (null =x)
     !bind! =x 1
     =goal>
     slot1 1)
  
  (p undefined-chunk
     =goal>
     isa test
     slot1 1
     ==>
     =goal>
     slot1 2
     slot2 not-a-chunk-yet)
  
  (p circular-reference-rejected
     =goal>
     isa test
     slot1 2
     ==>
     !bind! =x (+ =y 1)
     !bind! =y (- =z 2)
     !bind! =z (+ =x 3)
     )
  
  (p bad-buffer-name-rejected
     =goal>
     isa test
     slot1 2
     =not-a-buffer>
     isa test
     ==>
     )
  
  (p missing-separator-rejected
     =goal>
     isa test
     slot1 2
     )
  
  (p missing-isa-rejected
     =goal>
     slot1 2
     ==>)
  
  (p un-tested-modify-rejected
     =goal>
     isa test
     slot1 2
     ==>
     =imaginal>
     slot1 nil
     )
  (p illegal-modifier-rejected
     =goal>
     isa test
     * slot1 2
     ==>
     )
  (p unknown-rhs-action-rejected
     =goal>
     isa test
     slot1 2
     ==>
     !whats-this! (+ =goal 3)
     )
  
  (p finish
     =goal>
     isa test
     slot1 2
     ==>
     !stop!
     )
     
  
  
)
