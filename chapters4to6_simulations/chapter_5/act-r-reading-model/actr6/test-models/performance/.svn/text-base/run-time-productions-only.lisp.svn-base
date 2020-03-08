;;; Performance test of the run time while doing only basic production matching.
;;;
;;; - There are 100 productions have one buffer test and one slot tested in
;;;   that buffer.
;;;
;;; - All productions will match on the chunk-type of the buffer
;;;   and may only mismatch on the slot value.
;;;
;;; - There will only be one production which matches.
;;;
;;; - The production will not change the goal.
;;;
;;; - Subsymbolic calculations are off and no utility values
;;;   are specificed.
;;;
;;; - Sizes will be the length of time in seconds which the run
;;;   will take.


(setf *performance-test* '(run-time-simple-init 
                           run-time-simple-run
                           (10 100 500 1000 2000 5000 10000)))


(defun run-time-simple-init (n)
  (define-model test (sgp :esc nil :v nil)
    (chunk-type goal-test slot) 
    (define-chunks (g isa goal-test slot g))
    (goal-focus g)
    (p matches
       =goal>
       isa goal-test
       slot g
       ==>))
  (dotimes (i 99)
    (p-fct `(,(new-name "p") =goal> isa goal-test slot free ==>))))
  
  
  
(defun run-time-simple-run (n)
  (run (* 1.0 n)))
