;;; Performance test of the most basic production matching.
;;;
;;; - The productions have one buffer test and one slot tested in
;;;   that buffer.
;;;
;;; - All productions will match on the chunk-type of the buffer
;;;   and only mismatch on the slot value.
;;;
;;; - The production will not change the goal.
;;;
;;; - The model will be run for 1000 simulated seconds.
;;;
;;; - Subsymbolic calculations are off and no utility values
;;;   are specificed.
;;;
;;; - Sizes will be the number of competing productions with
;;;   a mismatched slot value.


(setf *performance-test* '(simple-production-test-init 
                           simple-production-test-run
                           (1 10 100 500 1000 2000 5000)))


(defun simple-production-test-init (n)
  (define-model-fct 'test `((sgp :esc nil :v nil)
    (chunk-type goal-test slot) 
    (define-chunks (g isa goal-test slot g))
    (goal-focus g)
    (p matches
       =goal>
       isa goal-test
       slot g
       ==>)
    (dotimes (i ,n)
      (p-fct `(,(new-name "p") =goal> isa goal-test slot ,(new-name) ==>))))))
  
  
  
(defun simple-production-test-run (n)
  (declare (ignore n))
  (run 1000.0))
