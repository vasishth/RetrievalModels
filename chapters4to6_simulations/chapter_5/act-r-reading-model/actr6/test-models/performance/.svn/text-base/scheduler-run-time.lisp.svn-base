;;; Performance test of the basic clock and scheduler execution.
;;; 
;;; - One dummy periodic event (doesn't run any code) gets executed
;;;   every 50ms.
;;;
;;; - Vary the length of time that it runs.

(setf *performance-test* '(sched-run-time-init 
                           run
                           (1.0 10.0 100.0 1000.0 10000.0 20000.0 50000.0 100000.0)))



(defun sched-run-time-init (n)
  (declare (ignore n))
  (define-model foo (sgp :v nil))
  (schedule-periodic-event .05 (lambda ()) :maintenance t :output nil))