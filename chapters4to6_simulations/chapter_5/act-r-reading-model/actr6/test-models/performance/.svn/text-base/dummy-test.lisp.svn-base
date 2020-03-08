;;; Very simple test just for verification purposes.
;;; It just sleeps for times based on the sizes given.

(setf *performance-test* '(dummy-test-init
                           dummy-test-run
                           (1 2 4)))


(defun dummy-test-init (n)
  (sleep (/ n 2)))
  
  
(defun dummy-test-run (n)
  (sleep n))