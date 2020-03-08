;;; Test model which uses dynamic real-time updating.
;;; Simple situation where sets the max-time-delta
;;; with a long action pending and toggles the allow-
;;; dynamics flag to show conflict-resolution rescheduled
;;; when dynamics are enabled.

(clear-all)

(defun run-it ()
  
  (reset)
  (mp-real-time-management :units-per-second (/ internal-time-units-per-second 5)
                           :allow-dynamics nil
                           :max-time-delta .5)
  
  
  (run 1.1 :real-time t)
  
  (reset)
  (mp-real-time-management :units-per-second (/ internal-time-units-per-second 5)
                           :allow-dynamics t
                           :max-time-delta .5)
  
  (run 1.1 :real-time t))


(define-model test-model
    
    (sgp :esc t :lf 1.0 :rt 0 :trace-detail medium)
  (sgp-fct (list :v *out-file*))
  
  
  (p start
     ?goal>
     buffer empty
     ==>
     +goal>
     isa chunk
     +retrieval>
     isa chunk)
  
)