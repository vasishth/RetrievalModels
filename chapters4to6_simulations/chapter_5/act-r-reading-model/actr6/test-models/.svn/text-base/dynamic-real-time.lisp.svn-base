;;; Test model which uses dynamic real-time updating.
;;; Simple situation where it just schedules a new event
;;; in the "past" during the slack hook to make sure
;;; that the conflict-resolution (which is dynamic)
;;; gets bumped back.

(clear-all)

(defvar *once* nil)


(defun simple-slack-test (delta next-time)
  (if (and *once* (> next-time .200))
      (progn
        (schedule-event .5 (lambda ()))
        (setf *once* nil))
    (sleep (/ delta 2))))

(defun run-it ()
  
  (reset)
  (mp-real-time-management  :units-per-second (/ internal-time-units-per-second 2)
                           :slack-function 'simple-slack-test
                           :allow-dynamics nil)
  
  
  (run 1.1 :real-time t)
  
  (reset)
  (mp-real-time-management :units-per-second (/ internal-time-units-per-second 2)
                           :slack-function 'simple-slack-test
                           :allow-dynamics t)
  
  (run 1.1 :real-time t))


(define-model test-model
    
    (sgp :esc t :lf 1.0 :rt 0 :trace-detail medium)
  (sgp-fct (list :v *out-file*))
  
  (setf *once* t)
  
  (p start
     ?goal>
     buffer empty
     ==>
     +goal>
     isa chunk
     +retrieval>
     isa chunk)
  
  (p test
     !eval! (> (mp-time) .4)
     ==>
     ))