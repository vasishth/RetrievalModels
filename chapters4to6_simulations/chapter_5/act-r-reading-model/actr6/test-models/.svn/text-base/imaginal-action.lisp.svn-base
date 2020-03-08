;;; Model tests the imaginal-action buffer requests.

(clear-all)

(defparameter *counter* 0)

(defun make-simple-chunk ()
  (car (define-chunks (isa simple count 0))))

(defun no-chunk-generated ()
  nil)

(defun update-imaginal ()
  (schedule-mod-buffer-chunk 'imaginal (list 'count (incf *counter*)) .150 :module 'imaginal)
  (schedule-event-relative .150 'set-imaginal-free :module 'imaginal :priority -10))


(defun run-it ()
  (reset)
  (run 5))

(define-model test-imaginal-action
    
    (sgp-fct (list :v *out-file* :do-not-harvest 'imaginal))
  
  (chunk-type simple count)
  
  (p start
     ?imaginal>
     state free
     buffer empty
     error nil
     ==>
     +imaginal-action>
     isa simple-action
     action make-simple-chunk)
  
  
  (p increment
     =imaginal>
     isa simple
     < count 4
     ?imaginal>
     state free
     ==>
     +imaginal-action>
     isa generic-action
     action update-imaginal)
  
    (p dont-increment
     =imaginal>
     isa simple
     count 4
     ?imaginal>
     state free
     ==>
     +imaginal-action>
     isa simple-action
     action no-chunk-generated)
  
  (p stop
     ?imaginal>
     error t
     ==>
     +imaginal>
     isa chunk))