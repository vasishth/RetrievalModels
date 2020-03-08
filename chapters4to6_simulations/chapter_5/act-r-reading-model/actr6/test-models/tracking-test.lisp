
(defvar *letter*)

(defun run-it ()
  
  (reset)
  (let ((window (open-exp-window "Moving X" :visible nil)))
    (setf *letter* (add-text-to-exp-window :text "x" :x 10 :y 150))
    
    (install-device window)
    (proc-display)
    (run 3)))


(defun move-it () 
  (setf (x-pos *letter*) (+ 10 (x-pos *letter*)))
  (proc-display))

(clear-all)

(define-model simple-tracking

    (sgp :needs-mouse nil :show-focus t :trace-detail high 
         
         ;; because the items are moving vision shouldn't
         ;; delete the chunks because the model may still
         ;; be referencing an old one when it moves to a
         ;; new location
         
         :delete-visicon-chunks nil)

(sgp-fct (list :v *out-file* :cmdt *out-file*))

(chunk-type report state index)


(define-chunks 
    (vl-1 isa visual-location screen-x x screen-y y)
    (vo-1 isa visual-object value unknown)
    (x isa chunk)
    (y isa chunk)
  (unknown isa chunk)
  (report isa chunk)
  (change isa chunk))

(goal-focus-fct (car (define-chunks (isa report state report index 0))))

(P found-letter
   
   =visual-location>
      ISA         visual-location
   
   ?visual>
      state        free
   
==>
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
)

(P track-letter
   =visual>
      ISA         text
      value       =letter
   ?visual>
      state       free
   ==>

   +visual>
      isa         start-tracking
)

(p report
   =goal>
   isa report
   state report
   index =i
   =visual-location>
      isa         visual-location
      screen-x    =x
      screen-y    =y
   =visual>
      isa         visual-object
      value       =letter
   ==>
   =visual-location>
   =visual>
   !output! (The =letter is at =x =y)
   !bind! =next (1+ =i)
   !eval! (buffer-chunk visual-location visual)
   =goal>
   state change
   index =next
   )

(p case-1
   =goal>
   isa report
   state change
   index 1
   ==>
   -visual-location>
   -visual>
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )

(p case-2
   =goal>
   isa report
   state change
   index 2
   ==>
   -visual-location>
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )

(p case-3
   =goal>
   isa report
   state change
   index 3
   ==>
   -visual-location>
   =visual> vo-1
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )
(p case-4
   =goal>
   isa report
   state change
   index 4
   ==>
   -visual>
=goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )
(p case-5
   =goal>
   isa report
   state change
   index 5
   ==>
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )
(p case-6
   =goal>
   isa report
   state change
   index 6
   ==>
   =visual> vo-1
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )
(p case-7
   =goal>
   isa report
   state change
   index 7
   ==>
   =visual-location> vl-1
   -visual>
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )
(p case-8
   =goal>
   isa report
   state change
   index 8
   ==>
   =visual-location> vl-1
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )
(p case-9
   =goal>
   isa report
   state change
   >= index 9
   ==>
   =visual-location> vl-1
   =visual> vo-1
   =goal>
   state report
   !eval! (schedule-event-relative .001 'move-it)
   )
)
