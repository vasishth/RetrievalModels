(clear-all)

(defvar *scale* 'word)

(defun run-it ()
  (setf *scale* 'word)
  (reset)
  (run-the-test)
  
  (setf *scale* 'phrase)
  (reset)
  (run-the-test)
  
  (setf *scale* 'letter)
  (reset)
  (sgp :optimize-visual nil)
  (run-the-test)
  
  (setf *scale* 'word)
  (reset)
  (sgp :optimize-visual nil)
  (run-the-test)
  
  (setf *scale* 'phrase)
  (reset)
  (sgp :optimize-visual nil)
  (run-the-test))

  
  
(defun run-the-test ()
  (install-device (open-exp-window "" :visible nil))
  (add-text-to-exp-window :text "This" :x 10 :y 20 :color 'red)
  (add-text-to-exp-window :text "is" :x 45 :y 20)
  (add-text-to-exp-window :text "a phrase" :x 65 :y 20)
  (proc-display)
  (print-visicon)
  (run .185)
  (buffer-chunk visual)
  (print-visicon))
  


(define-model foo
                (sgp-fct (list :v *out-file* :cmdt *out-file* :seed '(100 1)))
              (p-fct `(start
                       =visual-location>
                       isa visual-location
                       ?visual>
                       buffer empty
                       state free
                       ==>
                       +visual>
                       isa move-attention
                       screen-pos =visual-location
                       scale ,*scale*)))
