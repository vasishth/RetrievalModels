;;; Performance test of the basic declarative retrieval matching.
;;; 
;;; - One target chunk, and a variable number of mismatching ones.
;;;
;;; - All learning turned off.
;;;
;;; - Vary both the number of chunks in DM and the number of
;;;   productions in the model.
;;;
;;; - There will be 2 productions in the model which will
;;;   cycle back-and-forth.  One makes a request and the
;;;   other harvests it.  They'll also test the goal and
;;;   query some other modules just to fill them out.  The
;;;   non-matching productions will mis-match because of
;;;   a query and half will have the same test as the
;;;   first matching production and half will have the same
;;;   test as the second.
;;;
;;; - Runs the model for a fixed lenght of time - 1000s
;;;
;;; - Sizes will be a cons of the number of productions
;;;   and the number of chunks.

(setf *performance-test* '(vary-both-set-sizes-init 
                           vary-both-set-sizes-run
                           ((0 . 1) (9 . 10) (99 . 100) (499 . 500) (999 . 1000) (1999 . 2000) (9 . 2000) (1999 . 10))))



(defun vary-both-set-sizes-init (n)
  (let ((p-count (car n))
        (d-count (cdr n)))
    (define-model test (sgp :esc nil :v nil)
      
      (chunk-type goal-test slot) 
      (chunk-type mem-test slot)
      
      (add-dm (m isa mem-test slot m))
      
      (define-chunks (g isa goal-test slot start))
      (goal-focus g)
      
      (p retrievel
         ?retrieval>
         state free
         buffer empty
         ==>
         +retrieval>
         isa mem-test slot m
         )
      (p harvest
         =retrieval>
         isa mem-test
         ==>
         )
      )
    
    (dotimes (i p-count)
      (p-fct `(,(new-name "p") =retrieval> isa mem-test slot g ==>)))
    
    (dotimes (i d-count)
      (let ((name (new-name "mem-test")))
        (add-dm-fct `((,name isa mem-test slot ,name)))))))
  
(defun vary-both-set-sizes-run (n)
  (declare (ignore n))
  (run 1000.0))