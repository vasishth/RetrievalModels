;;; Performance test of the basic declarative retrieval matching.
;;; 
;;; - One target chunk and a fixed number of mismatching ones - 500.
;;;
;;; - All learning turned off.
;;;
;;; - Number of requests will be varied. 
;;;
;;; - Actually making the requests and running the model
;;;   because otherwise the event entry into the queue
;;;   may affect the timing.
;;;
;;; - Sizes will be the number of retrievals to attempt.

(setf *performance-test* '(retrieval-request-count-s-init 
                           retrieval-request-count-s-run
                           (100 500 1000 2000 5000 10000 15000 20000)))


(defvar *count* nil)

(defun retrieval-request-count-s-init (n)
  (declare (ignore n))
  (define-model test (sgp :esc nil :v nil :ncnar nil)
    (chunk-type mem-test slot) 
    (add-dm (g isa mem-test slot g)))
  (dotimes (i 500)
    (let ((name (new-name "mem-test")))
      (add-dm-fct `((,name isa mem-test slot ,name)))))
  (add-post-event-hook 'count-and-request)
  (setf *count* 0))

(defun count-and-request (event)
  (when (and (eq (evt-module event) 'declarative)
             (eq (evt-action event) 'set-buffer-chunk))
    (incf *count*)
    (schedule-module-request 'retrieval (define-chunk-spec isa mem-test slot g) .05)))
  
  
  
(defun retrieval-request-count-s-run (n)
  (schedule-module-request 'retrieval (define-chunk-spec isa mem-test slot g) .05)
  (run-until-condition (lambda () (= *count* n))))