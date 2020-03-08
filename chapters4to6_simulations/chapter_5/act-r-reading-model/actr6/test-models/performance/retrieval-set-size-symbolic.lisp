;;; Performance test of the basic declarative retrieval matching.
;;; 
;;; - One target chunk, and a variable number of mismatching ones.
;;;
;;; - All learning turned off.
;;;
;;; - A fixed number of retrieval requests will be made - 10000. 
;;;
;;; - Runs the model making the requests as it goes.
;;;
;;; - Sizes will be the number of other chunks of the same type
;;;   in DM.

(setf *performance-test* '(retrieval-set-size-s-init 
                           retrieval-set-size-s-run
                           (1 10 100 500 1000 2000 5000 10000)))

(defvar *count* nil)

(defun retrieval-set-size-s-init (n)
  (define-model test (sgp :esc nil :v nil :ncnar nil)
    (chunk-type mem-test slot) 
    (add-dm (g isa mem-test slot g)))
  (dotimes (i n)
    (let ((name (new-name "mem-test")))
      (add-dm-fct `((,name isa mem-test slot ,name)))))
  (add-post-event-hook 'count-and-request)
  (setf *count* 0))
  

(defun count-and-request (event)
  (when (and (eq (evt-module event) 'declarative)
             (eq (evt-action event) 'set-buffer-chunk))
    (incf *count*)
    (schedule-module-request 'retrieval (define-chunk-spec isa mem-test slot g) .05)))


(defun retrieval-set-size-s-run (n)
  (declare (ignore n))
  (schedule-module-request 'retrieval (define-chunk-spec isa mem-test slot g) .05)
  (run-until-condition (lambda () (= *count* 10000))))
