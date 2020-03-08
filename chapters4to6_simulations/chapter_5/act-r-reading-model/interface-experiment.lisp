;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CORPUS READING MODEL
;;; RUN EXPERIMENT AND ANALYZE
;;;


(defun run-paramset (&optional (name "psc") (iterations 1) (params1 nil))
  (unless *estimating* (delete-paramsearch) (setf *param-set* 0))
  (delete-output)
  (setf *param-set* (1+ *param-set*))
  ;; clean-up procedure to prevent process from slowing down 
  (when *estimating*
    (suppress-warnings (reload))
    (setf *estimating* t)
    (when (= (rem *param-set* 10) 0)
      (sleep 60)))
 (let* ((location (concatenate 'string "paramsearch/" name "/"))
 		(fixfile (concatenate 'string location (write-to-string *param-set*) "-fixations.txt")))
  (setf *show-window* nil)
  (setf *VERBOSE* nil)
  (setf *experiment* name)
  (setf *item* 0)
  (setf *simulation* 0)
  (setf *traces* nil)
  ;(setf params (append '(:v nil :ans nil :randomize-time nil :dcnn nil :trace-detail low :model-warnings nil) params1))
  ;; setting randomize time to nil results in qualitative differences
  (setf params (append '(:v nil :dcnn nil :trace-detail low :model-warnings nil) params1))

  (with-open-file (paramsfile (ensure-directories-exist (concatenate 'string location "paramsearch.txt")) :direction :output :if-exists :append :if-does-not-exist :create)
  	(format paramsfile "~A ~A ~A \"~A\"~%" name *param-set* fixfile params1)
  )

  (format t "~%_____________________________________________________________________
~%Experiment: ~A ~A~%" *param-set* params)
  
  (dotimes (j iterations "DONE")
    (setf *simulation* (1+ j))
	  (format t "Iteration ~A~%" (1+ j))
	  ;(suppress-warnings (reload))
	  ;(setf *show-window* nil)
	  ;(setf *VERBOSE* nil)
	  ;(setf *dcnn* nil)

    ;; sentences  
    (dotimes (n *n-sentences*)
      (let ((fixations (read-sentence (+ n 1) params)))
        (record-fixations plist *item* fixations fixfile)
;        (format t ".")
      ))
;  (format t "~%")
	)
;  (setf *show-window* T)
;  (setf *VERBOSE* T)
))



(defmacro search-param-space (&optional experiment (iterations 10) (param-space '*pspace*))
  (suppress-warnings (reload))
  (setf *estimating* T)
  (setf *record-times* nil)
  (setf *param-set* 0)
  (setf *trace-to-file* nil)
  (setf *experiment-results* nil)
  (setf *experiment* (concatenate 'string experiment "-" (datetimestamp)))
  (delete-output)

  (let ((code `(run-paramset ,*experiment* ,iterations))
;  (let ((code `(run-em-experiment ',experiment ,iterations))
	(param-vars nil)
	(parameters '(list :v nil)))
;	(parameters '(list :ans nil :randomize-time nil)))

    (dolist (p (eval param-space))
      (let ((new-var (gensym))
	    (parameter (first p)))
	(push (cons parameter new-var) param-vars)
	(push-last parameter parameters)
	(push-last new-var parameters)))
        
    (push-last parameters code)
    
    (dolist (p (eval param-space))
      (let* ((new-var (cdr (assoc (first p) param-vars)))
	     (init-val (second p))
	     (final-val (third p))
	     (step-val (fourth p))
	     (do-code `(do ((,new-var ,init-val (+ ,new-var
						   ,step-val)))
			   ((> ,new-var ,final-val)))))
	(setf code (push-last code do-code))
	))
;	(push-last '(setf *estimating* nil) code)
    code)
)



(defun aggregate-conditions (results conds)
  (let ((condresults nil))
    (dolist (c conds)
     (let* ((cname (first c))
       (regions (rest (rest c)))
       (cresult (list cname)))
     (dolist (r regions)
      (let* ((rname (first r))
        (humandata (third (assoc rname (rest (assoc cname (fifth (first results)))))))
        (modeldata (mean (mapcar
          #'(lambda(res)
           (let* ((conds (fifth res))
            (thiscond (cdr (assoc cname conds)))
            (thisreg (cdr (assoc rname
              thiscond))))
           (car thisreg)))
          results))))
      (push-last (list rname (float modeldata) (float humandata)) cresult)))
     (push-last cresult condresults)))
    ;(pprint condresults)
    condresults
    ))
    
(defun aggregate-contrasts (results contrasts)
  (let ((cntrstresults nil))
    (dolist (c contrasts)
     (let* ((name (first c))
       (humandata (second (cdr (assoc name (sixth (first results))))))
       (modeldata (mean (mapcar
        #'(lambda(res)
          (let* ((cntrsts (sixth res))
            (thiscntrst (cdr (assoc name
              cntrsts))))
          (car thiscntrst)))
        results))))
     (push-last (list name modeldata humandata) cntrstresults)))
    cntrstresults
    ))



(defun display-em-experiment-result (result measure)
  (let ((name (first result))
	(params (second result))
	(corr (cdr (third result)))
	(rmsd (cdr (fourth result)))
	(conditions (fifth result))
	(contrasts (sixth result))
    )
    (when *VERBOSE* (format t "
_____________________________________________________________________

Simulation results for experiment ~A with parameters ~A
R: ~6,3F  R-squared: ~6,3F  RMSD: ~6,3F
Empirical times inside []
_____________________________________________________________________
~A
" name params corr (* corr corr) rmsd measure))
    (dolist (c  conditions)
      (let ((cname (first c))
	    (regions (rest c)))
	(when *VERBOSE* (format t "
      ~A condition
" cname))
	(dolist (r regions)
	  (let ((rname (first r))
		(modeldata (second r))
		(humandata (third r)))
	    (when *VERBOSE* (format t "            ~20s: ~D    [~D]
" rname modeldata humandata))
	    ))))
    
    (when *VERBOSE* (format t"
    _________________________________________________________________

"
            ))
    
    (dolist (c contrasts)
      (let ((cntrname (first c))
	    (model (second c))
	    (human (third c)))
	(when *VERBOSE* (format t "      Contrast ~20s: ~6,3F    [~6,3F]
" cntrname model human
))))

     (when *VERBOSE* (format t "_____________________________________________________________________"))
corr
))


(defun delete-paramsearch ()
  (if (probe-file "paramsearch/paramsearch.txt") (delete-file "paramsearch/paramsearch.txt"))
  (if (probe-file "paramsearch/1-fixations.txt") (delete-file "paramsearch/1-fixations.txt"))
)
