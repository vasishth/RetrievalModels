
;;; ESTIMATION ;;;
(defun run-paramlist (paramlist runs)
  (reset)
  (delete-output)
  (setf *estimating* T)
  (setf *cycle* (1+ *cycle*))
  (setf *traces* nil)
;  (suppress-warnings (reload))

;  (with-open-stream (*standard-output* (make-broadcast-stream))
    (dotimes (i runs "ALL DONE")
      (suppress-warnings (reload))
      (setf *show-window* nil)
	  (setf *VERBOSE* nil)
  	  (setf *dcnn* nil)

      (setf *simulation* (+ i 1))
      (read-list *n-sentences*))
    ;)

  (setf thescore (analyze))
  (with-open-file (*standard-output* (ensure-directories-exist "output/estimation.txt") :direction :output :if-exists :append :if-does-not-exist :create)
    (format t "~%==================================================================================~%")
    (format t "Estimation Cycle: ~D~%" *cycle*)
	(format t "~D ~S~%" pscore paramlist)
;    (format t "CORs: ~D~%" (reverse *correlations*))
;    (format t "RMSEs: ~D~%" (reverse *rmses*))
    (print-tabbed "Mean-COR:  ~,3f~%" (mean *correlations*))
    (print-tabbed "Mean-RMSE:  ~,3f~%" (mean *rmses*))
    (analyze))

  (format t "Cycle: ~D~%" *cycle*)
  (format t "paramlist: ~S~%" paramlist)
  (format t "Score: ~F~%" thescore)
  thescore
)

(defun estimate-lisp (runs)
  (setf *estimating* T)
  (setf *runs* runs)
  (setf *cycle* 0)
  (delete-output)
  (with-open-file (*standard-output* (ensure-directories-exist "output/estimation.txt") :direction :output :if-exists :rename :if-does-not-exist :create)
    (format t "Runs per estimation cycle: ~D~%" *runs*)
    (format t "N Sentences: ~D~%" *n-sentences*)
    (format t "Saccadic-Suppression: ~S~%" *sacc-suppr*)
    (format t "Retrieval-ON: ~S~%" *retrieval-on*)
    (format t "Surprisal-ON: ~S~%" *surprisal-on*)
    (format t "Retrieval-Factor: ~D~%" *retrieval-factor*)
    (format t "Surprisal-Factor: ~D~%~%" *surprisal-factor*)
	(format t "Estimating: (visual-encoding-factor visual-encoding-exponent preptime) ~%~%"))
  (suppress-warnings (load "fit.lisp"))
;  (startfit '(preptime) 'float '(.100) '(.150) '(.005) 'run-paramlist 'min))
;  (startfit '(vencf preptime) 'float '(.001 .100) '(0.008 .150) '(.001 .005) 'run-paramlist 'min))
;  (startfit '(vencf vencexp preptime) 'float '(.0005 .1 .100) '(.008 .5 .150) '(.001 .1 .005) 'run-paramlist 'min))
  (startfit '(vencf vencexp preptime) 'float '(.0005 .1) '(.008 .5) '(.001 .1) 'run-paramlist 'min))
;  (startfit '(vencf vencexp preptime) 'float '(.002 .1 .110) '(.006 .6 .150) '(.001 .1 .001) 'run-paramlist 'min))


 

(defun estimate (runs)
  (setf *estimating* T)
  (setf *runs* runs)
  (setf *cycle* 0)
  (delete-output)
  (with-open-file (*standard-output* (ensure-directories-exist "output/estimation.txt") :direction :output :if-exists :rename :if-does-not-exist :create)
    (format t "Runs per estimation cycle: ~D~%" *runs*)
    (format t "N Sentences: ~D~%" *n-sentences*)
    (format t "Saccadic-Suppression: ~S~%" *sacc-suppr*)
    (format t "Retrieval-ON: ~S~%" *retrieval-on*)
    (format t "Surprisal-ON: ~S~%" *surprisal-on*)
    (format t "Retrieval-Factor: ~D~%" *retrieval-factor*)
    (format t "Surprisal-Factor: ~D~%~%" *surprisal-factor*)
    (format t "Estimating: (visual-encoding-factor visual-encoding-exponent preptime) ~%~%"))

;    (system "cd ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel")
    (system "rm ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener")
    (system "mkfifo ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener")
    (loop
      (format t "~%====================================~%")
      (format t "Cycle: ~D~%" (+ 1 *cycle*))
      ;; wait for R to send new parameters
      (format t "Waiting for R... ~%")
      (setf result (system "cat ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener"))
      (if (search "STOP" result) (return))
      ;; convert to list
      (setf result (split-by-one-space result))
      (setf paramlist nil)
      (dolist (x result) (push (read-from-string x) paramlist))
      (setf paramlist (reverse paramlist))
      (format t "~S~%" paramlist)
      ;; test whether one of the values is negative
      (if (neq 1 (dolist (val paramlist) (if (< val 0) (return 1))))
        (setf score (run-paramlist paramlist runs))  ;; if not: run model with new parameters
        (setf score "NA"))  ;; if negative: set score NA and skip model run
;      (setf score 1.2234)
      ;; submit the result to R
      (setf cm (with-output-to-string (stream)
         (format stream "echo ~F > /Users/felix/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener" score)))
      (system cm))
   (system "echo STOP > ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener")
   (system "rm ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener")
)

(defun stop-listener ()
   (system "echo STOP > ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener")
   (system "rm ~/Dropbox/Workspace/ACT-R_EMMA/readingmodel/listener"))

(defun system (command)
  (trivial-shell:shell-command command))