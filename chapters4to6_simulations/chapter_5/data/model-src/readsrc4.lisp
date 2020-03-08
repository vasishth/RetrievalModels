(setf *default-pathname-defaults* #P"/Path/to/model/")
(ql:quickload :trivial-shell)

(defvar *sentences* nil)
(load "sentences-Salvucci2001.lisp")

(defvar *estimating* nil)

(defvar *subject* 1)
(defvar *runs* 10)
(defvar *sentnr* 0)
(defvar *fix-start-time* 0)
(defvar *fix-target*)
(defvar *fix-loc*)
;(defvar *verbose* T)
(defvar *times* nil "list of attachment times for each word")
(defvar *begin-time* 0 "a variable to hold the start time of
processing a word after lexical access")
(defvar *end-time* 0 "a variable to hold the end time of processing a word")
(defvar *my-prep-time*)
(defvar *sacc-suppr*)
(defvar *retrieval-on*)
(defvar *surprisal-on*)
(defvar *traces* nil)
(defvar *rmses* nil)
(defvar *correlations* nil)
(defvar *rangediffs* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; estimated
(unless *estimating*
  (setf
;   *visual-encoding-factor*     .006  ;; Salvucci 2001
;   *visual-encoding-exponent*   .4    ;; Salvucci 2001
;   *my-prep-time*               .135  ;; Salvucci 2001

;   *visual-encoding-factor*     .003  ;; Felix
;   *visual-encoding-exponent*   .4    ;; Felix
;   *my-prep-time*               .125  ;; Felix

   *visual-encoding-factor*     .002  ;; FIT estimation
   *visual-encoding-exponent*   .4    ;; FIT estimation
   *my-prep-time*               .135  ;; FIT estimation

   *retrieval-factor*           .8
   *surprisal-factor*           4
   ))

;; preset
(setf
	*retrieval-on* 		nil
	*surprisal-on* 		nil
	*sacc-suppr* 		nil
	*time-out*			nil
	*rt*				nil  ;; realtime simulation? (bool)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *start-x* 25)
(defparameter *char-width* 7)
(defparameter *n-sentences* 48)
(defparameter *verbose* T)
;(defparameter *n-sentences* 5)

;;;
;;; Eye Tracing
;;;
(defun location->index (location sentence)
  (let ((lx (first location)))
    (do ((i 0 (1+ i))
         (x *start-x* x))
        ((or (< lx x) (>= i (length sentence)))
         (max 0 (- i 1)))
      (incf x (+ (* (length (format nil "~a" (word-name (nth i sentence)))) *char-width*)
                 *char-width*)))))


(defun em-trace->fixations (em-trace menu)
  (unless (or (null em-trace) (null (cdr em-trace)))
    (cons (list (location->index (coerce (cdr (first em-trace)) 'list) menu)
                (round (* 1000 (- (first (second em-trace)) (first (first em-trace))))))
          (em-trace->fixations (cdr em-trace) menu))))



;;; 
;;;   SIMULATION
;;;

(defun draw-stimulus (sentence)
	(let ((window (open-exp-window "Sentence Experiment" 
	                                 :visible t
	                                 :width 700 
	                                 :height 300))
	        (x *start-x*))
	(install-device window)
	(setq stringlist (mapcar (lambda (x) (format nil "~a" (word-name x))) sentence))
   
    (setf reg 1)
    (dolist (text stringlist)
      (add-text-to-exp-window :text text :x x :y 150)
      (unless *estimating*
        (if (eq *subject* 1) (with-open-file (outfile (ensure-directories-exist "output/regions.txt") :direction :output :if-exists :append :if-does-not-exist :create)
          (format outfile "~D ~D ~S ~D ~D ~D ~D~%" (+ *sentnr* 1) reg text x 150 (+ x (* *char-width* (length text))) 160))))
      (incf x (+ (* *char-width* (length text)) *char-width*))
      (incf reg 1)
      )
))



;;;
;;;   Analysis
;;;

(defun sqr (x)
  (* x x))

(defun my/ (x y)
  (handler-case (/ x y)
      (division-by-zero () (format t "division by zero caught!~%") 0)
      (FLOATING-POINT-INVALID-OPERATION () (format t "floating point invalid operation caught!~%") 0)))
  ;  (/ x y))

(defun average (lst) (when lst (my/ (apply #'+ lst) (length lst))))

(defun invert (matrix)
  (cond ((null matrix) nil)
        ((null (first matrix)) nil)
        (t (cons (mapcar #'first matrix) (invert (mapcar #'rest matrix))))))

(defun correlation (xs ys)
  (let* ((mx (average xs))
         (my (average ys))
         (dxs (mapcar #'(lambda (x) (- x mx)) xs))
         (dys (mapcar #'(lambda (y) (- y my)) ys)))
    (my/ (apply #'+ (mapcar #'* dxs dys))
         (* (sqrt (apply #'+ (mapcar #'* dxs dxs)))
            (sqrt (apply #'+ (mapcar #'* dys dys)))))))


(defun print-tabbed (string &rest args)
  (apply #'format (append (list t (substitute #\Tab #\Space string)) args)))


(defun left->right-p (fixations)
  (cond ((< (length fixations) 2) t)
        ((> (first (first fixations)) (first (second fixations))) nil)
        (t (left->right-p (rest fixations)))))

(defun left->right-traces (traces)
  (remove-if-not #'(lambda (trace) (left->right-p (second trace))) traces))


(defun collapse-fixations (fixations)
  (cond ((< (length fixations) 2) fixations)
        ((equalp (first (first fixations)) (first (second fixations)))
         (collapse-fixations
          (cons (list (first (first fixations))
                      (+ (second (first fixations)) (second (second fixations))))
                (rest (rest fixations)))))
        (t (cons (first fixations) (collapse-fixations (rest fixations))))))


(defun update-classes (traces gdurs gcnts ffdurs ffcnts sfdurs sfcnts
                                  0fcnts 1fcnts 2fcnts fcnts)
  (dolist (trace traces)
    (let* ((sentence (nth (first trace) *sentences*))
           (fixations (second trace))
           (gazes (collapse-fixations fixations))
           (last-n (- (length sentence) 1)))
      (dolist (gaze gazes)
        (let ((index (first gaze)))
          (unless (or (<= index 0) (>= index last-n))
            (let ((class (- (third (nth index sentence)) 1)))  ;; frequency class must be in sentence file
              (incf (aref gdurs class) (second gaze))
              (incf (aref gcnts class))))))
      (let ((last nil))
        (do ((fixs fixations (rest fixs)))
            ((null fixs))
          (let* ((fixation (first fixs))
                 (index (first fixation)))
            (unless (or (<= index 0) (>= index last-n))
              (let ((class (- (third (nth index sentence)) 1)))
                (unless (eq index last)
                  (incf (aref ffdurs class) (second fixation))
                  (incf (aref ffcnts class)))))
            (setf last index))))
      (dolist (fixation fixations)
        (let ((index (first fixation)))
          (unless (or (<= index 0) (>= index last-n))
            (let ((class (- (third (nth index sentence)) 1)))
              (when (= 1 (count index fixations :key #'first))
                (incf (aref sfdurs class) (second fixation))
                (incf (aref sfcnts class)))))))
      (do ((i 1 (1+ i)))
          ((>= i last-n))
        (let ((class (- (third (nth i sentence)) 1))
              (cnt (count i fixations :key #'first)))
          (when (= cnt 0) (incf (aref 0fcnts class)))
          (when (= cnt 1) (incf (aref 1fcnts class)))
          (when (= cnt 2) (incf (aref 2fcnts class)))
          (incf (aref fcnts class)))))))


(defun calculate-rmse (data model sds)
  (let* ((sse (apply #'+ (mapcar #'(lambda (d m sd) (sqr (my/ (- d m) sd)))
                                 data model sds)))
         (mse (my/ sse (length data))))
    (sqrt mse)))


(defun analyze ()
  (setf *rmses* nil)
  (setf *correlations* nil)
  (setf *rangediffs* nil)
  (setf *zeros* 0)
  (let ((lrtraces (left->right-traces *traces*))
        ;((lrtraces *traces*)
        (gdurs  (make-array '(5) :initial-element 0))
        (gcnts  (make-array '(5) :initial-element 0))
        (ffdurs (make-array '(5) :initial-element 0))
        (ffcnts (make-array '(5) :initial-element 0))
        (sfdurs (make-array '(5) :initial-element 0))
        (sfcnts (make-array '(5) :initial-element 0))
        (0fcnts (make-array '(5) :initial-element 0))
        (1fcnts (make-array '(5) :initial-element 0))
        (2fcnts (make-array '(5) :initial-element 0))
        (fcnts  (make-array '(5) :initial-element 0)))
    (update-classes lrtraces  gdurs gcnts ffdurs ffcnts sfdurs sfcnts
                    0fcnts 1fcnts 2fcnts fcnts)
    
	(print-tabbed "~%~%")
    (let* ((data '(293 272 256 234 214))
           (sds  '(147 123 132 122 130))
           (model (map 'list #'(lambda (x y) (round (my/ x y))) gdurs gcnts))
           (cor (correlation data model))
           (rmse (calculate-rmse data model sds))
           (drange (- (reduce #'max data) (reduce #'min data)))
           (mrange (- (reduce #'max model) (reduce #'min model)))
           (rangediff (sqrt (sqr (- drange mrange)))))
      (print-tabbed "Gaze-Durations~%")
      (print-tabbed "-----------------------------------------------------~%")
      (print-tabbed "Data ")
      (dolist (x data) (print-tabbed " ~a" (round x)))
      (print-tabbed "~%Model ")
      (dolist (x model) (print-tabbed " ~a" (round x)))
      (print-tabbed "~%~%Correlation: ~,3f~%" cor)
      (print-tabbed "RMSE:  ~,3f~%" rmse)
      (push cor *correlations*)
      (push rmse *rmses*)
      (push rangediff *rangediffs*)
      (if (mymember 0 model) (1+ *zeros*)))
    
    (print-tabbed "~%")
    
    (let* ((data '(248 233 230 223 208))
           (sds  '(147 123 132 122 130)) ;;XXX wrong
           (model (map 'list #'(lambda (x y) (round (my/ x y))) ffdurs ffcnts))
           (cor (correlation data model))
           (rmse (calculate-rmse data model sds))
           (drange (- (reduce #'max data) (reduce #'min data)))
           (mrange (- (reduce #'max model) (reduce #'min model)))
           (rangediff (sqrt (sqr (- drange mrange)))))
      (print-tabbed "First-Fixation-Durations~%")
      (print-tabbed "-----------------------------------------------------~%")
      (print-tabbed "Data ")
      (dolist (x data) (print-tabbed " ~a" (round x)))
      (print-tabbed "~%Model ")
      (dolist (x model) (print-tabbed " ~a" (round x)))
      (print-tabbed "~%~%Correlation: ~,3f~%" cor)
      (print-tabbed "RMSE:  ~,3f~%" rmse)
      (push cor *correlations*)
      (push rmse *rmses*)
      (push rangediff *rangediffs*)
      (if (mymember 0 model) (1+ *zeros*)))
    
    (print-tabbed "~%")
    
    (let* ((data '(265 249 243 235 216))
           (sds  '(147 123 132 122 130)) ;;XXX wrong
           (model (map 'list #'(lambda (x y) (round (my/ x y))) sfdurs sfcnts))
           (cor (correlation data model))
           (rmse (calculate-rmse data model sds))
           (drange (- (reduce #'max data) (reduce #'min data)))
           (mrange (- (reduce #'max model) (reduce #'min model)))
           (rangediff (sqrt (sqr (- drange mrange)))))
      (print-tabbed "Single-Fixation-Durations~%")
      (print-tabbed "-----------------------------------------------------~%")
      (print-tabbed "Data ")
      (dolist (x data) (print-tabbed " ~a" (round x)))
      (print-tabbed "~%Model ")
      (dolist (x model) (print-tabbed " ~a" (round x)))
      (print-tabbed "~%~%Correlation: ~,3f~%" cor)
      (print-tabbed "RMSE:  ~,3f~%" rmse)
      (push cor *correlations*)
      (push rmse *rmses*)
      (push rangediff *rangediffs*)
      (if (mymember 0 model) (1+ *zeros*)))
    
    (print-tabbed "~%")
    
    (let* ((data '(.10 .13 .22 .55 .67))
           (sds  '(.39 .40 .45 .50 .47))
           (model (map 'list #'(lambda (x y) (* 1.0 (my/ x y))) 0fcnts fcnts))
           (cor (correlation data model))
           (rmse (calculate-rmse data model sds))
           (drange (- (reduce #'max data) (reduce #'min data)))
           (mrange (- (reduce #'max model) (reduce #'min model)))
           (rangediff (sqrt (sqr (- drange mrange)))))
      (print-tabbed "Skip-Probabilities~%")
      (print-tabbed "-----------------------------------------------------~%")
      (print-tabbed "Data ")
      (dolist (x data) (print-tabbed " ~,2f" x))
      (print-tabbed "~%Model ")
      (dolist (x model) (print-tabbed " ~,2f" x))
      (print-tabbed "~%~%Correlation: ~,3f~%" cor)
      (print-tabbed "RMSE:  ~,3f~%" rmse)
      (push cor *correlations*)
      (push rmse *rmses*)
      (push rangediff *rangediffs*)
      (if (mymember 0 model) (1+ *zeros*)))
    
    (print-tabbed "~%")
    
    (let* ((data '(.68 .70 .68 .44 .32))
           (sds  '(.39 .40 .45 .50 .47)) ;;XXX wrong
           (model (map 'list #'(lambda (x y) (* 1.0 (my/ x y))) 1fcnts fcnts))
           (cor (correlation data model))
           (rmse (calculate-rmse data model sds))
           (drange (- (reduce #'max data) (reduce #'min data)))
           (mrange (- (reduce #'max model) (reduce #'min model)))
           (rangediff (sqrt (sqr (- drange mrange)))))
      (print-tabbed "One-Fixation-Probabilities~%")
      (print-tabbed "-----------------------------------------------------~%")
      (print-tabbed "Data ")
      (dolist (x data) (print-tabbed " ~,2f" x))
      (print-tabbed "~%Model ")
      (dolist (x model) (print-tabbed " ~,2f" x))
      (print-tabbed "~%~%Correlation: ~,3f~%" cor)
      (print-tabbed "RMSE:  ~,3f~%" rmse)
      (push cor *correlations*)
      (push rmse *rmses*)
      (push rangediff *rangediffs*)
      (if (mymember 0 model) (1+ *zeros*)))
    
    (print-tabbed "~%")
    
    (let* ((data '(.20 .16 .10 .02 .01))
           (sds  '(.39 .40 .45 .50 .47)) ;;XXX wrong
           (model (map 'list #'(lambda (x y) (* 1.0 (my/ x y))) 2fcnts fcnts))
           (cor (correlation data model))
           (rmse (calculate-rmse data model sds))
           (drange (- (reduce #'max data) (reduce #'min data)))
           (mrange (- (reduce #'max model) (reduce #'min model)))
           (rangediff (sqrt (sqr (- drange mrange)))))
      (print-tabbed "Two-Fixation-Probabilities~%")
      (print-tabbed "-----------------------------------------------------~%")
      (print-tabbed "Data ")
      (dolist (x data) (print-tabbed " ~,2f" x))
      (print-tabbed "~%Model ")
      (dolist (x model) (print-tabbed " ~,2f" x))
      (print-tabbed "~%~%Correlation: ~,3f~%" cor)
      (print-tabbed "RMSE:  ~,3f~%" rmse)
      (push cor *correlations*)
      (push rmse *rmses*)
      (push rangediff *rangediffs*)
      (if (mymember 0 model) (1+ *zeros*)))
    
    (print-tabbed "~%Left-to-Right: ~,1f%~%"
                  (* 100 (/ (length lrtraces) (length *traces*))))

    (print-tabbed "Mean-COR:  ~,3f~%" (mean *correlations*))
    (print-tabbed "Mean-RMSE:  ~,3f~%" (mean *rmses*))
    (setf scores (reverse (scores3)))
    (print-tabbed "Scores:  ~S~%" scores)
    (print-tabbed "Mean-Variance-Score:  ~,3f~%" (multiple-value-list (mean-variance scores)))
    (setf myscores scores)
;    (setf myscores (list (first scores) (second scores) (third scores) (fourth scores) (fifth scores)))	;; only GAZE, SFD, SKIP, FIX1
;    (setf myscores (list (third scores) (fourth scores)))	;; only SFD, SKIP
;    (setf myscores (list (first scores) (fourth scores)))	;; only Gaze, SKIP
	(setf pscore (mean myscores))
	(setf pscore2 (apply '+ (multiple-value-list (mean-variance myscores)))) ;; penalized with variance
	(setf pscore3 (+ pscore2 (* 0.1 *zeros*))) ;; penalized with zeros
    pscore3
    ))

;(defun mean-rmse ()
;	(/ (apply '+ *rmses*) (length *rmses*)))

;(defun mean-cor ()
;	(/ (apply '+ *correlations*) (length *correlations*)))

(defun mean (data)
	(/ (apply '+ data) (length data)))

(defun scores1 ()
  (mapcar (lambda (x y) (- x y)) *correlations* *rmses*))

(defun scores2 () ;; rmse + (1-cor)
  (mapcar (lambda (x y) (+ x y)) *rmses* (mapcar (lambda (a) (- 1 a)) *correlations*)))

(defun scores3 () ;; rmse + (1-cor) + 0.01*rangediff
  (mapcar (lambda (x y z) (+ x y z)) *rmses* (mapcar (lambda (a) (- 1 a)) *correlations*) (mapcar (lambda (b) (* 0.01 b)) *rangediffs*)))


;; mean-variance (list)
;; returns a list containing mean and variance of a given list
(defun mean-variance (data)
  (flet ((square (x) (* x x)))
    (destructuring-bind (n xs x2s)
        (reduce #'(lambda (accum xi)
                    (list (1+ (first accum))
                          (+ (second accum) xi)
                          (+ (third accum) (square xi))))
                data :initial-value '(0 0 0))
      (let ((mu (/ xs n)))
        (values mu (- (/ x2s n) (square mu)))))))

(defun mymember (e l)
  (if (eq (member e l) nil) nil t))

;; mymember (e l)
;; returns T if e is element l, nil otherwise
(defun mymember2 (e l)
  (cond
	((null l) nil)
	((eq e (car l)) T)
	(T (mymember2 e (cdr l)))))

;;;
;;;   Reading
;;;

(defun run-trial (sentence)
  (reset)
  (setf *fix-start-time* 0)
  (setf *fix-target* "0")
  (setf *fix-loc* #(0 0))
  (setf *times* nil)
  (load "src98.lexicon.lisp")
  ;    (set-all-base-levels 4)

  ;; ensure EMMA records eye trace
  (setf (trace-eye-p (get-module :vision)) t)
  (draw-stimulus sentence)

  (proc-display)
  (run 30 :real-time *rt*)
  (setf em-trace (reverse (eye-trace (get-module :vision))))
  (unless (neq (aref (cdr (first em-trace)) 1) 0)
    (pop em-trace))
  (em-trace->fixations em-trace sentence)
  ;    (em-trace->fixations (reverse (eye-trace (get-module :vision)))
  ;                       sentence) 
  )


(defun read-sentence (snr)
  (let ((*sentnr* (- snr 1))
        (sentence (append (nth (- snr 1) *sentences*) '((_ nil)))))
    (setf trace (run-trial sentence))
    (push (list *sentnr* trace) *traces*)
;    (unless *estimating*
      (with-open-file (outfile (ensure-directories-exist "output/fixations.txt") :direction :output :if-exists :append :if-does-not-exist :create)
        (dolist (fix trace)
          (if (>= (first fix) 0)
            (format outfile "~D ~D ~D ~S ~D~%" *subject* snr (1+ (first fix)) (format nil "~a" (word-name (nth (first fix) sentence))) (second fix))
            nil)));)
))


(defun read-list (limit)
  (dotimes (n limit "DONE")
    (read-sentence (+ n 1))))


(defun output-params ()
  (sgp :esc)
  (sgp :er)
  (sgp :lf)
  (sgp :dat)
  (sgp :visual-attention-latency)
  (sgp :VISUAL-ENCODING-FACTOR)
  (sgp :VISUAL-ENCODING-EXPONENT)
  (format t "Prep-Time: ~D~%" *my-prep-time*)
  (format t "Saccadic-Suppression: ~S~%" *sacc-suppr*)
  (format t "Retrieval-ON: ~S~%" *retrieval-on*)
  (format t "Surprisal-ON: ~S~%" *surprisal-on*)
  (format t "Retrieval-Factor: ~D~%" *retrieval-factor*)
  (format t "Surprisal-Factor: ~D~%" *surprisal-factor*)
  (format t "Time-Out Regressions ~D~%" *time-out*)
)

(defun output-params-s ()
  (format nil "~S~%~S~%~S~%~S~%~S~%~S~%~S
Prep-Time: ~D
Saccadic-Suppression: ~S
Retrieval-ON: ~S
Surprisal-ON: ~S
Retrieval-Factor: ~D
Surprisal-Factor: ~D
Time-Out Regressions OFF~%"
  (sgp :esc)
  (sgp :er)
  (sgp :lf)
  (sgp :dat)
  (sgp :visual-attention-latency)
  (sgp :VISUAL-ENCODING-FACTOR)
  (sgp :VISUAL-ENCODING-EXPONENT)
  *my-prep-time*
  *sacc-suppr*
  *retrieval-on*
  *surprisal-on*
  *retrieval-factor*
  *surprisal-factor*)
)



(defun read-all-times (times)
  (setf *estimating* nil)
  (setf *traces* nil)
  (delete-output)
  (suppress-warnings (reload))

  (with-open-file (*standard-output* (ensure-directories-exist "output/trace.txt") :direction :output :if-exists :rename :if-does-not-exist :create)
    (output-params)
    (dotimes (i times "ALL DONE")
      (suppress-warnings (reload))
      (setf *subject* (+ i 1))
      (read-list *n-sentences*)))

  (with-open-file (*standard-output* (ensure-directories-exist "output/analysis.txt") :direction :output :if-exists :rename :if-does-not-exist :create)
    (output-params)
    (analyze))

  (analyze)
)

(defun ra (times)
	(read-all-times times))

(defun run-paramlist (paramlist runs)
  (reset)
  (delete-output)
  (setf *estimating* T)
  (setf *verbose* nil)
  (setf *cycle* (1+ *cycle*))
  (setf *traces* nil)
;  (setf *n-sentences* 10)
  (setf
   *visual-encoding-factor*     (first paramlist)
;   *visual-encoding-factor*     0.003
   *visual-encoding-exponent*   (second paramlist)
;   *visual-encoding-exponent*   0.4
   *my-prep-time*               (third paramlist))
;   *my-prep-time*               (first paramlist))
  (suppress-warnings (reload))

  (with-open-stream (*standard-output* (make-broadcast-stream))
    (dotimes (i *runs* "ALL DONE")
      (suppress-warnings (reload))
      (setf *subject* (+ i 1))
      (read-list *n-sentences*)))

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
  (startfit '(vencf vencexp preptime) 'float '(.001 .2 .100) '(0.006 .5 .150) '(.001 .1 .005) 'run-paramlist 'min))
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

;    (system "cd ~/Dropbox/Workspace/ACT-R_EMMA/readpsc")
    (system "rm ~/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener")
    (system "mkfifo ~/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener")
    (loop
      (format t "~%====================================~%")
      (format t "Cycle: ~D~%" (+ 1 *cycle*))
      ;; wait for R to send new parameters
      (format t "Waiting for R... ~%")
      (setf result (system "cat ~/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener"))
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
         (format stream "echo ~F > /Users/felix/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener" score)))
      (system cm))
   (system "echo STOP > ~/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener")
   (system "rm ~/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener")
)

(defun stop-listener ()
   (system "echo STOP > ~/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener")
   (system "rm ~/Dropbox/Workspace/ACT-R_EMMA/readsrc98/listener"))

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun system (command)
  (trivial-shell:shell-command command))
  
(defun delete-output ()
  (if (probe-file "output/raw-fixations.txt") (delete-file "output/raw-fixations.txt"))
  (if (probe-file "output/fixations.txt") (delete-file "output/fixations.txt"))
  (if (probe-file "output/enctime.txt") (delete-file "output/enctime.txt"))
  (if (probe-file "output/regions.txt") (delete-file "output/regions.txt")))




;;;
;;;   MEASURES
;:;

(defun surprisal (obj)
  (let ((word (chunk-slot-value-fct obj 'value)))
    (setf wordsymb (intern (string-upcase word)))
    (if *surprisal-on*
      (setf surpr (word-surprisal (assoc wordsymb (nth *sentnr* *sentences*))))
      (setf surpr nil))
    (cond ((numberp surpr)
	   (setf surpr (* *surprisal-factor* surpr))
	   (format t "~%Surprisal: ~F" surpr) surpr)
	  (t 0))
    ))


(defun retrieval (obj)
  (let ((word (chunk-slot-value-fct obj 'value)))
    (setf wordsymb (intern (string-upcase word)))
    (if *retrieval-on*
      (setf retr (word-retrieval (assoc wordsymb (nth *sentnr* *sentences*))))
      (setf retr nil))
    (cond ((numberp retr) (format t "~%Retrieval: ~F" (* 0.001 (* *retrieval-factor* retr))) (* 0.001 (* *retrieval-factor* retr)))
          (t .001))   ;; if zero then goal modification event will finish BEFORE 
                      ;; goal modification of the actual production (wrong order!)
    ))


(defun frequency (obj)
  (let ((word (chunk-slot-value-fct obj 'value)))
    (setf wordsymb (intern (string-upcase word)))
    (setf freq (word-frequency (assoc wordsymb (nth *sentnr* *sentences*))))
    (if (numberp freq)
      (setf freq (/ freq 1e6))  ; divided by 1 million
;      (setf freq (/ freq 69974))  ; divided by the maximum
      (setf freq 0.01))
    (format t "~%Word: ~S" word)
    (format t "~%Frequency: ~F" freq)
    freq))



;;
;;   FIXATIONS
;;

(defun output-enctime (obj enctime location)
  (let ((word (chunk-slot-value-fct obj 'value))
        (wordnr (car (multiple-value-bind (item present?) (parse-integer (subseq location 15) :junk-allowed t) (list item present?)))))
  (if wordnr (setq wordnr (+ wordnr 1)))
  (unless *estimating*
    (with-open-file (outfile (ensure-directories-exist "output/enctime.txt") :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~S ~S ~S ~S ~D~%" *subject* (+ *sentnr* 1) wordnr word (round (* enctime 1000)))))))

;(defun output-fixtime (trgt fixtime)
;  (let ((wordnr (car (multiple-value-bind (item present?) (parse-integer (subseq trgt 4)) (list item present?)))))
;    (setf wordnr (- wordnr 1))   ; because first word is not 'real'
;    (if (> wordnr -1)
;      (setf word (word-name (nth wordnr (nth *sentnr* *sentences*))))
;      (setf word 'FIRST))
;    (with-open-file (outfile "fixations.txt" :direction :output :if-exists :append :if-does-not-exist :create)
;      (format outfile "~S ~S ~S ~S ~S ~D~%" *subject* (+ *sentnr* 1) (+ wordnr 2) word trgt (round (* fixtime 1000))))))

(defun output-fixtime (loc fixtime)
    (unless *estimating*
      (with-open-file (outfile (ensure-directories-exist "output/raw-fixations.txt") :direction :output :if-exists :append :if-does-not-exist :create)
        (format outfile "~S ~D ~D ~D ~D~%" *subject* (+ *sentnr* 1) (aref loc 0) (aref loc 1) (round (* fixtime 1000))))))



(defun report-fixation-time (nextloc)
   (setq fixtime (- (mp-time) *fix-start-time*))
    (format t "  
     ====================================
        FIXATION on ~A: ~6,3F
     ====================================~%" *fix-loc* fixtime)
  (output-fixtime *fix-loc* fixtime)
  (setq *fix-start-time* (mp-time))
  (setq *fix-loc* nextloc)
)


(defun wait-retrieval (obj)
  (setf retr (retrieval obj))
;  (mod-focus state "reading")
;  (schedule-event-relative retr 'mod-focus :priority :max :params '(list (state "reading")) :output 'medium)
  (schedule-event-relative retr 'finish-retrieval :priority :max :output 'medium :module 'parsing 
                           :details (concatenate 'string "Retrieval-finished " (write-to-string obj))
                           )
)

(defun finish-retrieval ()
  (mod-focus retrieval "free")
  (set-end-time)
)
    

(defun set-begin-time (word)
  (setq *begin-time* (mp-time))
  (setq *word* word)
  )


(defun set-end-time ()
  (setq *end-time* (mp-time))
  (let ((attach-time (- *end-time* *begin-time*)))
    (when *verbose*
      (format t "
     ====================================
        RETRIEVAL TIME on ~A: ~6,3F
     ====================================~%
     " *word* attach-time))
    (push-last attach-time *times*)
    )
  )

 
;;|#


;;;
;;;   THE MODEL
;;;

;; EMMA parameters
(setf global-params `(:VISUAL-ENCODING-FACTOR    ,*visual-encoding-factor*
				      :VISUAL-ENCODING-EXPONENT  ,*visual-encoding-exponent*
					  :v						 ,*verbose*))

(clear-all)

(define-model readsrc

(sgp :act nil
;     :esc t         ; enable subsymbolic computation (utilities and activations)
;     :egs 0         ; utility noise (default: 0)
     :er nil        ; enable randomness (default: nil) 
;     :ans 1         ; activation noise
     :dat 0.05      ; default action time (time in s for a production to fire) (default: 0.05; Salvucci2001: 0.01)
     :lf 1.0        ; latency factor (retrieval latency) (default: 1.0; Salvucci2001: 5.0)
;     :mas nil 
;     :ga 1.0 
;     :imaginal-activation 1.0 
     :show-focus t 
     :randomize-time t
     :VWT t          ;; default: NIL  : Virtual Window trace controls the << ... >> outputs from virtual windows


;     :visual-attention-latency 0
      ) 

(sgp-fct global-params)

(chunk-type meaning word)
(chunk-type read-sentence state retrieval last-loc last-vis)

(add-dm
    (goal ISA read-sentence state "looking" retrieval "free" last-loc nil last-vis nil)
    (STOP ISA meaning word "_"))



#|
(P find-previous-word
   =goal>
      ISA         read-sentence
      state       reading
   ?visual>
      state       free
   ==>
   +visual-location>
      ISA         visual-location
      ;:attended   nil
    < screen-x    current
   =goal>
      state       attending
)
|#

(P find-next-word
   =goal>
      ISA         read-sentence
      state       "reading"
   =visual-location>
       ISA         visual-location
   ?visual>
       processor       free
;   ?retrieval>
;       state      free
   =retrieval>
       ISA         meaning
;       word        =word   
   ==>
   +visual-location>
      ISA         visual-location
;      :attended   nil
    > screen-x    current
      screen-x    lowest
   =goal>
      state       "looking"
      last-loc    =visual-location

   ; !eval! (set-end-time)
)

(P attend-word
   =goal>
       ISA         read-sentence
       state       "looking"
   =visual-location>
       ISA         visual-location
   ?visual>
;       execution       free   ; use this for "saccadic suppression"
;       processor       free
==>
   +visual>
       ISA         move-attention
       screen-pos  =visual-location
   =goal>
       state       "attending"
   =visual-location>
)
(spp attend-word :at 0)


(p retrieve-word
    =goal>
       ISA         read-sentence
       state       "attending"
    =visual>
       ISA         text
       value       =word
     - value       "_"
    ?retrieval>
       state      free
==>
    +retrieval>
       ISA         meaning
       word        =word
    =goal>
       state       "reading"
;       retrieval   "busy"
       last-vis    =visual
    =visual>
)


(P stop-reading
    =goal>
       ISA         read-sentence
    =visual>
       ISA         text
       value       "_"
    ?retrieval>
       state      free
==>
    =goal>
       state       "stop-reading"
)  



(setf *actr-enabled-p* t)

;(spp find-previous-word :u -4)

(goal-focus goal)

)

(load "src98.lexicon.lisp")
