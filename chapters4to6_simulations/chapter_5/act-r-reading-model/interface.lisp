;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INTERFACE FOR READING MODEL
;;;
;;; Last change: 12-03-05
;;;
;;; Changes compared to older versions:
;;;
;;; - *width* as global parameter
;;; - *n-sentences* set automatically
;;; - surprisal, retrieval, and frequency are returned based on position in the sentence, not word-name as it used to be
;;; - initial eye location is set near to the beginning of the sentence (avoids gigantic eccentricity values, overshoots, and refixations of the first word)
;;; - missing frequency values are filled by 0.0001 (used to be 0.01, wich is very high)
;;;


;(ql:quickload :trivial-shell)

;(clear-all)
(load "interface-emma.lisp")
(load "interface-experiment.lisp")
(load "shortcuts.lisp")
(load "analysis.lisp")
;(load "estimation.lisp")

(defvar *sentences* nil)
(load "sentences-psc.lisp")

(defparameter *n-sentences* (length *sentences*))
;(defparameter *n-sentences* 144)

;;; INTERFACE PARAMETERS ;;;
(defvar *max-time* 30.0 "maximum time to run.")
(defparameter *VERBOSE* T "verbose parameter")
(defparameter *real-time* T "real time simulation")
(defparameter *width* 700)
(defparameter *start-x* 25)
(defparameter *char-width* 7)
(defparameter *dcnn* T "dynamic chunk name normalizing during run time. Switch off for better performance")
(defparameter *show-window* T) 
(defparameter *trace-to-file* nil)
(defparameter *record-times* T)
(defparameter *estimating* nil)

;;; ENVIRONMENT VARIABLES ;;;
(defvar *experiment* 1 "number or name")
(defvar *simulation* 1)
(defvar *item* 1 "current sentence or condition number (starting with 1) or name")
(defvar *sentence* nil "holds the current sentence string")
(defvar *sentence-plist* nil "current sentence augmented with word properties")
(defvar *word* nil "current word")
(defvar *sentnr* 0 "current sentence number (starting with 0)")
(defvar *fix-start-time* 0)
(defvar *fix-loc* '(0 0) "current fixation location")
;(defvar *subject* 1)
(defvar *runs* 10)


;;; EXPERIMENTAL VARIABLES ;;;
(defvar *experiment-results* nil "a variable to hold experiment results")
(defvar *times* nil "list of attachment times for each word")
(defvar *times2* nil "list of attachment times for each word augmented with word and pos")
(defvar *attached-items* nil "list of positions of words being attached so far")
(defvar *begin-time* 0 "a variable to hold the start time of processing a word after lexical access")
(defvar *end-time* 0 "a variable to hold the end time of processing a word")
(defvar *traces* nil "list of all eye movement traces")
(defvar *exp-traces* nil "eye movement traces of one experimental run")
(defvar *param-set* 0)
(defvar *rmses* nil)
(defvar *correlations* nil)
(defvar *rangediffs* nil)
(defvar *workflow* nil "when testing multiple model configurations in row")

;(defvar *my-prep-time*)


;;; MODEL PARAMETERS ;;;
(defvar *visual-encoding-factor* 0.006 "EMMA encoding time scaling factor")
(defvar *visual-encoding-exponent* 0.4 "EMMA encoding time exponent")
(defvar *prep-time* 0.135 "EMMA saccade preparation time")
(defvar *surprisal-factor* .001)
(defvar *surprisal-hl-factor* .01)
;(defvar *retrieval-factor* 0.3)
(defvar *retrieval-latency* .14 "Latency Factor F (0.14 for for LV2005, VL2006) (0.46 for VBRD2008)")


;;; EMMA PRESETS ;;;
(unless *workflow* 
  ;(defparameter *sacc-suppr* T)
  ;(defparameter *time-out* T)
  (defparameter *surprisal-on* nil)
  (defparameter *surprisal-hl-on* t)
  (defparameter *retrieval-on* t)
  )


;;; ESTIMATED MODEL PARAMETERS ;;;
(unless *estimating* (setf
  ;*visual-encoding-factor*    .006  ;; Salvucci 2001
  ;*visual-encoding-exponent*  .4    ;; Salvucci 2001
  ;*prep-time*                 .135  ;; Salvucci 2001

  *visual-encoding-factor*    .003  ;; FIT estimation
  *visual-encoding-exponent*  .4    ;; FIT estimation
  *prep-time*                 .120  ;; FIT estimation
  
;  *surprisal-factor*          .02
  *surprisal-factor*          .001
  *retrieval-latency*         0.2
  *surprisal-hl-factor*       .0
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;(defmethod end-simulation ((win exp-window))
;  (window-close win)
;  (find-window "Listener"))



;;;
;;;   Reading
;;;

(defun run-trial (sentence &optional params)
  (setf plist sentence)
  (if (not (listp plist)) (setf plist (string->listOflists plist)))
  (if (listp sentence)
    (progn
        (setf stringlist (mapcar (lambda (x) (format nil "~a" (word-name x))) sentence))
        (setf sentence (format nil "~{~a ~}" stringlist))))

  (reset)

  (setf *sentence* sentence)
  (setf *sentence-plist* plist)
  (setf *fix-start-time* 0)
  (setf *fix-loc* #(10 150))
  (setf *times* nil)
  (setf *times2* nil)
  (setf *attached-items* nil)
  
;  (load "psc.lexicon.lisp")
;  (set-all-base-levels 4)

  (let ((window (open-exp-window "Sentence Experiment"
				 :visible *show-window*
				 :width *width*
				 :x 300
				 :y 300))
        (x *start-x*))
    
    (add-text-to-exp-window :text sentence :x x :y 150)    
    (install-device window)
    (proc-display)
    
    (if params
      (sgp-fct params)
      (setf params nil))

  (when *VERBOSE* (format t "~%~%------------------------------------------------
Simulation ~A~%Sentence ~A: ~A~%~%" *simulation* (+ 1 *sentnr*) sentence))
  
  ;; ensure EMMA records eye trace
  (reset-emma)

  (run 30 :real-time *real-time*)

  (setf em-trace (get-em-trace))

  (setf fixations (em-trace->fixations em-trace plist))
  (if *record-times* (record-fixations plist *item* fixations))
  (record-attachment-times *item*)
  fixations
))


(defun read-sentence (snr &optional (params nil))
  (let ((sentence (append (nth (- snr 1) *sentences*) '((_ nil)))))
    (setf *item* snr)
    (setf *sentnr* (- snr 1))
    (setf trace (run-trial sentence params))
    (push (list *sentnr* trace) *traces*)
    trace
))
;    (unless *estimating*
;      (with-open-file (outfile (ensure-directories-exist "output/fixations1.txt") :direction :output :if-exists :append :if-does-not-exist :create)
;        (dolist (fix trace)
;          (if (>= (first fix) 0)
;            (format outfile "~D ~D ~D ~S ~D~%" *simulation* snr (1+ (first fix)) (format nil "~a" (word-name (nth (first fix) ;sentence))) (second fix))  ; for corpus like psc or src
            ;(format outfile "~D ~D ~D ~S ~D ~S ~D~%" *simulation* snr (format nil "~a" (itemnumber (nth (first fix) sentence))) (format nil "~a" (condname (nth (first fix) sentence))) (1+ (first fix)) (format nil "~a" (word-name (nth (first fix) sentence))) (second fix))  ; for corpus with itemnumber and condition
;            nil)));)
;))


(defun read-list (limit &optional (params nil))
  (dotimes (n limit "DONE")
    (read-sentence (+ n 1) params)))


(defun output-params ()
  (sgp 
    :esc
    :er
    :dat)
  (sgp 
    :visual-attention-latency
    :VISUAL-ENCODING-FACTOR
    :VISUAL-ENCODING-EXPONENT
    :SACCADE-PREPARATION-TIME
    :lf 
    :SURPRISAL-FACTOR)
  (format t ":SURPRISAL-HL-FACTOR ~D~%" *surprisal-hl-factor*)
  (format t ":Retrieval-ON ~S~%" *retrieval-on*)
  (format t ":Surprisal-ON ~S~%" *surprisal-on*)
  (format t ":Surprisal-HL-ON ~S~%" *surprisal-hl-on*)
  (format t ":Time-Out ~D~%" *time-out*)
  (format t ":Saccadic-Suppression ~S~%" *sacc-suppr*)
)


(defun read-all-times (times &optional (params1 nil))
 (let ((params (append '(:v nil :dcnn nil :trace-detail low :model-warnings nil) params1)))
  (suppress-warnings (reload))
  (setf *estimating* nil)
  (setf *traces* nil)
  (setf *experiment* 1)
  (setf *show-window* nil)
  (setf *VERBOSE* nil)
  (setf *dcnn* nil)
  (delete-output)

  (format t "reading ~D sentences ~D times~%" *n-sentences* times)
  ;(with-open-file (*standard-output* (ensure-directories-exist "output/trace.txt") :direction :output :if-exists :rename :if-does-not-exist :create)
    
  (dotimes (i times "ALL DONE")
      ;(suppress-warnings (reload))
      (setf *simulation* (+ i 1))
      (format t "Iteration ~A~%" (1+ i))
      (read-list *n-sentences* params));)
  (output-params)
  (with-open-file (*standard-output* (ensure-directories-exist "output/params.txt") :direction :output :if-exists :rename :if-does-not-exist :create)
    (output-params))
  (format t "ALL DONE")
  ;(analyze)
))



(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))
