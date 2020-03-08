;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CORPUS READING MODEL
;;; EMMA INTERFACE
;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; METRICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-frequency (obj)
;  (unless (not (listp *sentence-plist*))
  (let* ((location (obj->location obj))
      (word (chunk-slot-value-fct obj 'value))
      (freq (word-frequency (obj->wordinfo obj *sentence-plist*))))
    (if (numberp freq)
      (setf freq (/ freq 1e6))  ; divided by 1 million
;      (setf freq freq)
;      (setf freq 0.01))
      (setf freq nil))
;    (format t "~%Pos: ~a" (location->index location *sentence-plist*))
;    (format t "~%Word: ~S" word)
;    (format t "~%loc: ~F" location)
;    (format t "~%        Frequency: ~S" freq)
    freq))

(defun surprisal (obj)
; (let ((sf (car (with-open-stream (*standard-output* (make-broadcast-stream)) (sgp :SURPRISAL-FACTOR)))))
 (let ((sf (parsing-module-srpr (get-module PARSING))))
  (if *surprisal-on*
    (setf surpr (word-sp (obj->wordinfo obj *sentence-plist*)))
    (setf surpr nil)) ; else
  (cond ((numberp surpr)
      (setf surpr (* sf surpr))
      ;(format t "~%        Surprisal: ~F" surpr) 
      surpr)
    (t 0))))

(defun surprisal-hl (obj)
 (let ((sf (parsing-module-srprhl (get-module PARSING)))
      (word (word-name (obj->wordinfo obj *sentence-plist*)))) 
  (if *surprisal-hl-on*
    (setf surpr (word-sp (obj->wordinfo obj *sentence-plist*)))
    (setf surpr nil))
  (cond ((numberp surpr)
    (setf surpr (* sf surpr))
    (when *VERBOSE* (format t "~%   > HL-Surprisal (~S): ~3,3F" word surpr)) surpr)
  (t 0))))


(defun retrieval (obj)
 (let ((retr nil)
        (word (word-name (obj->wordinfo obj *sentence-plist*))))
  (if *retrieval-on*
    (setf retr (word-rv (obj->wordinfo obj *sentence-plist*)))
    (setf retr nil))
  (cond ((numberp retr) 
;      (setf retr (* 0.001 (* *retrieval-factor* retr)))
      (setf retr (* 0.001 retr))
      (when *VERBOSE* (format t "~%   > Retrieval (~S): ~3,3F" word retr)) retr)
    (t .001))   ;; if zero then goal modification event will finish BEFORE 
                ;; goal modification of the actual production (wrong order!)
))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun record-fixations (sentence snr trace &optional (location "output/fixations.txt"))
;  (push (list snr trace) *traces*)
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
      (dolist (fix trace)
          (if (>= (first fix) 0)
            (format outfile "~S ~D ~S ~D ~S ~D~%" *experiment* *simulation* snr (1+ (first fix)) (format nil "~a" (word-name (nth (first fix) sentence))) (second fix))  
            nil)))
)


(defun record-attachment-times (snr)
 (when *record-times*
  (with-open-file (outfile (ensure-directories-exist "output/attachments.txt") :direction :output :if-exists :append :if-does-not-exist :create)
      (dolist (word *times2*)
          (if (>= (first word) 0)
            (format outfile "~S ~D ~D ~D ~S ~D~%" *experiment* *simulation* snr (1+ (first word)) (second word) (third word))  
            nil))))
)


(defun output-enctime (obj enctime)
 (when *record-times*
  (let* ((location (obj->location obj))
        (wordnr (location->index location *sentence-plist*))
        (word (chunk-slot-value-fct obj 'value)))
        ;(wordnr (car (multiple-value-bind (item present?) (parse-integer (subseq location 15) :junk-allowed t) (list item present?)))))
  (if wordnr (setq wordnr (+ wordnr 1)))
  (unless *estimating*
    (with-open-file (outfile (ensure-directories-exist "output/enctime.txt") :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~S ~S ~S ~S ~S ~D~%" *experiment* *simulation*  *item* wordnr word (round (* enctime 1000))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Eye Tracing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-emma ()
    ;(reset-emma-module (get-module :vision))
    (setf (trace-eye-p (get-module :vision)) t)   ;; ensure EMMA records eye trace
    (set-eye-loc (get-module :vision) #(10 150))  ;; make sure that the eccentricity of first word is not too enormous
)

;;
(defun get-em-trace ()
    (let ((em-trace (reverse (eye-trace (get-module :vision)))))
        (unless (neq (aref (cdr (first em-trace)) 1) 0)
            (pop em-trace))
        em-trace))


(defun em-trace->fixations (em-trace sentence)
  (unless (or (null em-trace) (null (cdr em-trace)))
    (cons (list (location->index (coerce (cdr (first em-trace)) 'list) sentence)
                (round (* 1000 (- (first (second em-trace)) (first (first em-trace))))))
          (em-trace->fixations (cdr em-trace) sentence))))

;;;
;;; EYE-LOC ACCESS
;;;
(defun current-eye-loc ()
  (let ((eye-loc (eye-loc (get-module :vision))))
    (coerce eye-loc 'list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun output-fixation-time (loc fixtime)
;    (unless *estimating*
;      (with-open-file (outfile (ensure-directories-exist "output/raw-fixations.txt") :direction :output :if-exists :append :if-does-not-exist :create)
;        (format outfile "~S ~D ~D ~D ~D~%" *simulation* (+ *item* 1) (aref loc 0) (aref loc 1) (round (* fixtime 1000))))))


(defun report-fixation-time (newloc)
 (when *VERBOSE*
  (let* ((fixtime (- (mp-time) *fix-start-time*))
        (pos (location->index (coerce *fix-loc* 'list) *sentence-plist*))
        (word (word-name (nth pos *sentence-plist*)))
        (pos2 (location->index (coerce newloc 'list) *sentence-plist*))
        (word2 (word-name (nth pos2 *sentence-plist*))))
    (format t "  
     ==============================================
        FIXATION TIME on ~A (~D) (~S): ~6,3F
     ==============================================~%" *fix-loc* (+ 1 pos) word fixtime)
    (format t "~%******************** ~6,3F   FIXATING ~A (~D) (~S)   **********************~%" (mp-time) newloc (+ 1 pos2) word2)
;    (format t "  
;        NEW FIXATION on ~A ~A (~A)
;     ==============================================~%~%" newloc word2 (+ 1 pos2))
    
;  (output-fixation-time *fix-loc* fixtime)
  (setf *fix-start-time* (mp-time))
  (setf *fix-loc* newloc)
)))


(defun report-attention-shift (obj freq enctime eccentricity srpr)
 (when *VERBOSE*
  (let* ((location (obj->location obj))
      (word (chunk-slot-value-fct obj 'value))
      (pos (1+ (location->index location *sentence-plist*))))
    (format t "~%******************** ~6,3F   ENCODING ~D (~S) **********************" (mp-time) pos word)
;    (format t "     ------------------------------------------")
    (format t "~%   > Frequency: ~3,5F" freq)
    (format t "~%   > Eccentricity: ~3,5F" eccentricity)
    (format t "~%   > Surprisal: ~3,5F" srpr)
    (format t "~%   > Encoding Time: ~6,3F" enctime)
;    (format t "~%     ------------------------------------------") 
)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RETRIEVAL AND TIME OUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wait-retrieval (obj)
  (let* ((lf (car (no-output (sgp :lf))))
  ;(retr (retrieval obj))
  ;(integr (* *integration-factor* (retrieval obj) (surprisal-hl obj)))   ;; retrieval modulated by surprisal
        (integr (+ (* lf (retrieval obj)) (surprisal-hl obj)))   ;; retrieval modulated by surprisal
        (pos (location->index (obj->location obj) *sentence-plist*))
        (word (word-name (obj->wordinfo obj *sentence-plist*))))
    (if (mymember (+ 1 pos) *attached-items*) (setf integr 0.001))  ; check if word is already integrated
    (if (< integr 0.001) (setf integr 0.001))
    (when *VERBOSE* (format t "~%   > Integration Time (~S): ~3,3F" word integr))
    ;(mod-focus state "reading")
    ;(schedule-event-relative retr 'mod-focus :priority :max :params '(list (state "reading")) :output 'medium)
    (schedule-event-relative integr 'finish-retrieval :priority :max :params (list obj) :output 'medium :module 'parsing 
                           :details (concatenate 'string "Retrieval-finished " (write-to-string obj))
                           )))


(defun finish-retrieval (obj)
  (mod-focus retrieval "free")
;  (mod-focus state "reading")
  (set-end-time obj)
)
    
(defun set-begin-time (word)
  (setf *begin-time* (mp-time))
  (setq *word* word)
  )


(defun set-end-time (obj)
  (let ((attach-time (- (mp-time) *begin-time*))
        (word (chunk-slot-value-fct obj 'value))
        (pos (location->index (obj->location obj) *sentence-plist*)))
    (when *VERBOSE*
      (format t "
     =============================================
        INTEGRATION TIME on ~D (~S): ~6,3F
     =============================================~%~%" 
     (+ 1 pos) word attach-time))
    (push-last attach-time *times*)
    (push-last (list pos word attach-time)  *times2*)
    (push-last (+ 1 pos) *attached-items*)
    ))


(defun start-time-out (loc-obj)
  (let* ((eye-loc (eye-loc (get-module :vision)))
        (pos (location->index (coerce eye-loc 'list) *sentence-plist*))
        (word (word-name (nth pos *sentence-plist*)))
        (targetloc (list (chunk-slot-value-fct loc-obj 'screen-x) (chunk-slot-value-fct loc-obj 'screen-y)))
        (tpos (location->index targetloc *sentence-plist*))
        (tword (word-name (nth tpos *sentence-plist*))))
    (when *VERBOSE*
      (format t "
     =============================================
        TIME OUT: ~D (~S) <== ~D (~S)
     =============================================~%" 
     (+ 1 tpos) tword (+ 1 pos) word))
;        TIME OUT from: ~A ~A (~A) to: #~A ~A (~A)
;     eye-loc word (+ 1 pos) targetloc tword (+ 1 tpos)))
    (when *record-times*
     (with-open-file (outfile (ensure-directories-exist "output/timeout.txt") :direction :output :if-exists :append :if-does-not-exist :create)
            (format outfile "~D ~D ~D ~D ~S ~D ~S~%" *experiment* *simulation* *item* (+ 1 tpos) tword (+ 1 pos) word ))
    )))


(defun exit-time-out ()
    (when *VERBOSE*
      (format t "
     =============================================
        EXIT TIME OUT at ~6,3F
     =============================================~%" 
     (mp-time))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXATION MEASURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gaze (fixations)
  (let ((gazedurs (make-array (1+ (n-words fixations))))
        (collapsed (collapse-fixations fixations)))
    (dolist (f collapsed)
        (cond
            ((eq (aref gazedurs (first f)) 0) 
                (setf (aref gazedurs (first f)) (second f)))))
  (array-to-index-list gazedurs)    
))


;; first fixation iff progressive
(defun ffd (fixations)
  (let ((ffds (make-array (1+ (n-words fixations))))
        (firstfix (firstfixations fixations))
        (lastf -1))
    (dolist (f firstfix)
        (when (and (eq (aref ffds (first f)) 0) (< lastf (first f)))
                (setf (aref ffds (first f)) (second f)))
        (setf lastf (first f)))
  (array-to-index-list ffds)    
))


(defun tft (fixations)
  (let ((tfts (make-array (1+ (n-words fixations)))))
    (dolist (f fixations)
        (setf (aref tfts (first f)) (+ (aref tfts (first f)) (second f))))
  (array-to-index-list tfts)
))

(defun rrt (fixations)
  (let ((rrts nil)
        (tfts (tft fixations))
        (fprts (gaze fixations)))
    (loop for i from 0 to (- (length tfts) 1) do
        (push-last (list i (- (second (nth i tfts)) (second (nth i fprts)))) rrts))
    rrts))

(defun reread (fixations)
  (let ((rereads nil)
        (rrts (rrt fixations)))
    (loop for i from 0 to (- (length rrts) 1) do
        (if (> (second (nth i rrts)) 0)
        (push-last (list i 1) rereads)
        (push-last (list i 0) rereads)))
    rereads))

(defun refix (fixations)
  (let ((refixs nil)
        (ffds (ffd fixations))
        (gazes (gaze fixations)))
    (loop for i from 0 to (- (length ffds) 1) do
        (if (> (second (nth i gazes)) (second (nth i ffds)))
        (push-last (list i 1) refixs)
        (push-last (list i 0) refixs)))
    refixs))


(defun fpreg (fixations)
  (let ((fpregs (make-array (1+ (n-words fixations)) :initial-element nil))
        (collapsed (collapse-fixations fixations)))
    (loop for i from 0 to (- (length collapsed) 1) do
        (let ((w0 (first (nth i collapsed)))
              (w1 (first (nth (1+ i) collapsed))))
            (when (null (aref fpregs w0))
                (if (and w1 (< w1 w0)) 
                    (setf (aref fpregs w0) 1)
                    (setf (aref fpregs w0) 0)))
        )
    )
  (array-to-index-list fpregs)
))


(defun firstfixations (fixations)
      (let ((last nil)
            (ffds nil))
        (do ((fixs fixations (rest fixs)))
            ((null fixs))
          (let* ((fixation (first fixs))
                 (index (first fixation)))
            (unless (eq index last)
              (push-last fixation ffds))
            (setf last index)))
        ffds
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun datetimestamp ()
  (let ((day-names
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday")))
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
;       (format nil "It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
       (format nil "~d~2,'0d~d-~2,'0d~2,'0d"
          year
          month
          date
	      hour
	      minute)
    ;(concatenate 'string year month)	      
)))

(defun collapse-fixations (fixations)
  (cond ((< (length fixations) 2) fixations)
        ((equalp (first (first fixations)) (first (second fixations)))
         (collapse-fixations
          (cons (list (first (first fixations))
                      (+ (second (first fixations)) (second (second fixations))))
                (rest (rest fixations)))))
        (t (cons (first fixations) (collapse-fixations (rest fixations))))))

(defun n-words (trace)
  (let ((n 0))
    (dolist (w trace)
        (when (> (first w) n) (setf n (first w))))
    n))
    
(defun array-to-index-list (ar)
  (let ((ilist nil)
        (i 0))
    (loop for v across ar do
        (push-last (list i v) ilist)
        (incf i))
    ilist))


;;;
;;;   Word object helpers
;;;
(defun obj->wordinfo (obj sentence)
  (let* ((location (obj->location obj))
      (index (location->index location sentence)))
    (nth index sentence)))

(defun obj->location (obj)
  (let* ((screen-pos (chunk-slot-value-fct obj 'SCREEN-POS))
      (x (chunk-slot-value-fct screen-pos 'screen-x))
      (y (chunk-slot-value-fct screen-pos 'screen-y)))
    (list x y)))

;;
(defun location->index (location sentence)
  (let ((lx (first location)))
    (do ((i 0 (1+ i))
         (x *start-x* x))
        ((or (< lx x) (>= i (length sentence)))
         (max 0 (- i 1)))
      (incf x (+ (* (length (format nil "~a" (word-name (nth i sentence)))) *char-width*)
                 *char-width*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun delete-output ()
  (if (probe-file "output/raw-fixations.txt") (delete-file "output/raw-fixations.txt"))
  (if (probe-file "output/fixations.txt") (delete-file "output/fixations.txt"))
  (if (probe-file "output/enctime.txt") (delete-file "output/enctime.txt"))
  (if (probe-file "output/regions.txt") (delete-file "output/regions.txt"))
  (if (probe-file "output/attachments.txt") (delete-file "output/attachments.txt"))
  (if (probe-file "output/timeout.txt") (delete-file "output/timeout.txt")))

