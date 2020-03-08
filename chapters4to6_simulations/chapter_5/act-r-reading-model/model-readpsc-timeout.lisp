;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CORPUS READING MODEL WITH TIME OUT
;;;
;;; Written in 2012/2013 by Felix Engelmann.
;;; Some parts of the code are reused from the ACT-R Sentence Parsing Model
;;; by Shravan Vasishth, Rick Lewis (2006)
;;;
;;; Uses an adapted version of the EMMA (Salvucci, 2001) module and a simple 
;;; parsing module.
;;;
;;; For description and simulations see TopiCS article
;;; Felix Engelmann, Shravan Vasishth, Ralf Engbert, & Reinhold Kliegl (2013).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(setf *default-pathname-defaults* #P"/Path/to/model/") ;;; REPLACE by model directory

;;; EMMA PRESETS ;;;
(defparameter *sacc-suppr*  T)
(defparameter *time-out*    T)

(clear-all)
(load "interface.lisp")



;; Set parameters as preset in interface
(setf global-params `(:VISUAL-ENCODING-FACTOR    ,*visual-encoding-factor*
                      :VISUAL-ENCODING-EXPONENT  ,*visual-encoding-exponent*
                      :SACCADE-PREPARATION-TIME  ,*prep-time*
                      :FIXED-PREP-TIME           T
                      :EYE-SACCADE-RATE          0.002
                      :SACCADE-BASE-TIME         0.020
                      :vis-obj-freq              0.01
                      :lf 						           ,*retrieval-latency*
                      :SURPRISAL-FACTOR			     ,*surprisal-factor*
                      :SURPRISAL-HL-FACTOR       ,*surprisal-hl-factor*
                      :v                         ,*VERBOSE*
                      :dcnn                      ,*dcnn* ;; dynamic chunk name normalizing during run time.
                                                         ;; Switch off for better performance.
))



(define-model readingmodel-timeout
;(sgp :v t :act nil :esc t :egs 3 :er t :lf 1.0 :mas nil :ga 1.0 :imaginal-activation 1.0 :show-focus t :randomize-time nil) 

(sgp 
     :vwt t          ;; default: NIL  : Virtual Window trace controls the << ... >> outputs from virtual windows
     :act nil
     :trace-detail high        ; high | medium | low          

     :dat 0.05      ; default action time (time in s for a production to fire) (default: 0.05; Salvucci2001: 0.01)
     :esc t         ; enable subsymbolic computation (utilities and activations)
;     :egs 0         ; utility noise (default: 0)
     :er nil        ; enable randomness (default: nil)
     :randomize-time t  ; used mainly by the perceptual and motor modules to add noise to the action times
     :ncnar nil         ; normalize chunk names. Switch off for better performance.
     :short-copy-names t
     
;;; Activation noise settings
   ;; [epsilon] (0 and 0.15 for for LV2005, VL2006) (0.15, 0.30, 0.45 for VBRD2008)
;     :ans 0.15
     :ans nil

;; Latency Factor F (0.14 for for LV2005, VL2006) (0.46 for VBRD2008) (0.26 NPI ACTR6 replication)
     :lf 0.14       ; latency factor (retrieval latency) (default: 1.0; Salvucci2001: 5.0)

;     :mas nil 
;     :ga 1.0 
;     :imaginal-activation 1.0 

;; VISUAL SETTINGS
    :show-focus t                   ; show attention focus
;    :visual-attention-latency .085  ; time of visual attention shift in seconds
;    :viewing-distance 100

;    :visual-attention-latency 0
;    :visual-finst-span 3.0          ; how long a finst marker will remain on a location (in seconds) (default: 3)
;    :visual-num-finsts 3            ; how many finsts are available to the vision module (default: 4)
;    :visual-onset-span .02           ; specifies how long an item recently added to the visicon will 
                                     ;   be marked as new and how long a scene change notice will be 
                                     ;   available (in seconds) (default: 0.5)
     ) 

(sgp-fct global-params)


(chunk-type meaning word)
(chunk-type read-sentence state retrieval last-loc last-retr time-out)

(add-dm
    (goal ISA read-sentence state "looking" retrieval "free" last-loc nil last-retr nil time-out nil)
    (STOP ISA meaning word "_"))




(P find-next-word
   =goal>
      ISA         read-sentence
      state       "reading"
      time-out    nil
   =visual-location>
       ISA         visual-location
   ?visual>
       processor       free
   ==>
   +visual-location>
      ISA         visual-location
;      :attended   nil
    > screen-x    current
      screen-x    lowest
   =goal>
      state       "looking"
      ;last-loc    =visual-location

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
       ;last-loc       =visual-location
   =visual-location>
   
;   !eval! (update-attend-loc =visual-location)
)
(spp attend-word :at 0)


(p retrieve-word
    =goal>
       ISA         read-sentence
       state	   "attending"
       retrieval   "free"
       time-out	   nil
    =visual-location>
       ISA         visual-location
    =visual>
       ISA         text
       value       =word
     - value       "_"
    ?retrieval>
       state      free
==>
    =goal>
       state       "reading"
       retrieval   "busy"
       last-retr    =visual-location
    =visual>
    =visual-location>

    !eval! (set-begin-time =word)
    !eval! (wait-retrieval =visual)
)
;(spp retrieve-word :at 0)


(p time-out-regression
    =goal>
       ISA         read-sentence
       state	     "attending"
       retrieval   "busy"
       time-out    nil
       last-retr   =last-retr-loc     ; use this for direct regression to problematic word
;       last-loc    =loc     ; use this for regression to previous attention location
   ?visual>
       processor         free    ; no current encoding
       execution         free    ; no current saccade execution
       ;preparation       free    ; no current saccade preparation
    =visual-location>
       ISA         visual-location
 ==>
   =goal>
       state       "looking"
       last-loc    =visual-location
       time-out    T

 	!bind! =eye-loc (first (current-eye-loc))
    +visual-location>
       ISA         visual-location
       < screen-x    =eye-loc       ;; target before current fixation
       screen-x    highest
   !eval! (start-time-out =last-retr-loc)
)
(spp time-out-regression :at 0)


(P exit-time-out
   =goal>
      ISA         read-sentence
;      state       "time-out"
      time-out    T
      retrieval   "free"
      last-loc    =loc
 ==>
   =goal>
      time-out    nil
      state       "reading"
;   =visual-location> =loc           ;; back to last attended word (n+1)
;   +visual-location>                ;; back to normal reading
;      ISA               visual-location
;    > screen-x          current
;      screen-x          lowest

   !eval! (exit-time-out)
)
;(spp exit-time-out :at 0)


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
(goal-focus goal)
)

(reset)





(setf *pspace-all* '((:VISUAL-ENCODING-FACTOR .002 .005 .001) 
				(:SACCADE-PREPARATION-TIME .100 0.125 .005)
				(:SURPRISAL-FACTOR 0.002 0.005 0.001)
				(:lf .1 .3 .1)
))
; 4*6*4*3 = 288
; (search-param-space "psc-s+r" 20 *pspace-all*)
			

(setf *pspace-r-s-sh* '((:VISUAL-ENCODING-FACTOR .002 .003 .001) 
        (:SACCADE-PREPARATION-TIME .105 0.125 .005)
        (:lf .0 .2 .1)
        (:SURPRISAL-HL-FACTOR 0.00 0.02 0.01)
        (:SURPRISAL-FACTOR 0.0000 0.0010 0.0005)
        ))
; 2*5*3*3*3 = 270

