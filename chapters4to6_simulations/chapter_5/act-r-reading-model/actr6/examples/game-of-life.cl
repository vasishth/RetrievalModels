;;; To run this call run-game and provide
;;; a width and height of the board to use
;;; in cells:
;;;
;;; (run-game 10 10)
;;;
;;; I suggest keeping it fairly small because
;;; it will run pretty slowly once the number of 
;;; models/cells gets "large" (large of course is 
;;; relative to the machine/Lisp running it).
;;; If using the ACT-R Environment to display the
;;; GUI window for the game I suggest keeping it
;;; to 100 cells or fewer or slowing down the
;;; display rate.
;;; 
;;; That will run the game for 10 seconds with
;;; one cycle per second displayed.  To continue
;;; running it you can just call run again and
;;; should include the real-time flag:
;;;
;;; (run 10 :real-time t)
;;;
;;; It's also possible to specify a longer or
;;; shorter initial run time with the cycles 
;;; keyword parameter:
;;;
;;; (run-game 10 10 :cycles 15)
;;;
;;; Would run the game for 15 cycles before stopping.
;;;
;;; Similarly, the display rate can be changed.
;;; The default is one second per cycle drawn,
;;; but specifying the speed keyword parameter
;;; allows you to change that faster or slower.
;;; Speed must be a positive number and specifies
;;; how many cycles to draw per second.
;;;
;;; (run-game 10 10 :speed 2)
;;;
;;; Would run the game for 10 cycles at 2 cycles
;;; drawn per second.  Note however that depending
;;; on the size of the board and the speed of the
;;; machine the game may not display as fast as
;;; requested.
;;; 
;;; In the graphic representation (the default)
;;; An "on" cell will show an @ symbol and an "off" 
;;; cell will be blank.  If one doesn't want to
;;; (or can't) use the grapic representation then
;;; an text only output can be drawn as well if
;;; the :text parameter is given as t:
;;;
;;; (run-game 10 10 :text t)
;;; 
;;; That will result in every cycle being drawn
;;; as two text arrays.  The first array will be
;;; on and off cells (* and . respectively) and
;;; the other will be the count of on neighbors
;;; for each cell.  Here's a representation of
;;; a cycle from a 10x10 game in text output:
#|
........**
..........
**......**
**.....*..
..*..**.**
**.***.***
*****.**..
..*..**...
.....*.*..
..........

0000000111
2210000244
3320001221
3431123354
4533434553
3565556553
3555565442
2434445420
0111224110
0000112110
|#

;;; The default game starts with a random distribution
;;; of on cells where the probability of a cell being on
;;; can be specified by the :p parameter to run-game.  The
;;; default p is .25.
;;; 
;;; (run-game 10 10 :p .5)
;;;
;;; Finally, it is possible to manually configure the starting
;;; board when using the graphic representation by specifying 
;;; the :setup parameter as t:
;;;
;;; (run-game 10 10 :setup t)
;;;
;;; When that happens a random board will be generated, but
;;; the user can click on the cells to toggle them.  When 
;;; you are ready to run the model with the cell configuration
;;; created press the done button on the display.
;;; If one wants to setup initial states it can be useful to
;;; also specify :p as 0 or 1 to start with a board that is
;;; either all off or all on respectively.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code

;;; Some global variables to hold the game state used by
;;; all of the models.

(defvar *board*)   ; the array of cells
(defvar *x-size*)  ; board width
(defvar *y-size*)  ; board height
(defvar *win* nil) ; the window displaying the cells
(defvar *moves*)   ; list of changes to make per update
(defvar *done*)    ; indicates when the user is finished setting a custom state

;;; A structure which is being used as a device for the model
;;; which will just store the x,y coordinates of the model's 
;;; cell in the board.
;;; Alternatively, the game state could have been created as
;;; a device and shared by the models, but that would then
;;; require some other way to map a model to its cell so this
;;; simplifies the device code.

(defstruct pos x y)

;;; Define the motor methods for the device for completeness, 
;;; but don't do anything with them -- the model makes no
;;; motor actions.

(defmethod device-move-cursor-to ((device pos) loc))
(defmethod device-handle-click ((device pos)))
(defmethod device-handle-keypress ((device pos) key))
(defmethod cursor-to-vis-loc ((device pos)))
(defmethod get-mouse-coordinates ((device pos)) (vector 0 0))

;;; The only visual-location the model sees is one
;;; which reports the model's cell's current state
;;; and number of neighbors.  That chunk itself is
;;; defined in the model definition.

(defmethod build-vis-locs-for ((p pos) vis-mod)
  (declare (ignore vis-mod))
  (mod-chunk-fct 'vis-loc `(value ,(aref *board* (pos-x p) (pos-y p)) 
                            size ,(count-locs p))))

;;; The model won't be attending to the location chunks so
;;; just return nil for the device's visual object encoding.

(defmethod vis-loc-to-obj ((device pos) vis-loc))

;;; Save the model's vocal response as an action to take this
;;; step.  If the model says "on" then the cell will be set on
;;; and if the model says anything else it will be turned off.

(defmethod device-speak-string ((p pos) string)
  (push (list (pos-x p) (pos-y p) (if (string-equal string "on") t nil)) *moves*))


;;; The main function to run the game 

(defun run-game (w h &key (cycles 10) (p .25) (setup nil) (text nil) (speed 1.0))
  
  ;; setup the global game info
  
  (setf *board* (make-array (list w h)))
  (setf *moves* nil)
  (setf *x-size* w)
  (setf *y-size* h)
  (setf *done* nil)
  
  ;; remove any other models and meta-processes
  
  (clear-all)
  
  ;; create a starting model to use for generating
  ;; random numbers because I prefer to use act-r-random 
  
  (define-model base-model (sgp :v nil))
  
  ;; create the initial board and all the models
  
  (dotimes (x w)
    (dotimes (y h)
      
      ;; set the cell on or off based on probability p
      
      (if (< (with-model base-model (act-r-random 1.0)) p)
          (setf (aref *board* x y) t)
        (setf (aref *board* x y) nil))
      
      ;; create the models for each cell
      ;; Name the models based on thier cell coordinates.
      
      (let ((m (intern (format nil "M-~D-~D" x y))))
        
        ;; All models use the same code defined below.
        
        ;; Because the environment connection isn't built
        ;; for use with multiple models it will throw a
        ;; warning, but we'll just suppress that here and
        ;; ignore it since it will work for the GUI display
        ;; eventhough the model controls won't work.
        
        (with-model base-model
          (suppress-warnings (define-model-fct m *model-code*)))
        
        ;; create a new pos structure for the model's device
        
        (with-model-eval m
          (install-device (make-pos :x x :y y))))))
  
  ;; Get rid of the starting model because it's not needed.
  
  (delete-model base-model)
  
  ;; use the model for cell 0,0 to create the update event which
  ;; will redraw things every simulated second.
  
  (with-model m-0-0
    ;; every second starting at 1s run the update-cycle command passing it
    ;; a parameter indicating whether or not it's the text only version.
    
    (schedule-periodic-event 1 'update-cycle :params (list text) :maintenance t :initial-delay 1))
  
  ;; If there is an existing GUI window for it close that one
    
  (when *win*
    (close-exp-window))
  
  ;; If it's not text only create a new window for the game display
  
  (unless text
    (setf *win* (open-exp-window "Life" :width (+ 16 (* w 20)) :height (+ 25 32 (* h 20)))))
  
  ;; draw the initial game board
  
  (draw-board text)
  
  ;; let the user create a custom starting state for the gui when requeted
  
  (when (and setup (not text))
    
    ;; indicate a person is in control of the buttons to allow the setup
    
    (setf *actr-enabled-p* nil)
    
    (while (not *done*)
      (allow-event-manager *win*)))
  
  ;; Make sure the model is back in control of the GUI items
  
  (setf *actr-enabled-p* t)
  
  ;; Put the initial proc-display call for each model on the queue
  
  (dolist (m (mp-models))
    (with-model-eval m 
      (schedule-event-relative 0 'proc-display :maintenance t)))
  
  ;; If a speed other than 1.0 is given then adjust the
  ;; real-time configuration to scale the number of 
  ;; "ticks" per second while still using the standard
  ;; real-time clock.  The model will still run at one
  ;; cycle per simulated second but those simulated seconds
  ;; will be displayed at a different rate.
  
  (unless (= speed 1.0)
    (when (and (numberp speed) (plusp speed))
      (mp-real-time-management :units-per-second (/ internal-time-units-per-second speed))))
  
  ;; Run the game for the requested number of cycles (simulated seconds)
  
  (run cycles :real-time t))


;;; function to draw the board using either the GUI or just as text

(defun draw-board (text-only)
  
  (if text-only
      (terpri)
    (clear-exp-window))
        
  (dotimes (y *y-size*)
    (dotimes (x *x-size*)
      (if text-only
          (format t "~c" (if (aref *board* x y) #\* #\.))
        
        (let ((nx x)  ; need these to capture the x and y values
              (ny y)) ; for the lambda used in the button's action

          (add-button-to-exp-window :x (* x 20) :y (* y 20) :width 20 :height 20 :text (if (aref *board* x y) "@" " ")
                                    :action (lambda (but) (setf (aref *board* nx ny) (not (aref *board* nx ny))) (draw-board nil))))))
    (when text-only
      (terpri)))
    
  (if text-only
      (progn
        (terpri)
        (dotimes (y *y-size*)
          (dotimes (x *x-size*)
            (format t "~d" (count-locs (make-pos :x x :y y))))
          (terpri)))
    (progn
      (add-button-to-exp-window :x 0 :y (+ 5 (* *y-size* 20)) :height 20 :width 40 :text "done" :action (lambda (but) (setf *done* t)))
      (allow-event-manager *win*))))


;;; function to count the number of neighbors of a cell
;;; using a simple check for each adjacent space which exists

(defun count-locs (p)
  (let ((x (pos-x p))
        (y (pos-y p))
        (mx (1- *x-size*))
        (my (1- *y-size*))
        (count 0))
    (when (and (> x 0) (> y 0) (aref *board* (1- x) (1- y)))
      (incf count))
    (when (and (> y 0) (aref *board* x (1- y)))
      (incf count))
    (when (and (< x mx) (> y 0) (aref *board* (1+ x) (1- y)))
      (incf count))
    (when (and (> x 0) (aref *board* (1- x) y))
      (incf count))
    (when (and (< x mx) (aref *board* (1+ x) y))
      (incf count))
    (when (and (> x 0) (< y my) (aref *board* (1- x) (1+ y)))
      (incf count))
    (when (and  (< y my) (aref *board* x (1+ y)))
      (incf count))
    (when (and (< x mx) (< y my) (aref *board* (1+ x) (1+ y)))
      (incf count))
    count))

;;; The periodic event scheduled to drive the game.
;;; It updates the board with all of the models' moves
;;; then redraws it and tells each model to reprocess 
;;; their visual display.

(defun update-cycle (draw)
  
  ;; apply all the model changes
  
  (dolist (m *moves*)
    (setf (aref *board* (first m) (second m)) (third m)))
  
  (setf *moves* nil)
  
  (draw-board draw)
  
  ;; for each model schedule a proc-display
  (dolist (m (mp-models))
    (with-model-eval m 
      (schedule-event-relative 0 'proc-display :maintenance t))))


;;; This list holds the model definition code used for
;;; the model of each cell.

(defparameter *model-code* 
  '((sgp :v nil :needs-mouse nil)
    
    ;; make sure the location gets stuffed whether since the default would
    ;; stop once it's no longer "new"
    (set-visloc-default isa visual-location)
    
    ;; create the chunk for the visual percept
    
    (define-chunks (vis-loc isa visual-location))
    
    ;; productions to encode the rules of the game
    
    (p lonely
       "On with less than 2 neighbors turns off"
       =visual-location>
         isa visual-location
         value t
       < size 2
       ?vocal> 
         state 
         free
     ==>
       +vocal> 
         isa speak 
         string "off")
    
    (p overcrowded
       "On with more than 3 neighbors turns off"
       =visual-location>
         isa visual-location
         value t
       > size 3
       ?vocal> 
         state free
     ==>
       +vocal> 
         isa speak 
         string "off")
    
    (p born
       "Off with 3 neighbors turns on"
       =visual-location>
         isa visual-location
         value nil
         size 3
       ?vocal> 
         state free
     ==>
       +vocal> 
         isa speak 
         string "on")
    
    ;; These two productions detect the "do nothing" states
    ;; and need to be here to make sure the visual-location
    ;; buffer clears so the new state will restuff it with
    ;; the updated chunk.
    
    (p good
       "On with 2 or 3 neighbors stays on (does nothing)"
       =visual-location>
         isa visual-location
         value t
       >= size 2
       <= size 3
       ?vocal> 
         state free
     ==>
       )

    (p remain-dorment
       "Off without 3 neighbors stays off"
       =visual-location>
         isa visual-location
         value nil
       - size 3
       ?vocal> 
         state free
     ==>
       )))
