;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "FIT: An iterative parameter-estimation function in LISP"
;;;
;;; Authors: F, MICHAEL RABINOWITZ, MALCOLM J. GRANT, and H. LOUIS DINGLEY
;;;          (Memorial University, St. John's, Newfoundland, Canada)
;;;
;;; Published: Behavior Research Methods, Instruments, & Computers 1984, 16 (3), 307-314
;;;
;;;
;;; [copied and adjusted to current Lisp requirements in Sept. 2011 by FE]
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXAMPLE 1:
;;; 	(startfit '(p1 p2 p3) 'float '(1 1 1) '(5 5 5) '(.01 .01 .01) 'demofit 'max)
;;;
;;; EXAMPLE 2:
;;;		(startfit '(p1 p2 p3) 'integer '(1 1 1) '(5 5 5) '(1 1 1) 'demofit 'max)
;;;
;;; EXAMPLE 3:
;;;		(startfit '(angle-in-radians) 'float '(0) '(6.28) '(.01) 'mysin 'max)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nameset)
(defvar integer-float)
(defvar cutoff)
(defvar fname)
(defvar minmax)
(defvar maxval 0)

(defun startfit (nameset integer-float lower upper cutoff fname minmax)
	(setq
		nameset nameset
		integer-float integer-float
		cutoff cutoff
		fname fname
		minmax minmax)
	(setq middlevalue (mapcar (lambda (x5 y5) (/ (+ x5 y5) (mode 2))) lower upper))

	(setq halfrange (mapcar (lambda (x5 y5) (/ (- y5 x5) (mode 2))) lower upper))

	(setq ll (fit1 middlevalue halfrange)) ;; first call of fit1
	(prog () 
		loop		
		(cond ((equal (length ll) 1) (return (cons maxval ll))) ;; iteration finished
			(t (setq r1 '() tri (fit2 ll) maxval (car tri) val (cdr tri)) 
				;; fit2 returns maximum score and parameter set
				;; rl hold estimates of fit and associated parameter sets
				(setq ll (fit1 val range1)) ;; fit1 returns a list of all possible parameter sets for the next iteration
				(print paramlist)
				(go loop)))))

(defun mode (i-f)
	(cond ((equal integer-float 'float) (float i-f))
	(t i-f)))

(defun fit1 (l1 l2) ;; middle values and half ranges respectively
	(setq val (reverse l1) range '()) ;; val is reverse so that last parameter is dealt with first
	
	(prog ()
		(setq range1 (mapcar (lambda (x y) 
			(cond ((< x y) (setq x 0)) (t (/ x (mode 2)))))
		 	l2 cutoff)) ;; new halfrange
		
		(setq range (reverse range1)) ;; reverse so that last parameter occurs first
		(cond ((zerop (car range)) (setq ll (list (list (car val))))) ;; if the halfrange "hr" is zero 
			;; then just list the parameter "p" 
			(t (setq ll (list (list (- (car val) (car range)))
						(list (car val))
						(list (+ (car val) (car range))))))) ;; if halfrange not zero then list p-hr p and p+hr
		(setq val (cdr val) range (cdr range)) ;; shorten the lists by one 
		(setq l3 (reverse ll) l4 '()) ;; l3 is reverse working set of ll
									  ;; l4 is temporary storage of ll
		
		loop1 ;; to consider second and remaining parameters
		(cond ((null val) (return ll)) ;; parameters exhausted return list of all possible combinations
			((null l3) (go loop3)) ;; l3 exhausted 
			(t (cond ((zerop (car range))
				(setq workset (list (car val)))); if halfrange zero list parameter
				(t (setq workset (list (+ (car val) (car range)) (car val) ;; list p-hr p p+hr
					(- (car val) (car range))))))))

		loop2
		(cond ((null workset) (setq l3 (cdr l3)) (go loop1)) ;; if workset empty shorten l3
			(t (setq l4 (cons (cons (car workset) (car l3)) l4))
					;; every value in the workset is paired with first value in l3 - 
					;; l4 has parameters stored in the original order
				(setq workset (cdr workset)) (go loop2))) ;; shorten workset
		
		loop3 ;; reset l3 and ll - shorten val and range
		(setq l3 (reverse l4) ll l4 val (cdr val) range (cdr range) l4 '()) 
		(go loop1)))

(defun fit2 (ll)
	(cond ((null ll) ;; ll exhausted
			(setq m (apply minmax (mapcar (lambda (x) (car x)) r1))) ;; find best fit
			(setq val (car (mapcan (lambda (x) (cond ((equal (car x) m) (list x)) (t nil))) r1))))
			;; select best fit and associated parameter set and store in val 
		(t (setq namelist nameset paramlist (car ll)) ;; set parameter names and values
			(mapcar 'set namelist paramlist) ;; set names to values
			(setq r1 (cons (cons (funcall fname) (car ll)) r1)) ;; store fit and associated parameters in r1 
			(fit2 (cdr ll))))) ;; repeat with next parameter set


;;
;; AUXILIARY FUNCTIONS
;;
(defun demofit () ;; adds elements of a parameter list together 
	(apply '+ paramlist))

(defun linefit () ;; sum of squared deviations about a line
	(setq yprime (mapcar (lambda (x1) (+ (* slope x1) intercept)) x)) 
	(apply '+ (mapcar
		(lambda (y2 y3) (* (- y2 y3) (- y2 y3))) y yprime)))

(setq x ;; data used by linefit
	'(118 99 118 121 123 98 131 121 108 111 118 112 113 111 106 102 113 101))
(setq y ;; data used by linefit
	'(66 50 73 69 72 54 74 70 65 62 65 63 67 59 60 59 70 57))

(defun parabel () 
	(+ (* 1 (sqr (first paramlist))) (* 1 (first paramlist)) -2)) 

(defun f (x) 
	(+ (* 1 (sqr x)) (* 1 x) -6)) 

(defun fake ()
	(random 500))

(defun difference (x y)
	(sqrt (sqr (- x y))))
	
(defun mysin ()
	(apply 'sin paramlist))

(defun sqr (x)
	(* x x))