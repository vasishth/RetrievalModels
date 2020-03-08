;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Felix Engelmann
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : simple-goal-style.lisp
;;; Version     : 1.0
;;; 
;;; Description : Parsing module 
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2010.10.22 Felix
;;;             : * started file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This file defines a simple goal-style module called new-goal
;;; which has one buffer also called new-goal and which works
;;; exactly like the default ACT-R goal module and buffer except
;;; that it does not provide commands equivalent to goal-focus
;;; and mod-focus.
;;;
;;; To use this module either place this file into the modules
;;; directory before loading the system, or just load it once
;;; before loading the corresponding model file which uses it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These tests should appear at the top of all automatically loaded
;;; ACT-R files to ensure that things go into a specific package when
;;; requested.

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; This line should be included in any file which uses the 
;;; parsing-query or parsing-request functions to ensure
;;; that they are available.

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

(defstruct parsing-module srpr)

(defun create-parsing-module (model-name)
  (declare (ignore model-name))
  (make-parsing-module))

;(defun reset-parsing-module (demo)
;  (declare (ignore demo))
;  (chunk-type demo-output value))

;(defun delete-demo-module (demo)
;  (declare (ignore demo)))


(defun parsing-module-params (parsing param)
  (if (consp param)
    (case (car param)
      (:SURPRISAL-FACTOR
       (setf (parsing-module-srpr parsing) (cdr param)))  
		)
    (case param
      (:SURPRISAL-FACTOR
       (parsing-module-srpr parsing)))))


(define-module-fct 'parsing  ;; the module is named parsing
  '(IPb DPb NPb CPb VPb PPb VP2b AdjPb AdvPb lex)             ;; buffers
  
  (list (define-parameter :SURPRISAL-FACTOR					;; parameters
   					  :owner T
                      :valid-test #'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.001
                      :documentation "Scaling constant for surprisal"))

  
  :version "0.1"
  :documentation "A parsing module"
  
  ;; Just specify the goal-style functions for the module's interface
  
  :creation 'create-parsing-module
  :query 'goal-style-query
  :request 'goal-style-request
  :buffer-mod 'goal-style-mod-request
  :params 'parsing-module-params
)


