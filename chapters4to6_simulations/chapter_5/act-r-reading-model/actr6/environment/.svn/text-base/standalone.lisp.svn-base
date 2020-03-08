;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : standalone.lisp
;;; Version     : 3.0
;;; 
;;; Description : Contains the code for connecting an application
;;;             : version of the environment to the Tcl side.
;;; Bugs        : 
;;; 
;;; Todo        : [ ] Get rid of those global pathname variables.
;;; 
;;; ----- History -----
;;;
;;; 10/01/2002  Dan
;;;             : File creation
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; 2005.05.12 Dan
;;;             : * Added the safe-load and smart-load functions so that
;;;             :   the standalone can have the open dialog.
;;; 2008.08.21 Dan
;;;             : * Added code for the ACL version of the standalone to 
;;;             :   automatically exit the error which can get generated
;;;             :   during a define-model call.
;;; 2010.06.07 Dan
;;;             : * Changed run-standalone to print out the version info.
;;; 2010.06.07 Dan
;;;             : * Not useful since that wouldn't show up in the Listener
;;;             :   window of the environment anyway...
;;; 2010.08.11 Dan
;;;             : * Changed start-listener-outputer to print the framework version
;;;             :   info at the top of the listener.  Something other than an
;;;             :   empty string needed to be sent to avoid an error being
;;;             :   logged anyway now that the error logging works better.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-compiled "UNI-FILES" "ACT-R6:support;uni-files")

(defvar *global-output-stream*  (make-string-output-stream))
(defvar *stop-environment* nil)

(defun run-standalone ()
  #+(and :allegro :actr-env-alone) (setf *debugger-hook* 'standalone-debug-exit)
  (connect-to-environment))


#+(and :allegro :actr-env-alone)
(defun standalone-debug-exit (a b) 
  (declare (ignore a b))
  (model-warning "Error has been cleared automatically") 
  (invoke-restart-interactively (second (compute-restarts))))


(defun eval-command (cmd)
  (let ((*standard-output* *global-output-stream*)
        (*error-output* *global-output-stream*))
    (format t "~%> ~A~%" cmd)
    (multiple-value-bind (result err)
        (ignore-errors (read-from-string cmd))
      (if (and (subtypep (type-of err) 'condition)
             (not (equal (type-of err) 'unbound-variable)))
        (progn
          (format t "~S~%" (type-of err))
          (uni-report-error err (format nil "Error in command: ~s:~%" cmd)))
        (multiple-value-bind (res err)
            (ignore-errors (eval result))
          (if (and (subtypep (type-of err) 'condition)
                   (not (equal (type-of err) 'unbound-variable)))
              (progn
                (format t "~S~%" (type-of err))
                (uni-report-error err (format nil "Error executing command: ~s:~%" cmd)))
            (format t "~%~S~%" res)))))))

(defun start-listener-outputer (handler)
   (uni-run-process "Listener-outputer" #'(lambda () (run-listener-outputer handler)))
   (format nil ";;; ACT-R Standalone Environment version ~a" (meta-p-version (gethash 'default (mps-table *meta-processes*)))))

(defun run-listener-outputer (handler)
  (loop
    (uni-wait-for #'(lambda ()
                      (let ((text (get-output-stream-string *global-output-stream*)))
                        (if (> (length text) 0)
                            (progn
                              (setf (update-value handler) text)
                              t)
                          nil))))
    (send-update handler)))


#+:ACTR-ENV-ALONE
(defun smart-loader (file)
  (declare (ignore file))
  (list 0 "You must disable the 'Compile definitions when model opened or reloaded' option to be able to open a model"))

#-:ccl-5.0 

(defun create-valid-pathname (path) path)


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
