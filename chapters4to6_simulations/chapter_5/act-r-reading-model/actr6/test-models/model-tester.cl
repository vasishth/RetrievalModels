;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2007 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : model-tester.cl
;;; Version     : 1.0
;;; 
;;; Description : Runs test models and compares the output to reference runs.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 
;;; 07.10.11 Dan
;;;             : * Finally decided to start automating the version testing...
;;; 07.10.12 Dan
;;;             : * Added tests to strip off linefeed or return characters from
;;;             :   the reference files so that a single ref file can be used
;;;             :   to test regardless of the native format for the output.
;;; 07.12.20 Dan
;;;             : * Adding a function to run and test a single file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This provides a mechanims for comparing the outputs of models to a fixed
;;; reference run of that model.  The primary usage is for testing the public
;;; distributions of ACT-R to ensure they run the test models the same as the
;;; reference version across different Lisps/OSs.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; run-actr-tests
;;;
;;; This function takes no parameters.  For each file in the ACT-R6:test-models
;;; directory with a "lisp" file type it will load that file.  It will create
;;; a new file with the same name but of type "out" in that directory overwriting
;;; such a file if it already exists.  It will then bind the variable *out-file* 
;;; to an output stream for that file and then call the run-it function.  The
;;; output stream will then be closed.
;;; After that, if there is another file with that name of type "ref" the "out"
;;; file is compared line by line to the "ref" file.  If there are any differences
;;; then those lines are printed to *standard-output* and if no differences are
;;; found a message is printed to indicate that.
;;; 
;;; run-actr-test
;;;
;;; This function takes one parameter which should be a string naming a file in
;;; the ACT-R6:test-models directory which has a "lisp" file type.  It will load 
;;; that file.  It will create a new file with the same name but of type "out" 
;;; in that directory overwriting such a file if it already exists.  It will then 
;;; bind the variable *out-file*  to an output stream for that file and then call 
;;; the run-it function.  The output stream will then be closed.
;;; After that, if there is another file with that name of type "ref" the "out"
;;; file is compared line by line to the "ref" file.  If there are any differences
;;; then those lines are printed to *standard-output* and if no differences are
;;; found a message is printed to indicate that.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; The assumptions are that the models to be tested set the :v (and possibly
;;; :cmdt) parameter to the value of *out-file*, specify an explicit seed for
;;; the model and only use the act-r-random function to guarantee repeatability,
;;; and specify a function called run-it that is used to generate the output 
;;; for comparison.  If the model has any output other than the trace the appropriate 
;;; measures should be taken to have that output end up in the comparison file (like 
;;; using model-output or command-output for instance).
;;;
;;; The default set of test models is all of the tutorial models from r492 and 
;;; the reference runs are also from r492 (the last verified public release 
;;; at the time of creating this).
;;; 
;;; Note that the solution models and thier reference runs are in a protected
;;; zip file which is only available under the same restrictions as the regular
;;; solution models.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defvar *out-file*)

(defun run-actr-tests ()
  (dolist (file (directory (translate-logical-pathname "ACT-R6:test-models;*.lisp")))
    (let ((base-pathname (make-pathname :host (pathname-host file)
                                        :directory (pathname-directory file)
                                        :device (pathname-device file)
                                        :name (pathname-name file))))
      
      (format t "~%###########################~%Testing file ~s~%" (pathname-name file))
      (with-open-file (f (merge-pathnames base-pathname (make-pathname :type "out")) :direction :output :if-exists :supersede :if-does-not-exist :create)
        (setf *out-file* f)
        (load file)
        (terpri)
        (run-it))
      (if (probe-file (merge-pathnames base-pathname (make-pathname :type "ref")))
          (compare-test-model-files base-pathname)
        (format t "File does not have a reference for comparison~%")))))

(defun run-actr-test (file-name)
  (let ((file (translate-logical-pathname (format nil "ACT-R6:test-models;~a.lisp" file-name)))
        (base-pathname (translate-logical-pathname (format nil "ACT-R6:test-models;~a" file-name))))
      
      (format t "~%###########################~%Testing file ~s~%" (pathname-name file))
      (with-open-file (f (merge-pathnames base-pathname (make-pathname :type "out")) :direction :output :if-exists :supersede :if-does-not-exist :create)
        (setf *out-file* f)
        (load file)
        (terpri)
        (run-it))
      (if (probe-file (merge-pathnames base-pathname (make-pathname :type "ref")))
          (compare-test-model-files base-pathname)
        (format t "File does not have a reference for comparison~%"))))


(defun compare-test-model-files (file)
  (with-open-file (f1 (merge-pathnames file (make-pathname :type "ref")) :direction :input)
    (with-open-file (f2 (merge-pathnames file (make-pathname :type "out")) :direction :input)
      (let ((diff nil))
        (loop 
         (let ((l1 (read-line f1 nil :file-ended))
               (l2 (read-line f2 nil :file-ended)))
           
           (when (and (stringp l1)
                      (> (length l1) 0)
                      (eq (aref l1 0) #\linefeed))
             (setf l1 (subseq l1 1))
             ;; just assume that if the out file ends that
             ;; a corresponding blank line on the other file
             ;; is also file end...
             (when (and (eq l2 :file-ended)
                        (zerop (length l1)))
               (setf l1 :file-ended)))
           
           (when (and (stringp l1)
                      (> (length l1) 0)
                      (eq (aref l1 (1- (length l1))) #\return))
             (setf l1 (subseq l1 0 (1- (length l1)))))           
                         
           
          (cond ((and (eq l1 :file-ended) (eq l2 :file-ended))
                 (unless diff
                   (format t "No differences detected.~%"))
                 (return))
                ((eq l1 :file-ended)
                 (format t "Reference file shorter than output file.~%")
                 (format t "First extra line of output file is:~%~S~%" l2)
                 (return))
                ((eq l2 :file-ended)
                 (format t "Output file shorter than reference file.~%")
                 (format t "First extra line of reference file is:~%~S~%" l1)
                 (return))
                ((not (string-equal l1 l2))
                 (setf diff t)
                 (format t "Files differ.~%Reference file: ~s~%Output    file: ~S~%" l1 l2)))))))))
    

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
