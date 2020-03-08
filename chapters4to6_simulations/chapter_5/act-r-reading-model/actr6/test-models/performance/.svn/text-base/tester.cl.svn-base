;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : tester.cl
;;; Version     : 1.0
;;; 
;;; Description : Tools for running speed tests of ACT-R components/models.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2008.07.25  Dan
;;;             : * Initial creation.
;;; 2008.07.30  Dan
;;;             : * Added the code to save all the output into one file called
;;;             :   all-data.out when run-perf-tests is called.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; 
;;; Code to evaluate system performance.
;;; 
;;; The idea is to generate models/tests for all of the primary components 
;;; individually and run each of those under various sizes.  What constitutes a 
;;; size depends on the item being tested - number of productions, chunks in DM,
;;; length of the run, etc.
;;;
;;; The only thing being tested at this point is time to execute because space
;;; is difficult to deal with since room is implementation dependent and only 
;;; generates text output, not raw numbers.  Of course, space can be an 
;;; important thing to measure (especially since one can often trade off 
;;; between space and time), but for now that's going to be ignored.
;;;
;;; The absolute values of the tests are not likely to be very informative.  
;;; Comparing across Lisps/OS/machines is not the intended purpose here since 
;;; previous tests have shown large differences in those regards.  The purpose 
;;; of these metrics is to determine whether changes to the code show any
;;; significant effects on performance when tested under the same situation
;;; (either positively or negatively) and will reqire testing across a number
;;; of systems for a solid verification.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; There are two commands which can be used to run tests:
;;;
;;; run-perf-tests (&optional (n 5))
;;;
;;; Run-perf-tests runs all the test models found in the performance directory
;;; of the actr distribution: ACT-R6:test-models;performance.
;;;
;;; Every .lisp file in that directory will be compiled and then loaded.  If
;;; the file has set the *performance-test* variable as described below then it 
;;; will run n iterations over each of the test sizes specified and report the 
;;; average results of those runs in seconds.  For each run it will record
;;; the time needed to perform the init code specified, the running code given,
;;; and the time it takes to call clear-all after the run completes.  The timing
;;; will be done using the get-internal-real-time function.  It will report the 
;;; averages for each of those and for the sum of the three for each size given.
;;;
;;; That output will be sent to *standard-output* as well as to a file in the 
;;; performance directory with the same name as the .lisp file that was loaded 
;;; and a type of .out instead of .lisp.  If such a file already exists it will 
;;; be overwritten.  The output is reported in fixed width columns of 15 
;;; characters and indicates the sizes along the top row and then a row for each 
;;; of init, run, clear-all, and total.  Here's an example from the 
;;; dummy-test.lisp code:
;;;
;;;                     2              4              6         
;;; Init                   1.00000        2.00000        3.00000
;;; Run                    2.00000        4.00000        5.98450
;;; Clear                  0.00000        0.00000        0.00000
;;; Sum                    3.00000        6.00000        8.98450
;;;
;;; The data written to the individual files is also written to a single
;;; file called all-data.out which will also be created in the performance
;;; directory and overwritten if it already exists.
;;;
;;; In addition to that output, it will print the name of the file to 
;;; *standard-output* along with .'s as it iterates through the n runs over
;;; the sizes.  It also calls uni-process-system-events between runs so that
;;; it doesn't completely freeze out the system in Lisps where that's an issue.
;;;
;;; To specify a test the *performance-test* variable needs to be set to a list
;;; of three items.  The first must be the function or name of the function to 
;;; call to perform the initialization step of the test, the second must the
;;; function or name of the function to call to run the test, and the third
;;; must be a list of 'sizes' to use when running the test.  The items in the
;;; sizes list do not need to be numbers.  When the init and run functions 
;;; given are called they will be passed one of the items from the sizes list.
;;; That is the only parameter they will be given.
;;;
;;;
;;; run-perf-test (file &optional (n 5))
;;;
;;; This command works just like the previous one except that it only runs one
;;; test.  The name of the file to test must be provded as a string without
;;; the pathname type or any directory info.  It expects to find that file in 
;;; the performance directory.  It does not create the all-data.out file - only
;;; the specific file for the test file specified.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Decided to follow the same general plan as was done for the functionality
;;; tests - let it just pick up any files which may be present in the directory
;;; so that it's easy to extend later by just dropping in new ones.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defvar *performance-test*)

(defun performance-averager (vals)
  "Returns averages of the lists in seconds - assumes that it came in based on
   internal time units i.e. get-internal-real-time."
  (mapcar (lambda (x)  (/ (/ x (length vals)) internal-time-units-per-second)) (apply 'mapcar '+ vals)))

(defun run-perf-tests (&optional (n 5))
  (if (and (numberp n) (plusp n))
      (with-open-file (g (translate-logical-pathname "ACT-R6:test-models;performance;all-data.out") :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (file (directory (translate-logical-pathname "ACT-R6:test-models;performance;*.lisp")))
          (run-perf-test (pathname-name file) n g)))
    (print-warning "N must be a positive number no tests run.")))

(defun run-perf-test (file &optional (n 5) (global-out nil))
  (if (and (numberp n) (plusp n))
      (let ((path (translate-logical-pathname (format nil "ACT-R6:test-models;performance;~a.lisp" file))))
        (if (probe-file path)
            (let ((base-pathname (make-pathname :host (pathname-host path)
                                                :directory (pathname-directory path)
                                                :device (pathname-device path)
                                                :name (pathname-name path))))
              (setf *performance-test* nil)
              (compile-and-load path)
              (if (and (= (length *performance-test*) 3) (fctornil (first *performance-test*))
                       (fctornil (second *performance-test*)) (listp (third *performance-test*))
                       (> (length (third *performance-test*)) 0))
                                    
                  (with-open-file (f (merge-pathnames base-pathname (make-pathname :type "out")) :direction :output :if-exists :supersede :if-does-not-exist :create)
                    (let ((out (if global-out (make-broadcast-stream f *standard-output* global-out) (make-broadcast-stream f *standard-output*)))
                          (start-time nil)
                          (init-times nil)
                          (run-times nil)
                          (clear-times nil)
                          (init (first *performance-test*))
                          (runner (second *performance-test*))
                          (sizes (third *performance-test*)))
                      
                      (format t "~%###########################~%Performance testing file ~s~%" (pathname-name file))
                      
                      (when global-out
                        (format global-out "~S~%" (pathname-name file)))
                      
                      ;; make sure there's nothing left over from a previous or bad run
                      (clear-all)
                      
                      (dotimes (runs n)
                        (let ((i-times nil)
                              (r-times nil)
                              (c-times nil))
                          (dolist (size sizes)
                            (format t ".")
                            (setf start-time (get-internal-real-time))
                            (funcall init size)
                            (push (- (get-internal-real-time) start-time) i-times)
                            (setf start-time (get-internal-real-time))
                            (funcall runner size)
                            (push (- (get-internal-real-time) start-time) r-times)
                            (setf start-time (get-internal-real-time))
                            (clear-all)
                            (push (- (get-internal-real-time) start-time) c-times)
                            (uni-process-system-events))
                          (terpri)
                          (push (reverse i-times) init-times)
                          (push (reverse r-times) run-times)
                          (push (reverse c-times) clear-times)))
                      (let* ((i-average (performance-averager init-times))
                             (r-average (performance-averager run-times))
                             (c-average (performance-averager clear-times))
                             (sum (mapcar '+ i-average r-average c-average)))
                            (format out "               ~{     ~10s~}~%Init           ~{~15,5f~}~%Run            ~{~15,5f~}~%Clear          ~{~15,5f~}~%Sum            ~{~15,5f~}~%" 
                        sizes i-average r-average c-average sum))))
                (print-warning "Bad *performance-test* value in file ~S : ~S" (pathname-name file) *performance-test*)))
          (print-warning "File ~S was not found.  No test run." path)))
    (print-warning "N must be a positive number. No test run for file ~a." file)))




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
