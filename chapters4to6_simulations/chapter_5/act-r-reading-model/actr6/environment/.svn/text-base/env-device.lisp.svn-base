;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : env-device.lisp
;;; Version     : 3.0
;;; 
;;; Description : No system dependent code.
;;;             : This file contains the code that handles passing
;;;             : the virtual windows out through Tk - the visible
;;;             : virtuals, and the AGI support.
;;; Bugs        : 
;;; 
;;; Todo        :   
;;; ----- History -----
;;;
;;; 10/01/2002  Dan
;;;             : Added this header. 
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;; 12/04/2002  Dan
;;;             : Added the definition for *use-environment-window*
;;;             : even though it's still not used because it
;;;             : does get set by the environment and generates a
;;;             : warning.
;;; 01/06/2003  Dan
;;;             : Updated it so that the colors were sent out
;;;             : properly for all supported colors and so that
;;;             : text colors worked.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; -----------------------------------------------------------------------
;;; 2005.04.13  Dan [2.0]
;;;             : * Moved over to ACT-R 6.
;;; 2005.06.07 Dan
;;;             : * Replaced a call to device-interface with 
;;;             :   current-device-interface.
;;; 2005.06.28 Dan
;;;             : * Removed a pm-proc-display call that was still lingering
;;;             :   in the device-move-cursor-to method.
;;; 2007.07.13 Dan
;;;             : * Added the sending of a button's color to the env.
;;; 2010.05.21 Dan
;;;             : * Changed the name of the dialog items to add a vv on the
;;;             :   to avoid interning a name which could shift the names
;;;             :   for things like visual chunks (text items in particular).
;;;             :   Doesn't affect performance, just keeps traces consistent
;;;             :   when shifting between visible and virtual.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R6:support;uni-files")

;;; virtual windows for the environment that go out through
;;; Tcl/Tk to display i.e. a visible virtual
;;;
;;; Inherits much of the functionality from the rpm-virtual-window
;;; but needs to send the commands to Tcl/Tk as well

(defun visible-virtuals-available? () 
  "Return whether or not the visible-virtuals are available"
  (and (environment-control-connections *environment-control*)
       (environment-control-use-env-windows *environment-control*)))

;;; First define all the stuff necessary for it and its possible 
;;; 'subviews' as it relates to the device interface

(defclass visible-virtual-window (rpm-virtual-window)
  ())

(defmethod initialize-instance :after ((win visible-virtual-window) &key)
  (send-env-window-update (list 'open (window-title win) (x-pos win)
                                (y-pos win) (width win) (height win))))

(defclass env-text-vdi (static-text-vdi)
  ()
  (:default-initargs
      :id (new-name-fct "VVTEXT")))

(defclass env-button-vdi (button-vdi)
  ()
  (:default-initargs
      :id (new-name-fct "VVBUTTON")))

(defclass env-line-vdi (v-liner)
  ()
  (:default-initargs
      :id (new-name-fct "VVLINE")))


(defmethod vv-click-event-handler ((btn env-button-vdi) where)
  (declare (ignore where))
  (when (functionp (action-function btn))
    (funcall (action-function btn) btn))
  (when *actr-enabled-p* 
    (send-env-window-update (list 'click (id btn)))))


(defmethod device-move-cursor-to ((vw visible-virtual-window) (loc vector))
  (setf (cursor-pos vw) loc)
  (when (with-cursor-p (current-device-interface))
    (proc-display))
  (send-env-window-update (list 'cursor (px loc) (py loc))))

(defmethod device-update-attended-loc ((wind visible-virtual-window) xyloc)
  (send-env-window-update (list 'attention (px xyloc) (py xyloc))))


;;; Then define the UWI functions - which get called by the exp interface

(defmethod close-rpm-window ((win visible-virtual-window))
  (setf (window-open? win) nil)
  (send-env-window-update (list 'close)))


(defmethod select-rpm-window ((win visible-virtual-window))
  (send-env-window-update (list 'select)))

(defmethod add-visual-items-to-rpm-window ((win visible-virtual-window) 
                                           &rest items)
  (dolist (item items)
    (add-subviews win item)
    (send-env-window-update 
     (case (type-of item)
       (env-button-vdi
        (list 'button (id item) (x-pos item) (y-pos item) 
          (width item) (height item) (dialog-item-text item) (color-symbol->env-color (color item))))
       (env-text-vdi
        (list 'text 
          (id item) (x-pos item) (y-pos item) 
          (color-symbol->env-color (color item))  (dialog-item-text item)))
       (env-line-vdi
        (list 'line
              (id item) (x-pos item) (y-pos item) 
              (color-symbol->env-color (color item)) (width item) (height item)))))))



(defun color-symbol->env-color (color)
  (cond ((equal color 'red) "red")
        ((equal color 'blue) "blue")
        ((equal color 'green) "green")
        ((equal color 'black) "black")
        ((equal color 'white) "white")
        ((equal color 'pink)  "pink")
        ((equal color 'yellow) "yellow")
        ((equal color 'dark-green) "green4")
        ((equal color 'light-blue) "cyan")
        ((equal color 'purple) "purple")
        ((equal color 'brown) "brown")
        ((equal color 'light-gray) "gray90")
        ((equal color 'gray) "gray")
        ((equal color 'dark-gray) "gray40")
        (t "black")))



(defmethod remove-visual-items-from-rpm-window ((win visible-virtual-window) 
                                                &rest items)
  (dolist (item items)
    (remove-subviews win item)
    (send-env-window-update (list 'remove (id item)))))



(defmethod remove-all-items-from-rpm-window ((win visible-virtual-window))
  (apply #'remove-subviews win (subviews win))
  (send-env-window-update (list 'clear)))


(defmethod make-button-for-rpm-window ((win visible-virtual-window) 
                                       &key (x 0) (y 0) (text "Ok")  
                                       (action nil) (height 18) (width 60) (color 'gray))
  (make-instance 'env-button-vdi
    :x-pos x 
    :y-pos y
    :dialog-item-text text
    :action action
    :height height
    :width width
    :color color))


(defmethod make-static-text-for-rpm-window ((win visible-virtual-window) 
                                            &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black))
  (make-instance 'env-text-vdi
    :x-pos x 
    :y-pos y
    :dialog-item-text text
    :height height
    :width width
    :color color
    ))

(defmethod rpm-window-visible-status ((win visible-virtual-window))
  t)

(defmethod make-line-for-rpm-window ((wind visible-virtual-window) 
                                     start-pt end-pt &optional (color 'black))
  (make-instance 'env-line-vdi
    :color color
    :x-pos (first start-pt)
    :y-pos (second start-pt)
    :width (first end-pt)
    :height (second end-pt)))

;;; This is a little tricky, because if it's a real window in an IDE'ed
;;; Lisp, it's still got to do the right thing to prevent hanging, 
;;; which is lisp specific...

(defmethod allow-event-manager ((win visible-virtual-window))
  (uni-process-system-events) 
  )


;;; This function relies on the handlers from the environment existing...

(defun send-env-window-update (cmd)
  (dolist (x (environment-control-windows *environment-control*))
    (setf (update-form x) #'(lambda (x) (declare (ignore x)) cmd))
    (update-handler x nil)))

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
