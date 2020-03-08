# This file creates a window on the Environment side for showing
# Experiments generated with the UWI.
# There is only one experiment window on the environment side which is 
# reused for now.  May need to expand it later, but for now this is
# simple enough and I've tried to leave it open for future expansion.

toplevel .env_window

wm withdraw .env_window

canvas .env_window.can

place .env_window.can -x 0 -y 0 -relwidth 1.0 -relheight 1.0

# don't really close it when the close button pressed

wm protocol .env_window WM_DELETE_WINDOW {wm withdraw .env_window}

# could be problematic for "non-normal" keys, but for now
# quick and dirty...

bind .env_window <KeyPress> {
  send_environment_cmd \
   "update [get_handler_name %W] \
     (lambda (x) \
       (device-handle-keypress *library-experiment-window* #\\%K) \
       (list 'ignore))"
}
 

send_environment_cmd \
  "create env-window-handler .env_window .env_window \
    (lambda (x) \
      (setf (environment-control-use-env-windows *environment-control*) $options_array(use_env_window) ) \
      (list 'ignore)) ()"

bind .env_window <Destroy> {
  remove_handler %W
}

global id_references
global button_references

set cursor_id ""
set attention_id ""
set eyeloc_id ""

proc process_env_window {win cmd} {
  global id_references
  global cursor_id
  global attention_id
  global eyeloc_id
  global button_references
 
  switch [lindex $cmd 0] {
    ignore {
    }
    select {
      wm deiconify $win
      raise $win
      focus -force $win
    }
    close {
      $win.can delete all
      wm withdraw $win 
    }
    clear {
      $win.can delete all
    }
    open {
      wm title $win [lindex $cmd 1]
      wm geometry $win \
         "[lindex $cmd 4]x[lindex $cmd 5]+[lindex $cmd 2]+[lindex $cmd 3]"
      wm deiconify $win
      focus -force $win
    }
    remove {
      $win.can delete $id_references([lindex $cmd 1])
    }
    attention {
      if {$attention_id != ""} {$win.can delete $attention_id}
      set x [lindex $cmd 1]
      set y [lindex $cmd 2]
      set attention_id [$win.can create oval [expr $x - 10] [expr $y - 10] \
                                        [expr $x + 10] [expr $y + 10] \
                                        -outline red -width 4]                                     
    }
    eyeloc {
      if {$eyeloc_id != ""} {$win.can delete $eyeloc_id}
      set x [lindex $cmd 1]
      set y [lindex $cmd 2]
      set eyeloc_id [$win.can create oval [expr $x - 5] [expr $y - 5] \
                                         [expr $x + 5] [expr $y + 5] \
                                         -outline blue -width 3]                                     
    }

    cursor {
      if {$cursor_id != ""} {$win.can delete $cursor_id}
      set x [lindex $cmd 1]
      set y [lindex $cmd 2]
      set cursor_id [$win.can create polygon $x $y \
                                                $x [expr $y + 15] \
                                                [expr $x + 5] [expr $y + 12] \
                                                [expr $x + 9] [expr $y + 20] \
                                                [expr $x + 13] [expr $y + 19] \
                                                [expr $x + 9] [expr $y + 12] \
                                                [expr $x + 15] [expr $y + 12] \
                                        -fill white -outline black]
    }
    line {
      set id_references([lindex $cmd 1]) \
        [$win.can create line [lindex $cmd 2] [lindex $cmd 3] \
                         [lindex $cmd 5] [lindex $cmd 6] \
                         -fill [lindex $cmd 4] -width 2]
    }
    text {
       # report_status $cmd
      set id_references([lindex $cmd 1]) \
        [$win.can create text [lindex $cmd 2] [lindex $cmd 3] \
                              -font env_window_font -fill [lindex $cmd 4] \
                              -text [lindex $cmd 5] -anchor nw]
    }
    button {
      set but \
       [button [new_variable_name $win.can.but] -text [lindex $cmd 6] -bg [lindex $cmd 7] -font button_font\
               -command "send_environment_cmd \
                          \"update [get_handler_name $win] \
                             (lambda (x) \
                               (unless *actr-enabled-p* \
                                 (dolist (y (subviews \
                                             *library-experiment-window*)) \
                                   (when (string-equal (id y) \
                                                    (string '[lindex $cmd 1])) \
                                     (vv-click-event-handler y nil)))) \
                               (list 'ignore))\""]

      set button_references([lindex $cmd 1]) $but
      set id_references([lindex $cmd 1]) \
        [$win.can create window [lindex $cmd 2] [lindex $cmd 3] \
                                -window $but -anchor nw \
                                -height [lindex $cmd 5] -width [lindex $cmd 4]]
    }
    click {
      event generate $button_references([lindex $cmd 1]) <ButtonPress> -button 1
      after 100 \
       "event generate \
              $button_references([lindex $cmd 1]) <ButtonRelease> -button 1"
    }
  }
}
