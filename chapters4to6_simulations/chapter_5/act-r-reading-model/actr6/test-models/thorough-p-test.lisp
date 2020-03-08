(clear-all)

(defun run-it ()
  (dolist (x '(slot1 slot3 not-a-slot))
    (dolist (y '(dummy1 merged dummy2 dummy3 dummy4 free "dummy1" "dummy2" t nil 3 5 7))
      (mod-chunk-fct 'g1 `(slot2 ,x slot1 ,y))
      (overwrite-buffer-chunk 'goal 'g1)
      (model-output "~%~%############################~%")
      (buffer-chunk goal)
      (let ((matches (no-output (whynot)))
            (dummy-num 1))
        (dolist (z (list (cons "slot1" "P") (cons "p*-slot1" "P*") (cons "p*-implicit-slot" "P* indirect")))
          (incf dummy-num)
          (model-output "~%~a productions" (cdr z))
          (model-output "           dummy1   dummy~X   \"dummy1\"   5        t      nil" dummy-num)
          (dolist (a '("=" "-" "<" "<=" ">" ">="))
            (model-output "test ~2A     ~{~9s~}" a (mapcar (lambda (x)
                                                             (let ((name (intern (string-upcase (concatenate 'string (car z) a x)))))
                                                               (if (find name matches) t nil)))
                                                     '("chunk" "implicit-chunk" "string" "number" "t" "nil")))))))))

(define-model test-them-all
    (sgp-fct (list :v *out-file* :cmdt *out-file*))
  (sgp :dcnn nil :use-tree nil)

  (chunk-type test slot1 slot2 slot3)
  
  (define-chunks (dummy1 isa chunk)
      (g1 isa test)
    (merged isa chunk)
    (slot1 isa chunk)
    (slot2 isa chunk)
    (slot3 isa chunk)
    (not-a-slot isa chunk))
  
  (merge-chunks dummy1 merged)
  
  (p slot1=chunk
     =goal>
     isa test
     slot1 dummy1
     ==>)
  
  (p slot1=implicit-chunk
     =goal>
     isa test
     slot1 dummy2
     ==>)
  
  (p slot1=string
     =goal>
     isa test
     slot1 "dummy1"
     ==>)
  
  (p slot1=number
     =goal>
     isa test
     slot1 5
     ==>)
  
  (p slot1=t
     =goal>
     isa test
     slot1 t
     ==>)
  
  (p slot1=nil
     =goal>
     isa test
     slot1 nil
     ==>)
  
  
  (p slot1-chunk
     =goal>
     isa test
     - slot1 dummy1
     ==>)
  
  (p slot1-implicit-chunk
     =goal>
     isa test
     - slot1 dummy2
     ==>)
  
  (p slot1-string
     =goal>
     isa test
     - slot1 "dummy1"
     ==>)
  
  (p slot1-number
     =goal>
     isa test
     - slot1 5
     ==>)
  
  (p slot1-t
     =goal>
     isa test
     - slot1 t
     ==>)
  
  (p slot1-nil
     =goal>
     isa test
     - slot1 nil
     ==>)
  
  (p slot1<chunk
     =goal>
     isa test
     < slot1 dummy1
     ==>)
  
  (p slot1<implicit-chunk
     =goal>
     isa test
     < slot1 dummy2
     ==>)
  
  (p slot1<string
     =goal>
     isa test
     < slot1 "dummy1"
     ==>)
  
  (p slot1<number
     =goal>
     isa test
     < slot1 5
     ==>)
  
  (p slot1<t
     =goal>
     isa test
     < slot1 t
     ==>)
  
  (p slot1<nil
     =goal>
     isa test
     < slot1 nil
     ==>)
  
  (p slot1<=chunk
     =goal>
     isa test
     <= slot1 dummy1
     ==>)
  
  (p slot1<=implicit-chunk
     =goal>
     isa test
     <= slot1 dummy2
     ==>)
  
  (p slot1<=string
     =goal>
     isa test
     <= slot1 "dummy1"
     ==>)
  
  (p slot1<=number
     =goal>
     isa test
     <= slot1 5
     ==>)
  
  (p slot1<=t
     =goal>
     isa test
     <= slot1 t
     ==>)
  
  (p slot1<=nil
     =goal>
     isa test
     <= slot1 nil
     ==>)
  
  (p slot1>chunk
     =goal>
     isa test
     > slot1 dummy1
     ==>)
  
  (p slot1>implicit-chunk
     =goal>
     isa test
     > slot1 dummy2
     ==>)
  
  (p slot1>string
     =goal>
     isa test
     > slot1 "dummy1"
     ==>)
  
  (p slot1>number
     =goal>
     isa test
     > slot1 5
     ==>)
  
  (p slot1>t
     =goal>
     isa test
     > slot1 t
     ==>)
  
  (p slot1>nil
     =goal>
     isa test
     > slot1 nil
     ==>)

  
  (p slot1=>chunk
     =goal>
     isa test
     >= slot1 dummy1
     ==>)
  
  (p slot1>=implicit-chunk
     =goal>
     isa test
     >= slot1 dummy2
     ==>)
  
  (p slot1>=string
     =goal>
     isa test
     >= slot1 "dummy1"
     ==>)
  
  (p slot1>=number
     =goal>
     isa test
     >= slot1 5
     ==>)
  
  (p slot1>=t
     =goal>
     isa test
     >= slot1 t
     ==>)
  
  (p slot1>=nil
     =goal>
     isa test
     >= slot1 nil
     ==>)
  
  ;;;;;;;;;;
  
  (p* p*-slot1=chunk
     =goal>
     isa test
     slot1 dummy1
     ==>)
  
  (p* p*-slot1=implicit-chunk
     =goal>
     isa test
     slot1 dummy3
     ==>)
  
  (p* p*-slot1=string
     =goal>
     isa test
     slot1 "dummy1"
     ==>)
  
  (p* p*-slot1=number
     =goal>
     isa test
     slot1 5
     ==>)
  
  (p* p*-slot1=t
     =goal>
     isa test
     slot1 t
     ==>)
  
  (p* p*-slot1=nil
     =goal>
     isa test
     slot1 nil
     ==>)
  
  
  (p* p*-slot1-chunk
     =goal>
     isa test
     - slot1 dummy1
     ==>)
  
  (p* p*-slot1-implicit-chunk
     =goal>
     isa test
     - slot1 dummy3
     ==>)
  
  (p* p*-slot1-string
     =goal>
     isa test
     - slot1 "dummy1"
     ==>)
  
  (p* p*-slot1-number
     =goal>
     isa test
     - slot1 5
     ==>)
  
  (p* p*-slot1-t
     =goal>
     isa test
     - slot1 t
     ==>)
  
  (p* p*-slot1-nil
     =goal>
     isa test
     - slot1 nil
     ==>)
  
  (p* p*-slot1<chunk
     =goal>
     isa test
     < slot1 dummy1
     ==>)
  
  (p* p*-slot1<implicit-chunk
     =goal>
     isa test
     < slot1 dummy3
     ==>)
  
  (p* p*-slot1<string
     =goal>
     isa test
     < slot1 "dummy1"
     ==>)
  
  (p* p*-slot1<number
     =goal>
     isa test
     < slot1 5
     ==>)
  
  (p* p*-slot1<t
     =goal>
     isa test
     < slot1 t
     ==>)
  
  (p* p*-slot1<nil
     =goal>
     isa test
     < slot1 nil
     ==>)
  
  (p* p*-slot1<=chunk
     =goal>
     isa test
     <= slot1 dummy1
     ==>)
  
  (p* p*-slot1<=implicit-chunk
     =goal>
     isa test
     <= slot1 dummy3
     ==>)
  
  (p* p*-slot1<=string
     =goal>
     isa test
     <= slot1 "dummy1"
     ==>)
  
  (p* p*-slot1<=number
     =goal>
     isa test
     <= slot1 5
     ==>)
  
  (p* p*-slot1<=t
     =goal>
     isa test
     <= slot1 t
     ==>)
  
  (p* p*-slot1<=nil
     =goal>
     isa test
     <= slot1 nil
     ==>)
  
  (p* p*-slot1>chunk
     =goal>
     isa test
     > slot1 dummy1
     ==>)
  
  (p* p*-slot1>implicit-chunk
     =goal>
     isa test
     > slot1 dummy3
     ==>)
  
  (p* p*-slot1>string
     =goal>
     isa test
     > slot1 "dummy1"
     ==>)
  
  (p* p*-slot1>number
     =goal>
     isa test
     > slot1 5
     ==>)
  
  (p* p*-slot1>t
     =goal>
     isa test
     > slot1 t
     ==>)
  
  (p* p*-slot1>nil
     =goal>
     isa test
     > slot1 nil
     ==>)

  
  (p* p*-slot1=>chunk
     =goal>
     isa test
     >= slot1 dummy1
     ==>)
  
  (p* p*-slot1>=implicit-chunk
     =goal>
     isa test
     >= slot1 dummy3
     ==>)
  
  (p* p*-slot1>=string
     =goal>
     isa test
     >= slot1 "dummy1"
     ==>)
  
  (p* p*-slot1>=number
     =goal>
     isa test
     >= slot1 5
     ==>)
  
  (p* p*-slot1>=t
     =goal>
     isa test
     >= slot1 t
     ==>)
  
  (p* p*-slot1>=nil
     =goal>
     isa test
     >= slot1 nil
     ==>)
  
  
  ;;;;;   
  
  
  (p* p*-implicit-slot=chunk
      =goal>
      isa test
      slot2 =slot
      =slot dummy1
      ==>)
  
  (p* p*-implicit-slot=implicit-chunk
     =goal>
      isa test
      slot2 =slot
      =slot dummy4
     ==>)
  
  (p* p*-implicit-slot=string
     =goal>
       isa test
      slot2 =slot
     =slot "dummy1"
     ==>)
  
  (p* p*-implicit-slot=number
     =goal>
       isa test
      slot2 =slot
     =slot 5
     ==>)
  
  (p* p*-implicit-slot=t
     =goal>
       isa test
      slot2 =slot
     =slot t
     ==>)
  
  (p* p*-implicit-slot=nil
     =goal>
       isa test
      slot2 =slot
     =slot nil
     ==>)
  
  
  (p* p*-implicit-slot-chunk
     =goal>
       isa test
      slot2 =slot
     - =slot dummy1
     ==>)
  
  (p* p*-implicit-slot-implicit-chunk
     =goal>
       isa test
      slot2 =slot
     - =slot dummy4
     ==>)
  
  (p* p*-implicit-slot-string
     =goal>
       isa test
      slot2 =slot
     - =slot "dummy1"
     ==>)
  
  (p* p*-implicit-slot-number
     =goal>
       isa test
      slot2 =slot
     - =slot 5
     ==>)
  
  (p* p*-implicit-slot-t
     =goal>
       isa test
      slot2 =slot
     - =slot t
     ==>)
  
  (p* p*-implicit-slot-nil
     =goal>
       isa test
      slot2 =slot
     - =slot nil
     ==>)
  
  (p* p*-implicit-slot<chunk
     =goal>
       isa test
      slot2 =slot
     < =slot dummy1
     ==>)
  
  (p* p*-implicit-slot<implicit-chunk
     =goal>
       isa test
      slot2 =slot
     < =slot dummy4
     ==>)
  
  (p* p*-implicit-slot<string
     =goal>
       isa test
      slot2 =slot
     < =slot "dummy1"
     ==>)
  
  (p* p*-implicit-slot<number
     =goal>
       isa test
      slot2 =slot
     < =slot 5
     ==>)
  
  (p* p*-implicit-slot<t
     =goal>
       isa test
      slot2 =slot
     < =slot t
     ==>)
  
  (p* p*-implicit-slot<nil
     =goal>
       isa test
      slot2 =slot
     < =slot nil
     ==>)
  
  (p* p*-implicit-slot<=chunk
     =goal>
       isa test
      slot2 =slot
     <= =slot dummy1
     ==>)
  
  (p* p*-implicit-slot<=implicit-chunk
     =goal>
       isa test
      slot2 =slot
     <= =slot dummy4
     ==>)
  
  (p* p*-implicit-slot<=string
     =goal>
       isa test
      slot2 =slot
     <= =slot "dummy1"
     ==>)
  
  (p* p*-implicit-slot<=number
     =goal>
       isa test
      slot2 =slot
     <= =slot 5
     ==>)
  
  (p* p*-implicit-slot<=t
     =goal>
       isa test
      slot2 =slot
     <= =slot t
     ==>)
  
  (p* p*-implicit-slot<=nil
     =goal>
       isa test
      slot2 =slot
     <= =slot nil
     ==>)
  
  (p* p*-implicit-slot>chunk
     =goal>
       isa test
      slot2 =slot
     > =slot dummy1
     ==>)
  
  (p* p*-implicit-slot>implicit-chunk
     =goal>
       isa test
      slot2 =slot
     > =slot dummy4
     ==>)
  
  (p* p*-implicit-slot>string
     =goal>
       isa test
      slot2 =slot
     > =slot "dummy1"
     ==>)
  
  (p* p*-implicit-slot>number
     =goal>
       isa test
      slot2 =slot
     > =slot 5
     ==>)
  
  (p* p*-implicit-slot>t
     =goal>
       isa test
      slot2 =slot
     > =slot t
     ==>)
  
  (p* p*-implicit-slot>nil
     =goal>
       isa test
      slot2 =slot
     > =slot nil
     ==>)

  
  (p* p*-implicit-slot=>chunk
     =goal>
       isa test
      slot2 =slot
     >= =slot dummy1
     ==>)
  
  (p* p*-implicit-slot>=implicit-chunk
     =goal>
       isa test
      slot2 =slot
     >= =slot dummy4
     ==>)
  
  (p* p*-implicit-slot>=string
     =goal>
       isa test
      slot2 =slot
     >= =slot "dummy1"
     ==>)
  
  (p* p*-implicit-slot>=number
     =goal>
       isa test
      slot2 =slot
     >= =slot 5
     ==>)
  
  (p* p*-implicit-slot>=t
     =goal>
       isa test
      slot2 =slot
     >= =slot t
     ==>)
  
  (p* p*-implicit-slot>=nil
     =goal>
       isa test
      slot2 =slot
     >= =slot nil
     ==>)
)
