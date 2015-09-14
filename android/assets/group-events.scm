(define frag-ev-grpint
  (fragment
   "ev-grpint"
   (linear-layout
    (make-id "") 'vertical fillwrap gp-col
    (list
     (build-grid-selector "gp-int-leader" "single" "<b>Inter-group interaction</b> Leader mongoose")
     (horiz
      (linear-layout
       (make-id "") 'vertical (layout 400 'fill-parent '1 'left 0) trans-col
       (list
        (mtext "text" "Outcome")
        (mspinner "gp-int-out" list-interaction-outcome
                  (lambda (v)
                    (set-current! 'entity-type "group-interaction")
                    (entity-set-value! "outcome" "varchar" (spinner-choice list-interaction-outcome v)) '()))
        (mtext "text" "Duration (mins)")
        (edit-text (make-id "gp-int-dur") "" 30 "numeric" fillwrap
                   (lambda (v)
                     (set-current! 'entity-type "group-interaction")
                     (entity-set-value! "duration" "int" (string->number v)) '()))))
      (build-grid-selector "gp-int-pack" "single" "Other pack"))
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 80 '1 'left 0) trans-col
      (list
       (mbutton "pf-grpint-done" "Done"
                (lambda ()
                  (set-current! 'entity-type "group-interaction")
                  (entity-set-value! "id-pack" "varchar" (ktv-get (get-current 'pack ()) "unique_id"))
                  (entity-record-values!)
                  (list (replace-fragment (get-id "event-holder") "events"))))
       (mbutton "pf-grpint-cancel" "Cancel"
                (lambda ()
                  (list (replace-fragment (get-id "event-holder") "events"))))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "group-interaction" '())
     (append
      (force-pause)
      (list
       (populate-grid-selector
        "gp-int-pack" "single"
        (db-mongoose-packs) #f
        (lambda (pack)
          (set-current! 'entity-type "group-interaction")
          (entity-set-value! "id-other-pack" "varchar" (ktv-get pack "unique_id"))
          (list)))
       (populate-grid-selector
        "gp-int-leader" "single"
        (db-mongooses-by-pack) #t
        (lambda (individual)
          (set-current! 'entity-type "group-interaction")
          (entity-set-value! "id-leader" "varchar" (ktv-get individual "unique_id"))
          (list)))
       )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-ev-grpalarm
  (fragment
   "ev-grpalarm"
   (linear-layout
    (make-id "") 'vertical fillwrap gp-col
    (list
     (mtitle "title" "Event: Group alarm")
     (build-grid-selector "gp-alarm-caller" "single" "Alarm caller")

     (linear-layout
      (make-id "") 'horizontal fillwrap trans-col
      (list
       (vert
        (mtext "text" "Cause")
        (mspinner "gp-alarm-cause" list-alarm-cause
                  (lambda (v)
                    (set-current! 'entity-type "group-alarm")
                    (entity-set-value! "cause" "varchar" (spinner-choice list-alarm-cause v)) '())))

       (tri-state "group-alarm" "gp-alarm-join" "Did the others join in?" "others-join")))

     (horiz
      (mbutton "pf-grpalarm-done" "Done"
               (lambda ()
                 (set-current! 'entity-type "group-alarm")
                 (entity-set-value! "id-pack" "varchar" (ktv-get (get-current 'pack ()) "unique_id"))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-grpalarm-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "group-alarm" '())
     (append
      (force-pause)
      (list
       (populate-grid-selector
        "gp-alarm-caller" "single"
        (db-mongooses-by-pack) #t
        (lambda (individual)
          (set-current! 'entity-type "group-alarm")
          (entity-set-value! "id-caller" "varchar" (ktv-get individual "unique_id"))
          (list))))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-ev-grpmov
  (fragment
   "ev-grpmov"
   (linear-layout
    (make-id "") 'vertical fillwrap gp-col
    (list
     (build-grid-selector "gp-mov-leader" "single" "<b>Group movement</b>: Leader")
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content -1 'left 0) trans-col
      (list
       (medit-text "gp-mov-w" "Pack width (m)" "numeric"
                   (lambda (v)
                     (set-current! 'entity-type "group-move")
                     (entity-set-value! "pack-width" "int" (string->number v)) '()))
       (medit-text "gp-mov-l" "Pack depth (m)" "numeric"
                   (lambda (v)
                     (set-current! 'entity-type "group-move")
                     (entity-set-value! "pack-depth" "int" (string->number v)) '()))
       (medit-text "gp-mov-c" "How many?" "numeric"
                   (lambda (v)
                     (set-current! 'entity-type "group-move")
                     (entity-set-value! "pack-count" "int" (string->number v)) '()))))
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content -1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Direction")
        (mspinner "gp-mov-dir" list-move-direction
                  (lambda (v)
                    (set-current! 'entity-type "group-move")
                    (entity-set-value! "direction" "varchar" (spinner-choice list-move-direction v))  '())))

       (vert
        (mtext "" "Where to")
        (mspinner "gp-mov-to" list-move-to
                  (lambda (v)
                    (set-current! 'entity-type "group-move")
                    (entity-set-value! "destination" "varchar" (spinner-choice list-move-to v))  '())))))

     (horiz
      (mbutton "pf-grpmov-done" "Done"
               (lambda ()
                 (set-current! 'entity-type "group-move")
                 (entity-set-value! "id-pack" "varchar" (ktv-get (get-current 'pack ()) "unique_id"))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-grpalarm-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "group-move" '())
     (append
      (force-pause)
      (list
       (populate-grid-selector
        "gp-mov-leader" "single"
        (db-mongooses-by-pack) #t
        (lambda (individual)
          (set-current! 'entity-type "group-move")
          (entity-set-value! "id-leader" "varchar" (ktv-get individual "unique_id"))
          (list)))
       )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-note
  (fragment
   "note"
   (linear-layout
    (make-id "") 'vertical fillwrap gp-col
    (list
     (mtitle "title" "Make a note")
     (horiz
      (mbutton "note-done" "Done"
               (lambda ()
                 (set-current! 'entity-type "note")
                 ;; parent is the current pup focal or group comp, or nothing
                 (let ((parent (get-current 'parent-id #f)))
                   (if parent
                       (entity-set-value! "parent" "varchar" parent)
                       (entity-set-value! "parent" "varchar" "not-set")))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "note-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))
     (edit-text (make-id "note-text") "" 30 "text" fillwrap
                (lambda (v)
                  (set-current! 'entity-type "note")
                  (entity-set-value! "text" "varchar" v)
                  '()))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init!  db "stream" "note" '())
     (append
      (force-pause)
      (list
       (update-widget 'edit-text (get-id "note-text") 'request-focus 1))))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))
