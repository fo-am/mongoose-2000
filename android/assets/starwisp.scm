;; Starwisp Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fragments

(define-fragment-list

  (fragment
   "pf-timer"
   (linear-layout
    (make-id "") 'vertical fillwrap trans-col
    (list
     (mtitle "pf-details" "Pack: xxx Pup: xxx")))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (update-widget 'text-view (get-id "pf-details") 'text
                     (string-append
                      "Pack: " (ktv-get (get-current 'pack '()) "name") " "
                      "Pup: " (ktv-get (get-current 'individual '()) "name"))
                     )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "events"
   (linear-layout
    0 'vertical fillwrap trans-col
    (list
     (linear-layout
      (make-id "ev-pf") 'vertical fill pf-col
      (list
       (mtitle "ev-pf-text" "Pup Focal Events")
       (horiz
        (mbutton2 "evb-pupfeed" "Pup Feed" (lambda () (list (replace-fragment (get-id "event-holder") "ev-pupfeed"))))
        (mbutton2 "evb-pupfind" "Pup Find" (lambda () (list (replace-fragment (get-id "event-holder") "ev-pupfind"))))
        (mbutton2 "evb-pupcare" "Pup Care" (lambda () (list (replace-fragment (get-id "event-holder") "ev-pupcare"))))
        (mbutton2 "evb-pupagg" "Pup Aggression" (lambda () (list (replace-fragment (get-id "event-holder") "ev-pupaggr")))))))
     (linear-layout
      (make-id "ev-pf") 'vertical fill gp-col
      (list
       (mtitle "text" "Group Events")
       (horiz
        (mbutton2 "evb-grpint" "Interaction" (lambda () (list (replace-fragment (get-id "event-holder") "ev-grpint"))))
        (mbutton2 "evb-grpalarm" "Alarm" (lambda () (list (replace-fragment (get-id "event-holder") "ev-grpalarm"))))
        (mbutton2 "evb-grpmov" "Movement" (lambda () (list (replace-fragment (get-id "event-holder") "ev-grpmov"))))
        (mbutton2 "evb-grpnote" "Note" (lambda () (list (replace-fragment (get-id "event-holder") "note")))))))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (if (equal? (get-current 'observation "none") obs-pf)
         (list
          (update-widget 'text-view (get-id "ev-pf-text") 'show 0)
          (update-widget 'linear-layout (get-id "ev-pf") 'show 0))
         (list
          (update-widget 'text-view (get-id "ev-pf-text") 'hide 0)
          (update-widget 'linear-layout (get-id "ev-pf") 'hide 0))))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "pf-scan1"
   (linear-layout
    (make-id "") 'vertical fillwrap pf-col
    (list
     (build-grid-selector "pf-scan-nearest" "single" "<b>Nearest Neighbour Scan</b>: Closest Mongoose")
     (build-grid-selector "pf-scan-close" "toggle" "Mongooses within 2m")
     (mbutton "pf-scan-done" "Done"
              (lambda ()
                (set-current! 'entity-type "pup-focal-nearest")
                (entity-set-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                (entity-record-values!)
                (list (replace-fragment (get-id "pf-top") "pf-timer"))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pup-focal-nearest" '())
     (entity-set-value! "scan-time" "varchar" (date-time->string (date-time)))
     (list
      (play-sound "ping")
      (vibrate 300)
      (populate-grid-selector
       "pf-scan-nearest" "single"
       (db-mongooses-by-pack-adults) #t
       (lambda (individual)
         (set-current! 'entity-type "pup-focal-nearest")
         (entity-set-value! "id-nearest" "varchar" (ktv-get individual "unique_id"))
         (list)))
      (populate-grid-selector
       "pf-scan-close" "toggle"
       (db-mongooses-by-pack-adults) #t
       (lambda (individuals)
         (set-current! 'entity-type "pup-focal-nearest")
         (entity-set-value! "id-list-close" "varchar" (assemble-array individuals))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "ev-pupfeed"
   (linear-layout
    (make-id "") 'vertical fillwrap pf-col
    (list
     (mtitle "title" "Event: Pup is fed")
     (build-grid-selector "pf-pupfeed-who" "single" "Who fed the pup?")
     (spacer 20)
     (horiz
      (mtext "text" "Food size")
      (mspinner "pf-pupfeed-size" list-sizes
                (lambda (v)
                  (set-current! 'entity-type "pup-focal-pupfeed")
                  (entity-set-value! "size" "varchar" (spinner-choice list-sizes v)) '())))
     (spacer 20)
     (horiz
      (mbutton "pf-pupfeed-done" "Done"
               (lambda ()
                 (set-current! 'entity-type "pup-focal-pupfeed")
                 (entity-set-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupfeed-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init!  db "stream" "pup-focal-pupfeed" '())
     (list
      (populate-grid-selector
       "pf-pupfeed-who" "single"
       (db-mongooses-by-pack-adults) #t
       (lambda (individual)
         (set-current! 'entity-type "pup-focal-pupfeed")
         (entity-set-value! "id-who" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "ev-pupfind"
   (linear-layout
    (make-id "") 'vertical fillwrap pf-col
    (list
     (mtitle "title" "Event: Pup found food")
     (horiz
      (mtext "text" "Food size")
      (mspinner "pf-pupfind-size" list-sizes
                (lambda (v)
                  (set-current! 'entity-type "pup-focal-pupfind")
                  (entity-set-value! "size" "varchar" (spinner-choice list-sizes v)) '())))
     (spacer 20)
     (horiz
      (mbutton "pf-pupfind-done" "Done"
               (lambda ()
                 (set-current! 'entity-type "pup-focal-pupfind")
                 (entity-set-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupfind-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pup-focal-pupfind" '())
     (list
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "ev-pupcare"
   (linear-layout
    (make-id "") 'vertical fillwrap pf-col
    (list
     (mtitle "title" "Event: Pup is cared for")
     (build-grid-selector "pf-pupcare-who" "single" "Who cared for the pup?")
     (spacer 20)
     (horiz
      (mtext "text" "Type of care")
      (mspinner "pf-pupcare-type" list-pupcare-type
               (lambda (v)
                 (set-current! 'entity-type "pup-focal-pupcare")
                 (entity-set-value! "type" "varchar" (spinner-choice list-pupcare-type v)) '())))
     (spacer 20)
     (horiz
      (mbutton "pf-pupcare-done" "Done"
               (lambda ()
                 (set-current! 'entity-type "pup-focal-pupcare")
                 (entity-set-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupcare-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pup-focal-pupcare" '())
     (list
      (populate-grid-selector
       "pf-pupcare-who" "single"
       (db-mongooses-by-pack-adults) #t
       (lambda (individual)
         (set-current! 'entity-type "pup-focal-pupcare")
         (entity-set-value! "id-who" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "ev-pupaggr"
   (linear-layout
    (make-id "") 'vertical fillwrap pf-col
    (list
     (mtitle "title" "Event: Pup aggression")
     (build-grid-selector "pf-pupaggr-partner" "single" "Other aggressive mongoose")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Fighting over")
        (mspinner "pf-pupaggr-over" list-aggression-over
                  (lambda (v)
                    (set-current! 'entity-type "pup-focal-pupaggr")
                    (entity-set-value! "over" "varchar" (spinner-choice list-aggression-over v)) '())))
       (vert
        (mtext "" "Level")
        (mspinner "pf-pupaggr-level" list-aggression-level
                  (lambda (v)
                    (set-current! 'entity-type "pup-focal-pupaggr")
                    (entity-set-value! "level" "varchar" (spinner-choice list-aggression-level v)) '())))

       (tri-state "pup-focal-pupaggr" "pf-pupaggr-in" "Initiate?" "initiate")

       ;(mtoggle-button "pf-pupaggr-in" "Initiate?"
       ;                (lambda (v)
       ;                  (entity-set-value! "initiate" "varchar" (if v "yes" "no")) '()))


       (tri-state "pup-focal-pupaggr" "pf-pupaggr-win" "Win?" "win")))

     (spacer 10)
     (horiz
      (mbutton "pf-pupaggr-done" "Done"
               (lambda ()
                 (set-current! 'entity-type "pup-focal-pupaggr")
                 (entity-set-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupaggr-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pup-focal-pupaggr" '())
     (list
      (populate-grid-selector
       "pf-pupaggr-partner" "single"
       (db-mongooses-by-pack) #t
       (lambda (individual)
         (set-current! 'entity-type "pup-focal-pupaggr")
         (entity-set-value! "id-with" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (lambda (fragment) '()))


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
   (lambda (fragment) '()))

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
   (lambda (fragment) '()))

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
   (lambda (fragment) '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;(replace-fragment (get-id "gc-top") (cadr frag))))))))

  (fragment
   "gc-start"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "title" "Start")
     (horiz
      (mtoggle-button "gc-start-main-obs" "I'm the main observer"
                      (lambda (v)
                        (set-current! 'entity-type "group-comp")
                        (entity-update-single-value!
                         (ktv "main-observer" "varchar" v)) '()))
      (vert
       (mtext "" "Code")
       (edit-text (make-id "gc-start-code") "" 30 "numeric" fillwrap
                  (lambda (v)
                    (set-current! 'entity-type "group-comp")
                    (entity-update-values!
                     (ktv "group-comp-code" "varchar" v)) '()))))

     (mtitle "title" "Weights and Group Composition")
     (build-grid-selector "gc-weigh-choose" "single" "Choose mongoose")
     (spacer 20)
     (horiz
      (edit-text (make-id "gc-weigh-weight") "" 30 "numeric" fillwrap
                 (lambda (v)
                   (set-current! 'entity-type "group-comp-weight")
                   (entity-update-single-value! (ktv "weight" "real" (string->number v)))
                   '()))
      (mtoggle-button "gc-weigh-accurate" "Accurate?"
                      (lambda (v)
                        (set-current! 'entity-type "group-comp-weight")
                        (entity-update-single-value! (ktv "accurate" "int" (if v 1 0)))
                        '()))
      (mtoggle-button "gc-weigh-present" "Present but not weighed"
                      (lambda (v)
                        (set-current! 'entity-type "group-comp-weight")
                        (entity-update-single-value! (ktv "present" "int" (if v 1 0)))
                        '()))
      )


     (next-button "gc-start-" "Go to pregnant females, have you finished here?" "gc-start" "gc-preg"
                  (lambda ()
                    (set-current! 'entity-type "group-comp")
                    (entity-update-values!)

                    ;; reset main entity
                    (entity-init! db "stream" "group-comp"
                                  (get-entity-by-unique db "stream" (get-current 'group-composition-id #f)))

                    (entity-set-value!
                     "present" "varchar"
                     (dbg (assemble-array-with-ids
                           (foldl
                            (lambda (m r)
                              (if (or (> (ktv-get m "weight") 0)
                                      (not (eqv? (ktv-get m "present") 0)))
                                  (cons (ktv-get m "id-mongoose") r) r))
                            '()
                            (db-filter
                             db "stream" "group-comp-weight"
                             (list (list "parent" "varchar" "=" (get-current 'group-composition-id #f)))))
                           )))

                    (set-current!
                     'gc-not-present
                     (invert-mongoose-selection
                      (string-split-simple
                       (entity-get-value "present") #\,)))

                    '()))
     ))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     ;; in case we come back from weights...
     (msg "frag start:" (get-current 'group-composition-id #f))
     (entity-init! db "stream" "group-comp"
                   (get-entity-by-unique db "stream" (get-current 'group-composition-id #f)))

     (append
      (list
       (update-widget 'edit-text (get-id "gc-start-code") 'text
                      (entity-get-value "group-comp-code"))
       (update-widget 'toggle-button (get-id "gc-start-main-obs") 'checked
                      (entity-get-value "main-observer"))

       (populate-grid-selector
        "gc-weigh-choose" "single"
        (db-mongooses-by-pack) #f
        (lambda (individual)
          ;; search for a weight for this individual...
          (let ((s (db-filter
                    db "stream" "group-comp-weight"
                    (list (list "parent" "varchar" "=" (get-current 'group-composition-id 0))
                          (list "id-mongoose" "varchar" "=" (ktv-get individual "unique_id"))))))
            (if (null? s)
                ;; not there, make a new one
                (let* ((collar-weight (ktv-get individual "collar-weight")))
                  (entity-init&save! db "stream" "group-comp-weight"
                                     (list
                                      (ktv "name" "varchar" "")
                                      (ktv "weight" "real" 0)
                                      (ktv "accurate" "int" 0)
                                      (ktv "present" "int" 0)
                                      (ktv "parent" "varchar" (get-current 'group-composition-id 0))
                                      (ktv "cur-collar-weight" "real" (if (number? collar-weight) collar-weight 0))
                                      (ktv "id-mongoose" "varchar" (ktv-get individual "unique_id")))))
                (entity-init! db "stream" "group-comp-weight" (car s)))
            (append
             (list
              (update-widget 'edit-text (get-id "gc-weigh-weight") 'text
                             (if (null? s) "" (ktv-get (car s) "weight")))
              (update-widget 'toggle-button (get-id "gc-weigh-accurate") 'checked
                             (if (null? s) 0 (ktv-get (car s) "accurate")))
              (update-widget 'toggle-button (get-id "gc-weigh-present") 'checked
                             (if (null? s) 0 (ktv-get (car s) "present"))))
             (update-selector-colours2-or "gc-weigh-choose" "group-comp-weight"
                                          (list
                                           (list "weight" "real" "!=" 0)
                                           (list "present" "int" "!=" 0)))))))
       )
      (update-selector-colours2-or "gc-weigh-choose" "group-comp-weight"
                                   (list
                                    (list "weight" "real" "!=" 0)
                                    (list "present" "int" "!=" 0)))

      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

;  (fragment
;   "gc-weights"
;   (linear-layout
;    (make-id "") 'vertical fill gc-col
;    (list
;     (build-grid-selector "gc-start-present" "toggle" "Who's present?")
;
;     (next-button "gc-weigh-" "Go to pregnancies, have you finished here?" "gc-start" "gc-preg"
;                  (lambda ()
;                    (set-current! 'gc-not-present (invert-mongoose-selection (string-split-simple (entity-get-value "present") #\,)))
;                    '()))))
;
;   (lambda (fragment arg)
;     (activity-layout fragment))
;   (lambda (fragment arg)
;     (entity-init! db "stream" "group-comp-weight" '())
;     (append
;      (list
;
;       (populate-grid-selector
;        "gc-start-present" "toggle"
;        (db-mongooses-by-pack) #f
;        (lambda (individuals)
;          (entity-set-value! "present" "varchar" (assemble-array individuals))
;          (list))
;        ;; need to invert, but not () if there are none set yet...
;        (let ((r (get-current 'gc-not-present #f)))
;          (if (not r) '() (invert-mongoose-selection r))))
;       )
;      (update-grid-selector-checked "gc-start-present" "present")
;      ))
;   (lambda (fragment) '())
;   (lambda (fragment) '())
;   (lambda (fragment) '())
;   (lambda (fragment) '()))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  (fragment
   "gc-preg"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "title" "Pregnant females")
     (build-grid-selector "gc-preg-choose" "toggle" "Choose")
     (next-button "gc-preg-" "Going to pup associations, have you finished here?" "gc-start" "gc-pup-assoc"
                  (lambda () '()))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     ;; in case we come back from pup assoc...
     (entity-init! db "stream" "group-comp"
                   (get-entity-by-unique db "stream" (get-current 'group-composition-id #f)))

     (append
      (list
       (populate-grid-selector
        "gc-preg-choose" "toggle"
        (db-mongooses-by-pack-female) #f
        (lambda (individuals)
          (set-current! 'entity-type "group-comp")
          (entity-update-single-value! (ktv "pregnant" "varchar" (assemble-array individuals)))
          (list)))
       )
      (update-grid-selector-enabled "gc-preg-choose" (get-current 'gc-not-present '()))
      (update-grid-selector-checked "gc-preg-choose" "pregnant")))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "gc-pup-assoc"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "title" "Pup Associations")
     (build-grid-selector "gc-pup-choose" "single" "Choose pup")
     (horiz
      (vert
       (mtext "" "Strength")
       (mspinner "gc-pup-strength" list-strength
                 (lambda (v)
                   (set-current! 'entity-type "group-comp-pup-assoc")
                   (entity-update-single-value! (ktv "strength" "varchar" (spinner-choice list-strength v)))
                   '())))
      (vert
       (mtext "" "Accuracy")
       (mspinner "gc-pup-accuracy" list-strength
                 (lambda (v)
                   (set-current! 'entity-type "group-comp-pup-assoc")
                   (entity-update-single-value! (ktv "accurate" "varchar" (spinner-choice list-strength v)))
                   '()))))
     (build-grid-selector "gc-pup-escort" "toggle" "Escort")
     (next-skip-button
      "gc-pup-assoc-" "Going to oestrus, have you finished here?" "gc-preg" "gc-oestrus"
      (lambda ()
        ;; reset main entity
        (entity-init! db "stream" "group-comp"
                      (get-entity-by-unique
                       db "stream" (get-current 'group-composition-id #f)))
        '())
      (lambda ()
        ;; reset main entity
        (entity-init! db "stream" "group-comp"
                      (get-entity-by-unique
                       db "stream" (get-current 'group-composition-id #f)))
        (entity-update-single-value! (ktv "pup-assoc-skipped" "int" 1))
        '()))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "group-comp-pup-assoc" '())
     (append
      (list
       (populate-grid-selector
        "gc-pup-escort" "single"
        (db-mongooses-by-pack-adults) #t
        (lambda (escort-individual)
          ;; no pup yet...
          (list)))

       (populate-grid-selector
        "gc-pup-choose" "single"
        (db-mongooses-by-pack-pups) #f
        (lambda (pup-individual)
          (append
           (list
            (populate-grid-selector
             "gc-pup-escort" "single"
             (db-mongooses-by-pack-adults) #t
             (lambda (escort-individual)
               (msg "escort-individual clicked")
               (let ((s (db-filter
                         db "stream" "group-comp-pup-assoc"
                         (list (list "parent" "varchar" "=" (get-current 'group-composition-id 0))
                               (list "id-other" "varchar" "=" (ktv-get escort-individual "unique_id"))
                               (list "id-mongoose" "varchar" "=" (ktv-get pup-individual "unique_id"))))))
                 (if (null? s)
                     ;; not there, make a new one
                     (entity-init&save! db "stream" "group-comp-pup-assoc"
                                        (list
                                         (ktv "name" "varchar" "")
                                         (ktv "id-other" "varchar" (ktv-get escort-individual "unique_id"))
                                         (ktv "accurate" "varchar" "none")
                                         (ktv "strength" "varchar" "none")
                                         (ktv "parent" "varchar" (get-current 'group-composition-id 0))
                                         (ktv "id-mongoose" "varchar" (ktv-get pup-individual "unique_id"))))
                     (entity-init! db "stream" "group-comp-pup-assoc" (car s)))
                 (append
                  (list
                   (update-widget 'spinner (get-id "gc-pup-strength") 'selection (spinner-index list-strength (entity-get-value "strength")))
                   (update-widget 'spinner (get-id "gc-pup-accuracy") 'selection (spinner-index list-strength (entity-get-value "accurate"))))

                  (update-selector-colours2
                   "gc-pup-escort" "group-comp-pup-assoc"
                   (list
                    (list "id-mongoose" "varchar" "=" (ktv-get pup-individual "unique_id"))
                    (list "strength" "varchar" "!=" "none")
                    (list "accurate" "varchar" "!=" "none"))))

                 ))))
           (update-selector-colours2
            "gc-pup-escort" "group-comp-pup-assoc"
            (list
             (list "id-mongoose" "varchar" "=" (ktv-get pup-individual "unique_id"))
             (list "strength" "varchar" "!=" "none")
             (list "accurate" "varchar" "!=" "none")))
           (update-selector-colours3 "gc-pup-choose" "group-comp-pup-assoc")
           (update-grid-selector-enabled "gc-pup-escort" (get-current 'gc-not-present '()))
           ))))
      (update-grid-selector-enabled "gc-pup-escort" (get-current 'gc-not-present '()))
      (update-grid-selector-enabled "gc-pup-choose" (get-current 'gc-not-present '()))
      (update-selector-colours3 "gc-pup-choose" "group-comp-pup-assoc")
      ))

   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))



  (fragment
   "gc-oestrus"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "" "Oestrus")
     (build-grid-selector "gc-oestrus-female" "single" "Choose female")
     (horiz
      (vert
       (mtext "" "Strength")
       (mspinner "gc-oestrus-strength" list-strength
                 (lambda (v)
                   (set-current! 'entity-type "group-comp-mate-guard")
                   (entity-update-single-value! (ktv "strength" "varchar" (spinner-choice list-strength v)))
                   '())))
      (vert
       (mtext "" "Accuracy")
       (mspinner "gc-oestrus-accuracy" list-strength
                 (lambda (v)
                   (set-current! 'entity-type "group-comp-mate-guard")
                   (entity-update-single-value! (ktv "accurate" "varchar" (spinner-choice list-strength v)))
                   '())))

      (mtoggle-button "gc-oestrus-pester" "Pestering?"
                      (lambda (v)
                        (set-current! 'entity-type "group-comp-mate-guard")
                        (entity-update-single-value! (ktv "pester" "int" (if v 1 0)))
                        '()))

      )
     (build-grid-selector "gc-oestrus-guard" "toggle" "Choose mate guard")
     (next-skip-button
      "gc-pup-oestrus-" "Going to babysitters, have you finished here?" "gc-pup-assoc" "gc-babysitting"
      (lambda ()
        ;; reset main entity
        (entity-init! db "stream" "group-comp"
                      (get-entity-by-unique db "stream" (get-current 'group-composition-id #f)))
        '())
      (lambda ()
        ;; reset main entity
        (entity-init! db "stream" "group-comp"
                      (get-entity-by-unique
                       db "stream" (get-current 'group-composition-id #f)))
        (entity-update-single-value! (ktv "oestrus-skipped" "int" 1))
        '()))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "group-comp-mate-guard" '())
     (append
      (list
       (populate-grid-selector
        "gc-oestrus-guard" "single"
        (db-mongooses-by-pack-male) #t
        (lambda (escort-individual)
          ;; no pup yet...
          (list)))

       (populate-grid-selector
        "gc-oestrus-female" "single"
        (db-mongooses-by-pack-female) #f
        (lambda (pup-individual)
          (append
           (list
            (populate-grid-selector
             "gc-oestrus-guard" "single"
             (db-mongooses-by-pack-male) #t
             (lambda (escort-individual)
               (let ((s (db-filter
                         db "stream" "group-comp-mate-guard"
                         (list (list "parent" "varchar" "=" (get-current 'group-composition-id 0))
                               (list "id-other" "varchar" "=" (ktv-get escort-individual "unique_id"))
                               (list "id-mongoose" "varchar" "=" (ktv-get pup-individual "unique_id"))))))
                 (if (null? s)
                     ;; not there, make a new one
                     (entity-init&save! db "stream" "group-comp-mate-guard"
                                        (list
                                         (ktv "name" "varchar" "")
                                         (ktv "id-other" "varchar" (ktv-get escort-individual "unique_id"))
                                         (ktv "accurate" "varchar" "none")
                                         (ktv "strength" "varchar" "none")
                                         (ktv "pester" "int" 0)
                                         (ktv "parent" "varchar" (get-current 'group-composition-id 0))
                                         (ktv "id-mongoose" "varchar" (ktv-get pup-individual "unique_id"))))
                     (entity-init! db "stream" "group-comp-mate-guard" (car s)))
                 (append
                  (list
                   (update-widget 'spinner (get-id "gc-oestrus-strength") 'selection (spinner-index list-strength (entity-get-value "strength")))
                   (update-widget 'spinner (get-id "gc-oestrus-accuracy") 'selection (spinner-index list-strength (entity-get-value "accurate")))
                   (update-widget 'toggle-button (get-id "gc-oestrus-pester") 'checked (entity-get-value "pester")))

                  (update-selector-colours3-or
                   "gc-oestrus-guard" "group-comp-mate-guard"
                   (ktv-get pup-individual "unique_id")
                   (list
                    (list "strength" "varchar" "!=" "none")
                    (list "accurate" "varchar" "!=" "none")
                    (list "pester" "int" "!=" 0)
                    )))
                 ))))
           (update-selector-colours3-or
            "gc-oestrus-guard" "group-comp-mate-guard"
            (ktv-get pup-individual "unique_id")
            (list
             (list "strength" "varchar" "!=" "none")
             (list "accurate" "varchar" "!=" "none")
             (list "pester" "int" "!=" 0)
             ))
           (update-selector-colours3 "gc-oestrus-female" "group-comp-mate-guard")
           (update-grid-selector-enabled "gc-oestrus-guard" (get-current 'gc-not-present '()))
           ))))
      (update-grid-selector-enabled "gc-oestrus-guard" (get-current 'gc-not-present '()))
      (update-grid-selector-enabled "gc-oestrus-female" (get-current 'gc-not-present '()))
      (update-selector-colours3 "gc-oestrus-female" "group-comp-mate-guard")
      ))

   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-babysitting"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "" "Babysitters")
     (mtitle "title" "Seen")
     (build-grid-selector "gc-baby-seen" "toggle" "Choose")
     (mtitle "title" "By elimination")
     (build-grid-selector "gc-baby-byelim" "toggle" "Choose")
     (next-skip-button "gc-pup-baby-" "Ending, have you finished here?" "gc-oestrus" "gc-end"
                       (lambda () '())
                       (lambda ()
                         (entity-update-single-value!
                          (ktv "babysitting-skipped" "int" 1))
                         '()))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (append
      (list
       (populate-grid-selector
        "gc-baby-seen" "toggle"
        (db-mongooses-by-pack-adults) #f
        (lambda (individuals)
          (set-current! 'entity-type "group-comp")
          (entity-update-single-value! (ktv "baby-seen" "varchar" (assemble-array individuals)))
          (list)))
       )
      (update-grid-selector-enabled "gc-baby-seen" (get-current 'gc-not-present '()))
      (update-grid-selector-checked "gc-baby-seen" "baby-seen")
      (list
       (populate-grid-selector
        "gc-baby-byelim" "toggle"
        (db-mongooses-by-pack-adults) #f
        (lambda (individuals)
          (set-current! 'entity-type "group-comp")
          (entity-update-single-value! (ktv "baby-byelim" "varchar" (assemble-array individuals)))
          (list)))
       )
      (update-grid-selector-enabled "gc-baby-byelim"
                                    (invert-mongoose-selection
                                     (get-current 'gc-not-present '())))
      (update-grid-selector-checked "gc-baby-byelim" "baby-byelim")))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-end"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "" "Finish group composition")
     (next-button "gc-pup-baby-" "Ending, have you finished here?" "gc-babysitting" "gc-end"
                  (lambda ()
                    ;; clean up...
                    (get-current 'gc-not-present '())
                    (set-current! 'parent-id #f)
                    (set-current! 'group-composition-id #f)
                    (list (finish-activity 0))))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))



  )

(msg "one")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activities

(define-activity-list
;  (activity
;   "splash"
;   (vert
;    (text-view (make-id "splash-title") "Mongoose 2000" 40 fillwrap)
;    (mtext "splash-about" "Advanced mongoose technology")
;    (spacer 20)
;    (mbutton2 "f2" "Get started!" (lambda () (list (start-activity-goto "main" 2 ""))))
;    )
;
;   (lambda (activity arg)
;     (activity-layout activity))
;   (lambda (activity arg)
;     (list))
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity requestcode resultcode) '()))


  (activity
   "main"
   (vert
    (text-view (make-id "main-title") "Mongoose 2000" 50 fillwrap)
    (text-view (make-id "main-about") "Advanced mongoose technology" 30 fillwrap)
    (spacer 10)
    (horiz
     (mbutton2 "main-observations" "Observations" (lambda () (list (start-activity "observations" 2 ""))))
     (mbutton2 "main-manage" "Manage Packs" (lambda () (list (start-activity "manage-packs" 2 "")))))

    (image-view 0 "mongooses" fillwrap)
    (text-view (make-id "version") app-version 10 fillwrap)
    (mtext "foo" "Your ID")
    (edit-text (make-id "main-id-text") "" 30 "text" fillwrap
               (lambda (v)
                 (set-current! 'user-id v)
                 (update-entity
                  db "local" 1 (list (ktv "user-id" "varchar" v)))
                 '()))
    (mtext "foo" "Database")
    (horiz
     (mbutton2 "main-review" "Review changes" (lambda () (list (start-activity "review" 0 ""))))
     (mbutton2 "main-sync" "Sync database" (lambda () (list (start-activity "sync" 0 ""))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (msg "on-start")
     (setup-database!)
     (let ((user-id (ktv-get (get-entity db "local" 1) "user-id")))
       (set-current! 'user-id user-id)
       (msg "on-start 2")
       (list
        (gps-start "gps" (lambda (loc)
                           (set-current! 'location loc)
                           (list (toast (string-append
                                         (number->string (car loc)) ", "
                                         (number->string (cadr loc))))))
                   (* 3 60 1000) 5)
        (update-widget 'edit-text (get-id "main-id-text") 'text user-id))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "observations"
   (vert
    (text-view (make-id "title") "Start Observation" 40 fillwrap)
    (vert
     (mtext "type" "Choose observation type")
     (horiz
      (linear-layout
       0 'vertical fillwrap gc-col
       (list
        (mtoggle-button2 "choose-obs-gc" obs-gc
                         (lambda (v)
                           (set-current! 'observation obs-gc)
                           (mclear-toggles (list "choose-obs-pf" "choose-obs-gp"))))))
      (linear-layout
       0 'vertical fillwrap pf-col
       (list
        (mtoggle-button2 "choose-obs-pf" obs-pf
                         (lambda (v)
                           (set-current! 'observation obs-pf)
                           (mclear-toggles (list "choose-obs-gc" "choose-obs-gp"))))))
      (linear-layout
       0 'vertical fillwrap gp-col
       (list
        (mtoggle-button2 "choose-obs-gp" obs-gp
                         (lambda (v)
                           (set-current! 'observation obs-gp)
                           (mclear-toggles (list "choose-obs-pf" "choose-obs-gc"))))))))
    (build-grid-selector "choose-obs-pack-selector" "single" "Choose pack")

    (horiz
     (mbutton2 "choose-obs-back" "Back" (lambda () (list (finish-activity 1))))
     (mbutton2
      "choose-obs-start" "Start"
      (lambda ()
        ;; set up the observation fragments
        (let ((obs (get-current 'observation "none")))
          (when (not (equal? obs "none"))
                (set-current!
                 'observation-fragments
                 (cond
                  ((equal? obs obs-gc) gc-fragments)
                  (else '())))))

        ;; go to observation
        (if (and (current-exists? 'pack)
                 (current-exists? 'observation))
            (cond
             ((eq? (get-current 'observation "none") obs-pf)
              (list (start-activity "pup-focal-start" 2 "")))
             ((eq? (get-current 'observation "none") obs-gp)
              (list (start-activity "group-events" 2 "")))
             (else
              ;; check if there is currently a gc activity active
              (when (not (get-current 'group-composition-id #f))
                    ;; create a new gc entity
                    ;; initialise it to the current memory entity
                    (set-current!
                     'group-composition-id
                     (entity-init&save!
                      db "stream" "group-comp"
                      (list (ktv "pack" "varchar" (ktv-get (get-current 'pack ()) "unique_id"))
                            (ktv "pup-assoc-skipped" "int" 0)
                            (ktv "oestrus-skipped" "int" 0)
                            (ktv "babysitting-skipped" "int" 0)
                            (ktv "group-comp-code" "varchar" ""))))
                    (set-current! 'parent-id (get-current 'group-composition-id #f))
                    )
              (list
               (start-activity "group-composition" 2 ""))))
            (list
             (ok-dialog
              "choose-obs-finish"
              "Need to specify a pack and an observation"
              (lambda () '())))))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "choose-obs-pack-selector" "single"
       (db-mongoose-packs) #f
       (lambda (pack)
         (when (and
                (get-current 'pack #f) ;; if we have a current pack...
                (not (equal? (ktv-get pack "unique_id")
                             (ktv-get (get-current 'pack '()) "unique_id"))))
               ;; need to clear the current group comp
               ;; id here if we are changing the pack
               (set-current! 'parent-id #f)
               (set-current! 'group-composition-id #f))
         (set-current! 'pack pack)
         '()))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "group-composition"
   (linear-layout
    0 'vertical (layout 'fill-parent 'fill-parent 1 'left 0)
    gc-col
    (list
     (relative
      '(("parent-top"))
      (list 0 0 0 0)
      (horiz
       (text-view (make-id "obs-title") "" 40 fillwrap)
       (mbutton-small "gc-done" "Exit"
                      (lambda ()
                        (list
                         (alert-dialog
                          "gc-end-done"
                          "Finish group composition: are you sure?"
                          (lambda (v)
                            (cond
                             ((eqv? v 1)
                              (list (finish-activity 1)))
                             (else
                              (list))))))))))


     (build-fragment "gc-start" (make-id "gc-top") (layout 'fill-parent 'wrap-content -1 'left 0))

     (linear-layout
      0 'vertical (layout 'fill-parent 'fill-parent 1 'left 0)
      (list 0 0 0 0) (list (spacer 10)))


     (relative
      '(("parent-bottom"))
      (list 0 0 0 0)
      (build-fragment "events" (make-id "event-holder") (layout 'fill-parent 'wrap-content -1 'left 0)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (msg "creating gc activity")
     (list
      (update-widget 'text-view (get-id "obs-title") 'text
                     (string-append
                      (get-current 'observation "No observation")
                      " with pack " (ktv-get (get-current 'pack '()) "name")))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "pup-focal-start"
   (vert
    (mtitle "" "Pup focal setup")
    (mtext "pf1-pack" "Pack")
    (build-grid-selector "pf1-grid" "single" "Select pup")
    (horiz
     (medit-text "pf1-width" "Pack width - left to right (m)" "numeric"
                 (lambda (v) (entity-set-value! "pack-width" "int" v) '()))
     (medit-text "pf1-height" "Pack depth - front to back (m)" "numeric"
                 (lambda (v) (entity-set-value! "pack-depth" "int" v) '())))
    (medit-text "pf1-count" "How many mongooses can you see?" "numeric"
                (lambda (v) (entity-set-value! "pack-count" "int" v) '()))
    (horiz
     (mbutton2 "pf1-back" "Back" (lambda () (list (finish-activity 1))))
     (mbutton2 "pf1-done" "Done"
               (lambda ()
                 (cond
                  ((current-exists? 'individual)
                   (set-current! 'pup-focal-id (entity-record-values!))
                   (set-current! 'parent-id (get-current 'pup-focal-id #f))
                   (set-current! 'timer-minutes pf-length)
                   (set-current! 'timer-seconds 0)
                   (list
                    (start-activity "pup-focal" 2 "")))
                  (else
                   (list
                    (ok-dialog
                     "pup-focal-check"
                     "You need to specify an pup for the focal"
                     (lambda () '())))))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "stream" "pup-focal" '())
     (list
      (populate-grid-selector
       "pf1-grid" "single"
       (db-mongooses-by-pack-pups) #f
       (lambda (individual)
         (set-current! 'individual individual)
         (entity-set-value! "id-focal-subject" "varchar" (ktv-get individual "unique_id"))
         '()))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "pup-focal"
   (linear-layout
    0 'vertical (layout 'fill-parent 'fill-parent 1 'left 0)
    pf-col
    (list
     (relative
      '(("parent-top"))
      (list 0 0 0 0)
      (horiz
       (mtitle "title" "Pup Focal")
       (linear-layout
        0 'vertical fillwrap trans-col
        (list
         (mtext "title" "Time left:")
         (mtitle "pf-timer-time-minutes"
                 (number->string (get-current 'timer-minutes pf-length)))))
       (linear-layout
        0 'vertical fillwrap trans-col
        (list
         (mtext "title" "Next scan:")
         (mtitle "pf-timer-time"
                 (number->string (get-current 'timer-seconds 60)))))
       (mtoggle-button "pf-pause" "Pause"
                       (lambda (v)
                         (msg "pausing")
                         (if v
                             (list (delayed "timer" 1000 (lambda () '())))
                             (list (delayed "timer" 1000 timer-cb)))))
       (mbutton-small "pf-done" "Exit"
                      (lambda ()
                        (list
                         (alert-dialog
                          "pup-focal-end-done"
                          "Finish pup focal are you sure?"
                          (lambda (v)
                            (cond
                             ((eqv? v 1)
                              (set-current! 'parent-id #f)
                              (set-current! 'pup-focal-id #f)
                              (list (finish-activity 1)))
                             (else
                              (list))))))))))

    (build-fragment "pf-timer" (make-id "pf-top") (layout 'fill-parent 'wrap-content -1 'left 0))

    (linear-layout
     0 'vertical (layout 'fill-parent 'fill-parent 1 'left 0)
     (list 0 0 0 0) (list (spacer  10)))

    (relative
     '(("parent-bottom"))
     (list 0 0 0 0)
     (build-fragment "events" (make-id "event-holder") (layout 'fill-parent 'wrap-content 1 'left 0)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'text-view (get-id "pf-timer-time-minutes") 'text
                     (number->string (get-current 'timer-minutes pf-length)))
      (update-widget 'text-view (get-id "pf-timer-time") 'text
                     (number->string (get-current 'timer-seconds 60)))
      (delayed "timer" 1000 timer-cb)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) (list (delayed "timer" 1000 (lambda () '()))))
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "group-events"
   (vert
    (build-fragment "events" (make-id "event-holder") (layout 'fill-parent 'wrap-content 1 'left 0))
    (horiz
     (mbutton "gpe-done" "Done" (lambda () (list (finish-activity 0))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (activity
   "manage-packs"
   (vert
    (text-view (make-id "title") "Manage packs" 40 fillwrap)
    (build-grid-selector "manage-packs-list" "button" "Choose pack")
    (horiz
     (mbutton2 "choose-obs-back" "Back" (lambda () (list (finish-activity 1))))
     (mbutton2 "manage-packs-new" "New pack" (lambda () (list (start-activity "new-pack" 2 "")))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "manage-packs-list" "button" (db-mongoose-packs) #f
       (lambda (pack)
         (set-current! 'pack pack)
         (list (start-activity "manage-individual" 2 ""))))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "new-pack"
   (vert
    (text-view (make-id "title") "New pack" 40 fillwrap)
    (spacer 10)
    (text-view (make-id "new-pack-name-text") "Pack name" 30 fillwrap)
    (edit-text (make-id "new-pack-name") "" 30 "text" fillwrap
               (lambda (v) (entity-set-value! "name" "varchar" v) '()))
    (spacer 10)
    (horiz
     (mbutton2 "new-pack-cancel" "Cancel"
               (lambda ()
                 (list (finish-activity 2))))
     (mbutton2 "new-pack-done" "Done"
               (lambda ()
                 (entity-record-values!)
                 (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "sync" "pack" '())
     (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "manage-individual"
   (vert
    (text-view (make-id "title") "Manage individuals" 40 fillwrap)
    (text-view (make-id "manage-individual-pack-name") "Pack:" 30 fillwrap)
    (build-grid-selector "manage-individuals-list" "button" "Choose individual")
    (horiz
     (mbutton2 "choose-obs-back" "Back" (lambda () (list (finish-activity 1))))
     (mbutton2 "manage-individuals-new" "New individual" (lambda () (list (start-activity "new-individual" 2 ""))))
     (mbutton2 "manage-individuals-delete" "Delete pack"
               (lambda ()
                 (list
                  (alert-dialog
                   "delete-pack-dialog"
                   "Delete this pack: are you sure?"
                   (lambda (v)
                     (cond
                      ((eqv? v 1)
                       (list
                        (alert-dialog
                         "delete-really-pack-dialog"
                         "Really delete this pack: are you absolutely sure?"
                         (lambda (v)
                           (cond
                            ((eqv? v 1)
                             (entity-update-single-value! (ktv "deleted" "int" 1))
                             (list (finish-activity 1)))
                            (else
                             (list)))))))
                      (else (list))))))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "sync" "pack" (get-current 'pack #f))
     (list
      (populate-grid-selector
       "manage-individuals-list" "button"
       (db-mongooses-by-pack) #f
       (lambda (individual)
         (set-current! 'individual individual)
         (list (start-activity "update-individual" 2 ""))))
      (update-widget 'text-view (get-id "manage-individual-pack-name") 'text
                     (string-append "Pack: " (ktv-get (get-current 'pack '()) "name")))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "new-individual"
   (vert
    (text-view (make-id "title") "New Mongoose" 40 fillwrap)
    (text-view (make-id "new-individual-pack-name") "Pack:" 30 fillwrap)
    (text-view (make-id "new-individual-name-text") "Name" 30 fillwrap)
    (edit-text (make-id "new-individual-name") "" 30 "text" fillwrap
               (lambda (v) (entity-set-value! "name" "varchar" v) '()))
    (text-view (make-id "new-individual-name-text") "Gender" 30 fillwrap)
    (mspinner "new-individual-gender" list-gender
             (lambda (v) (entity-set-value! "gender" "varchar" (spinner-choice list-gender v)) '()))
    (text-view (make-id "new-individual-dob-text") "Date of Birth" 30 fillwrap)
    (horiz
     (text-view (make-id "new-individual-dob") (date->string (date-time)) 25 fillwrap)
     (button (make-id "date") "Set date" 30 fillwrap
             (lambda ()
               (list (date-picker-dialog
                      "new-individual-date"
                      (lambda (day month year)
                        (let ((datestring (date->string (list year (+ month 1) day))))
                          (entity-set-value! "dob" "varchar" datestring)
                          (list
                           (update-widget
                            'text-view
                            (get-id "new-individual-dob") 'text datestring))))))))
     (button (make-id "unknown-date") "Unknown" 30 fillwrap
             (lambda ()
               (entity-set-value! "dob" "varchar" "Unknown")
               (list (update-widget 'text-view (get-id "new-individual-dob") 'text "Unknown"))))
     )
    (text-view (make-id "new-individual-litter-text") "Litter code" 30 fillwrap)
    (edit-text (make-id "new-individual-litter-code") "" 30 "text" fillwrap
               (lambda (v) (entity-set-value! "litter-code" "varchar" v) '()))
    (text-view (make-id "new-individual-chip-text") "Chip code" 30 fillwrap)
    (edit-text (make-id "new-individual-chip-code") "" 30 "text" fillwrap
               (lambda (v) (entity-set-value! "chip-code" "varchar" v) '()))
    (text-view (make-id "new-individual-collar-text") "Collar weight" 30 fillwrap)
    (edit-text (make-id "new-individual-collar-weight") "" 30 "numeric" fillwrap
               (lambda (v) (entity-set-value! "collar-weight" "real" (string->number v)) '()))
    (horiz
     (mbutton2 "new-individual-cancel" "Cancel"
               (lambda () (list (finish-activity 2))))
     (mbutton2 "new-individual-done" "Done"
               (lambda ()
                 (entity-set-value! "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
                 (entity-record-values!)
                 (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "sync" "mongoose" '())
     ;; make sure all fields exist
     (entity-set-value! "name" "varchar" "noname")
     (entity-set-value! "gender" "varchar" "female")
     (entity-set-value! "dob" "varchar" "00-00-00")
     (entity-set-value! "litter-code" "varchar" "")
     (entity-set-value! "chip-code" "varchar" "")
     (entity-set-value! "collar-weight" "real" "")
     (list
      (update-widget 'text-view (get-id "new-individual-pack-name") 'text
                     (string-append "Pack: " (ktv-get (get-current 'pack '()) "name")))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "update-individual"
   (vert
    (text-view (make-id "title") "Update Mongoose" 40 fillwrap)
    (spacer 10)
    (text-view (make-id "update-individual-name-text") "Name" 30 fillwrap)
    (edit-text (make-id "update-individual-name") "" 30 "text" fillwrap
               (lambda (v) (entity-set-value! "name" "varchar" v) '()))
    (text-view (make-id "update-individual-name-text") "Gender" 30 fillwrap)
    (mspinner "update-individual-gender" list-gender
              (lambda (v) (entity-set-value! "gender" "varchar" (spinner-choice list-gender v)) '()))
    (text-view (make-id "update-individual-dob-text") "Date of Birth" 30 fillwrap)
    (horiz
     (text-view (make-id "update-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 30 fillwrap
             (lambda ()
               (list (date-picker-dialog
                      "update-individual-date"
                      (lambda (day month year)
                        (let ((datestring (date->string (list year (+ month 1) day))))
                          (entity-set-value! "dob" "varchar" datestring)
                          (list
                           (update-widget
                            'text-view
                            (get-id "update-individual-dob") 'text datestring))))))))
     (button (make-id "update-unknown-date") "Unknown" 30 fillwrap
             (lambda ()
               (entity-set-value! "dob" "varchar" "Unknown")
               (list (update-widget 'text-view (get-id "update-individual-dob") 'text "Unknown"))))
     )

    (text-view (make-id "update-individual-litter-text") "Litter code" 30 fillwrap)
    (edit-text (make-id "update-individual-litter-code") "" 30 "text" fillwrap
               (lambda (v) (entity-set-value! "litter-code" "varchar" v) '()))
    (text-view (make-id "update-individual-chip-text") "Chip code" 30 fillwrap)
    (edit-text (make-id "update-individual-chip-code") "" 30 "text" fillwrap
               (lambda (v) (entity-set-value! "chip-code" "varchar" v) '()))
    (text-view (make-id "update-individual-collar-text") "Collar weight" 30 fillwrap)
    (edit-text (make-id "update-individual-collar-weight") "" 30 "numeric" fillwrap
               (lambda (v) (entity-set-value! "collar-weight" "real" (string->number v)) '()))
    (spacer 10)
    (horiz
     (mtoggle-button2 "update-individual-delete" "Delete"
                      (lambda (v)
                        (entity-set-value! "deleted" "int" (if v 1 0))
                        (list)))
     (mtoggle-button2 "update-individual-died" "Died"
                      (lambda (v)
                        (entity-set-value! "deleted" "int" (if v 2 0))
                        (list))))
    (horiz
     (mbutton2 "update-individual-cancel" "Cancel"
               (lambda () (list (finish-activity 2))))
     (mbutton2 "update-individual-done" "Done"
               (lambda ()
                 (entity-update-values!)
                 (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "sync" "individual" (get-current 'individual #f))
     (let ((individual (get-current 'individual '())))
       (msg "deleted = " (ktv-get individual "deleted"))
       (list
        (update-widget 'edit-text (get-id "update-individual-name") 'text
                       (ktv-get individual "name"))
        (update-widget 'text-view (get-id "update-individual-dob") 'text
                       (ktv-get individual "dob"))
        (update-widget 'spinner (get-id "update-individual-gender") 'selection
                       (spinner-index list-gender (ktv-get individual "gender")))
        (update-widget 'edit-text (get-id "update-individual-litter-code") 'text
                       (ktv-get individual "litter-code"))
        (update-widget 'edit-text (get-id "update-individual-chip-code") 'text
                       (ktv-get individual "chip-code"))
        (update-widget 'edit-text (get-id "update-individual-collar-weight") 'text
                       (let ((v (ktv-get individual "collar-weight"))) (if v v 0)))

        (update-widget 'toggle-button (get-id "update-individual-delete") 'checked
                       (if (eqv? (ktv-get individual "deleted") 1) 1 0))
        (update-widget 'toggle-button (get-id "update-individual-died") 'checked
                       (if (eqv? (ktv-get individual "deleted") 2) 1 0))
        )))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "tag-location"
   (vert
    (text-view (make-id "title") "Tag Location" 40 fillwrap)
    (text-view (make-id "tag-location-gps-text") "GPS" 30 fillwrap)
    (horiz
     (text-view (make-id "tag-location-gps-lat") "LAT" 30 fillwrap)
     (text-view (make-id "tag-location-gps-lng") "LNG" 30 fillwrap))

    (text-view (make-id "tag-location-name-text") "Name" 30 fillwrap)
    (edit-text (make-id "tag-location-name") "" 30 "text" fillwrap (lambda (v) '()))

    (text-view (make-id "tag-location-pack-text") "Associated pack" 30 fillwrap)
    (spinner (make-id "tag-location-pack") (list "Pack 1" "Pack 2") fillwrap (lambda (v) '()))

    (text-view (make-id "tag-location-radius-text") "Approx radius of area" 30 fillwrap)
    (seek-bar (make-id "tag-location-radius") 100 fillwrap (lambda (v) '()))
    (text-view (make-id "tag-location-radius-value") "10m" 30 fillwrap)

    (horiz
     (button (make-id "tag-location-cancel") "Cancel" 30 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "tag-location-done") "Done" 30 fillwrap (lambda () (list (finish-activity 2)))))

    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "sync"
   (vert
    (text-view (make-id "sync-title") "Sync database" 40 fillwrap)
    (mtext "sync-dirty" "...")
    (mtoggle-button2 "sync-all" "Sync me" (lambda (v) (set-current! 'sync-on v) '()))
    (mtitle "" "Export data")
    (horiz
     (mbutton2 "sync-download" "Download"
               (lambda ()
                 (debug! (string-append "Downloading whole db"))
                 (append
                  (foldl
                   (lambda (e r)
                     (debug! (string-append "Downloading /sdcard/mongoose/" e ".csv"))
                     (append r
                             (list
                              (http-download
                               (string-append "getting-" e)
                               (string-append url "fn=entity-csv&table=stream&type=" e)
                               (string-append "/sdcard/mongoose/" e ".csv"))
                              )))
                  (list
                   ;(http-download
                   ; "getting-db"
                   ; "http://192.168.2.1:8888/mongoose.db"
                   ; (string-append "/sdcard/mongoose/mongoose.db"))
                   ;; save paranoid backup
                   (http-download
                    "getting-backup-db"
                    "http://192.168.2.1:8888/mongoose.db"
                    (string-append "/sdcard/mongoose/backup/mongoose-"
                                   (date-time->string (date-time)) ".db"))
                  ; (http-download
                  ;  "getting-log"
                  ;  "http://192.168.2.1:8888/log.txt"
                  ;  (string-append "/sdcard/mongoose/server-log.txt"))

                   )
                  entity-types)
                  (list))))
     (mbutton2 "sync-export" "Email data"
               (lambda ()
                 (debug! "Sending mail")
                 (list
                  (send-mail
                   ""
                   "From Mongoose2000" "Please find attached your mongoose data"
                   (append
                    (list
                     "/sdcard/mongoose/local-mongoose.db"
                     "/sdcard/mongoose/mongoose.db"
                     "/sdcard/mongoose/server-log.txt")
                    (map
                     (lambda (e)
                       (string-append "/sdcard/mongoose/" e ".csv"))
                     entity-types))))))
     )
    (spacer 10)
    (mtitle "" "Debug")
    (scroll-view-vert
     0 (layout 'fill-parent 200 1 'left 0)
     (list
      (vert
       (debug-text-view (make-id "sync-debug") "..." 15 (layout 'fill-parent 400 1 'left 0)))))
    (spacer 10)
    (mbutton2 "sync-back" "Back" (lambda () (list (finish-activity 1))))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'sync-on #f)
     (append
      (debug-timer-cb)
      (list
       (update-widget 'debug-text-view (get-id "sync-debug") 'text (get-current 'debug-text ""))
       (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty db))
       )))
   (lambda (activity) '())
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "review"
   (vert
    (text-view 0 "Review changes" 40 fillwrap)
    (scroll-view-vert
     0 (layout 'fill-parent 'fill-parent 1 'left 0)
     (list
      (linear-layout
       (make-id "review-list")
       'vertical
       (layout 'fill-parent 'fill-parent 1 'left 0)
       (list 0 0 0 0)
       (list))
      ))
    (mbutton2 "review-back" "Back" (lambda () (list (finish-activity 1)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (review-update-list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "review-collection"
   (vert
    (text-view 0 "Review changes" 40 fillwrap)
    (scroll-view-vert
     0 (layout 'fill-parent 'fill-parent 1 'left 0)
     (list
      (linear-layout
       (make-id "review-list")
       'vertical
       (layout 'fill-parent 'fill-parent 1 'left 0)
       (list 0 0 0 0)
       (list))
      ))
    (mbutton2 "review-back" "Back" (lambda () (list (finish-activity 1)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (review-update-collection (get-current 'review-collection #f)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "review-item"
   (vert
    (text-view (make-id "title") "Review item" 40 fillwrap)
    (linear-layout
     (make-id "review-item-container")
     'vertical
     (layout 'fill-parent 'wrap-content 1 'left 0)
     (list 0 0 0 0)
     (list))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (review-item-build))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  )
