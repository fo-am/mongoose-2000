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

  frag-events

  frag-pf-timer
  frag-pf-scan1
  frag-ev-pupfeed
  frag-ev-pupfind
  frag-ev-pupcare
  frag-ev-pupaggr

  frag-of-timer
  frag-of-scan1
  frag-ev-oesaggr
  frag-ev-oesaffil
  frag-ev-oesmate
  frag-ev-oesmaleaggr

  frag-prf-timer
  frag-prf-scan1
  frag-ev-pregaggr
  frag-ev-pregaffil

  frag-ev-grpint
  frag-ev-grpalarm
  frag-ev-grpmov
  frag-note

  frag-gc-start
  frag-gc-preg
  frag-gc-pup-assoc
  frag-gc-oestrus
  frag-gc-babysitting
  frag-gc-end
  )

(define (clear-obs-toggles not-this)
  (mclear-toggles
   (filter (lambda (a) (not (equal? a not-this)))
           (list "choose-obs-pf" "choose-obs-gc"
                 "choose-obs-gp" "choose-obs-of" "choose-obs-prf"))))

(define (is-observation? obs)
  (equal? (get-current 'observation "none") obs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activities

(define-activity-list
  (activity
   "main"
   (vert
    (text-view (make-id "main-title") "Mongoose 2000" 50 fillwrap)
    (text-view (make-id "main-about") "Advanced mongoose technology" 30 fillwrap)
    (spacer 10)
    (text-view (make-id "gps-state") "Waiting for GPS..." 30 fillwrap)
    (horiz
     (mbutton2 "main-observations" "Observations" (lambda () (list (start-activity "observations" 2 ""))))
     (mbutton2 "main-manage" "Manage Packs" (lambda () (list (start-activity "manage-packs" 2 "")))))

    (image-view 0 "mongooses" fillwrap)
    (text-view (make-id "version")
               (string-append "Version " (number->string app-version)) 20 fillwrap)
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
        (gps-start
         "gps" (lambda (loc)
                 (set-current! 'location loc)
                 (let ((focal-id (if (get-current 'focal-id #f)
                                     (get-current 'focal-id "none")
                                     "none")))
                   ;; append to gps log
                   (log-to-file "sdcard/mongoose/gpslog.csv"
                                (string-append
                                 (date-time->string (date-time)) ", "
                                 (number->string (car loc)) ", "
                                 (number->string (cadr loc)) ", "
                                 focal-id ", "
                                 (get-current 'group-composition-id "none") "\n")))

                 (list
                  (update-widget 'text-view (get-id "gps-state") 'text "GPS working")
                  (toast (string-append
                          (number->string (car loc)) ", "
                          (number->string (cadr loc))))))
         (* 30 1000) 1)
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
     (vert
      (horiz
       (mtoggle-button2 "choose-obs-gc" obs-gc
                        (lambda (v)
                          (set-current! 'observation obs-gc)
                          (clear-obs-toggles "choose-obs-gc")))

       (mtoggle-button2 "choose-obs-gp" obs-gp
                        (lambda (v)
                          (set-current! 'observation obs-gp)
                          (clear-obs-toggles "choose-obs-gp"))))

      (horiz
       (mtoggle-button2 "choose-obs-pf" obs-pf
                        (lambda (v)
                          (set-current! 'observation obs-pf)
                          (clear-obs-toggles "choose-obs-pf")))
       (mtoggle-button2 "choose-obs-of" obs-of
                        (lambda (v)
                          (set-current! 'observation obs-of)
                          (clear-obs-toggles "choose-obs-of")))

       (mtoggle-button2 "choose-obs-prf" obs-prf
                        (lambda (v)
                          (set-current! 'observation obs-prf)
                          (clear-obs-toggles "choose-obs-prf"))))))

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
             ((or
               ;; pup and oestrus focal go to the same activity
               (is-observation? obs-pf)
               (is-observation? obs-of)
               (is-observation? obs-prf))
              (list (start-activity "pup-focal-start" 2 "")))
             ((is-observation? obs-gp)
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
     ;; clear individual so we can't get through without one
     (set-current! 'individual #f)
     (list
      (update-widget 'toggle-button (get-id "choose-obs-gc") 'background-colour gc-col)
      (update-widget 'toggle-button (get-id "choose-obs-gp") 'background-colour gp-col)
      (update-widget 'toggle-button (get-id "choose-obs-pf") 'background-colour pf-col)
      (update-widget 'toggle-button (get-id "choose-obs-of") 'background-colour of-col)
      (update-widget 'toggle-button (get-id "choose-obs-prf") 'background-colour prf-col)

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
   (linear-layout
    (make-id "focal-start-bg")
    'vertical (layout 'fill-parent 'wrap-content 1 'centre 20)
    pf-col
    (list
     (mtitle "focal-title" "Pup focal setup")
     (mtext "pf1-pack" "Pack")
     (build-grid-selector "pf1-grid" "single" "Select individual")
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
                   ((get-current 'individual #f)
                    (set-current! 'focal-id (entity-record-values!))
                    (set-current! 'parent-id (get-current 'focal-id #f))
                    (set-current! 'timer-minutes pf-length)
                    (set-current! 'timer-seconds 0)
                    (list
                     (start-activity "pup-focal" 2 "")))
                   (else
                    (list
                     (ok-dialog
                      "pup-focal-check"
                      (string-append
                       "You need to specify a "
                       (cond
                        ((is-observation? obs-pf) "pup")
                        ((is-observation? obs-of) "female")
                        ((is-observation? obs-prf) "female"))
                       " for the focal")
                      (lambda () '()))))))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (cond
      ((is-observation? obs-pf) (entity-init! db "stream" "pup-focal" '()))
      ((is-observation? obs-of) (entity-init! db "stream" "oestrus-focal" '()))
      ((is-observation? obs-prf) (entity-init! db "stream" "preg-focal" '())))
     (list
      (cond
       ((is-observation? obs-pf) (update-widget 'linear-layout (get-id "focal-start-bg") 'background-colour pf-col))
       ((is-observation? obs-of) (update-widget 'linear-layout (get-id "focal-start-bg") 'background-colour of-col))
       ((is-observation? obs-prf) (update-widget 'linear-layout (get-id "focal-start-bg") 'background-colour prf-col)))
      (update-widget 'text-view (get-id "focal-title") 'text
                     (cond
                      ((is-observation? obs-pf) "Pup focal setup")
                      ((is-observation? obs-of) "Oestrus focal setup")
                      ((is-observation? obs-prf) "Pregnancy focal setup")))
      (populate-grid-selector
       "pf1-grid" "single"
       (cond
        ((is-observation? obs-pf) (db-mongooses-by-pack-pups))
        ((is-observation? obs-of) (db-mongooses-by-pack-adult-females))
        ((is-observation? obs-prf) (db-mongooses-by-pack-adult-females)))
       #f
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
    (make-id "focal-bg")
    'vertical (layout 'fill-parent 'fill-parent 1 'left 0)
    prf-col
    (list
     (relative
      '(("parent-top"))
      (list 0 0 0 0)
      (horiz
       (mtext "pf-title" "Pup Focal")
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
                          "Finish focal - are you sure?"
                          (lambda (v)
                            (cond
                             ((eqv? v 1)
                              (set-current! 'parent-id #f)
                              (set-current! 'focal-id #f)
                              (list (finish-activity 1)))
                             (else
                              (list))))))))))

     (build-fragment "pf-timer" (make-id "pf-top")
                     (layout 'fill-parent 'wrap-content -1 'left 0))

     (linear-layout
      0 'vertical (layout 'fill-parent 'fill-parent 1 'left 0)
      (list 0 0 0 0) (list (spacer  10)))

     (relative
      '(("parent-bottom"))
      (list 0 0 0 0)
      (build-fragment "events" (make-id "event-holder")
                      (layout 'fill-parent 'wrap-content 1 'left 0)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list

      (cond
       ((is-observation? obs-pf) (update-widget 'linear-layout (get-id "focal-bg") 'background-colour pf-col))
       ((is-observation? obs-of) (update-widget 'linear-layout (get-id "focal-bg") 'background-colour of-col))
       ((is-observation? obs-prf) (update-widget 'linear-layout (get-id "focal-bg") 'background-colour prf-col)))

      (update-widget 'text-view (get-id "pf-title") 'text (get-current 'observation "none"))
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
         (list (start-activity "manage-individual" 2 (ktv-get pack "unique_id")))))
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
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
     (vert
      (text-view (make-id "title") "Update pack" 40 fillwrap)
      (text-view (make-id "manage-pack-name") "Pack:" 30 fillwrap)
      (build-grid-selector "manage-pack-list" "button" "Choose individual")
      (horiz
       (mbutton2 "manage-pack-new" "New individual" (lambda () (list (start-activity "new-individual" 2 ""))))
       (mbutton2 "manage-pack-delete" "Delete pack"
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
			       (set-current! 'entity-type "pack")
			       (entity-update-single-value! (ktv "deleted" "int" 1))
			       (list (finish-activity 1)))
			      (else
			       (list)))))))
			(else (list)))))))))
      (vert
       (mtext (make-id "") "Current litter code")
       (horiz
	(medit-text "litter-code-letter" "Letter"
		    "normal"
		    (lambda (v)
		      (entity-set-value! "litter-code-letter" "varchar" v)
		      '()))
	(medit-text "litter-code-number" "Number"
		    "numeric"
		    (lambda (v)
		      (entity-set-value! "litter-code-number" "int" (string->number v))
		      '()))))

      (build-list-widget db "sync" "Recent litters (last 90 days)" (list "name" "date") 
			 "litter" "update-litter" 
			 (lambda () (ktv-get (get-current 'pack '()) "unique_id"))
			 (lambda () (init-litter (ktv-get (get-current 'pack '()) "unique_id"))))
      (build-lifehist 'pack)
      (horiz
       (mbutton2 "manage-pack-back" "Cancel" (lambda () (list (finish-activity 1))))
       (mbutton2 "manage-pack-done" "Done"
		 (lambda ()
		   (entity-update-values!)
		   (list (finish-activity 2))))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'pack (get-entity-by-unique db "sync" arg))
     (entity-init! db "sync" "pack" (get-current 'pack '()))
     (list
      (update-list-widget 
       db "sync" (list "name" "date") "litter" "update-litter" (entity-get-value "unique_id"))
      (populate-grid-selector
       "manage-pack-list" "button"
       (db-mongooses-by-pack) #f
       (lambda (individual)
         (list (start-activity "update-individual" 2 (ktv-get individual "unique_id")))))
      (update-widget 'text-view (get-id "manage-pack-name") 'text
                     (string-append "Pack: " (entity-get-value "name")))
      (update-widget 'text-view (get-id "litter-code-letter") 'text
		     (or (entity-get-value "litter-code-letter") ""))
      (update-widget 'text-view (get-id "litter-code-number") 'text
		     (or (entity-get-value "litter-code-number") ""))

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
     (entity-set-value! "dob" "varchar" (date->string (date-time)))
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
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
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

      (build-grid-selector "move-pack-list" "button" "Move pack")
      
      (build-lifehist 'mongoose)
      
      (horiz
       (mbutton2 "update-individual-cancel" "Cancel"
		 (lambda () (list (finish-activity 2))))
       (mbutton2 "update-individual-done" "Done"
		 (lambda ()
		   (entity-update-values!)
		   (list (finish-activity 2)))))
      )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'individual (get-entity-by-unique db "sync" arg))
     (entity-init! db "sync" "individual" (get-current 'individual '()))
     (append
      (update-lifehist (entity-get-value "gender"))
      (list
       (update-widget 'edit-text (get-id "update-individual-name") 'text
		      (entity-get-value "name"))
       (update-widget 'text-view (get-id "update-individual-dob") 'text
		      (entity-get-value "dob"))
       (update-widget 'spinner (get-id "update-individual-gender") 'selection
		      (spinner-index list-gender (entity-get-value "gender")))
       (update-widget 'edit-text (get-id "update-individual-litter-code") 'text
		      (entity-get-value "litter-code"))
       (update-widget 'edit-text (get-id "update-individual-chip-code") 'text
		      (entity-get-value "chip-code"))
       (update-widget 'edit-text (get-id "update-individual-collar-weight") 'text
		      (let ((v (entity-get-value "collar-weight"))) (if v v 0)))
       
       (update-widget 'toggle-button (get-id "update-individual-delete") 'checked
		      (if (eqv? (entity-get-value "deleted") 1) 1 0))
       (update-widget 'toggle-button (get-id "update-individual-died") 'checked
		      (if (eqv? (entity-get-value "deleted") 2) 1 0))
       (populate-grid-selector
	"move-pack-list" "button" (db-mongoose-packs) #f
	(lambda (pack)
	  (list
	   (alert-dialog
	    "move-pack-done"
	    "Move mongoose into new pack: are you sure?"
	    (lambda (v)
	      (cond
	       ((eqv? v 1)
		(let ((src-pack (get-entity-by-unique db "sync" (entity-get-value "pack-id"))))	    
		  (entity-create! 
		   db "stream" "movepack-event" 
		   (list
		    (ktv "mongoose-id" "varchar" (entity-get-value "unique_id"))
		    (ktv "mongoose-name" "varchar" (entity-get-value "name"))
		    (ktv "pack-dst-id" "varchar" (ktv-get pack "unique_id"))
		    (ktv "pack-dst-name" "varchar" (ktv-get pack "name"))
		    (ktv "pack-src-id" "varchar" (entity-get-value "pack-id"))
		    (ktv "pack-src-name" "varchar" (ktv-get src-pack "name"))))
		  (entity-set-value! "pack-id" "varchar" (ktv-get pack "unique_id"))
		  (entity-update-values!)
		  (list (finish-activity 1))))
		 (else
		  (list)))))))
	))))
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
                 ;(append
                 ; (foldl
                 ;  (lambda (e r)
                 ;    (debug! (string-append "Downloading /sdcard/mongoose/" e ".csv"))
                 ;    (append r
                 ;            (list
                 ;             (http-download
                 ;              (string-append "getting-" e)
                 ;              (string-append url "fn=entity-csv&table=stream&type=" e)
                 ;              (string-append "/sdcard/mongoose/" e ".csv"))
                 ;             )))
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
                  ;entity-types)
                  ;(list))
                  ))
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


  (activity
   "update-litter"
   (vert
    (text-view (make-id "title") "Update Litter" 40 fillwrap)
    (text-view (make-id "litter-pack-text") "" 30 fillwrap)
    (spacer 10)
    (vert
     (text-view (make-id "update-litter-date-text") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 30 fillwrap
             (lambda ()
               (list (date-picker-dialog
                      "update-litter-date"
                      (lambda (day month year)
                        (let ((datestring (date->string (list year (+ month 1) day))))
                          (entity-set-value! "date" "varchar" datestring)
                          (list
                           (update-widget
                            'text-view
                            (get-id "update-litter-date") 'text datestring)))))))))
     

    (build-lifehist 'litter)

    (mbutton2 "update-litter-delete" "Delete"
	      (lambda ()
		(list
		 (alert-dialog
		  "litter-delete-done"
		  "Delete litter: are you sure?"
		  (lambda (v)
		    (cond
		     ((eqv? v 1)
		      (entity-set-value! "deleted" "int" 1)
		      (entity-update-values!)
		      (list (finish-activity 2)))
		     (else
		      (list))))))))
    
    (horiz
     (mbutton2 "update-litter-cancel" "Cancel"
	       (lambda () (list (finish-activity 2))))
     (mbutton2 "update-litter-done" "Done"
	       (lambda ()
		 (entity-update-values!)
		 (list (finish-activity 2)))))
     
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'litter (get-entity-by-unique db "sync" arg))
     (entity-init! db "sync" "litter" (get-current 'litter '()))
     (list
      (update-widget 
       'text-view (get-id "litter-pack-text") 'text
       (string-append "Pack: " (get-current "pack-name" "none")))
      (update-widget 'text-view (get-id "update-litter-date-text") 'text
		     (entity-get-value "date"))
      
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))
  
  
  
  )
