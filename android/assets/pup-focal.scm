(define frag-pf-timer
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
   (lambda (fragment) '())))

(define frag-pf-events
  (fragment
   "pf-events"
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
   (lambda (fragment) '())))

(define frag-pf-scan1
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
   (lambda (fragment) '())))

(define frag-ev-pupfeed
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
                 (list (replace-fragment (get-id "event-holder") "pf-events"))))
      (mbutton "pf-pupfeed-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "pf-events")))))))

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
   (lambda (fragment) '())))

(define frag-ev-pupfind
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
                 (list (replace-fragment (get-id "event-holder") "pf-events"))))
      (mbutton "pf-pupfind-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "pf-events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pup-focal-pupfind" '())
     (list
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-ev-pupcare
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
                 (list (replace-fragment (get-id "event-holder") "pf-events"))))
      (mbutton "pf-pupcare-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "pf-events")))))))

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
   (lambda (fragment) '())))

(define frag-ev-pupaggr
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
                 (list (replace-fragment (get-id "event-holder") "pf-events"))))
      (mbutton "pf-pupaggr-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "pf-events")))))))


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
   (lambda (fragment) '())))
