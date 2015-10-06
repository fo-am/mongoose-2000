(define frag-prf-timer
  (fragment
   "prf-timer"
   (linear-layout
    (make-id "") 'vertical fillwrap trans-col
    (list
     (mtitle "prf-details" "Pack: xxx Female: xxx")))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (update-widget 'text-view (get-id "prf-details") 'text
                     (string-append
                      "Pack: " (ktv-get (get-current 'pack '()) "name") " "
                      "Subject: " (ktv-get (get-current 'individual '()) "name"))
                     )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))


(define frag-prf-scan1
  (fragment
   "prf-scan1"
   (linear-layout
    (make-id "") 'vertical fillwrap prf-col
    (list
     (build-grid-selector "prf-scan-nearest" "single" "<b>Nearest Neighbour Scan</b>: Closest Mongoose")
     (build-grid-selector "prf-scan-close" "toggle" "Mongooses within 2m")
     (mbutton "prf-scan-done" "Done"
              (lambda ()
                (check-not-same-focal
                 "pregnancy-focal-nearest"
                 "id-nearest"
                 (lambda ()
                   (set-current! 'entity-type "pregnancy-focal-nearest")
                   (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                   (entity-record-values!)
                   (list (replace-fragment (get-id "pf-top") "prf-timer"))))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pregnancy-focal-nearest" '())
     (entity-set-value! "scan-time" "varchar" (date-time->string (date-time)))
     (list
      (play-sound "ping")
      (vibrate 300)
      (populate-grid-selector
       "prf-scan-nearest" "single"
       (db-mongooses-by-pack-adults-3mth) #t
       (lambda (individual)
         (set-current! 'entity-type "pregnancy-focal-nearest")
         (entity-set-value! "id-nearest" "varchar" (ktv-get individual "unique_id"))
         (list)))
      (populate-grid-selector
       "prf-scan-close" "toggle"
       (db-mongooses-by-pack-adults-3mth) #t
       (lambda (individuals)
         (set-current! 'entity-type "pregnancy-focal-nearest")
         (entity-set-value! "id-list-close" "varchar" (assemble-array individuals))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-ev-pregaggr
  (fragment
   "ev-pregaggr"
   (linear-layout
    (make-id "") 'vertical fillwrap prf-col
    (list
     (mtitle "title" "Event: Aggression")
     (build-grid-selector "prf-aggr-partner" "single" "Other aggressive mongoose")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Fighting over")
        (mspinner "prf-aggr-over" list-oesaggression-over
                  (lambda (v)
                    (set-current! 'entity-type "pregnancy-focal-aggr")
                    (entity-set-value! "over" "varchar" (spinner-choice list-oesaggression-over v)) '())))
       (vert
        (mtext "" "Level")
        (mspinner "prf-aggr-level" list-aggression-level
                  (lambda (v)
                    (set-current! 'entity-type "pregnancy-focal-aggr")
                    (entity-set-value! "level" "varchar" (spinner-choice list-aggression-level v)) '())))

       (tri-state "pregnancy-focal-aggr" "prf-aggr-in" "Initiate?" "initiate")

       (tri-state "pregnancy-focal-aggr" "prf-aggr-win" "Win?" "win")))

     (spacer 10)
     (horiz
      (mbutton "prf-aggr-done" "Done"
               (lambda ()
                (check-not-same-focal
                 "pregnancy-focal-aggr"
                 "id-with"
                 (lambda ()
                   (set-current! 'entity-type "pregnancy-focal-aggr")
                   (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                   (entity-record-values!)
                   (list (replace-fragment (get-id "event-holder") "events"))))))
      (mbutton "prf-aggr-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pregnancy-focal-aggr" '())
     (list
      (populate-grid-selector
       "prf-aggr-partner" "single"
       (db-mongooses-by-pack) #t
       (lambda (individual)
         (set-current! 'entity-type "pregnancy-focal-aggr")
         (entity-set-value! "id-with" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))


(define frag-ev-pregaffil
  (fragment
   "ev-pregaffil"
   (linear-layout
    (make-id "") 'vertical fillwrap prf-col
    (list
     (mtitle "title" "Event: Affiliation")
     (build-grid-selector "prf-affil-partner" "single" "Other mongoose")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Behaviour type")
        (mspinner "prf-affil-over" list-affiliation-over
                  (lambda (v)
                    (set-current! 'entity-type "pregnancy-focal-affil")
                    (entity-set-value! "over" "varchar" (spinner-choice list-affiliation-over v)) '())))

       (tri-state "pregnancy-focal-affil" "prf-affil-in" "Initiate?" "initiate")

       ))

     (spacer 10)
     (horiz
      (mbutton "prf-affil-done" "Done"
               (lambda ()
                (check-not-same-focal
                 "pregnancy-focal-affil"
                 "id-with"
                 (lambda ()
                   (set-current! 'entity-type "pregnancy-focal-affil")
                   (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                   (entity-record-values!)
                   (list (replace-fragment (get-id "event-holder") "events"))))))
      (mbutton "prf-affil-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "pregnancy-focal-affil" '())
     (list
      (populate-grid-selector
       "prf-affil-partner" "single"
       (db-mongooses-by-pack) #t
       (lambda (individual)
         (set-current! 'entity-type "pregnancy-focal-affil")
         (entity-set-value! "id-with" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))
