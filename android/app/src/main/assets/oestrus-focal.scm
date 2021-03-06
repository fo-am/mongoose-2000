(define frag-of-timer
  (fragment
   "of-timer"
   (linear-layout
    (make-id "") 'vertical fillwrap trans-col
    (list
     (mtitle "of-details" "Pack: xxx Focal: xxx")))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (update-widget 'text-view (get-id "of-details") 'text
                     (string-append
                      "Pack: " (ktv-get (get-current 'pack '()) "name") " "
                      "Subject: " (ktv-get (get-current 'individual '()) "name"))
                     )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-of-scan1
  (fragment
   "of-scan1"
   (linear-layout
    (make-id "") 'vertical fillwrap of-col
    (list
     (build-grid-selector "of-scan-nearest" "single" "<b>Nearest Neighbour Scan</b>: Closest Male")
     (build-grid-selector "of-scan-close" "toggle" "Males within 2m")
     (mbutton "of-scan-done" "Done"
              (lambda ()
                (set-current! 'entity-type "oestrus-focal-nearest")
                (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                (entity-record-values!)
                (list (replace-fragment (get-id "pf-top") "of-timer"))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "oestrus-focal-nearest" '())
     (entity-set-value! "scan-time" "varchar" (date-time->string (date-time)))
     (list
      (play-sound "ping")
      (vibrate 300)
      (populate-grid-selector
       "of-scan-nearest" "single"
       (db-mongooses-by-pack-adult-males-9mth) #t
       (lambda (individual)
         (set-current! 'entity-type "oestrus-focal-nearest")
         (entity-set-value! "id-nearest" "varchar" (ktv-get individual "unique_id"))
         (list)))
      (populate-grid-selector
       "of-scan-close" "toggle"
       (db-mongooses-by-pack-adult-males-9mth) #t
       (lambda (individuals)
         (set-current! 'entity-type "oestrus-focal-nearest")
         (entity-set-value! "id-list-close" "varchar" (assemble-array individuals))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))


(define frag-ev-oesaggr
  (fragment
   "ev-oesaggr"
   (linear-layout
    (make-id "") 'vertical fillwrap of-col
    (list
     (mtitle "title" "Event: Aggression")
     (build-grid-selector "of-aggr-partner" "single" "Other aggressive mongoose")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Fighting over")
        (mspinner "of-aggr-over" list-oesaggression-over
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-aggr")
                    (entity-set-value! "over" "varchar" (spinner-choice list-oesaggression-over v)) '())))
       (vert
        (mtext "" "Level")
        (mspinner "of-aggr-level" list-preg-oes-aggression-level
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-aggr")
                    (entity-set-value! "level" "varchar" (spinner-choice list-preg-oes-aggression-level v)) '())))

       (tri-state "oestrus-focal-aggr" "of-aggr-in" "Initiate?" "initiate")

       ;(mtoggle-button "of-pupaggr-in" "Initiate?"
       ;                (lambda (v)
       ;                  (entity-set-value! "initiate" "varchar" (if v "yes" "no")) '()))


       (tri-state "oestrus-focal-aggr" "of-aggr-win" "Win?" "win")))

     (spacer 10)
     (horiz
      (mbutton "of-aggr-done" "Done"
               (lambda ()
                 (check-not-same-focal
                  "oestrus-focal-aggr"
                  "id-with"
                  (lambda ()
                    (set-current! 'entity-type "oestrus-focal-aggr")
                    (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                    (entity-record-values!)
                    (list (replace-fragment (get-id "event-holder") "events"))))))
      (mbutton "of-aggr-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "oestrus-focal-aggr" '())
     (list
      (populate-grid-selector
       "of-aggr-partner" "single"
       (db-mongooses-by-pack) #t
       (lambda (individual)
         (set-current! 'entity-type "oestrus-focal-aggr")
         (entity-set-value! "id-with" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))


(define frag-ev-oesaffil
  (fragment
   "ev-oesaffil"
   (linear-layout
    (make-id "") 'vertical fillwrap of-col
    (list
     (mtitle "title" "Event: Affiliation")
     (build-grid-selector "of-affil-partner" "single" "Other mongoose")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Behaviour type")
        (mspinner "of-affil-over" list-affiliation-over
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-affil")
                    (entity-set-value! "over" "varchar" (spinner-choice list-affiliation-over v)) '())))

       (tri-state "oestrus-focal-affil" "of-affil-in" "Initiate?" "initiate")

       ))

     (spacer 10)
     (horiz
      (mbutton "of-affil-done" "Done"
               (lambda ()
                 (check-not-same-focal
                  "oestrus-focal-affil"
                  "id-with"
                  (lambda ()
                    (set-current! 'entity-type "oestrus-focal-affil")
                    (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                    (entity-record-values!)
                    (list (replace-fragment (get-id "event-holder") "events"))))))
      (mbutton "of-affil-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "oestrus-focal-affil" '())
     (list
      (populate-grid-selector
       "of-affil-partner" "single"
       (db-mongooses-by-pack) #t
       (lambda (individual)
         (set-current! 'entity-type "oestrus-focal-affil")
         (entity-set-value! "id-with" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-ev-oesmate
  (fragment
   "ev-oesmate"
   (linear-layout
    (make-id "") 'vertical fillwrap of-col
    (list
     (mtitle "title" "Event: Mating behaviour")
     (build-grid-selector "of-mate-partner" "single" "Other male")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Behaviour type")
        (mspinner "of-mate-behaviour" list-mate-behaviour
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-mate")
                    (entity-set-value! "behaviour" "varchar" (spinner-choice list-mate-behaviour v)) '())))

       (vert
        (mtext "" "Female response")
        (mspinner "of-mate-fresponse" list-female-response
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-mate")
                    (entity-set-value! "female-response" "varchar"
                                       (spinner-choice list-female-response v)) '())))

       (vert
        (mtext "" "Male response")
        (mspinner "of-mate-mresponse" list-male-response
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-mate")
                    (entity-set-value! "male-response" "varchar"
                                       (spinner-choice list-male-response v)) '())))

       (tri-state "oestrus-focal-mate" "of-mate-suc" "Successful mating?" "success")

       ))

     (spacer 10)
     (horiz
      (mbutton "of-mate-done" "Done"
               (lambda ()
                 ;; no check as different genders
                 (set-current! 'entity-type "oestrus-focal-mate")
                 (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                 (entity-record-values!)
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "of-mate-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "oestrus-focal-mate" '())
     (list
      (populate-grid-selector
       "of-mate-partner" "single"
       (db-mongooses-by-pack-adult-males-9mth) #t
       (lambda (individual)
         (set-current! 'entity-type "oestrus-focal-mate")
         (entity-set-value! "id-with" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-ev-oesmaleaggr
  (fragment
   "ev-oesmaleaggr"
   (linear-layout
    (make-id "") 'vertical fillwrap of-col
    (list
     (mtitle "title" "Event: Male aggression")
     (build-grid-selector "of-maleaggr-initiator" "single" "Initiator")
     (build-grid-selector "of-maleaggr-receiver" "single" "Receiver")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Level")
        (mspinner "of-maleaggr-level" list-aggression-level
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-maleaggr")
                    (entity-set-value! "level" "varchar" (spinner-choice list-aggression-level v)) '())))

       (vert
        (mtext "" "Winner")
        (mspinner "of-maleaggr-win" list-maleaggression
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-maleaggr")
                    (entity-set-value! "winner" "varchar" (spinner-choice list-maleaggression v)) '())))

       (vert
        (mtext "" "Owner")
        (mspinner "of-maleaggr-owner" list-maleaggression
                  (lambda (v)
                    (set-current! 'entity-type "oestrus-focal-maleaggr")
                    (entity-set-value! "owner" "varchar" (spinner-choice list-maleaggression v)) '())))


       ))

     (spacer 10)
     (horiz
      (mbutton "of-maleaggr-done" "Done"
               (lambda ()
                 (check-not-same
                  "oestrus-focal-maleaggr"
                  "id-receiver" "id-initiator"
                  "Initiator and receiver are the same!"
                  (lambda ()
                    (set-current! 'entity-type "oestrus-focal-maleaggr")
                    (entity-set-value! "parent" "varchar" (get-current 'focal-id ""))
                    (entity-record-values!)
                    (list (replace-fragment (get-id "event-holder") "events"))))))
      (mbutton "of-maleaggr-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "oestrus-focal-maleaggr" '())
     (list
      (populate-grid-selector
       "of-maleaggr-initiator" "single"
       (db-mongooses-by-pack-adult-males-9mth) #t
       (lambda (individual)
         (set-current! 'entity-type "oestrus-focal-maleaggr")
         (entity-set-value! "id-initiator" "varchar" (ktv-get individual "unique_id"))
         (list)))
      (populate-grid-selector
       "of-maleaggr-receiver" "single"
       (db-mongooses-by-pack-adult-males-9mth) #t
       (lambda (individual)
         (set-current! 'entity-type "oestrus-focal-maleaggr")
         (entity-set-value! "id-receiver" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))
