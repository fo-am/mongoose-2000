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
                      "Female: " (ktv-get (get-current 'individual '()) "name"))
                     )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())))

(define frag-prf-events
  (fragment
   "prf-events"
   (linear-layout
    0 'vertical fillwrap trans-col
    (list
     (linear-layout
      (make-id "ev-prf") 'vertical fill prf-col
      (list
       (mtitle "ev-prf-text" "Oestrus Focal Events")
       (horiz
        (mbutton2 "evb-oesaggr" "Aggression" (lambda () (list (replace-fragment (get-id "event-holder") "ev-oesaggr"))))
        (mbutton2 "evb-oesaffil" "Affiliation" (lambda () (list (replace-fragment (get-id "event-holder") "ev-oesaffil"))))
        (mbutton2 "evb-oesmating" "Mating" (lambda () (list (replace-fragment (get-id "event-holder") "ev-oesmating"))))
        (mbutton2 "evb-oesmaleaggr" "Male-Male Aggression" (lambda () (list (replace-fragment (get-id "event-holder") "ev-oesmaleaggr")))))))
     (linear-layout
      (make-id "ev-prf") 'vertical fill gp-col
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
     (if (equal? (get-current 'observation "none") obs-prf)
         (list
          (update-widget 'text-view (get-id "ev-prf-text") 'show 0)
          (update-widget 'linear-layout (get-id "ev-prf") 'show 0))
         (list
          (update-widget 'text-view (get-id "ev-prf-text") 'hide 0)
          (update-widget 'linear-layout (get-id "ev-prf") 'hide 0))))
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
                (set-current! 'entity-type "oestrus-focal-nearest")
                (entity-set-value! "parent" "varchar" (get-current 'oestrus-focal-id ""))
                (entity-record-values!)
                (list (replace-fragment (get-id "prf-top") "prf-timer"))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-init! db "stream" "oestrus-focal-nearest" '())
     (entity-set-value! "scan-time" "varchar" (date-time->string (date-time)))
     (list
      (play-sound "ping")
      (vibrate 300)
      (populate-grid-selector
       "prf-scan-nearest" "single"
       (db-mongooses-by-pack-adult-males-9mth) #t
       (lambda (individual)
         (set-current! 'entity-type "oestrus-focal-nearest")
         (entity-set-value! "id-nearest" "varchar" (ktv-get individual "unique_id"))
         (list)))
      (populate-grid-selector
       "prf-scan-close" "toggle"
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
