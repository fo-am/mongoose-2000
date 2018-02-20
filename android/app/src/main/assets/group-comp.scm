  ;;(replace-fragment (get-id "gc-top") (cadr frag))))))))

(define frag-gc-start
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
   (lambda (fragment) '())))

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



(define frag-gc-preg
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
   (lambda (fragment) '())))

(define frag-gc-pup-assoc
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
   (lambda (fragment) '())))


(define frag-gc-oestrus
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
   (lambda (fragment) '())))

(define frag-gc-babysitting
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
   (lambda (fragment) '())))

(define frag-gc-end
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
   (lambda (fragment) '())))
