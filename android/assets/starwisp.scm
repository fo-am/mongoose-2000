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

(define obs-gc "Group Composition")
(define obs-pf "Pup Focal")

(define (get-fragment-index name frag)
  (define (_ i l)
    (cond
     ((null? l) 0)
     ((equal? name (cadr (car l))) i)
     (else (_ (+ i 1) (cdr l)))))
  (_ 0 frag))

(define gc-fragments
  (list
   (list "Start" "gc-start")
   (list "Weights" "gc-weights")
   (list "Pregnant" "gc-preg")
   (list "Pup assoc" "gc-pup-assoc")
   (list "Oestrus" "gc-oestrus")
   (list "Babysit" "gc-babysitting")
   (list "End" "gc-end")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent database

(define db "/sdcard/test.db")
(db-open db)
(setup db "local")
(setup db "sync")
(setup db "stream")

(insert-entity-if-not-exists
 db "local" "app-settings" "null" 1
 (list
  (ktv "user-id" "varchar" "No name yet...")))

(display (db-all db "local" "app-settings"))(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff in memory

(define (store-set store key value)
  (cond
   ((null? store) (list (list key value)))
   ((eq? key (car (car store)))
    (cons (list key value) (cdr store)))
   (else
    (cons (car store) (store-set (cdr store) key value)))))

(define (store-get store key default)
  (cond
   ((null? store) default)
   ((eq? key (car (car store)))
    (cadr (car store)))
   (else
    (store-get (cdr store) key default))))

(define (store-exists? store key)
  (cond
   ((null? store) #f)
   ((eq? key (car (car store)))
    #t)
   (else
    (store-exists? (cdr store) key))))


(define store '())

(define (set-current! key value)
  (set! store (store-set store key value)))

(define (get-current key default)
  (store-get store key default))

(define (current-exists? key)
  (store-exists? store key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syncing code

(define url "http://192.168.2.1:8888/mongoose?")

(define (build-url-from-ktv ktv)
  (string-append "&" (ktv-key ktv) ":" (ktv-type ktv) "=" (stringify-value-url ktv)))

(define (build-url-from-ktvlist ktvlist)
  (foldl
   (lambda (ktv r)
     (string-append r (build-url-from-ktv ktv)))
   "" ktvlist))

(define (build-url-from-entity table e)
  (string-append
   url
   "fn=sync"
   "&table=" table
   "&entity-type=" (list-ref (car e) 0)
   "&unique-id=" (list-ref (car e) 1)
   "&dirty=" (number->string (list-ref (car e) 2))
   "&version=" (number->string (list-ref (car e) 3))
   (build-url-from-ktvlist (cadr e))))

;; spit all dirty entities to server
(define (spit-dirty db table)
  (map
   (lambda (e)
     (http-request
      (string-append "req-" (list-ref (car e) 1))
      (build-url-from-entity table e)
      (lambda (v)
        (display v)(newline)
        (if (equal? (car v) "inserted")
            (begin
              (update-entity-clean db table (cadr v))
              (toast "Uploaded " (ktv-get (cadr e) "name")))
            (toast "Problem uploading " (ktv-get (cadr e) "name"))))))
   (dirty-entities db table)))

(define (suck-entity-from-server db table unique-id exists)
  ;; ask for the current version
  (http-request
   (string-append unique-id "-update-new")
   (string-append url "fn=entity&table=" table "&unique-id=" unique-id)
   (lambda (data)
     (msg "data from server request" data)
     ;; check "sync-insert" in sync.ss raspberry pi-side for the contents of 'entity'
     (let ((entity (list-ref data 0))
           (ktvlist (list-ref data 1)))
       (if (not exists)
           (begin
             (insert-entity-wholesale
              db table
              (list-ref entity 0) ;; entity-type
              (list-ref entity 1) ;; unique-id
              0 ;; dirty
              (list-ref entity 2) ;; version
              ktvlist))
           (update-to-version
            db table (get-entity-id db table unique-id)
            (list-ref entity 4) ktvlist))
       (list
        (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty))
        (toast (string-append "Downloaded " (ktv-get ktvlist "name"))))))))

;; repeatedly read version and request updates
(define (suck-new db table)
  (list
   (http-request
    "new-entities-req"
    (string-append url "fn=entity-versions&table=" table)
    (lambda (data)
      (let ((r (foldl
                (lambda (i r)
                  (let* ((unique-id (car i))
                         (version (cadr i))
                         (exists (entity-exists? db table unique-id))
                         (old
                          (if exists
                              (> version (get-entity-version
                                          db table
                                          (get-entity-id db table unique-id)))
                              #f)))
                    ;; if we don't have this entity or the version on the server is newer
                    (if (or (not exists) old)
                        (cons (suck-entity-from-server db table unique-id exists) r)
                        r)))
                '()
                data)))
        (if (null? r)
            (cons (toast "All files up to date") r)
            (cons (toast "Requesting " (length r) " entities") r)))))))

(define (build-dirty)
  (let ((sync (get-dirty-stats db "sync"))
        (stream (get-dirty-stats db "stream")))
    (msg sync stream)
    (string-append
     "Pack data: " (number->string (car sync)) "/" (number->string (cadr sync)) " "
     "Focal data: " (number->string (car stream)) "/" (number->string (cadr stream)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface abstraction

(define (mbutton id title fn)
  (button (make-id id) title 20 fillwrap fn))

(define (mbutton2 id title fn)
  (button (make-id id) title 20 (layout 100 100 1 'left) fn))

(define (mtoggle-button id title fn)
  (toggle-button (make-id id) title 20 fillwrap fn))

(define (mtext id text)
  (text-view (make-id id) text 20 fillwrap))

(define (mtitle id text)
  (text-view (make-id id) text 40 fillwrap))

(define (medit-text id text type fn)
  (vert
   (mtext (string-append id "-title") text)
   (edit-text (make-id id) "" 20 type fillwrap fn)))

(define (mclear-toggles id-list)
  (map
   (lambda (id)
     (update-widget 'toggle-button (get-id id) 'checked 0))
   id-list))

(define (mclear-toggles-not-me me id-list)
  (foldl
   (lambda (id r)
     (if (equal? me id)
         r (cons (update-widget 'toggle-button (get-id id) 'checked 0) r)))
   '() id-list))

(define (xwise n l)
  (define (_ c l)
    (cond
      ((null? l) (if (null? c) '() (list c)))
      ((eqv? (length c) (- n 1))
       (cons (append c (list (car l))) (_ '() (cdr l))))
      (else
       (_ (append c (list (car l))) (cdr l)))))
  (_ '() l))

(define (build-grid-selector name type title)
  (vert
   (mtext "title" title)
   (horiz
    (image-view (make-id "im") "arrow_left" (layout 100 'fill-parent 1 'left))
    (scroll-view
     (make-id "scroller")
     (layout 'wrap-content 'wrap-content 1 'left)
     (list
      (linear-layout
       (make-id name) 'horizontal
       (layout 'wrap-content 'wrap-content 1 'centre)
       (list
        (button-grid (make-id name) type 3 20 (layout 100 40 1 'left)
                     (list) (lambda (v) '()))))))
    (image-view (make-id "im") "arrow_right" (layout 100 'fill-parent 1 'right)))))

(define (fast-get-name item)
  (list-ref (list-ref item 1) 2))

(define (populate-grid-selector name type items fn)
  (let ((id->items
         (map
          (lambda (item)
            (let ((item-name (fast-get-name item)))
              (list (make-id (string-append name item-name))
                           item
                           item-name)))
          items)))
    (update-widget
     'button-grid (get-id name) 'grid-buttons
     (list
      type 3 20 (layout 100 40 1 'left)
      (map
       (lambda (ii)
         (list (car ii) (caddr ii)))
       id->items)
      (lambda (v)
        (fn (cadr (findv v id->items))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fragments

(define-fragment-list

  (fragment
   "pf-timer"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtitle "title" "Time left: 20 mins")
     (mtitle "title" "Next nearest neighbour: 60 secs")
     (mbutton "pft-trigger" "NN scan"
              (lambda () (list (replace-fragment (get-id "pf-top") "pf-scan"))))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "pf-events"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtext "text" "Pup Focal Events")
     (horiz
      (mbutton2 "evb-pupfeed" "Pup Feed" (lambda () (list (replace-fragment (get-id "pf-bot") "ev-pupfeed"))))
      (mbutton2 "evb-pupfind" "Pup Find" (lambda () (list (replace-fragment (get-id "pf-bot") "ev-pupfind"))))
      (mbutton2 "evb-pupcare" "Pup Care" (lambda () (list (replace-fragment (get-id "pf-bot") "ev-pupcare"))))
      (mbutton2 "evb-pupagg" "Pup Aggression" (lambda () (list (replace-fragment (get-id "pf-bot") "ev-pupaggr")))))
     (mtext "text" "Group Events")
     (horiz
      (mbutton2 "evb-grpint" "Interaction" (lambda () (list (replace-fragment (get-id "pf-bot") "ev-grpint"))))
      (mbutton2 "evb-grpalarm" "Pup Find" (lambda () (list (replace-fragment (get-id "pf-bot") "ev-grpalarm"))))
      (mbutton2 "evb-grpmov" "Pup Care" (lambda () (list (replace-fragment (get-id "pf-bot") "ev-grpmov")))))
     ))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))



  (fragment
   "gc-start"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtitle "title" "Start")
     (mtoggle-button "gc-start-main-obs" "Main observer" (lambda (v) '()))
     (mtext "" "Code")
     (edit-text (make-id "gc-start-code") "" 20 "numeric" fillwrap (lambda (v) '()))
     (build-grid-selector "gc-start-present" "toggle" "Who's present?")))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gc-start-present" "toggle"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-weights"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtitle "title" "Weights")
     (build-grid-selector "gc-weigh-choose" "toggle" "Choose mongoose")
     (edit-text (make-id "gc-weigh-weight") "" 20 "numeric" fillwrap (lambda (v) '()))
     (mtoggle-button "gc-weigh-accurate" "Accurate?" (lambda (v) '()))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gc-weigh-choose" "toggle"
       (db-all-where
        db "sync" "mongoose"
        (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-preg"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtitle "title" "Pregnant females")
     (build-grid-selector "gc-preg-choose" "toggle" "Choose")))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gc-preg-choose" "toggle"
       (db-all-where
        db "sync" "mongoose"
        (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "gc-pup-assoc"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtitle "title" "Pregnant females")
     (build-grid-selector "gc-pup-choose" "toggle" "Choose pup")
     (build-grid-selector "gc-pup-escort" "toggle" "Escort")))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gc-pup-choose" "toggle"
       (db-all-where
        db "sync" "mongoose"
        (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (list)))
      (populate-grid-selector
       "gc-pup-escort" "toggle"
       (db-all-where
        db "sync" "mongoose"
        (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-oestrus"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtext "" "Oestrus...")))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-babysitting"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtext "" "Babysittings...")))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-end"
   (linear-layout
    (make-id "") 'vertical fillwrap
    (list
     (mtext "" "end!...")))
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
  (activity
   "splash"
   (vert
    (text-view (make-id "splash-title") "Mongoose 2000" 40 fillwrap)
    (mtext "splash-about" "Advanced mongoose technology")
    (spacer 20)
    (mbutton "f2" "Get started!" (lambda () (list (start-activity-goto "main" 2 ""))))
    (button-grid (make-id "bg") "toggle" 3 20 (layout 100 40 1 'left)
                 (list) (lambda (v) (msg v) '()))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget
       'button-grid (get-id "bg") 'grid-buttons
       (list
        "toggle" 3 20 (layout 100 40 1 'left)
        (list
         (list (make-id "1") "one")
         (list (make-id "2") "two")
         (list (make-id "3") "three")
         (list (make-id "4") "four")
         (list (make-id "5") "five")
         (list (make-id "6") "six")
         (list (make-id "7") "seven")
         (list (make-id "8") "eight")
         (list (make-id "9") "nine")
         (list (make-id "10") "ten")
         )
        (lambda (v) (msg "updated" v) '())))
     ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "main"
   (vert
    (text-view (make-id "main-title") "Mongoose 2000" 40 fillwrap)
    (text-view (make-id "main-about") "Advanced mongoose technology" 20 fillwrap)
    (spacer 10)
    (mbutton "main-observations" "Observations" (lambda () (list (start-activity "observations" 2 ""))))
    (mbutton "main-manage" "Manage Packs" (lambda () (list (start-activity "manage-packs" 2 ""))))
    (mbutton "main-tag" "Tag Location" (lambda () (list (start-activity "tag-location" 2 ""))))
    (mtext "foo" "Your ID")
    (edit-text (make-id "main-id-text") "" 30 "text" fillwrap
               (lambda (v)
                 (set-current! 'user-id v)
                 (update-entity
                  db "local" 1 (list (ktv "user-id" "varchar" v)))))
    (mtext "foo" "Database")
    (horiz
     (mbutton "main-send" "Email" (lambda () (list)))
     (mbutton "main-sync" "Sync" (lambda () (list (start-activity "sync" 0 ""))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (let ((user-id (ktv-get (get-entity db "local" 1) "user-id")))
       (set-current! 'user-id user-id)
       (list
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
     (mtoggle-button "choose-obs-gc" obs-gc
              (lambda (v)
                (set-current! 'observation obs-gc)
                (mclear-toggles (list "choose-obs-pf"))))
     (mtoggle-button "choose-obs-pf" obs-pf
              (lambda (v)
                (set-current! 'observation obs-pf)
                (mclear-toggles (list "choose-obs-gc")))))
    (build-grid-selector "choose-obs-pack-selector" "single" "Choose pack")
    (mbutton
     "choose-obs-start" "Start"
     (lambda ()
       ;; set up the observation fragments
       (let ((obs (get-current 'observation "none")))
         (when (not (equal? obs "none"))
           (set-current!
            'observation-fragments
            (cond
             ((equal? obs obs-gc) gc-fragments)
             ((equal? obs obs-pf) '())))))

       ;; go to observation
       (if (and (current-exists? 'pack)
                (current-exists? 'observation))
           (if (eq? (get-current 'observation "none") obs-pf)
               (list (start-activity "individual-select" 2 ""))
               (list (start-activity "observation" 2 "")))
           (list
            (alert-dialog
             "choose-obs-finish"
             "Need to specify a pack and an observation"
             (lambda () '()))))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "choose-obs-pack-selector" "single"
       (db-all db "sync" "pack")
       (lambda (pack)
         (msg "in selector" pack)
         (set-current! 'pack pack)
         '()))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))



  (activity
   "observation"
   (vert
    (text-view (make-id "obs-title") "" 40 fillwrap)
    (linear-layout
     (make-id "obs-buttons-bar") 'horizontal fillwrap '())
    (view-pager
     (make-id "obs-container") (layout 'wrap-content 700 1 'left) '())
    (mbutton "obs-done" "Done" (lambda () '())))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'linear-layout (get-id "obs-buttons-bar") 'contents
                     (let ((all-toggles
                            (map
                             (lambda (i) (string-append "obs-bar-" (cadr i)))
                             (get-current 'observation-fragments '()))))
                       (map
                        (lambda (frag)
                          (let ((id (string-append "obs-bar-" (cadr frag))))
                            (toggle-button
                             (make-id id) (car frag) 12 fillwrap
                             (lambda (v)
                               (append
                                (mclear-toggles-not-me id all-toggles)
                                (list
                                 (update-widget
                                  'view-pager (get-id "obs-container") 'switch
                                  (get-fragment-index
                                   (cadr frag)
                                   (get-current 'observation-fragments '())))))))))
                        (get-current 'observation-fragments '()))))
      (update-widget 'text-view (get-id "obs-title") 'text
                     (string-append
                      (get-current 'observation "No observation")
                      " with " (ktv-get (get-current 'pack '()) "name")))
      (update-widget 'view-pager (get-id "obs-container") 'contents
                     (map
                      (lambda (frag)
                        (cadr frag))
                      (get-current 'observation-fragments '())))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "individual-select" ;; pup focal #1
   (vert
    (mtitle "" "Pup focal setup")
    (mtext "pf1-pack" "Pack")
    (build-grid-selector "pf1-grid" "single" "Select pup")
    (horiz
     (medit-text "pf1-width" "Pack width" 20 "numeric" (lambda (v) '()))
     (medit-text "pf1-height" "Pack height" 20 "numeric" (lambda (v) '())))
    (medit-text "pf1-count" "How many mongooses present?" 20 "numeric" (lambda (v) '()))
    (mbutton "pf1-done" "Done"
             (lambda ()
               (list (start-activity "pup-focal" 2 ""))))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "pf1-grid" "single"
       (db-all-where db "sync" "mongoose" (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         '()))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "pup-focal" ;; pup focal #2
   (vert
    (horiz
     (mtitle "title" "Pup Focal")
     (mtext "pf-details" "")
     (mtoggle-button "pf-pause" "Pause" (lambda (v) '())))
    (build-fragment "pf-timer" (make-id "pf-top") (layout 550 350 1 'left))
    (mtitle "title" "Events")
    (build-fragment "pf-events" (make-id "pf-bot") (layout 550 350 1 'left))
    (mbutton "pf-done" "Done" (lambda () (list (start-activity-goto "observations" 2 "")))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'text-view (get-id "pf-details") 'text
                     (string-append
                      "Pack: " (ktv-get (get-current 'pack '()) "name") " "
                      "Pup: " (ktv-get (get-current 'individual '()) "name")))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "event-self"
   (vert
    (text-view (make-id "main-title") "Self feeding event" 40 fillwrap)
    (spacer 10)
    (toggle-button (make-id "event-self-found") "Found item?" 20 fillwrap
                   (lambda (v) '()))
    (toggle-button (make-id "event-self-kept") "Kept item?" 20 fillwrap
                   (lambda (v) '()))

    (text-view (make-id "event-self-type-text") "Food type" 20 fillwrap)
    (spinner (make-id "event-self-type") (list "Beetle" "Millipede") fillwrap (lambda (v) '()))

    (text-view (make-id "event-self-type-text") "Food size" 20 fillwrap)
    (spinner (make-id "event-self-type") (list "Small" "Medium" "Large") fillwrap (lambda (v) '()))
    (spacer 10)
    (horiz
     (button (make-id "event-self-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 0))))
     (button (make-id "event-self-done") "Done" 20 fillwrap (lambda () (list (finish-activity 0)))))
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
   "event-fed"
   (vert
    (text-view (make-id "main-title") "Being fed event" 40 fillwrap)
    (spacer 10)
    (text-view (make-id "event-fed-who-text") "Who by" 20 fillwrap)
    (spinner (make-id "event-fed-who") (list "Mongoose 1" "Mongoose 2" "Mongoose 3") fillwrap (lambda (v) '()))
    (toggle-button (make-id "event-fed-closest") "Closest to feeder?" 20 fillwrap
                   (lambda (v) '()))

    (text-view (make-id "event-self-type-text") "Who moved?" 20 fillwrap)
    (spinner (make-id "event-self-type") (list "Pup" "Feeder") fillwrap (lambda (v) '()))

    (text-view (make-id "event-fed-type-text") "Food type" 20 fillwrap)
    (spinner (make-id "event-fed-type") (list "Beetle" "Millipede") fillwrap (lambda (v) '()))

    (text-view (make-id "event-fed-type-text") "Food size" 20 fillwrap)
    (spinner (make-id "event-fed-type") (list "Small" "Medium" "Large") fillwrap (lambda (v) '()))
    (spacer 10)
    (horiz
     (button (make-id "event-fed-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 0))))
     (button (make-id "event-fed-done") "Done" 20 fillwrap (lambda () (list (finish-activity 0)))))
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
   "event-aggression"
   (vert
    (text-view (make-id "main-title") "Aggression event" 40 fillwrap)
    (spacer 10)
    (text-view (make-id "event-agg-who-text") "Other individual" 20 fillwrap)
    (spinner (make-id "event-agg-who") (list "Mongoose 1" "Mongoose 2" "Mongoose 3") fillwrap (lambda (v) '()))
    (text-view (make-id "event-agg-severity-text") "Severity" 20 fillwrap)
    (seek-bar (make-id "event-agg-severity") 100 fillwrap (lambda (v) '()))
    (spacer 10)
    (horiz
     (button (make-id "event-agg-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 0))))
     (button (make-id "event-agg-done") "Done" 20 fillwrap (lambda () (list (finish-activity 0)))))
    )
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
    (button (make-id "manage-packs-new") "New pack" 20 fillwrap (lambda () (list (start-activity "new-pack" 2 ""))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "manage-packs-list" "button" (db-all db "sync" "pack")
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
    (text-view (make-id "new-pack-name-text") "Pack name" 20 fillwrap)
    (edit-text (make-id "new-pack-name") "" 30 "text" fillwrap
               (lambda (v) (set-current! 'pack-name v) '()))
    (spacer 10)
    (horiz
     (button (make-id "new-pack-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "new-pack-done") "Done" 20 fillwrap
             (lambda ()
               (insert-entity
                db "sync" "pack" (get-current 'user-id "no id")
                (list
                 (ktv "name" "varchar" (get-current 'pack-name "no name"))))
               (list (finish-activity 2)))))
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
   "manage-individual"
   (vert
    (text-view (make-id "title") "Manage individuals" 40 fillwrap)
    (text-view (make-id "manage-individual-pack-name") "Pack:" 20 fillwrap)
    (build-grid-selector "manage-individuals-list" "button" "Choose individual")
    (button (make-id "manage-individuals-new") "New individual" 20 fillwrap (lambda () (list (start-activity "new-individual" 2 ""))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "manage-individuals-list" "button"
       (db-all-where db "sync" "mongoose" (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (list (start-activity "manage-individual" 2 ""))))
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
    (text-view (make-id "new-individual-pack-name") "Pack:" 20 fillwrap)
    (text-view (make-id "new-individual-name-text") "Name" 20 fillwrap)
    (edit-text (make-id "new-individual-name") "" 30 "text" fillwrap
               (lambda (v) (set-current! 'individual-name v) '()))
    (text-view (make-id "new-individual-name-text") "Gender" 20 fillwrap)
    (spinner (make-id "new-individual-gender") (list "Female" "Male") fillwrap
             (lambda (v) (set-current! 'individual-gender v) '()))
    (text-view (make-id "new-individual-dob-text") "Date of Birth" 20 fillwrap)
    (horiz
     (text-view (make-id "new-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 20 fillwrap (lambda () '())))
    (text-view (make-id "new-individual-litter-text") "Litter code" 20 fillwrap)
    (edit-text (make-id "new-individual-litter-code") "" 30 "text" fillwrap
               (lambda (v) (set-current! 'individual-litter-code v) '()))
    (text-view (make-id "new-individual-chip-text") "Chip code" 20 fillwrap)
    (edit-text (make-id "new-individual-chip-code") "" 30 "text" fillwrap
               (lambda (v) (set-current! 'individual-chip-code v) '()))
    (horiz
     (button (make-id "new-individual-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "new-individual-done") "Done" 20 fillwrap
             (lambda ()
               (insert-entity
                db "sync" "mongoose" (get-current 'user-id "no id")
                (list
                 (ktv "name" "varchar" (get-current 'individual-name "no name"))
                 (ktv "gender" "varchar" (get-current 'individual-gender "Female"))
                 (ktv "litter-code" "varchar" (get-current 'individual-litter-code ""))
                 (ktv "chip-code" "varchar" (get-current 'individual-chip-code ""))
                 (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
                 ))
               (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
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
    (text-view (make-id "update-individual-name-text") "Name" 20 fillwrap)
    (edit-text (make-id "update-individual-name") "" 30 "text" fillwrap (lambda (v) '()))
    (text-view (make-id "update-individual-name-text") "Gender" 20 fillwrap)
    (spinner (make-id "update-individual-gender") (list "Female" "Male") fillwrap (lambda (v) '()))
    (text-view (make-id "update-individual-dob-text") "Date of Birth" 20 fillwrap)
    (horiz
     (text-view (make-id "update-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 20 fillwrap (lambda () '())))
    (text-view (make-id "update-individual-litter-text") "Litter code" 20 fillwrap)
    (edit-text (make-id "update-individual-litter-code") "" 30 "text" fillwrap (lambda (v) '()))
    (text-view (make-id "update-individual-chip-text") "Chip code" 20 fillwrap)
    (edit-text (make-id "update-individual-chip-code") "" 30 "text" fillwrap (lambda (v) '()))
    (spacer 10)
    (horiz
     (button (make-id "update-individual-delete") "Delete" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "update-individual-died") "Died" 20 fillwrap (lambda () (list (finish-activity 2)))))
    (horiz
     (button (make-id "update-individual-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "update-individual-done") "Done" 20 fillwrap (lambda () (list (finish-activity 2)))))
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
   "tag-location"
   (vert
    (text-view (make-id "title") "Tag Location" 40 fillwrap)
    (text-view (make-id "tag-location-gps-text") "GPS" 20 fillwrap)
    (horiz
     (text-view (make-id "tag-location-gps-lat") "LAT" 20 fillwrap)
     (text-view (make-id "tag-location-gps-lng") "LNG" 20 fillwrap))

    (text-view (make-id "tag-location-name-text") "Name" 20 fillwrap)
    (edit-text (make-id "tag-location-name") "" 30 "text" fillwrap (lambda (v) '()))

    (text-view (make-id "tag-location-pack-text") "Associated pack" 20 fillwrap)
    (spinner (make-id "tag-location-pack") (list "Pack 1" "Pack 2") fillwrap (lambda (v) '()))

    (text-view (make-id "tag-location-radius-text") "Approx radius of area" 20 fillwrap)
    (seek-bar (make-id "tag-location-radius") 100 fillwrap (lambda (v) '()))
    (text-view (make-id "tag-location-radius-value") "10m" 20 fillwrap)

    (horiz
     (button (make-id "tag-location-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "tag-location-done") "Done" 20 fillwrap (lambda () (list (finish-activity 2)))))

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
    (horiz
     (mbutton "sync-connect" "Connect"
              (lambda ()
                (list
                 (network-connect
                  "network"
                  "mongoose-web"
                  (lambda (state)
                      (list
                       (update-widget 'text-view (get-id "sync-connect") 'text state)))))))
     (mbutton "sync-sync" "Push"
              (lambda ()
                (let ((r (spit-dirty db "sync")))
                  (cons (if (> (length r) 0)
                            (toast "Uploading data...")
                            (toast "No data changed to upload")) r))))
     (mbutton "sync-pull" "Pull"
              (lambda ()
                (cons (toast "Downloading data...") (suck-new db "sync")))))
    (text-view (make-id "sync-console") "..." 15 (layout 300 'wrap-content 1 'left))
    (mbutton "main-send" "Done" (lambda () (list (finish-activity 2)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty))
      ;;(update-widget 'text-view (get-id "sync-console") 'text (build-sync-debug db "sync"))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  )
