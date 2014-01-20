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
(define obs-gp "Group Events")

(define entity-types
  (list
   "pup-focal"
   "pup-focal-nearest"
   "pup-focal-pupfeed"
   "pup-focal-pupfind"
   "pup-focal-pupcare"
   "pup-focal-pupaggr"
   "group-interaction"
   "group-alarm"
   "group-move"))

;; colours

(define pf-col (list 255 204 51 255))
(define gp-col (list 255 102 0 255))
(define gc-col (list 164 82 9 255))

(define pf-bgcol (list 255 204 51 127))
(define gp-bgcol (list 255 102 0 127))
(define gc-bgcol (list 164 82 9 127))

;(define pf-col (list  22  19 178  127))
;(define gp-col (list 255  97   0  127))
;(define gc-col (list 255 236   0  127))



(define trans-col (list 0 0 0 0))

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

(define db "/sdcard/mongoose/local-mongoose.db")

(define (setup-database!)
  (msg "setting up database")
  (db-close db) ;; close just in case (sorts out db file delete while running problem)
  (db-open db)
  (msg "setting up tables")
  (setup db "local")
  (setup db "sync")
  (setup db "stream")
  (msg (db-status db))
  (insert-entity-if-not-exists
   db "local" "app-settings" "null" 1
   (list
    (ktv "user-id" "varchar" "No name yet...")))
  (msg (db-all-sort-normal db "local" "app-settings")))

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
;; db abstraction

;; store a ktv, replaces existing with same key
(define (entity-add-value! key type value)
  (set-current!
   'entity-values
   (ktv-set
    (get-current 'entity-values '())
    (ktv key type value))))

(define (entity-set! ktv-list)
  (set-current! 'entity-values ktv-list))

(define (date-time->string dt)
  (string-append
   (number->string (list-ref dt 0)) "-"
   (number->string (list-ref dt 1)) "-"
   (number->string (list-ref dt 2)) " "
   (number->string (list-ref dt 3)) ":"
   (number->string (list-ref dt 4)) ":"
   (substring (number->string (+ 100 (list-ref dt 5))) 1 2)))

;; build entity from all ktvs, insert to db, return unique_id
(define (entity-record-values db table type)
  ;; standard bits
  (entity-add-value! "user" "varchar" (get-current 'user-id "none"))
  (entity-add-value! "time" "varchar" (date-time->string (date-time)))
  (entity-add-value! "lat" "real" (car (get-current 'location '(0 0))))
  (entity-add-value! "lon" "real" (cadr (get-current 'location '(0 0))))
  (let ((values (get-current 'entity-values '())))
    (cond
     ((not (null? values))
      (let ((r (insert-entity/get-unique
                db table type (get-current 'user-id "no id")
                values)))
        (msg "inserted a " type)
        (entity-reset!) r))
     (else
      (msg "no values to add as entity!") #f))))

(define (entity-update-values db table)
  ;; standard bits
  (let ((values (get-current 'entity-values '()))
        (unique-id (ktv-get (get-current 'entity-values '()) "unique_id")))
    (cond
     ((and unique-id (not (null? values)))
      (update-entity db table (entity-id-from-unique db table unique-id) values)
      (msg "updated " unique-id)
      (entity-reset!))
     (else
      (msg "no values or no id to update as entity:" unique-id "values:" values)))))

(define (entity-reset!)
  (set-current! 'entity-values '()))

(define (assemble-array entities)
  (foldl
   (lambda (i r)
     (if (equal? r "") (ktv-get i "unique_id")
         (string-append r "," (ktv-get i "unique_id"))))
   ""
   entities))

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
(define (spit db table entities)
  (foldl
   (lambda (e r)
     (debug! (string-append "Sending a " (car (car e)) " to Raspberry Pi"))
     (append
      (list
       (http-request
        (string-append "req-" (list-ref (car e) 1))
        (build-url-from-entity table e)
        (lambda (v)
          (cond
           ((or (equal? (car v) "inserted") (equal? (car v) "match"))
            (update-entity-clean db table (cadr v))
            (debug! (string-append "Uploaded " (car (car e)))))
           ((equal? (car v) "no change")
            (debug! (string-append "No change for " (car (car e)))))
           ((equal? (car v) "updated")
            (update-entity-clean db table (cadr v))
            (debug! (string-append "Updated changed " (car (car e)))))
           (else
            (debug! (string-append
                     "Problem uploading "
                     (car (car e)) " : " (car v)))))
          (list
           (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty))))))
      r))
   '()
   entities))

(define (suck-entity-from-server db table unique-id)
  ;; ask for the current version
  (http-request
   (string-append unique-id "-update-new")
   (string-append url "fn=entity&table=" table "&unique-id=" unique-id)
   (lambda (data)
     ;; check "sync-insert" in sync.ss raspberry pi-side for the contents of 'entity'
     (let ((entity (list-ref data 0))
           (ktvlist (list-ref data 1))
           (exists (entity-exists? db table unique-id)))
       (if (not exists)
           (insert-entity-wholesale
            db table
            (list-ref entity 0) ;; entity-type
            (list-ref entity 1) ;; unique-id
            0 ;; dirty
            (list-ref entity 2) ;; version
            ktvlist)
           (update-to-version
            db table (get-entity-id db table unique-id)
            (list-ref entity 2) ktvlist))
       (debug! (string-append (if exists "Got new: " "Updated: ") (ktv-get ktvlist "name")))
       (list
        (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty)))))))

;; repeatedly read version and request updates
(define (suck-new db table)
  (debug! "Requesting new entities")
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
                        (cons (suck-entity-from-server db table unique-id) r)
                        r)))
                '()
                data)))
        (cond
         ((null? r)
          (debug! "No new data to download")
          (set-current! 'download 1)
          (append
           (if (eqv? (get-current 'upload 0) 1)
               (list (play-sound "ping")) '())
           (list
            (toast "No new data to download")) r))
         (else
          (debug! (string-append
                   "Requesting "
                   (number->string (length r)) " entities"))
          (cons
           (play-sound "active")
           r))))))))

(define (build-dirty)
  (let ((sync (get-dirty-stats db "sync"))
        (stream (get-dirty-stats db "stream")))
    (if (or (not sync) (not stream))
        "No data yet"
        (string-append
         "Pack data: " (number->string (car sync)) "/" (number->string (cadr sync)) " "
         "Focal data: " (number->string (car stream)) "/" (number->string (cadr stream))))))

(define (upload-dirty db)
  (let ((r (append
            (spit db "sync" (dirty-entities db "sync"))
            (spit db "stream" (dirty-entities db "stream")))))
    (append (cond
             ((> (length r) 0)
              (debug! (string-append "Uploading " (number->string (length r)) " items..."))
              (list
               (toast "Uploading data...")
               (play-sound "active")))
             (else
              (debug! "No data changed to upload")
              (set-current! 'upload 1)
              (append
               (if (eqv? (get-current 'download 0) 1)
                   (list (play-sound "ping")) '())
               (list
                (toast "No data changed to upload"))))) r)))

(define (connect-to-net fn)
  (list
   (network-connect
    "network"
    "mongoose-web"
    (lambda (state)
      (debug! (string-append "Raspberry Pi connection state now: " state))
      (append
       (if (equal? state "Connected") (fn) '())
       (list
        ;;(update-widget 'text-view (get-id "sync-connect") 'text state)
        ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface abstraction

(define (mbutton id title fn)
  (button (make-id id) title 30 fillwrap fn))

(define (mbutton2 id title fn)
  (button (make-id id) title 30 (layout 150 100 1 'centre 0) fn))

(define (mtoggle-button id title fn)
  (toggle-button (make-id id) title 30 (layout 'fill-parent 'wrap-content 1 'centre 0) "fancy" fn))

(define (mtoggle-button-yes id title fn)
  (toggle-button (make-id id) title 30 (layout 49 43 1 'centre 0) "yes" fn))

(define (mtoggle-button-maybe id title fn)
  (toggle-button (make-id id) title 30 (layout  49 43 1 'centre 0) "maybe" fn))

(define (mtoggle-button-no id title fn)
  (toggle-button (make-id id) title 30 (layout  49 43 1 'centre 0) "no" fn))

(define (mtoggle-button2 id title fn)
  (toggle-button (make-id id) title 30 (layout 150 100 1 'centre 0) "plain" fn))

(define (mtext id text)
  (text-view (make-id id) text 30 fillwrap))

(define (mtitle id text)
  (text-view (make-id id) text 50 fillwrap))

(define (medit-text id text type fn)
  (vert
   (mtext (string-append id "-title") text)
   (edit-text (make-id id) "" 30 type fillwrap fn)))

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

;;;;

(define (build-grid-selector name type title)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'left 0)
   (list 0 0 0 0)
   (list
    (mtext "title" title)
    (linear-layout
     0 'horizontal
     (layout 'fill-parent 'wrap-content 1 'left 2) trans-col
     (list
      (image-view (make-id "im") "arrow_left" (layout 200 'fill-parent 1 'left 0))
      (scroll-view
       (make-id "scroller")
       (layout 'wrap-content 'wrap-content 1 'left 20)
       (list
        (linear-layout
         (make-id name) 'horizontal
         (layout 'wrap-content 'wrap-content 1 'centre 20) trans-col
         (list
          (button-grid (make-id name) type 3 30 (layout 100 60 1 'left 40)
                       (list) (lambda (v) '()))))))
      (image-view (make-id "im") "arrow_right" (layout 200 'fill-parent 1 'right 0)))))))

;; assumes grid selectors on mongeese only
(define (fast-get-name item)
  (list-ref (list-ref item 1) 2))

(define (build-button-items name items unknown)
  (append
   (map
    (lambda (item)
      (let ((item-name (fast-get-name item)))
        (list (make-id (string-append name item-name))
              item
              item-name)))
    items)
   (if unknown
       (list
        (list (make-id (string-append name "-unknown"))
              (list (ktv "name" "varchar" "Unknown")
                    (ktv "unique_id" "varchar" "Unknown"))
              "???"))
       '())))

(define (populate-grid-selector name type items unknown fn)
  (prof-start "popgrid")
  (prof-start "popgrid setup")
  (let ((id->items (build-button-items name items unknown))
        (selected-set '()))
    (prof-end "popgrid setup")
    (let ((r (update-widget
     'button-grid (get-id name) 'grid-buttons
     (list
      type 3 30 (layout 100 60 1 'left 0)
      (map
       (lambda (ii)
         (list (car ii) (caddr ii)))
       id->items)
      (lambda (v state)
        (cond
         ((equal? type "toggle")
          ;; update list of selected items
          (if state
              (set! selected-set (set-add v selected-set))
              (set! selected-set (set-remove v selected-set)))
          ;; find all items currently selected
          (fn (map
               (lambda (v)
                 (cadr (findv v id->items)))
               selected-set)))
         (else
          ;;(msg (findv v id->items))
          (fn (cadr (findv v id->items))))))))))
      (prof-end "popgrid")
      r)))

(define (db-mongooses-by-pack)
  (db-all-where
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))))

(define (db-mongooses-by-pack-ignore-delete)
  (db-all-where-ignore-delete
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))))


(define (db-mongooses-by-pack-male)
  (db-all-where2or
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "gender" "varchar" "Male") "Unknown"))

(define (db-mongooses-by-pack-female)
  (db-all-where2or
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "gender" "varchar" "Female") "Unknown"))


;; (y m d h m s)
(define (date-minus-months d ms)
  (let ((year (list-ref d 0))
        (month (- (list-ref d 1) 1)))
    (let ((new-month (- month ms)))
      (list
       (if (< new-month 0) (- year 1) year)
       (+ (if (< new-month 0) (+ new-month 12) new-month) 1)
       (list-ref d 2)
       (list-ref d 3)
       (list-ref d 4)
       (list-ref d 5)))))

(define (db-mongooses-by-pack-pups)
  (db-all-newer
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "dob" "varchar" (date->string (date-minus-months (date-time) 6)))))

(define (db-mongooses-by-pack-adults)
  (db-all-older
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "dob" "varchar" (date->string (date-minus-months (date-time) 6)))))



(define (tri-state id text key)
  (linear-layout
   (make-id "") 'vertical (layout 'fill-parent 'wrap-content '1 'centre 0) trans-col
   (list
    (linear-layout
     (make-id "") 'horizontal (layout 'wrap-content 'wrap-parent '1 'centre 0) trans-col
     (list
      (mtoggle-button-yes
       (string-append id "-y") ""
       (lambda (v)
         (cond
          (v
           (entity-add-value! key "varchar" "yes")
           (list
            (update-widget 'toggle-button (get-id (string-append id "-n")) 'checked 0)
            (update-widget 'toggle-button (get-id (string-append id "-m")) 'checked 0)))
               (else
                (list
                 (update-widget 'toggle-button (get-id (string-append id "-y")) 'checked 1))))
              ))
      (mtoggle-button-maybe
       (string-append id "-m")  ""
       (lambda (v)
         (cond
          (v
           (entity-add-value! key "varchar" "maybe")
           (list
            (update-widget 'toggle-button (get-id (string-append id "-y")) 'checked 0)
            (update-widget 'toggle-button (get-id (string-append id "-n")) 'checked 0)))
          (else
           (list
            (update-widget 'toggle-button (get-id (string-append id "-m")) 'checked 1))))
         ))

      (mtoggle-button-no
       (string-append id "-n") ""
       (lambda (v)
         (cond
          (v
           (entity-add-value! key "varchar" "no")
           (list
            (update-widget 'toggle-button (get-id (string-append id "-y")) 'checked 0)
            (update-widget 'toggle-button (get-id (string-append id "-m")) 'checked 0)))
          (else
           (list
            (update-widget 'toggle-button (get-id (string-append id "-n")) 'checked 1))))
         ))))

    (text-view 0 text 30 (layout 'wrap-content 'wrap-parent '1 'centre 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (debug! txt)
  (set-current! 'debug-text (string-append txt "\n" (get-current 'debug-text ""))))

(define (update-debug)
  (update-widget 'debug-text-view (get-id "sync-debug") 'text
                 (get-current 'debug-text "")))

(define (debug-timer-cb)
  (append
   (cond
    ((get-current 'sync-on #f)
     (set-current! 'upload 0)
     (set-current! 'download 0)
     (connect-to-net
      (lambda ()
        (append
         (list (toast "sync-cb"))
         (upload-dirty db)
         (suck-new db "sync")))))
    (else '()))
   (list
    (delayed "debug-timer" (+ 10000 (random 5000)) debug-timer-cb)
    (update-debug))))


(define pf-length 20) ;; minutes...

(define (timer-cb)
  (set-current!
   'timer-seconds
   (- (get-current 'timer-seconds 59) 1))
  (append
   (cond
    ((< (get-current 'timer-seconds 59) 0)
     (set-current! 'timer-minutes (- (get-current 'timer-minutes pf-length) 1))
     (set-current! 'timer-seconds 59)
     (cond ((< (get-current 'timer-minutes pf-length) 1)
            (list
             (alert-dialog
              "pup-focal-end"
              "Pup focal time is up, have you finished?"
              (lambda (v)
                (cond
                 ((eqv? v 1)
                  (list (finish-activity 1)))
                 (else
                  (set-current! 'timer-minutes 1)
                  (list)))))))
           (else
            (list (replace-fragment (get-id "pf-top") "pf-scan1")))))
    (else '()))
   (list
    (delayed "timer" 1000 timer-cb)
    (update-widget
     'text-view (get-id "pf-timer-time-minutes") 'text
     (string-append (number->string (get-current 'timer-minutes pf-length))))
    (update-widget
     'text-view (get-id "pf-timer-time") 'text
     (string-append (number->string (get-current 'timer-seconds 59))))
    )))

(define (next-button id dialog-msg next-frag fn)
     (mbutton (string-append id "-nextb") "Next"
              (lambda ()
                (list
                 (alert-dialog
                  (string-append id "-d")
                  dialog-msg
                  (lambda (v)
                    (cond
                     ((eqv? v 1)
                      (append
                       (fn) (list (replace-fragment
                                   (get-id "gc-top") next-frag))))
                     (else '()))))))))

(define (force-pause)
  (list
   (delayed "timer" 1000 (lambda () '()))
   (update-widget 'toggle-button (get-id "pf-pause") 'checked 1)))

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
                (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                (entity-record-values db "stream" "pup-focal-nearest")
                (list (replace-fragment (get-id "pf-top") "pf-timer"))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (entity-add-value! "scan-time" "varchar" (date-time->string (date-time)))
     (list
      (play-sound "ping")
      (vibrate 300)
      (populate-grid-selector
       "pf-scan-nearest" "single"
       (db-mongooses-by-pack-adults) #t
       (lambda (individual)
         (entity-add-value! "id-nearest" "varchar" (ktv-get individual "unique_id"))
         (list)))
      (populate-grid-selector
       "pf-scan-close" "toggle"
       (db-mongooses-by-pack-adults) #t
       (lambda (individuals)
         (entity-add-value! "id-list-close" "varchar" (assemble-array individuals))
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
      (spinner (make-id "pf-pupfeed-size") (list "Small" "Medium" "Large") fillwrap
               (lambda (v)
                 (entity-add-value! "size" "varchar" v) '())))
     (spacer 20)
     (horiz
      (mbutton "pf-pupfeed-done" "Done"
               (lambda ()
                 (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values db "stream" "pup-focal-pupfeed")
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupfeed-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (list
      (populate-grid-selector
       "pf-pupfeed-who" "single"
       (db-mongooses-by-pack-adults) #t
       (lambda (individual)
         (entity-add-value! "id-who" "varchar" (ktv-get individual "unique_id"))
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
      (spinner (make-id "pf-pupfind-size") (list "Small" "Medium" "Large") fillwrap
               (lambda (v) (entity-add-value! "size" "varchar" v) '())))
     (spacer 20)
     (horiz
      (mbutton "pf-pupfind-done" "Done"
               (lambda ()
                 (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values db "stream" "pup-focal-pupfind")
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupfind-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
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
      (spinner (make-id "pf-pupcare-type") (list "Carry" "Lead" "Sniff" "Play" "Ano-genital sniff") fillwrap
               (lambda (v)
                 (entity-add-value! "type" "varchar" v) '())))
     (spacer 20)
     (horiz
      (mbutton "pf-pupcare-done" "Done"
               (lambda ()
                 (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values db "stream" "pup-focal-pupcare")
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupcare-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (list
      (populate-grid-selector
       "pf-pupcare-who" "single"
       (db-mongooses-by-pack-adults) #t
       (lambda (individual)
         (entity-add-value! "id-who" "varchar" (ktv-get individual "unique_id"))
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
     (build-grid-selector "pf-pupaggr-partner" "single" "Aggressive mongoose")

     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 100 '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Fighting over")
        (spinner (make-id "pf-pupaggr-over") (list "Food" "Escort" "Nothing" "Other") fillwrap
                 (lambda (v)
                   (entity-add-value! "over" "varchar" v) '())))
       (vert
        (mtext "" "Level")
        (spinner (make-id "pf-pupaggr-level") (list "Block" "Snap" "Chase" "Push" "Fight") fillwrap
                 (lambda (v)
                   (entity-add-value! "level" "varchar" v) '())))

       (tri-state "pf-pupaggr-in" "Initiate?" "initiate")

       ;(mtoggle-button "pf-pupaggr-in" "Initiate?"
       ;                (lambda (v)
       ;                  (entity-add-value! "initiate" "varchar" (if v "yes" "no")) '()))


       (tri-state "pf-pupaggr-win" "Win?" "win")))

     (spacer 20)
     (horiz
      (mbutton "pf-pupaggr-done" "Done"
               (lambda ()
                 (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values db "stream" "pup-focal-pupaggr")
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-pupaggr-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (list
      (populate-grid-selector
       "pf-pupaggr-partner" "single"
       (db-mongooses-by-pack) #t
       (lambda (individual)
         (entity-add-value! "id-with" "varchar" (ktv-get individual "unique_id"))
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
        (spinner (make-id "gp-int-out") (list "Retreat" "Advance" "Fight retreat" "Fight win") fillwrap
                 (lambda (v)
                   (entity-add-value! "outcome" "varchar" v) '()))
        (mtext "text" "Duration")
        (edit-text (make-id "gp-int-dur") "" 30 "numeric" fillwrap
                   (lambda (v) (entity-add-value! "duration" "int" (string->number v)) '()))))
      (build-grid-selector "gp-int-pack" "single" "Other pack"))
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 80 '1 'left 0) trans-col
      (list
       (mbutton "pf-grpint-done" "Done"
                (lambda ()
                  (entity-record-values db "stream" "group-interaction")
                  (list (replace-fragment (get-id "event-holder") "events"))))
       (mbutton "pf-grpint-cancel" "Cancel"
                (lambda ()
                  (list (replace-fragment (get-id "event-holder") "events"))))))))


   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (append
      (force-pause)
      (list
       (populate-grid-selector
        "gp-int-pack" "single"
        (db-all-sort-normal db "sync" "pack") #f
        (lambda (pack)
          (entity-add-value! "id-other-pack" "varchar" (ktv-get pack "unique_id"))
          (list)))
       (populate-grid-selector
        "gp-int-leader" "single"
        (db-mongooses-by-pack) #t
        (lambda (individual)
          (entity-add-value! "id-leader" "varchar" (ktv-get individual "unique_id"))
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
        (spinner (make-id "gp-alarm-cause") (list "Predator" "Other mongoose pack" "Humans" "Other" "Unknown") fillwrap
                 (lambda (v)
                   (entity-add-value! "cause" "varchar" v) '())))

       (tri-state "gp-alarm-join" "Did the others join in?" "others-join")))

     (horiz
      (mbutton "pf-grpalarm-done" "Done"
               (lambda ()
                 (entity-record-values db "stream" "group-alarm")
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-grpalarm-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (append
      (force-pause)
      (list
       (populate-grid-selector
        "gp-alarm-caller" "single"
        (db-mongooses-by-pack) #t
        (lambda (individual)
          (entity-add-value! "id-caller" "varchar" (ktv-get individual "unique_id"))
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
      (make-id "") 'horizontal (layout 'fill-parent 90 '1 'left 0) trans-col
      (list
       (medit-text "gp-mov-w" "Pack width" "numeric"
                   (lambda (v) (entity-add-value! "pack-width" "int" (string->number v)) '()))
       (medit-text "gp-mov-l" "Pack depth" "numeric"
                   (lambda (v) (entity-add-value! "pack-depth" "int" (string->number v)) '()))
       (medit-text "gp-mov-c" "How many?" "numeric"
                   (lambda (v) (entity-add-value! "pack-count" "int" (string->number v)) '()))))
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 'wrap-content '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Direction")
        (spinner (make-id "gp-mov-dir") (list "To" "From") fillwrap
                 (lambda (v) (entity-add-value! "direction" "varchar" v)  '())))

       (vert
        (mtext "" "Where to")
        (spinner (make-id "gp-mov-to") (list "Latrine" "Water" "Food" "Nothing" "Den" "Unknown") fillwrap
                 (lambda (v) (entity-add-value! "destination" "varchar" v)  '())))))

     (spacer 20)
     (horiz
      (mbutton "pf-grpmov-done" "Done"
               (lambda ()
                 (entity-record-values db "stream" "group-move")
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "pf-grpalarm-cancel" "Cancel"
               (lambda ()
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (append
      (force-pause)
      (list
       (populate-grid-selector
        "gp-mov-leader" "single"
        (db-mongooses-by-pack) #t
        (lambda (individual)
          (entity-add-value! "id-leader" "varchar" (ktv-get individual "unique_id"))
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
     (edit-text (make-id "note-text") "" 30 "text" fillwrap
                (lambda (v)
                  (entity-add-value! "text" "varchar" v)
                  '()))
     (horiz
      (mbutton "note-done" "Done"
               (lambda ()
                 (entity-record-values db "stream" "note")
                 (list (replace-fragment (get-id "event-holder") "events"))))
      (mbutton "note-cancel" "Cancel"
               (lambda ()
                 (entity-reset!)
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
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
     (mtoggle-button "gc-start-main-obs" "Main observer"
                     (lambda (v) (entity-add-value! "main-observer" "varchar" v) '()))
     (mtext "" "Code")
     (edit-text (make-id "gc-start-code") "" 30 "numeric" fillwrap
                (lambda (v) (entity-add-value! "group-comp-code" "varchar" v) '()))
     (build-grid-selector "gc-start-present" "toggle" "Who's present?")
     (next-button "gc-start-" "Go to weighing, have you finished here?" "gc-weights"
                  (lambda () '()))
     ))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (set-current! 'group-composition-id (entity-record-values db "stream" "group-composition"))

     (entity-add-value!

     (list
      (populate-grid-selector
       "gc-start-present" "toggle"
       (db-mongooses-by-pack) #f
       (lambda (individual)
         (lambda (v) (entity-add-value! "group-comp-code" "varchar" v) '()))

         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-weights"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "title" "Weights")
     (build-grid-selector "gc-weigh-choose" "single" "Choose mongoose")
     (horiz
      (edit-text (make-id "gc-weigh-weight") "" 30 "numeric" fillwrap
                 (lambda (v)
                   (entity-add-value! "weight" "varchar" v)
                   '()))
      (mbutton "gc-weigh-save" "Save"
               (lambda ()
                 (msg "saving")
                 (entity-add-value! "parent" "varchar" (get-current 'group-composition-id 0))
                 (msg "saving to " (get-current 'entity-id "0"))
                 (if (get-current 'updating #f)
                     (entity-update-values db "stream")
                     (entity-record-values db "stream" "weight")
                 (entity-reset!)
                 '()))))

     (mtoggle-button "gc-weigh-accurate" "Accurate?" (lambda (v) '()))
     (next-button "gc-weigh-" "Go to pregnancies, have you finished here?" "gc-preg"
                  (lambda () '()))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (entity-reset!)
     (list
      (populate-grid-selector
       "gc-weigh-choose" "single"
       (db-mongooses-by-pack) #f
       (lambda (individual)
         (msg "loading")
         (entity-add-value! "id-mongoose" "varchar" (ktv-get individual "unique_id"))
         (set-current! 'updating #f)
         (let ((s (db-all-where2
                   db "stream" "weight"
                   (ktv "parent" "varchar" (get-current 'group-composition-id 0))
                   (ktv "id-mongoose" "varchar" (ktv-get individual "unique_id")))))
           (when (not (null? s))
                 (msg "found previous")
                 (entity-add-value! "unique_id" "varchar" (ktv-get (car s) "unique_id"))
                 (set-current! 'updating #t))
           (msg "-->" s)
           (list
            (update-widget 'edit-text (get-id "gc-weigh-weight") 'text
                           (if (null? s) "" (ktv-get (car s) "weight")))))))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))

  (fragment
   "gc-preg"
   (linear-layout
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "title" "Pregnant females")
     (build-grid-selector "gc-preg-choose" "toggle" "Choose")
     (next-button "gc-preg-" "Going to pup associations, have you finished here?" "gc-pup-assoc"
                  (lambda () '()))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gc-preg-choose" "toggle"
       (db-mongooses-by-pack-female) #f
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
    (make-id "") 'vertical fill gc-col
    (list
     (mtext "title" "Pup Associations")
     (build-grid-selector "gc-pup-choose" "toggle" "Choose pup")
     (horiz
      (vert
       (mtext "" "Strength")
       (spinner (make-id "gc-pup-strength") (list "Weak" "Medium" "Strong") fillwrap
                (lambda (v) '())))
      (vert
       (mtext "" "Accuracy")
       (spinner (make-id "gc-pup-accuracy") (list "Weak" "Medium" "Strong") fillwrap
                (lambda (v) '()))))
     (build-grid-selector "gc-pup-escort" "toggle" "Escort")
     (next-button "gc-pup-assoc-" "Going to oestrus, have you finished here?" "gc-oestrus"
                  (lambda () '()))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector "gc-pup-choose" "toggle"
       (db-mongooses-by-pack-pups) #f
       (lambda (individual)
         (list)))
      (populate-grid-selector "gc-pup-escort" "toggle"
       (db-mongooses-by-pack-adults) #t
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
    (make-id "") 'vertical fill gc-col
    (list
     (mtext "" "Oestrus")
     (build-grid-selector "gc-oestrus-female" "single" "Choose female")
     (horiz
      (vert
       (mtext "" "Strength")
       (spinner (make-id "gc-oestrus-strength") (list "Weak" "Medium" "Strong") fillwrap
                (lambda (v) '())))
      (vert
       (mtext "" "Accuracy")
       (spinner (make-id "gc-oestrus-accuracy") (list "Weak" "Medium" "Strong") fillwrap
                (lambda (v) '()))))
     (build-grid-selector "gc-oestrus-guard" "single" "Choose mate guard")
     (next-button "gc-pup-oestrus-" "Going to babysitters, have you finished here?" "gc-babysitting"
                  (lambda () '()))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gc-oestrus-female" "single"
       (db-mongooses-by-pack-female) #f
       (lambda (individual)
         (list)))
      (populate-grid-selector
       "gc-oestrus-guard" "single"
       (db-mongooses-by-pack-male) #f
       (lambda (individual)
       ))))
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
     (next-button "gc-pup-baby-" "Ending, have you finished here?" "gc-end"
                  (lambda () '()))))
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
    (make-id "") 'vertical fill gc-col
    (list
     (mtitle "" "Finish group composition")
     (next-button "gc-pup-baby-" "Ending, have you finished here?" "gc-end"
                  (lambda () (list (finish-activity 0))))))
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
     (mbutton2 "main-manage" "Manage Packs" (lambda () (list (start-activity "manage-packs" 2 ""))))
     (mbutton2 "main-tag" "Tag Location" (lambda () (list (start-activity "tag-location" 2 "")))))
    (mtext "foo" "Your ID")
    (edit-text (make-id "main-id-text") "" 30 "text" fillwrap
               (lambda (v)
                 (set-current! 'user-id v)
                 (update-entity
                  db "local" 1 (list (ktv "user-id" "varchar" v)))))
    (mtext "foo" "Database")
    (mbutton2 "main-sync" "Sync database" (lambda () (list (start-activity "sync" 0 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (setup-database!)
     (let ((user-id (ktv-get (get-entity db "local" 1) "user-id")))
       (set-current! 'user-id user-id)
       (list
        (gps-start "gps" (lambda (loc)
                           (set-current! 'location loc)
                           (list (toast (string-append
                                         (number->string (car loc)) ", "
                                         (number->string (cadr loc)))))))
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
       0 'vertical wrap gc-col
       (list
        (mtoggle-button2 "choose-obs-gc" obs-gc
                         (lambda (v)
                           (set-current! 'observation obs-gc)
                           (mclear-toggles (list "choose-obs-pf" "choose-obs-gp"))))))
      (linear-layout
       0 'vertical wrap pf-col
       (list
        (mtoggle-button2 "choose-obs-pf" obs-pf
                         (lambda (v)
                           (set-current! 'observation obs-pf)
                           (mclear-toggles (list "choose-obs-gc" "choose-obs-gp"))))))
      (linear-layout
       0 'vertical wrap gp-col
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
              (entity-reset!)
              (entity-add-value! "pack" "varchar" (ktv-get (get-current 'pack ()) "unique_id"))
              (set-current! 'group-composition-id (entity-record-values db "stream" "group-composition"))
              (list
               (start-activity "group-composition" 2 ""))))
            (list
             (alert-dialog
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
       (db-all-sort-normal db "sync" "pack") #f
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
   "group-composition"
    (linear-layout
     0 'vertical fillwrap gc-bgcol
     (list
      (text-view (make-id "obs-title") "" 40 fillwrap)
      (build-fragment "gc-start" (make-id "gc-top") (layout 'fill-parent 520 1 'left 0))
      (build-fragment "events" (make-id "event-holder") (layout 'fill-parent 520 1 'left 0))
      (mbutton "gc-done" "Done" (lambda () (list (finish-activity 0))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
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
    0 'vertical fillwrap pf-bgcol
    (list
     (vert
      (mtitle "" "Pup focal setup")
      (mtext "pf1-pack" "Pack")
      (build-grid-selector "pf1-grid" "single" "Select pup")
      (horiz
       (medit-text "pf1-width" "Pack width - left to right" "numeric"
                   (lambda (v) (entity-add-value! "pack-width" "int" v) '()))
       (medit-text "pf1-height" "Pack depth - front to back" "numeric"
                   (lambda (v) (entity-add-value! "pack-depth" "int" v) '())))
      (medit-text "pf1-count" "How many mongooses can you see?" "numeric"
                  (lambda (v) (entity-add-value! "pack-count" "int" v) '()))
      (horiz
       (mbutton2 "pf1-back" "Back" (lambda () (list (finish-activity 1))))
       (mbutton2 "pf1-done" "Done"
                 (lambda ()
                   (cond
                    ((current-exists? 'individual)
                     (set-current! 'pup-focal-id (entity-record-values db "stream" "pup-focal"))
                     (set-current! 'timer-minutes pf-length)
                     (set-current! 'timer-seconds 0)
                     (list
                      (start-activity "pup-focal" 2 "")))
                    (else
                     (list
                      (alert-dialog
                       "pup-focal-check"
                       "You need to specify an pup for the focal"
                       (lambda () '()))))))))
      )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-reset!)
     (list
      (populate-grid-selector
       "pf1-grid" "single"
       (db-mongooses-by-pack-pups) #f
       (lambda (individual)
         (set-current! 'individual individual)
         (entity-add-value! "id-focal-subject" "varchar" (ktv-get individual "unique_id"))
         '()))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "pup-focal"
    (linear-layout
     0 'vertical fillwrap pf-bgcol
     (list
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
                             (list (delayed "timer" 1000 timer-cb))))))
      (build-fragment "pf-timer" (make-id "pf-top") (layout 'fill-parent 515 1 'left 0))
      (build-fragment "events" (make-id "event-holder") (layout 'fill-parent 515 1 'left 0))
      (mbutton "pf-done" "Done"
               (lambda ()
                 (list
                  (alert-dialog
                   "pup-focal-end-done"
                   "Finish pup focal are you sure?"
                   (lambda (v)
                     (cond
                      ((eqv? v 1)
                       (list (finish-activity 1)))
                      (else
                       (list))))))))))

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
   (linear-layout
    0 'vertical fill gp-col
    (list
     (build-fragment "events" (make-id "event-holder") (layout 'fill-parent 515 1 'left 0))
     (horiz
      (mbutton "gpe-done" "Done" (lambda () (list (finish-activity 0)))))))
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
       "manage-packs-list" "button" (db-all-sort-normal db "sync" "pack") #f
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
               (lambda (v) (entity-add-value! "name" "varchar" v) '()))
    (spacer 10)
    (horiz
     (mbutton2 "new-pack-cancel" "Cancel"
               (lambda () (entity-reset!) (list (finish-activity 2))))
     (mbutton2 "new-pack-done" "Done"
               (lambda ()
                 (entity-record-values db "sync" "pack")
                 (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-reset!)
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
     (mbutton2 "manage-individuals-new" "New individual" (lambda () (list (start-activity "new-individual" 2 "")))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "manage-individuals-list" "button"
       (db-mongooses-by-pack-ignore-delete) #f
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
               (lambda (v) (entity-add-value! "name" "varchar" v) '()))
    (text-view (make-id "new-individual-name-text") "Gender" 30 fillwrap)
    (spinner (make-id "new-individual-gender") (list "Female" "Male" "Unknown") fillwrap
             (lambda (v) (entity-add-value! "gender" "varchar" v) '()))
    (text-view (make-id "new-individual-dob-text") "Date of Birth" 30 fillwrap)
    (horiz
     (text-view (make-id "new-individual-dob") (date->string (date-time)) 25 fillwrap)
     (button (make-id "date") "Set date" 30 fillwrap
             (lambda ()
               (list (date-picker-dialog
                      "new-individual-date"
                      (lambda (day month year)
                        (let ((datestring (date->string (list year (+ month 1) day))))
                          (entity-add-value! "dob" "varchar" datestring)
                          (list
                           (update-widget
                            'text-view
                            (get-id "new-individual-dob") 'text datestring))))))))
     (button (make-id "unknown-date") "Unknown" 30 fillwrap
             (lambda ()
               (entity-add-value! "dob" "varchar" "Unknown")
               (list (update-widget 'text-view (get-id "update-individual-dob") 'text "Unknown"))))
     )
    (text-view (make-id "new-individual-litter-text") "Litter code" 30 fillwrap)
    (edit-text (make-id "new-individual-litter-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "litter-code" "varchar" v) '()))
    (text-view (make-id "new-individual-chip-text") "Chip code" 30 fillwrap)
    (edit-text (make-id "new-individual-chip-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "chip-code" "varchar" v) '()))
    (horiz
     (mbutton2 "new-individual-cancel" "Cancel"
               (lambda () (entity-reset!) (list (finish-activity 2))))
     (mbutton2 "new-individual-done" "Done"
               (lambda ()
                 (entity-add-value! "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
                 (entity-record-values db "sync" "mongoose")
                 (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-reset!)
     ;; make sure all fields exist
     (entity-add-value! "name" "varchar" "noname")
     (entity-add-value! "gender" "varchar" "Female")
     (entity-add-value! "dob" "varchar" "00-00-00")
     (entity-add-value! "litter-code" "varchar" "")
     (entity-add-value! "chip-code" "varchar" "")
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
               (lambda (v) (entity-add-value! "name" "varchar" v) '()))
    (text-view (make-id "update-individual-name-text") "Gender" 30 fillwrap)
    (spinner (make-id "update-individual-gender") (list "Female" "Male" "Unknown") fillwrap
             (lambda (v) (entity-add-value! "gender" "varchar" v) '()))
    (text-view (make-id "update-individual-dob-text") "Date of Birth" 30 fillwrap)
    (horiz
     (text-view (make-id "update-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 30 fillwrap
             (lambda ()
               (list (date-picker-dialog
                      "update-individual-date"
                      (lambda (day month year)
                        (let ((datestring (date->string (list year (+ month 1) day))))
                          (entity-add-value! "dob" "varchar" datestring)
                          (list
                           (update-widget
                            'text-view
                            (get-id "update-individual-dob") 'text datestring))))))))
     (button (make-id "update-unknown-date") "Unknown" 30 fillwrap
             (lambda ()
               (entity-add-value! "dob" "varchar" "Unknown")
               (list (update-widget 'text-view (get-id "update-individual-dob") 'text "Unknown"))))
     )

    (text-view (make-id "update-individual-litter-text") "Litter code" 30 fillwrap)
    (edit-text (make-id "update-individual-litter-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "litter-code" "varchar" v) '()))
    (text-view (make-id "update-individual-chip-text") "Chip code" 30 fillwrap)
    (edit-text (make-id "update-individual-chip-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "chip-code" "varchar" v) '()))
    (spacer 10)
    (horiz
     (mtoggle-button2 "update-individual-delete" "Delete"
                      (lambda (v)
                        (entity-add-value! "deleted" "int" (if v 1 0))
                        (list)))
     (mtoggle-button2 "update-individual-died" "Died"
                      (lambda (v)
                        (entity-add-value! "deleted" "int" (if v 2 0))
                        (list))))
    (horiz
     (mbutton2 "update-individual-cancel" "Cancel"
               (lambda () (entity-reset!) (list (finish-activity 2))))
     (mbutton2 "update-individual-done" "Done"
               (lambda ()
                 (entity-update-values db "sync")
                 (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-reset!)
     (entity-set! (get-current 'individual '()))
     (let ((individual (get-current 'individual '())))
       (msg "deleted = " (ktv-get individual "deleted"))
       (list
        (update-widget 'edit-text (get-id "update-individual-name") 'text
                       (ktv-get individual "name"))
        (update-widget 'text-view (get-id "update-individual-dob") 'text
                       (ktv-get individual "dob"))
        (update-widget 'spinner (get-id "update-individual-gender") 'selection
                       (cond
                        ((equal? (ktv-get individual "gender") "Female") 0)
                        ((equal? (ktv-get individual "gender") "Male") 1)
                        (else 2)))
        (update-widget 'edit-text (get-id "update-individual-litter-code") 'text
                       (ktv-get individual "litter-code"))
        (update-widget 'edit-text (get-id "update-individual-chip-code") 'text
                       (ktv-get individual "chip-code"))

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
    (horiz
     (mtoggle-button2 "sync-all" "Sync me" (lambda (v) (set-current! 'sync-on v)))
     (mbutton2 "sync-syncall" "Push all"
               (lambda ()
                 (let ((r (append
                           (spit db "sync" (dirty-and-all-entities db "sync"))
                           (spit db "stream" (dirty-and-all-entities db "stream")))))
                   (cons (toast "Uploading data...") r)))))
    (mtitle "" "Export data")
    (horiz
     (mbutton2 "sync-download" "Download"
               (lambda ()
                 (debug! (string-append "Downloading whole db"))
                 (append
                 (foldl
                  (lambda (e r)
                    (debug! (string-append "Downloading /sdcard/mongoose/" e ".csv"))
                    (cons
                     (http-download
                      (string-append "getting-" e)
                      (string-append url "fn=entity-csv&table=stream&type=" e)
                      (string-append "/sdcard/mongoose/" e ".csv"))
                     r))
                  (list
                   (http-download
                    "getting-db"
                    "http://192.168.2.1:8888/mongoose.db"
                    (string-append "/sdcard/mongoose/mongoose.db"))
                   )
                  entity-types)
                 (list))))
     (mbutton2 "sync-export" "Email"
               (lambda ()
                 (debug! "Sending mail")
                 (list
                  (send-mail
                   ""
                   "From Mongoose2000" "Please find attached your mongoose data"
                   (cons
                    "/sdcard/mongoose/mongoose.db"
                    (map
                     (lambda (e)
                       (string-append "/sdcard/mongoose/" e ".csv"))
                     entity-types))))))
     (mbutton2 "sync-export" "Email local data"
               (lambda ()
                 (debug! "Sending mail")
                 (list
                  (send-mail
                   ""
                   "From Mongoose2000" "Please find attached your local mongoose data"
                   (list "/sdcard/mongoose/local-mongoose.db")))))
     )
    (spacer 10)
    (mtitle "" "Debug")
    (scroll-view-vert
     0 (layout 'fill-parent 200 1 'left 0)
     (list
      (vert
       (debug-text-view (make-id "sync-debug") "..." 15 (layout 'fill-parent 400 1 'left 0)))))
    (spacer 10)
    (horiz
     (mbutton2 "sync-back" "Back" (lambda () (list (finish-activity 1))))
     (mbutton2 "sync-send" "[Prof]" (lambda () (prof-print) (list))))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'sync-on #f)
     (append
      (debug-timer-cb)
      (list
       (update-widget 'debug-text-view (get-id "sync-debug") 'text (get-current 'debug-text ""))
       (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty))
       )))
   (lambda (activity) '())
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  )
