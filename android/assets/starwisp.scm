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

(define (dt->string dt)
  (string-append
   (number->string (list-ref dt 0)) "-"
   (number->string (list-ref dt 1)) "-"
   (number->string (list-ref dt 2)) "T"
   (number->string (list-ref dt 3)) ":"
   (number->string (list-ref dt 4)) ":"
   (substring (number->string (+ 100 (list-ref dt 5))) 1 2)))

;; build entity from all ktvs, insert to db, return unique_id
(define (entity-record-values db table type)
  ;; standard bits
  (entity-add-value! "user" "varchar" (get-current 'user-id "none"))
  (entity-add-value! "time" "varchar" (dt->string (date-time)))
  (entity-add-value! "lat" "real" 0)
  (entity-add-value! "lon" "real" 0)
  (let ((values (get-current 'entity-values '())))
    (msg values)
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
  (button (make-id id) title 20 (layout 150 100 1 'centre 0) fn))

(define (mtoggle-button id title fn)
  (toggle-button (make-id id) title 20 fillwrap fn))

(define (mtoggle-button2 id title fn)
  (toggle-button (make-id id) title 20 (layout 150 100 1 'centre 0) fn))

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

;;;;

(define (build-grid-selector name type title)
  (vert
   (mtext "title" title)
   (linear-layout
    0 'horizontal
    (layout 'fill-parent 'fill-parent 1 'left 2) trans-col
    (list
     (image-view (make-id "im") "arrow_left" (layout 100 'fill-parent 1 'left 0))
     (scroll-view
      (make-id "scroller")
      (layout 'wrap-content 'wrap-content 1 'left 20)
      (list
       (linear-layout
        (make-id name) 'horizontal
        (layout 'wrap-content 'wrap-content 1 'centre 20) trans-col
        (list
         (button-grid (make-id name) type 3 20 (layout 100 40 1 'left 40)
                      (list) (lambda (v) '()))))))
     (image-view (make-id "im") "arrow_right" (layout 100 'fill-parent 1 'right 0))))))

;; assumes grid selectors on mongeese only
(define (fast-get-name item)
  (list-ref (list-ref item 1) 2))

(define (build-button-items name items)
  (map
   (lambda (item)
     (let ((item-name (fast-get-name item)))
       (list (make-id (string-append name item-name))
             item
             item-name)))
   items))

(define (populate-grid-selector name type items fn)
  (let ((id->items (build-button-items name items))
        (selected-set '()))
    (update-widget
     'button-grid (get-id name) 'grid-buttons
     (list
      type 3 20 (layout 100 40 1 'left 0)
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
          (msg (findv v id->items))
          (fn (cadr (findv v id->items))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (timer-cb)
  (set-current!
   'timer-seconds
   (- (get-current 'timer-seconds 59) 1))
  (append
   (cond
    ((< (get-current 'timer-seconds 59) 0)
     (set-current! 'timer-minutes (- (get-current 'timer-minutes 20) 1))
     (set-current! 'timer-seconds 59)
     (list
      (replace-fragment (get-id "pf-top") "pf-scan1")))
    (else '()))
   (list
    (delayed "timer" 1000 timer-cb)
    (update-widget
     'text-view (get-id "pf-timer-time-minutes") 'text
     (string-append (number->string (get-current 'timer-minutes 20))))
    (update-widget
     'text-view (get-id "pf-timer-time") 'text
     (string-append (number->string (get-current 'timer-seconds 59))))
    )))

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
      (make-id "ev-pf") 'vertical wrapfill pf-col
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
        (mbutton2 "evb-grpmov" "Movement" (lambda () (list (replace-fragment (get-id "event-holder") "ev-grpmov")))))))))
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
     (mtext "title" "Nearest Neighbour Scan")
     (build-grid-selector "pf-scan-nearest" "single" "Closest Mongoose")
     (build-grid-selector "pf-scan-close" "toggle" "Mongooses within 2m")
     (mbutton "pf-scan-done" "Done"
              (lambda ()
                (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                (entity-record-values db "stream" "pup-focal-nearest")
                (list (replace-fragment (get-id "pf-top") "pf-timer"))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "pf-scan-nearest" "single"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (entity-add-value! "id-nearest" "varchar" (ktv-get individual "unique_id"))
         (list)))
      (populate-grid-selector
       "pf-scan-close" "toggle"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
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
     (mtext "text" "Food size")
     (horiz
      (spinner (make-id "pf-pupfeed-size") (list "Small" "Medium" "Large") fillwrap
               (lambda (v)
                 (entity-add-value! "size" "varchar" v) '()))
      (mbutton "pf-pupfeed-done" "Done"
               (lambda ()
                 (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values db "stream" "pup-focal-pupfeed")
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "pf-pupfeed-who" "single"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
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
     (mtext "text" "Food size")
     (horiz
      (spinner (make-id "pf-pupfind-size") (list "Small" "Medium" "Large") fillwrap
               (lambda (v) (entity-add-value! "size" "varchar" v) '()))
      (mbutton "pf-pupfind-done" "Done"
               (lambda ()
                 (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values db "stream" "pup-focal-pupfind")
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
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
     (mtext "text" "Type of care")
     (horiz
      (spinner (make-id "pf-pupcare-type") (list "Carry" "Lead" "Sniff" "Play" "Ano-genital sniff") fillwrap
               (lambda (v)
                 (entity-add-value! "type" "varchar" v) '()))
      (mbutton "pf-pupcare-done" "Done"
               (lambda ()
                 (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                 (entity-record-values db "stream" "pup-focal-pupcare")
                 (list (replace-fragment (get-id "event-holder") "events")))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "pf-pupcare-who" "single"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
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
       (mtoggle-button "pf-pupaggr-in" "Initiate?"
                       (lambda (v)
                         (entity-add-value! "initiate" "varchar" (if v "yes" "no")) '()))
       (mtoggle-button "pf-pupaggr-win" "Win?"
                       (lambda (v)
                         (entity-add-value! "win" "varchar" (if v "yes" "no")) '()))))
     (mbutton "pf-pupaggr-done" "Done"
              (lambda ()
                (entity-add-value! "parent" "varchar" (get-current 'pup-focal-id ""))
                (entity-record-values db "stream" "pup-focal-pupaggr")
                (list (replace-fragment (get-id "event-holder") "events"))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "pf-pupaggr-partner" "single"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
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
     (mtitle "title" "Event: Group Interaction")
     (build-grid-selector "gp-int-pack" "single" "Inter-group interaction: Other pack identity")
     (build-grid-selector "gp-int-leader" "single" "Leader")
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 80 '1 'left 0) trans-col
      (list
       (vert
        (mtext "text" "Outcome")
        (spinner (make-id "gp-int-out") (list "Retreat" "Advance" "Fight & retreat" "Fight & win") fillwrap
                 (lambda (v)
                   (entity-add-value! "outcome" "varchar" v) '())))
       (vert
        (mtext "text" "Duration")
        (edit-text (make-id "gp-int-dur") "" 20 "numeric" fillwrap
                   (lambda (v) (entity-add-value! "duration" "int" (string->number v)) '())))
       (mbutton "pf-grpint-done" "Done"
                (lambda ()
                  (entity-record-values db "stream" "group-interaction")
                  (list (replace-fragment (get-id "event-holder") "events"))))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gp-int-pack" "single"
       (db-all db "sync" "pack")
       (lambda (pack)
         (entity-add-value! "id-other-pack" "varchar" (ktv-get pack "unique_id"))
         (list)))
      (populate-grid-selector
       "gp-int-leader" "single"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (entity-add-value! "id-leader" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
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
     (mtext "text" "Cause")
     (horiz
      (spinner (make-id "gp-alarm-cause") (list "Predator" "Other mongoose pack" "Humans" "Other" "Unknown") fillwrap
               (lambda (v)
                 (entity-add-value! "cause" "varchar" v) '()))
      (mtoggle-button "gp-alarm-join" "Did the others join in?"
                      (lambda (v)
                        (entity-add-value! "others-join" "varchar"
                                           (if v "yes" "no")) '())))
     (mbutton "pf-grpalarm-done" "Done"
              (lambda ()
                (entity-record-values db "stream" "group-alarm")
                (list (replace-fragment (get-id "event-holder") "events"))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gp-alarm-caller" "single"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (entity-add-value! "id-caller" "varchar" (ktv-get individual "unique_id"))
         (list)))
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
     (mtitle "title" "Event: Group movement")
     (build-grid-selector "gp-mov-leader" "single" "Leader")
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 90 '1 'left 0) trans-col
      (list
       (medit-text "gp-mov-w" "Width" "numeric"
                   (lambda (v) (entity-add-value! "pack-width" "int" (string->number v)) '()))
       (medit-text "gp-mov-l" "Length" "numeric"
                   (lambda (v) (entity-add-value! "pack-height" "int" (string->number v)) '()))
       (medit-text "gp-mov-l" "How many" "numeric"
                   (lambda (v) (entity-add-value! "pack-count" "int" (string->number v)) '()))))
     (linear-layout
      (make-id "") 'horizontal (layout 'fill-parent 90 '1 'left 0) trans-col
      (list
       (vert
        (mtext "" "Where to")
        (spinner (make-id "gp-mov-to") (list "Latrine" "Water" "Food" "Nothing" "Unknown") fillwrap
                 (lambda (v) (entity-add-value! "destination" "varchar" v)  '())))
       (mbutton "pf-grpmov-done" "Done"
                (lambda ()
                  (entity-record-values db "stream" "group-move")
                  (list (replace-fragment (get-id "event-holder") "events"))))))))

   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (populate-grid-selector
       "gp-mov-leader" "single"
       (db-all-where db "sync" "mongoose"
                     (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
       (lambda (individual)
         (entity-add-value! "id-leader" "varchar" (ktv-get individual "unique_id"))
         (list)))
      ))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  (fragment
   "gc-start"
   (linear-layout
    (make-id "") 'vertical fillwrap gc-col
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
    (make-id "") 'vertical fillwrap gc-col
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
    (make-id "") 'vertical fillwrap gc-col
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
    (make-id "") 'vertical fillwrap gc-col
    (list
     (mtitle "title" "Pup Associations")
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
    (make-id "") 'vertical fillwrap gc-col
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
    (make-id "") 'vertical fillwrap gc-col
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
    (make-id "") 'vertical fillwrap gc-col
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
    (text-view (make-id "main-title") "Mongoose 2000" 40 fillwrap)
    (text-view (make-id "main-about") "Advanced mongoose technology" 20 fillwrap)
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
    (horiz
     (mbutton2 "main-send" "Email" (lambda () (list)))
     (mbutton2 "main-sync" "Sync" (lambda () (list (start-activity "sync" 0 ""))))))
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
             (list (start-activity "group-composition" 2 ""))))
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
   "group-composition"
    (linear-layout
     0 'vertical fillwrap gc-bgcol
     (list
      (text-view (make-id "obs-title") "" 40 fillwrap)
      (linear-layout
       (make-id "obs-buttons-bar") 'horizontal fillwrap trans-col '())
      (build-fragment "gc-start" (make-id "gc-top") (layout 595 400 1 'left 0))
      (build-fragment "events" (make-id "event-holder") (layout 595 450 1 'left 0))
      (mbutton "gc-done" "Done" (lambda () (list (finish-activity 0))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (msg (get-current 'observation-fragments '()))
     (list
      (update-widget 'linear-layout (get-id "obs-buttons-bar") 'contents
                     (let ((all-toggles
                            (map
                             (lambda (i) (string-append "obs-bar-" (cadr i)))
                             (get-current 'observation-fragments '()))))
                       (map
                        (lambda (frag)
                          (msg "button-bar" frag)
                          (let ((id (string-append "obs-bar-" (cadr frag))))
                            (toggle-button
                             (make-id id) (car frag) 12 fillwrap
                             (lambda (v)
                               (append
                                (mclear-toggles-not-me id all-toggles)
                                (list
                                 (replace-fragment (get-id "gc-top") (cadr frag))))))))
                        (get-current 'observation-fragments '()))))
      (update-widget 'text-view (get-id "obs-title") 'text
                     (string-append
                      (get-current 'observation "No observation")
                      " with " (ktv-get (get-current 'pack '()) "name")))
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
       (medit-text "pf1-width" "Pack width" "numeric"
                   (lambda (v) (entity-add-value! "pack-width" "int" v) '()))
       (medit-text "pf1-height" "Pack height" "numeric"
                   (lambda (v) (entity-add-value! "pack-height" "int" v) '())))
      (medit-text "pf1-count" "How many mongooses present?" "numeric"
                  (lambda (v) (entity-add-value! "pack-count" "int" v) '()))
      (mbutton "pf1-done" "Done"
               (lambda ()
                 (set-current! 'pup-focal-id (entity-record-values db "stream" "pup-focal"))
                 (list
                  (start-activity "pup-focal" 2 ""))))
      )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (populate-grid-selector
       "pf1-grid" "single"
       (db-all-where db "sync" "mongoose" (list "pack-id" (ktv-get (get-current 'pack '()) "unique_id")))
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
       (vert
        (mtext "title" "Time left:")
        (mtitle "pf-timer-time-minutes" "20"))
       (vert
        (mtext "title" "Next scan:")
        (mtitle "pf-timer-time" "60"))
       (mtoggle-button "pf-pause" "Pause"
                       (lambda (v)
                         (msg "pausing")
                         (if v
                             (list (delayed "timer" 1000 (lambda () '())))
                             (list (delayed "timer" 1000 timer-cb))))))
      (build-fragment "pf-timer" (make-id "pf-top") (layout 595 400 1 'left 0))
      (build-fragment "events" (make-id "event-holder") (layout 595 450 1 'left 0))
      (mbutton "pf-done" "Done" (lambda () (list (finish-activity 0))))))

    (lambda (activity arg)
      (activity-layout activity))
    (lambda (activity arg)
      (set-current! 'timer-minutes 20)
      (set-current! 'timer-seconds 59)
      (list
       (delayed "timer" 1000 timer-cb)))
    (lambda (activity) '())
    (lambda (activity) (list (delayed "timer" 1000 (lambda () '()))))
    (lambda (activity) (list (delayed "timer" 1000 (lambda () '()))))
    (lambda (activity) '())
    (lambda (activity requestcode resultcode) '()))


  (activity
   "group-events"
   (linear-layout
    0 'vertical wrap gp-col
    (list
     (build-fragment "events" (make-id "event-holder") (layout 580 450 1 'left 0))
     (horiz
      (mbutton "gpe-save" "Save" (lambda () (list)))
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
               (lambda (v) (entity-add-value! "name" "varchar" v) '()))
    (spacer 10)
    (horiz
     (button (make-id "new-pack-cancel") "Cancel" 20 fillwrap
             (lambda () (entity-reset!) (list (finish-activity 2))))
     (button (make-id "new-pack-done") "Done" 20 fillwrap
             (lambda ()
               (entity-record-values db "sync" "pack")
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
    (text-view (make-id "new-individual-pack-name") "Pack:" 20 fillwrap)
    (text-view (make-id "new-individual-name-text") "Name" 20 fillwrap)
    (edit-text (make-id "new-individual-name") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "name" "varchar" v) '()))
    (text-view (make-id "new-individual-name-text") "Gender" 20 fillwrap)
    (spinner (make-id "new-individual-gender") (list "Female" "Male") fillwrap
             (lambda (v) (entity-add-value! "gender" "varchar" v) '()))
    (text-view (make-id "new-individual-dob-text") "Date of Birth" 20 fillwrap)
    (horiz
     (text-view (make-id "new-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 20 fillwrap (lambda () '())))
    (text-view (make-id "new-individual-litter-text") "Litter code" 20 fillwrap)
    (edit-text (make-id "new-individual-litter-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "litter-code" "varchar" v) '()))
    (text-view (make-id "new-individual-chip-text") "Chip code" 20 fillwrap)
    (edit-text (make-id "new-individual-chip-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "chip-code" "varchar" v) '()))
    (horiz
     (button (make-id "new-individual-cancel") "Cancel" 20 fillwrap
             (lambda () (entity-reset!) (list (finish-activity 2))))
     (button (make-id "new-individual-done") "Done" 20 fillwrap
             (lambda ()
               (entity-add-value! "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
               (entity-record-values db "sync" "mongoose")
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
    (edit-text (make-id "update-individual-name") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "name" "varchar" v) '()))
    (text-view (make-id "update-individual-name-text") "Gender" 20 fillwrap)
    (spinner (make-id "update-individual-gender") (list "Female" "Male") fillwrap
             (lambda (v) (entity-add-value! "gender" "varchar" v) '()))
    (text-view (make-id "update-individual-dob-text") "Date of Birth" 20 fillwrap)
    (horiz
     (text-view (make-id "update-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 20 fillwrap (lambda () '())))
    (text-view (make-id "update-individual-litter-text") "Litter code" 20 fillwrap)
    (edit-text (make-id "update-individual-litter-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "litter-code" "varchar" v) '()))
    (text-view (make-id "update-individual-chip-text") "Chip code" 20 fillwrap)
    (edit-text (make-id "update-individual-chip-code") "" 30 "text" fillwrap
               (lambda (v) (entity-add-value! "chip-code" "varchar" v) '()))
    (spacer 10)
    (horiz
     (button (make-id "update-individual-delete") "Delete" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "update-individual-died") "Died" 20 fillwrap (lambda () (list (finish-activity 2)))))
    (horiz
     (button (make-id "update-individual-cancel") "Cancel" 20 fillwrap
             (lambda () (entity-reset!) (list (finish-activity 2))))
     (button (make-id "update-individual-done") "Done" 20 fillwrap
             (lambda ()
               (entity-update-values db "sync")
               (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-set! (get-current 'individual '()))
     (let ((individual (get-current 'individual '())))
       (list
        (update-widget 'edit-text (get-id "update-individual-name") 'text
                       (ktv-get individual "name"))
        (update-widget 'spinner (get-id "update-individual-gender") 'selection
                       (if (equal? (ktv-get individual "gender") "Female") 0 1))
        (update-widget 'edit-text (get-id "update-individual-litter-code") 'text
                       (ktv-get individual "litter-code"))
        (update-widget 'edit-text (get-id "update-individual-chip-code") 'text
                       (ktv-get individual "chip-code")))

       ))
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
     (mbutton2 "sync-connect" "Connect"
              (lambda ()
                (list
                 (network-connect
                  "network"
                  "mongoose-web"
                  (lambda (state)
                      (list
                       (update-widget 'text-view (get-id "sync-connect") 'text state)))))))
     (mbutton2 "sync-sync" "Push"
              (lambda ()
                (let ((r (append
                          (spit-dirty db "sync")
                          (spit-dirty db "stream"))))
                  (cons (if (> (length r) 0)
                            (toast "Uploading data...")
                            (toast "No data changed to upload")) r))))
     (mbutton2 "sync-pull" "Pull"
              (lambda ()
                (cons (toast "Downloading data...") (suck-new db "sync")))))
    (text-view (make-id "sync-console") "..." 15 (layout 300 'wrap-content 1 'left 0))
    (horiz
     (mbutton2 "sync-prof" "Profile" (lambda () (prof-print) '()))
     (mbutton2 "sync-prof" "CSV"
               (lambda ()
                 (for-each
                  (lambda (e)
                    (msg e)
                    (msg (csv db "stream" e)))
                  entity-types)
                 '()))
     (mbutton2 "sync-send" "Done" (lambda () (list (finish-activity 2)))))


    )

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
