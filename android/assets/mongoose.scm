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
   "group-move"
   "group-composition"
   "weight"
   "pup-assoc"
   "mate-guard"
   ))

(define download-entity-types
  (list
   "pup-focal"
   ))

(define pup-focal-export
  (list
   "pup-focal-nearest"
   "pup-focal-pupfeed"
   "pup-focal-pupfind"
   "pup-focal-pupcare"
   "pup-focal-pupaggr"))

(define list-sizes (list
                    (list 'small "Small")
                    (list 'medium "Medium")
                    (list 'large "Large")))

(define list-pupcare-type
  (list (list 'carry "Carry")
        (list 'lead "Lead")
        (list 'sniff "Sniff")
        (list 'play "Play")
        (list 'sniff "Ano-genital sniff")))

(define list-aggression-over
  (list (list 'food "Food")
        (list 'escort "Escort")
        (list 'nothing "Nothing")
        (list 'other "Other")))

(define list-aggression-level
  (list (list 'block "Block")
        (list 'snap "Snap")
        (list 'chase "Chase")
        (list 'push "Push")
        (list 'fight "Fight")))

(define list-interaction-outcome
  (list (list 'retreat "Retreat")
        (list 'advance "Advance")
        (list 'fight-retreat "Fight retreat")
        (list 'fight-win "Fight win")))

(define list-alarm-cause
  (list (list 'predator "Predator")
        (list 'other-pack "Other mongoose pack")
        (list 'humans "Humans")
        (list 'other "Other")
        (list 'unknown "Unknown")))

(define list-move-direction
  (list (list 'to "To")
        (list 'from "From")))

(define list-move-to
  (list (list 'latrine "Latrine")
        (list 'water "Water")
        (list 'food "Food")
        (list 'nothing "Nothing")
        (list 'den "Den")
        (list 'unknown "Unknown")))

(define list-strength
  (list
   (list 'none "None")
   (list 'weak "Weak")
   (list 'medium "Medium")
   (list 'strong "Strong")))

(define list-gender
  (list (list 'male "Male")
        (list 'female "Female")
        (list 'unknown "Unknown")))


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
(define main-db "/sdcard/mongoose/mongoose.db")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface abstraction

(define (mbutton id title fn)
  (button (make-id id) title 20 (layout 'fill-parent 'wrap-content 1 'centre 5) fn))

(define (mbutton2 id title fn)
  (button (make-id id) title 20 (layout 150 100 1 'centre 5) fn))

(define (mbutton-small id title fn)
  (button (make-id id) title 30 (layout 'wrap-content 'wrap-content -1 'right 5) fn))

(define (mtoggle-button id title fn)
  (toggle-button (make-id id) title 20 (layout 'fill-parent 'wrap-content 1 'centre 5) "fancy" fn))

(define (mtoggle-button-yes id title fn)
  (toggle-button (make-id id) title 20 (layout 49 43 1 'centre 0) "yes" fn))

(define (mtoggle-button-maybe id title fn)
  (toggle-button (make-id id) title 20 (layout  49 43 1 'centre 0) "maybe" fn))

(define (mtoggle-button-no id title fn)
  (toggle-button (make-id id) title 20 (layout  49 43 1 'centre 0) "no" fn))

(define (mtoggle-button2 id title fn)
  (toggle-button (make-id id) title 20 (layout 150 100 1 'centre 5) "plain" fn))

(define (mspinner id list fn)
  (spinner (make-id id) (map cadr list) fillwrap fn))

(define (mtext id text)
  (text-view (make-id id) text 20 fillwrap))

(define (mtitle id text)
  (text-view (make-id id) text 40 fillwrap))

(define (medit-text id text type fn)
  (vert
   (mtext (string-append id "-title") text)
   (edit-text (make-id id) "" 20 type fillwrap fn)))

(define (medit-text-value id text value type fn)
  (vert
   (mtext (string-append id "-title") text)
   (edit-text (make-id id) value 20 type fillwrap fn)))

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
       (layout 'wrap-content 'wrap-content 1 'left 5)
       (list
        (linear-layout
         (make-id name) 'horizontal
         (layout 'wrap-content 'wrap-content 1 'centre 5) trans-col
         (list
          (button-grid (make-id name) type 3 20 (layout 100 60 1 'left 5)
                       (list) (lambda (v) '()))))))
      (image-view (make-id "im") "arrow_right" (layout 200 'fill-parent 1 'right 0)))))))

;; assumes grid selectors on mongeese only
;; assumes order of ktv elements?
(define (fast-get-name item)
  (list-ref (list-ref item 1) 2))

(define (fast-get-id item)
  (list-ref (list-ref item 0) 2))

(define (build-button-items name items unknown)
  (append
   (map
    (lambda (item)
      (list (make-id (string-append name (fast-get-id item)))
            item (fast-get-name item)))
    items)
   (if unknown
       (list
        (list (make-id (string-append name "-unknown"))
              (list (ktv "name" "varchar" "Unknown")
                    (ktv "unique_id" "varchar" "Unknown"))
              "???"))
       '())))

(define (populate-grid-selector name type items unknown fn . args)
  (let ((id->items (build-button-items name items unknown))
        (selected-set (if (null? args)
                          '()
                          (foldl
                           (lambda (uid r)
                             (if (not (equal? uid "none"))
                                 (cons (get-id (string-append name uid)) r) r))
                           '()
                           (car args)))))
    (let ((r (update-widget
              'button-grid (get-id name) 'grid-buttons
              (list
               type 3 20 (layout 80 50 1 'left 2)
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
      r)))


(define (update-grid-selector-colours id item-id items)
  (map
   (lambda (item)
     (update-widget 'button (get-id (string-append id (ktv-get item item-id)))
                    'background-colour (list 255 255 0 155)))
   items))

(define (update-grid-selector-enabled id items)
  (map
   (lambda (item)
     (update-widget 'button (get-id (string-append id item))
                    'set-enabled 0))
   items))

(define (update-grid-selector-checked id items-id)
  (let ((items-str (entity-get-value items-id)))
    (msg "selector-checked for" id items-id items-str)
    (if items-str
        (map
         (lambda (item)
           (update-widget 'toggle-button (get-id (string-append id item)) 'checked 1))
         (string-split-simple items-str #\,))
        '())))

(define (get-grid-select-init-state key)
  (let ((v (entity-get-value key)))
    (if v
        (string-split-simple v #\,)
        '())))

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
           (entity-set-value! key "varchar" "yes")
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
           (entity-set-value! key "varchar" "maybe")
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
           (entity-set-value! key "varchar" "no")
           (list
            (update-widget 'toggle-button (get-id (string-append id "-y")) 'checked 0)
            (update-widget 'toggle-button (get-id (string-append id "-m")) 'checked 0)))
          (else
           (list
            (update-widget 'toggle-button (get-id (string-append id "-n")) 'checked 1))))
         ))))

    (text-view 0 text 30 (layout 'wrap-content 'wrap-parent '1 'centre 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review

(define (ktv-key-is-id? ktv)
  (or
   (equal? (ktv-key ktv) "pack")
   (equal? (ktv-key ktv) "present")
   (equal? (substring (ktv-key ktv) 0 3) "id-")))

;; search for a comma in a list of ids
(define (ktv-value-is-list? ktv)
  (foldl
   (lambda (c r)
     (if (or r (eqv? c #\,)) #t r))
   #f
   (string->list (ktv-value ktv))))

(define (uid->name uid)
  (let* ((entity-id (entity-id-from-unique db "sync" uid)))
    (ktv-get (get-entity-only db "sync" entity-id
                              (list (list "name" "varchar")))
             "name")))

(define (review-build-id ktv)
  (list (medit-text-value
         (string-append (ktv-value ktv) (ktv-key ktv))
         (ktv-key ktv)
         (uid->name (ktv-value ktv)) "normal"
         (lambda (v)
           (entity-set-value! (ktv-key ktv) (ktv-type ktv) v)
           '()))))

(define (review-build-list ktv)
  (let ((ids (string-split-simple (ktv-value ktv) #\,)))
    (list (medit-text-value
           (ktv-key ktv)
           (ktv-key ktv)
           (foldl
            (lambda (id r)
              (if (equal? r "")
                  (uid->name id)
                  (string-append r ", " (uid->name id))))
            ""
            ids)
           "normal"
           (lambda (v)
             (entity-set-value! (ktv-key ktv) (ktv-type ktv) v)
             '())))))


(define (convert-id name)
  (let ((name (string-remove-whitespace name)))
    ;; search for unique id first
    (if (entity-exists? db "sync" name)
        name
        (let ((new-entity (db-filter-only
                           db "sync" "*"
                           (list (list "name" "varchar" "=" name))
                           (list))))
          (if (null? new-entity)
              #f
              (ktv-get (car new-entity) "unique_id"))))))

(define (convert-id-list str)
  (let ((names (string-split-simple str #\,)))
    (foldl
     (lambda (name r)
       (if (string? r)
           (let ((id (convert-id name)))
             (if id
                 (if (equal? r "") id (string-append r "," id))
                 #f))
           #f))
     "" names)))

;; replace entity with names -> uids, or name of not found
(define (review-validate-contents uid entity)
  (foldl
   (lambda (ktv r)
     (cond
      ((string? r) r) ;; we have already found an error
      ((ktv-key-is-id? ktv)
       (let ((replacement
              (if (ktv-value-is-list? ktv)
                  (convert-id-list (ktv-value ktv))
                  (convert-id (ktv-value ktv)))))
         (if replacement
             (cons (list (ktv-key ktv) (ktv-type ktv) replacement) r)
             ;; ditch the entity and return error
             (ktv-value ktv))))
      (else (cons ktv r))))
   '()
   entity))


(define (review-build-contents uid entity)
  (append
   (foldl
    (lambda (ktv r)
      (append
       r (cond
          ((or
            (equal? (ktv-key ktv) "user")
            (equal? (ktv-key ktv) "lat")
            (equal? (ktv-key ktv) "lon")
            (equal? (ktv-key ktv) "time")
            (equal? (ktv-key ktv) "parent")
            (equal? (ktv-key ktv) "unique_id")
            (equal? (ktv-key ktv) "deleted")) '())
          ((equal? (ktv-type ktv) "varchar")
           (if (ktv-key-is-id? ktv)
               (if (ktv-value-is-list? ktv)
                   (review-build-list ktv)
                   (review-build-id ktv))
               ;; normal varchar
               (list (medit-text-value (string-append uid (ktv-key ktv))
                                       (ktv-key ktv)
                                       (ktv-value ktv) "normal"
                                       (lambda (v)
                                         (entity-set-value! (ktv-key ktv) (ktv-type ktv) v) '())))))
          ((equal? (ktv-type ktv) "int")
           (list (medit-text-value (string-append uid (ktv-key ktv))
                                   (ktv-key ktv)
                                   (number->string (ktv-value ktv)) "numeric"
                                   (lambda (v)
                                     (entity-set-value! (ktv-key ktv) (ktv-type ktv) v) '()))))
          ((equal? (ktv-type ktv) "real")
           (list (medit-text-value (string-append uid (ktv-key ktv))
                                   (ktv-key ktv)
                                   ;; get around previous bug, should remove
                                   (if (number? (ktv-value ktv))
                                       (number->string (ktv-value ktv))
                                       (ktv-value ktv)) "numeric"
                                   (lambda (v)
                                     (entity-set-value! (ktv-key ktv) (ktv-type ktv) v) '()))))
          (else (mtext "" (string-append (ktv-type ktv) " not handled")) '()))))
    '()
    entity)
   (list
    (horiz
     (mbutton "review-item-cancel" "Cancel" (lambda () (list (finish-activity 0))))
     (mbutton (string-append uid "-save") "Save"
              (lambda ()
                (let ((new-entity (review-validate-contents uid (get-current 'entity-values '()))))
                  (cond
                   ((list? new-entity)
                    ;; replace with converted ids
                    (set-current! 'entity-values new-entity)
                    ;;(entity-update-values!)
                    (list (finish-activity 0)))
                   (else
                    (list
                     (alert-dialog
                      "mongoose-not-found"
                      (string-append "Can't find mongoose or pack: " new-entity)
                      (lambda (v)
                        (cond
                         ((eqv? v 1) (list))
                         (else (list)))))))))))))))


(define (review-item-build)
  (let ((uid (entity-get-value "unique_id")))
    (list
     (update-widget
      'linear-layout
      (get-id "review-item-container")
      'contents
      (review-build-contents
       uid (get-current 'entity-values '()))))))

(define (review-update-list)
  (list
   (update-widget
    'linear-layout (get-id "review-list") 'contents
    (map
     (lambda (dirty-entity)
       ;; consists of ((type,uid,dirty,version) (ktvlist))
       (let* ((data (car dirty-entity))
              (entity (cadr dirty-entity))
              (time (ktv-get entity "time"))
              (type (list-ref data 0))
              (uid (list-ref data 1)))
         (mbutton
          (string-append "review-" uid)
          (string-append type (if time (string-append "-" time) ""))
          (lambda ()
            (entity-init! db "stream" type (get-entity-by-unique db "stream" uid))
            (list (start-activity "review-item" 0 ""))))))
     (dirty-entities-for-review db "stream")))))

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
        (msg "connected, going in...")
        (append
         (list (toast "Syncing..."))
         (upload-dirty db)
         ;; important - don't receive until all are sent...
         (if (or (have-dirty? db "sync")
                 (have-dirty? db "stream")) '()
             (append
              (suck-new db "sync")
              (start-sync-files)))))))
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

(define (next-button id dialog-msg last-frag next-frag fn)
  (vert
   (spacer 30)
   (horiz
    (button (make-id (string-append id "-backb")) "Back" 30 (layout 'fill-parent 'wrap-content 1 'centre 5)
            (lambda ()
              (list (replace-fragment (get-id "gc-top") last-frag))))

    (button (make-id (string-append id "-nextb")) "Next" 30 (layout 'fill-parent 'wrap-content 1 'centre 5)
            (lambda ()
              (entity-update-values!)
              (append
               (fn)
               (list
                (replace-fragment (get-id "gc-top") next-frag))))))))

(define (force-pause)
  (list
   (delayed "timer" 1000 (lambda () '()))
   (update-widget 'toggle-button (get-id "pf-pause") 'checked 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-selector-colours id entity-type where)
  (update-grid-selector-colours
   id "id-mongoose"
   (db-filter
    db "stream" entity-type
    (list
     (list "parent" "varchar" "=" (get-current 'group-composition-id 0))
     where))))

(define (update-selector-colours2 id entity-type where)
  (update-grid-selector-colours
   id "id-escort"
   (db-filter
    db "stream" entity-type
    (cons
     (list "parent" "varchar" "=" (get-current 'group-composition-id 0))
     where))))

(define (update-selector-colours3 id entity-type)
  (update-grid-selector-colours
   id "id-mongoose"
   (db-filter
    db "stream" entity-type
    (list
     (list "parent" "varchar" "=" (get-current 'group-composition-id 0))))))

(define (invert-mongoose-selection individuals)
  (filter
   (lambda (m)
     (not (in-list? m individuals)))
   (map (lambda (m) (ktv-get m "unique_id")) (db-mongooses-by-pack))))
