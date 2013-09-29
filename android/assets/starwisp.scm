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
;; persistent database

(define db "/sdcard/test.db")
(db-open db)

(display "hello one two three")(newline)

(setup db "local")
(setup db "sync")
(setup db "stream")

(insert-entity-if-not-exists
 db "local" "app-settings" "null" 1
 (list
  (ktv "user-id" "varchar" "No name yet...")))

(display "sonwassa")(newline)

(msg "001")
(display (db-all db "local" "app-settings"))(newline)
(msg "002")

(display (db-status db))(newline)
(msg "003")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff in memory

(define (store-set store key value)
  (cond
    ((null? store) (list (list key value)))
    ((eq? key (car (car store)))
     (cons (list key value) (cdr store)))
    (else
     (cons (car store) (store-set (cdr store) key value)))))

(define (store-get store key)
  (cond
    ((null? store) #f)
    ((eq? key (car (car store)))
     (cadr (car store)))
    (else
     (store-get (cdr store) key))))


(define store '())

(define (set-current! key value)
  (set! store (store-set store key value)))

(define (get-current key)
  (store-get store key))

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
   "&dirty=" (list-ref (car e) 2)
   "&version=" (list-ref (car e) 3)
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
            (update-entity-clean db table (cadr v))
            (display "somefink went wrong")(newline)))))
   (dirty-entities db table)))

(define (suck-entity-from-server db table unique-id exists)
  (msg "suck-entity-from-server" unique-id)
  ;; ask for the current version
  (http-request
   (string-append unique-id "-update-new")
   (string-append url "fn=entity&table=" table "&unique-id=" unique-id)
   (lambda (data)
     (msg "data from server request" data)
     ;; check "sync-insert" in sync.ss raspberry pi-side for the contents of 'entity'
     (let ((entity (list-ref data 0))
           (ktvlist (list-ref data 1)))
       (msg "1111" exists)
       (if (not exists)
           (begin
             (msg entity)
             (msg (string? (list-ref entity 2)))
             (insert-entity-wholesale
              db table
              (list-ref entity 0) ;; entity-type
              (list-ref entity 1) ;; unique-id
              0 ;; dirty
              (list-ref entity 2) ;; version
              ktvlist))
           (update-to-version
            db table (get-entity-id db table unique-id)
            (list-ref entity 4) ktvlist)))
     (msg "2222" exists)

     '())))

;; repeatedly read version and request updates
(define (suck-new db table)
  (msg "suck-new")
  (list
   (http-request
    "new-entities-req"
    (dbg (string-append url "fn=entity-versions&table=" table))
    (lambda (data)
      (msg data)
      (dbg (foldl
       (lambda (i r)
         (msg "inner loop" i)
         (let* ((unique-id (car i))
                (version (cadr i))
                (exists (entity-exists? db table unique-id))
                (old
                 (if exists
                     (> version (string->number
                                 (dbg (get-entity-version db table unique-id))))
                     #f)))
           ;; if we don't have this entity or the version on the server is newer
           (if (or (not exists) old)
               (cons (suck-entity-from-server db table unique-id exists) r)
               r)))
       '()
       data))))))

(msg "004")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mbutton id title fn)
  (button (make-id id) title 20 fillwrap fn))

(define (mtext id text)
  (text-view (make-id id) text 20 fillwrap))

(define-activity-list
  (activity
   "splash"
   (vert
    (text-view (make-id "splash-title") "Mongoose 2000" 40 fillwrap)
    (mtext "splash-about" "Advanced mongoose technology")
    (spacer 20)
    (mbutton "f2" "Get started!" (lambda () (list (start-activity-goto "main" 2 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
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
    (mbutton "main-experiments" "Experiments" (lambda () (list (start-activity "experiments" 2 ""))))
    (mbutton "main-manage" "Manage Packs" (lambda () (list (start-activity "manage-packs" 2 ""))))
    (mbutton "main-tag" "Tag Location" (lambda () (list (start-activity "tag-location" 2 ""))))
    (mtext "foo" "Your ID")
    (edit-text (make-id "main-id-text") "" 30 fillwrap
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
   "experiments"
   (vert
    (text-view (make-id "title") "Experiments" 40 fillwrap)
    (spacer 10)
    (button (make-id "main-sync") "Pup Focal" 20 fillwrap (lambda () (list (start-activity "pack-select" 2 ""))))
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
   "pack-select"
   (vert
    (text-view (make-id "title") "Select a Pack" 40 fillwrap)
    (spacer 10)
    (horiz
     (button (make-id "pack-select-pack-0") "Pack 1" 20 fillwrap (lambda () (list (start-activity "individual-select" 2 ""))))
     (button (make-id "pack-select-pack-1") "Pack 2" 20 fillwrap (lambda () (list (start-activity "individual-select" 2 "")))))
    (horiz
     (button (make-id "pack-select-pack-2") "Pack 3" 20 fillwrap (lambda () (list (start-activity "individual-select" 2 ""))))
     (button (make-id "pack-select-pack-3") "Pack 4" 20 fillwrap (lambda () (list (start-activity "individual-select" 2 "")))))
    (horiz
     (button (make-id "pack-select-pack-4") "Pack 5" 20 fillwrap (lambda () (list (start-activity "individual-select" 2 ""))))
     (button (make-id "pack-select-pack-5") "Pack 6" 20 fillwrap (lambda () (list (start-activity "individual-select" 2 "")))))
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
   "individual-select"
   (vert
    (text-view (make-id "title") "Select an individual" 40 fillwrap)
    (spacer 10)
    (horiz
     (button (make-id "individual-select-pack-0") "Mongoose 1" 20 fillwrap (lambda () (list (start-activity "pup-focal" 2 ""))))
     (button (make-id "individual-select-pack-1") "Mongoose 2" 20 fillwrap (lambda () (list (start-activity "pup-focal" 2 "")))))
    (horiz
     (button (make-id "individual-select-pack-2") "Mongoose 3" 20 fillwrap (lambda () (list (start-activity "pup-focal" 2 ""))))
     (button (make-id "individual-select-pack-3") "Mongoose 4" 20 fillwrap (lambda () (list (start-activity "pup-focal" 2 "")))))
    (horiz
     (button (make-id "individual-select-pack-4") "Mongoose 5" 20 fillwrap (lambda () (list (start-activity "pup-focal" 2 ""))))
     (button (make-id "individual-select-pack-5") "Mongoose 6" 20 fillwrap (lambda () (list (start-activity "pup-focal" 2 "")))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (let ((clear-focal-toggles
         (lambda ()
           (list
            (update-widget 'toggle-button (get-id "pup-focal-moving") 'checked 0)
            (update-widget 'toggle-button (get-id "pup-focal-foraging") 'checked 0)
            (update-widget 'toggle-button (get-id "pup-focal-resting") 'checked 0)))))

    (activity
     "pup-focal"
     (vert
      (horiz
       (text-view (make-id "pup-focal-title") "Pup Focal" 40 fillwrap)
       (vert
        (text-view (make-id "pup-focal-timer-text") "Time left" 20 fillwrap)
        (text-view (make-id "pup-focal-timer") "30" 40 fillwrap)))
      (text-view (make-id "pup-focal") "Current Activity" 20 fillwrap)
      (horiz
       (toggle-button (make-id "pup-focal-moving") "Moving" 20 fillwrap (lambda (v) (clear-focal-toggles)))
       (toggle-button (make-id "pup-focal-foraging") "Foraging" 20 fillwrap (lambda (v) (clear-focal-toggles)))
       (toggle-button (make-id "pup-focal-resting") "Resting" 20 fillwrap (lambda (v) (clear-focal-toggles))))
      (text-view (make-id "pup-focal-escort-text") "Current Escort" 20 fillwrap)
      (spinner (make-id "pup-focal-escort") (list "Mongoose 1" "Mongoose 2" "Mongoose 3" "Mongoose 4") fillwrap (lambda (v) '()))
      (horiz
       (button (make-id "pup-focal-event") "New event" 20 fillwrap (lambda () (list (start-activity "pup-focal-event" 2 ""))))
       (toggle-button (make-id "pup-focal-pause") "Pause" 20 fillwrap (lambda (v) '()))
       ))
     (lambda (activity arg)
       (activity-layout activity))
     (lambda (activity arg) (list))
     (lambda (activity) '())
     (lambda (activity) '())
     (lambda (activity) '())
     (lambda (activity) '())
     (lambda (activity requestcode resultcode) '())))

  (activity
   "pup-focal-event"
   (vert
    (text-view (make-id "main-title") "Pup focal event" 40 fillwrap)
    (spacer 10)
    (button (make-id "event-self") "Self feeding" 20 fillwrap
            (lambda () (list (start-activity "event-self" 2 ""))))
    (button (make-id "event-fed") "Being fed" 20 fillwrap
            (lambda () (list (start-activity "event-fed" 2 ""))))
    (button (make-id "event-aggression") "Aggression" 20 fillwrap
            (lambda () (list (start-activity "event-aggression" 2 ""))))
    (spacer 10)
    (button (make-id "event-cancel") "Cancel" 20 fillwrap
            (lambda () (list (finish-activity 0))))
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

  (let ((build-pack-buttons
         (lambda ()
           (foldl
            (lambda (pack r)
              (let ((name (ktv-get pack "name")))
                (msg name)
                (if (not (null? name))
                    (cons (button (make-id (string-append "manage-packs-pack-" name))
                                  name 20 fillwrap
                                  (lambda ()
                                    (msg "going to manage individuals")
                                    (msg pack)
                                    (set-current! 'pack pack)
                                    (list (start-activity "manage-individual" 2 ""))))
                          r)
                    r)))
            '()
            (db-all db "sync" "pack")))))
  (activity
   "manage-packs"
   (vert
    (text-view (make-id "title") "Manage packs" 40 fillwrap)
    (linear-layout
     (make-id "manage-packs-pack-list")
     'vertical fill (list))
    (button (make-id "manage-packs-new") "New pack" 20 fillwrap (lambda () (list (start-activity "new-pack" 2 ""))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'linear-layout (get-id "manage-packs-pack-list") 'contents
                     (dbg (build-pack-buttons)))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '())))

  (activity
   "new-pack"
   (vert
    (text-view (make-id "title") "New pack" 40 fillwrap)
    (spacer 10)
    (text-view (make-id "new-pack-name-text") "Pack name" 20 fillwrap)
    (edit-text (make-id "new-pack-name") "" 30 fillwrap
               (lambda (v) (msg "edit callback" v) (set-current! 'pack-name v) '()))
    (spacer 10)
    (horiz
     (button (make-id "new-pack-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "new-pack-done") "Done" 20 fillwrap
             (lambda ()
               (insert-entity
                db "sync" "pack" (get-current 'user-id)
                (list
                 (ktv "name" "varchar" (get-current 'pack-name))))
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

  (let ((build-individual-buttons
         (lambda ()
           (msg "building individual buttons")
           (map
            (lambda (individual)
              (let ((name (ktv-get individual "name")))
                (button (make-id (string-append "manage-individuals-ind-" name))
                        name 20 fillwrap
                        (lambda ()
                          (list (start-activity "manage-individual" 2 ""))))))
            (db-all-where
             db "sync" "mongoose"
             (list "pack-id" (ktv-get (get-current 'pack) "unique_id")))
            ))))
  (activity
   "manage-individual"
   (vert
    (text-view (make-id "title") "Manage individuals" 40 fillwrap)
    (text-view (make-id "manage-individual-pack-name") "Pack:" 20 fillwrap)
    (linear-layout
     (make-id "manage-individuals-list")
     'vertical fill (list))
    (button (make-id "manage-individuals-new") "New individual" 20 fillwrap (lambda () (list (start-activity "new-individual" 2 ""))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'linear-layout (get-id "manage-individuals-list") 'contents
                     (build-individual-buttons))
      (update-widget 'text-view (get-id "manage-individual-pack-name") 'text
                     (string-append "Pack: " (ktv-get (get-current 'pack) "name")))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '())))

  (activity
   "new-individual"
   (vert
    (text-view (make-id "title") "New Mongoose" 40 fillwrap)
    (text-view (make-id "new-individual-pack-name") "Pack:" 20 fillwrap)
    (text-view (make-id "new-individual-name-text") "Name" 20 fillwrap)
    (edit-text (make-id "new-individual-name") "" 30 fillwrap
               (lambda (v) (set-current! 'individual-name v) '()))
    (text-view (make-id "new-individual-name-text") "Gender" 20 fillwrap)
    (spinner (make-id "new-individual-gender") (list "Female" "Male") fillwrap
             (lambda (v) (set-current! 'individual-gender v) '()))
    (text-view (make-id "new-individual-dob-text") "Date of Birth" 20 fillwrap)
    (horiz
     (text-view (make-id "new-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 20 fillwrap (lambda () '())))
    (text-view (make-id "new-individual-litter-text") "Litter code" 20 fillwrap)
    (edit-text (make-id "new-individual-litter-code") "" 30 fillwrap
               (lambda (v) (set-current! 'individual-litter-code v) '()))
    (text-view (make-id "new-individual-chip-text") "Chip code" 20 fillwrap)
    (edit-text (make-id "new-individual-chip-code") "" 30 fillwrap
               (lambda (v) (set-current! 'individual-chip-code v) '()))
    (horiz
     (button (make-id "new-individual-cancel") "Cancel" 20 fillwrap (lambda () (list (finish-activity 2))))
     (button (make-id "new-individual-done") "Done" 20 fillwrap
             (lambda ()
               (insert-entity
                db "sync" "mongoose" (get-current 'user-id)
                (list
                 (ktv "name" "varchar" (get-current 'individual-name))
                 (ktv "gender" "varchar" (get-current 'individual-gender))
                 (ktv "litter-code" "varchar" (get-current 'individual-litter-code))
                 (ktv "chip-code" "varchar" (get-current 'individual-chip-code))
                 (ktv "pack-id" "varchar" (ktv-get (get-current 'pack) "unique_id"))
                 ))
               (list (finish-activity 2)))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'text-view (get-id "new-individual-pack-name") 'text
                     (string-append "Pack: " (ktv-get (get-current 'pack) "name")))))
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
    (edit-text (make-id "update-individual-name") "" 30 fillwrap (lambda (v) '()))
    (text-view (make-id "update-individual-name-text") "Gender" 20 fillwrap)
    (spinner (make-id "update-individual-gender") (list "Female" "Male") fillwrap (lambda (v) '()))
    (text-view (make-id "update-individual-dob-text") "Date of Birth" 20 fillwrap)
    (horiz
     (text-view (make-id "update-individual-dob") "00/00/00" 25 fillwrap)
     (button (make-id "date") "Set date" 20 fillwrap (lambda () '())))
    (text-view (make-id "update-individual-litter-text") "Litter code" 20 fillwrap)
    (edit-text (make-id "update-individual-litter-code") "" 30 fillwrap (lambda (v) '()))
    (text-view (make-id "update-individual-chip-text") "Chip code" 20 fillwrap)
    (edit-text (make-id "update-individual-chip-code") "" 30 fillwrap (lambda (v) '()))
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
    (edit-text (make-id "tag-location-name") "" 30 fillwrap (lambda (v) '()))

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


  (let ((build-dirty
         (lambda ()
           (let ((sync (get-dirty-stats db "sync"))
                 (stream (get-dirty-stats db "stream")))
             (msg sync stream)
             (dbg (string-append
                   "Pack data: " (car sync) "/" (cadr sync) " "
                   "Focal data: " (car stream) "/" (cadr stream)))))))
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
                      (msg state)
                      (list
                       (update-widget 'text-view (get-id "sync-connect") 'text state)))))))
       (mbutton "sync-sync" "Push"
                (lambda ()
                  (spit-dirty db "sync")))
       (mbutton "sync-pull" "Pull"
                (lambda ()
                  (dbg (suck-new db "sync")))))
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
     (lambda (activity requestcode resultcode) '())))




  )
