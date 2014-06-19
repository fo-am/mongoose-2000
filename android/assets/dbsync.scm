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

;; abstractions for synced databased

(msg "dbsync.scm")


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

(define (entity-init! db table entity-type ktv-list)
  (entity-reset!)
  (entity-set! ktv-list)
  (set-current! 'db db)
  (set-current! 'table table)
  (set-current! 'entity-type entity-type))

(define (entity-init&save! db table entity-type ktv-list)
  (entity-init! db table entity-type ktv-list)
  (let ((id (entity-create! db table entity-type ktv-list)))
    (msg "1")
    (entity-set-value! "unique_id" "varchar" id)
    (msg "2")
    id))

;; store a ktv, replaces existing with same key
;;(define (entity-add-value! key type value)
;;  (set-current!
;;   'entity-values
;;   (ktv-set
;;    (get-current 'entity-values '())
;;    (ktv key type value))))

(define (entity-add-value-create! key type value)
  (msg "entity-add-value-create!" key type value)
  (set-current!
   'entity-values
   (ktv-set
    (get-current 'entity-values '())
    (ktv key type value))))

(define (entity-set! ktv-list)
  (set-current! 'entity-values ktv-list))

(define (entity-get-value key)
  (ktv-get (get-current 'entity-values '()) key))

;; version to check the entity has the key
(define (entity-set-value! key type value)
;  (let ((existing-type (ktv-get-type (get-current 'entity-values '()) key)))
;    (if (equal? existing-type type)
        (set-current!
         'entity-values
         (ktv-set
          (get-current 'entity-values '())
          (ktv key type value)))
        ;;
;        (begin
;          (msg "entity-set-value! - adding new " key "of type" type "to entity")
;          (entity-add-value-create! key type value)))
    ;; save straight to local db every time
    ;;(entity-update-single-value! (list key type value))
        ;;       )
        )

(define (date-time->string dt)
  (string-append
   (number->string (list-ref dt 0)) "-"
   (substring (number->string (+ (list-ref dt 1) 100)) 1 3) "-"
   (substring (number->string (+ (list-ref dt 2) 100)) 1 3) " "
   (substring (number->string (+ (list-ref dt 3) 100)) 1 3) ":"
   (substring (number->string (+ (list-ref dt 4) 100)) 1 3) ":"
   (substring (number->string (+ (list-ref dt 5) 100)) 1 3)))

;; build entity from all ktvs, insert to db, return unique_id
(define (entity-record-values!)
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f))
        (type (get-current 'entity-type #f)))
    ;; standard bits
    (let ((r (entity-create! db table type (get-current 'entity-values '()))))
      (entity-reset!) r)))


(define (entity-create! db table entity-type ktv-list)
  (msg "creating:" entity-type ktv-list)
  (let ((values
         (append
          (list
           (ktv "user" "varchar" (get-current 'user-id "none"))
           (ktv "time" "varchar" (date-time->string (date-time)))
           (ktv "lat" "real" (car (get-current 'location '(0 0))))
           (ktv "lon" "real" (cadr (get-current 'location '(0 0))))
           (ktv "deleted" "int" 0))
          ktv-list)))
    (msg "about to insert")
    (let ((r (insert-entity/get-unique
              db table entity-type (get-current 'user-id "no id")
              values)))
      (msg "entity-create: " entity-type)
      r)))


(define (entity-update-values!)
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f)))
    (msg "entity-update-values" db table)
    (msg (get-current 'entity-values '()))
    ;; standard bits
    (let ((values (get-current 'entity-values '()))
          (unique-id (ktv-get (get-current 'entity-values '()) "unique_id")))
      (cond
       ((and unique-id (not (null? values)))
        (msg "entity-update-values inner" values)
        (update-entity db table (entity-id-from-unique db table unique-id) values)
        ;; removed due to save button no longer exiting activity - need to keep!
        ;;(entity-reset!)
        )
       (else
        (msg "no values or no id to update as entity:" unique-id "values:" values))))))

(define (entity-update-single-value! ktv)
  (entity-set-value! (ktv-key ktv) (ktv-type ktv) (ktv-value ktv))
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f))
        (unique-id (ktv-get (get-current 'entity-values '()) "unique_id")))
    (cond
     (unique-id
      (update-entity db table (entity-id-from-unique db table unique-id) (list ktv)))
     (else
      (msg "no values or no id to update as entity:" unique-id "values:" ktv)))))


(define (entity-reset!)
  (set-current! 'entity-values '())
  (set-current! 'db "reset")
  (set-current! 'table "reset")
  (set-current! 'entity-type "reset"))

(define (assemble-array entities)
  (foldl
   (lambda (i r)
     (if (equal? r "") (ktv-get i "unique_id")
         (string-append r "," (ktv-get i "unique_id"))))
   ""
   entities))

(define (string-split-simple str delim)
  (let ((r (foldl
            (lambda (c r)
              (cond
                ((eqv? c delim)
                 (list "" (append (cadr r) (list (car r)))))
                (else
                 (list (string-append (car r) (string c))
                       (cadr r)))))
            (list "" '())
            (string->list str))))
    (if (equal? (car r) "")
        (cadr r)
        (append (cadr r) (list (car r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syncing code

;; todo - separate logic from gui and stick this in common code
;; then we can unit test this stuff...

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


;; todo fix all hardcoded paths here
(define (send-files ktvlist)
  (foldl
   (lambda (ktv r)
     (if (equal? (ktv-type ktv) "file")
         (begin
           (cons (http-upload
                  (string-append "upload-" (ktv-value ktv))
                  "http://192.168.2.1:8888/mongoose?fn=upload"
                  (string-append "/sdcard/mongoose/files/" (ktv-value ktv)))
                 r))
         r))
   '() ktvlist))

;; redundant second pass to syncronise files - independant of the
;; rest of the syncing system
(define (sync-files server-list)
  (let ((local-list (dir-list "/sdcard/mongoose/files/")))
    ;; search for all local files in server list
    (crop
     (append
      (foldl
       (lambda (file r)
         ;; send files not present
         (if (or
              (eqv? (string-ref file 0) #\.)
              (in-list? file server-list))
             r (cons
                (http-upload
                 (string-append "upload-" file)
                 "http://192.168.2.1:8888/mongoose?fn=upload"
                 (string-append "/sdcard/mongoose/files/" file)) r)))
       '()
       local-list)
      ;; search for all server files in local list
      (foldl
       (lambda (file r)
         ;; request files not present
         (if (in-list? file local-list)
             r (cons
                (http-download
                 (string-append "download-" file)
                 (string-append "http://192.168.2.1:8888/files/" file)
                 (string-append "/sdcard/mongoose/files/" file)) r)))
       '()
       server-list))
     ;; restrict the number of uploads each time round
     2)))

(define (start-sync-files)
  (list
   (http-request
    (string-append "file-list")
    (string-append url "fn=file-list")
    (lambda (file-list)
      (let ((r (sync-files file-list)))
        (when (not (null? r))
              (set-current! 'upload 0)
              (debug! "Found a mismatch with files on raspberry pi - fixing..."))
        r)))))


;; spit all dirty entities to server
(define (spit db table entities)
  (foldl
   (lambda (e r)
     ;;(msg (car (car e)))
     (debug! (string-append "Sending a " (car (car e)) " to Raspberry Pi"))
     (append
      (list
       (http-request
        (string-append "req-" (list-ref (car e) 1))
        (build-url-from-entity table e)
        (lambda (v)
          (msg "in spit..." v)
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
          (append
           ;; check for file uploads
           (if (or (equal? (car v) "updated")
                   (equal? (car v) "inserted")
                   (equal? (car v) "match"))
               (send-files (cadr e)) ;; takes a ktvlist
               '())
           (list
            (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty db)))))))
      r))
   '()
   entities))

;; todo fix all hardcoded paths here
(define (request-files ktvlist)
  (foldl
   (lambda (ktv r)
     (if (equal? (ktv-type ktv) "file")
         (begin
           (cons (http-download
                  (string-append "download-" (ktv-value ktv))
                  (string-append "http://192.168.2.1:8888/files/" (ktv-value ktv))
                  (string-append "/sdcard/mongoose/files/" (ktv-value ktv)))
                 r))
         r))
   '() ktvlist))

(msg "suck ent")

(define (suck-entity-from-server db table unique-id)
  ;; ask for the current version
  (http-request
   (string-append unique-id "-update-new")
   (string-append url "fn=entity&table=" table "&unique-id=" unique-id)
   (lambda (data)
     ;; check "sync-insert" in sync.ss raspberry pi-side for the contents of 'entity'
     (let* ((entity (list-ref data 0))
            (ktvlist (list-ref data 1))
            (unique-id (list-ref entity 1))
            (exists (entity-exists? db table unique-id)))
       ;; need to check exists again here, due to delays back and forth
       (if (not exists)
           (insert-entity-wholesale
            db table
            (list-ref entity 0) ;; entity-type
            unique-id
            0 ;; dirty
            (list-ref entity 2) ;; version
            ktvlist)
           (update-to-version
            db table (get-entity-id db table unique-id)
            (list-ref entity 2) ktvlist))
       (debug! (string-append (if exists "Got new: " "Updated: ") (ktv-get ktvlist "name")))
       (cons
        (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty db))
        (request-files ktvlist))))))

(define (build-entity-requests db table version-data)
  (foldl
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
       (if (and (or (not exists) old)
                ;; limit this to 5 a time
                (< (length r) 5))
           (cons (suck-entity-from-server db table unique-id) r)
           r)))
   '()
   version-data))

(define (mark-unlisted-entities-dirty! db table version-data)
  (msg "mark-unlisted...")
  ;; load all local entities
  (let ((ids (all-unique-ids db table))
        (server-ids (map car version-data)))
    ;; look for each one in data
    (for-each
     (lambda (id)
       (when (not (in-list? id server-ids))
             (msg "can't find " id " in server data, marking dirty")
             (debug! "Have an entity here not on raspberry pi - marking for upload...")
             ;; mark those not present as dirty for next spit cycle
             (update-entity-dirtify db table id)))
     ids)))

;; repeatedly read version and request updates
(define (suck-new db table)
  (debug! "Requesting new entities")
  (list
   (http-request
    "new-entities-req"
    (string-append url "fn=entity-versions&table=" table)
    (lambda (data)
      (let ((new-entity-requests (build-entity-requests db table data)))
        (alog "suck-new: marking dirty")
        (mark-unlisted-entities-dirty! db table data)
        (alog "suck-new: done marking dirty")
        (cond
         ((null? new-entity-requests)
          (debug! "No new data to download")
          (set-current! 'download 1)
          (append
           (if (eqv? (get-current 'upload 0) 1)
               (list (play-sound "ping")) '())
           (list
            (toast "No new data to download"))))
         (else
          (debug! (string-append
                   "Requesting "
                   (number->string (length new-entity-requests)) " entities"))
          (cons
           (play-sound "active")
           new-entity-requests))))))))

(msg "build-dirty defined...")

(define (build-dirty db)
  (let ((sync (get-dirty-stats db "sync"))
        (stream (get-dirty-stats db "stream")))
    (string-append
     "Sync data: " (number->string (car sync)) "/" (number->string (cadr sync)) " "
     "Stream data: " (number->string (car stream)) "/" (number->string (cadr stream)))))

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


(define i18n-lang 0)

(define i18n-text
  (list))

(define (mtext-lookup id)
  (define (_ l)
    (cond
     ((null? l) (string-append (symbol->string id) " not translated"))
     ((eq? (car (car l)) id)
      (let ((translations (cadr (car l))))
        (if (<= (length translations) i18n-lang)
            (string-append (symbol->string id) " not translated")
            (let ((r (list-ref translations i18n-lang)))
              (if (or (equal? r "") (equal? r " "))
                  (list-ref translations 0) r)))))
     (else (_ (cdr l)))))
  (_ i18n-text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbol->id id)
  (when (not (symbol? id))
        (msg "symbol->id: [" id "] is not a symbol"))
  (make-id (symbol->string id)))

(define (get-symbol-id id)
  (when (not (symbol? id))
        (msg "symbol->id: [" id "] is not a symbol"))
  (get-id (symbol->string id)))

(define (mbutton id fn)
  (button (symbol->id id)
          (mtext-lookup id)
          40 (layout 'fill-parent 'wrap-content -1 'centre 5) fn))

(define (mbutton-scale id fn)
  (button (symbol->id id)
          (mtext-lookup id)
          40 (layout 'fill-parent 'wrap-content 1 'centre 5) fn))

(define (mtoggle-button id fn)
  (toggle-button (symbol->id id)
                 (mtext-lookup id)
                 30 (layout 'fill-parent 'wrap-content -1 'centre 0) "fancy"
                 ;; convert to 0/1 for easier db storage
                 (lambda (v) (fn (if v 1 0)))))

(define (mtoggle-button-scale id fn)
  (toggle-button (symbol->id id)
                 (mtext-lookup id)
                 30 (layout 'fill-parent 'wrap-content 1 'centre 0) "fancy"
                 (lambda (v) (fn (if v 1 0)))))

(define (mtext id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             30 (layout 'wrap-content 'wrap-content -1 'centre 0)))

(define (mtext-fixed w id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             30 (layout w 'wrap-content -1 'centre 0)))

(define (mtext-small id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             20 (layout 'wrap-content 'wrap-content -1 'centre 0)))

(define (mtext-scale id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             30 (layout 'wrap-content 'wrap-content 1 'centre 0)))

(define (mtitle id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             50 (layout 'fill-parent 'wrap-content -1 'centre 5)))

(define (mtitle-scale id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             50 (layout 'fill-parent 'wrap-content 1 'centre 5)))

(define (medit-text id type fn)
  (linear-layout
   (make-id (string-append (symbol->string id) "-container"))
   'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   (list 0 0 0 0)
   (list
    (text-view 0 (mtext-lookup id)
               30 (layout 'wrap-content 'wrap-content -1 'centre 0))
    (edit-text (symbol->id id) "" 30 type
               (layout 'fill-parent 'wrap-content -1 'centre 0)
               fn))))

(define (medit-text-scale id type fn)
  (linear-layout
   (make-id (string-append (symbol->string id) "-container"))
   'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   (list 0 0 0 0)
   (list
    (text-view 0 (mtext-lookup id)
               30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (edit-text (symbol->id id) "" 30 type
               (layout 'fill-parent 'wrap-content 1 'centre 0)
               fn))))

(define (mspinner id types fn)
  (vert
   (text-view (symbol->id id)
              (mtext-lookup id)
              30 (layout 'wrap-content 'wrap-content 1 'centre 10))
   (spinner (make-id (string-append (symbol->string id) "-spinner"))
            (map mtext-lookup types)
            (layout 'wrap-content 'wrap-content 1 'centre 0)
            (lambda (c) (fn c)))))

(define (mspinner-other id types fn)
  (linear-layout
   (make-id (string-append (symbol->string id) "-container"))
   'horizontal
   (layout 'fill-parent 'wrap-content 1 'centre 5)
   (list 0 0 0 0)
   (list
    (vert
     (text-view (symbol->id id)
                (mtext-lookup id)
                30 (layout 'wrap-content 'wrap-content 1 'centre 10))
     (spinner (make-id (string-append (symbol->string id) "-spinner"))
              (map mtext-lookup types)
              (layout 'wrap-content 'wrap-content 1 'centre 0)
              (lambda (c)
                ;; dont call if set to "other"
                (if (< c (- (length types) 1))
                    (fn c)
                    '()))))
    (vert
     (mtext-scale 'other)
     (edit-text (make-id (string-append (symbol->string id) "-edit-text"))
                "" 30 "normal"
                (layout 'fill-parent 'wrap-content 1 'centre 0)
                (lambda (t) (fn t)))))))

(define (mspinner-other-vert id text-id types fn)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 5)
   (list 0 0 0 0)
   (list
    (text-view (symbol->id id)
               (mtext-lookup text-id)
               30 (layout 'wrap-content 'wrap-content 1 'centre 5))
    (spinner (make-id (string-append (symbol->string id) "-spinner"))
             (map mtext-lookup types)
             (layout 'wrap-content 'wrap-content 1 'centre 0)
             (lambda (c)
               ;; dont call if set to "other"
               (if (< c (- (length types) 1))
                   (fn c) '())))
    (mtext-scale 'other)
    (edit-text (make-id (string-append (symbol->string id) "-edit-text"))
               "" 30 "normal"
               (layout 'fill-parent 'wrap-content 1 'centre 0)
               (lambda (t) (fn t))))))


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

(define (image-invalid? image-name)
  (or (null? image-name)
      (not image-name)
      (equal? image-name "none")))

;; fill out the widget from the current entity in the memory store
;; dispatches based on widget type
(define (mupdate widget-type id-symbol key)
  (cond
   ((or (eq? widget-type 'edit-text) (eq? widget-type 'text-view))
    (update-widget widget-type (get-symbol-id id-symbol) 'text
                   (entity-get-value key)))
   ((eq? widget-type 'toggle-button)
    (update-widget widget-type (get-symbol-id id-symbol) 'checked
                   (entity-get-value key)))
   ((eq? widget-type 'image-view)
    (let ((image-name (entity-get-value key)))
      (if (image-invalid? image-name)
          (update-widget widget-type (get-symbol-id id-symbol) 'image "face")
          (update-widget widget-type (get-symbol-id id-symbol) 'external-image
                         (string-append dirname "files/" image-name)))))
   (else (msg "mupdate-widget unhandled widget type" widget-type))))

;(define (spinner-choice l i)
;  (if (number? i)
;      (symbol->string (list-ref l i))
;      i))

;; spinner options are (list 'id-sym "Name") ...

(define (spinner-choice l i)
  (if (number? i)
      (symbol->string (car (list-ref l i)))
      i))

(define (spinner-index l s)
  (define (_ l n s)
    (cond
     ((null? l) 0)
     ((eq? (car (car l)) s) n)
     ((_ (cdr l) (+ n 1) s))))
  (_ l 0 (string->symbol s)))

(define (spinner-index->name l i)
  (define (_ l n)
    (cond
     ((null? l) "Unknown")
     ((zero? n) (cadr (car l)))
     ((_ (cdr l) (- n 1)))))
  (_ l i))

(define (mupdate-spinner id-symbol key choices)
  (let* ((val (entity-get-value key)))
    (if (not val)
        (update-widget 'spinner
                       (get-id (string-append (symbol->string id-symbol) "-spinner"))
                       'selection 0)
        (let ((index (index-find (string->symbol val) choices)))
          (if index
              (update-widget 'spinner
                             (get-id (string-append (symbol->string id-symbol) "-spinner"))
                             'selection index)
              (begin
                (msg "spinner item in db " val " not found in list of items")
                (update-widget 'spinner
                               (get-id (string-append (symbol->string id-symbol) "-spinner"))
                               'selection 0)))))))

(define (mupdate-spinner-other id-symbol key choices)
  (let* ((val (entity-get-value key)))
    (if (not val)
        (list (update-widget 'spinner
                             (get-id (string-append (symbol->string id-symbol) "-spinner"))
                             'selection 0))
        (let ((index (index-find (string->symbol val) choices)))
          (if index
              (list (update-widget 'spinner
                                   (get-id (string-append (symbol->string id-symbol) "-spinner"))
                                   'selection index))
              (list
               (update-widget 'spinner
                              (get-id (string-append (symbol->string id-symbol) "-spinner"))
                              'selection (- (length choices) 1))
               (update-widget 'edit-text
                              (get-id (string-append (symbol->string id-symbol) "-edit-text"))
                              'text val)))))))

;;;;
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

(define (do-gps display-id key-prepend)
  (list
   (alert-dialog
    "gps-check"
    (mtext-lookup 'gps-are-you-sure)
    (lambda (v)
      (cond
       ((eqv? v 1)
        (list
         (alert-dialog
          "gps-check2"
          (mtext-lookup 'gps-are-you-sure-2)
          (lambda (v)
            (cond
             ((eqv? v 1)
              (let ((loc (get-current 'location '(0 0))))
                (entity-set-value! (string-append key-prepend "-lat") "real" (car loc))
                (entity-set-value! (string-append key-prepend "-lon") "real" (cadr loc))
                (list
                 (update-widget
                  'text-view
                  (get-id (string-append (symbol->string display-id) "-lat"))
                  'text
                  (number->string (car loc)))
                 (update-widget
                  'text-view
                  (get-id (string-append (symbol->string display-id) "-lon"))
                  'text
                  (number->string (cadr loc))))))
             (else '()))))))
       (else '()))))))

(define (mupdate-gps display-id key-prepend)
  (let ((lat (entity-get-value (string-append key-prepend "-lat")))
        (lon (entity-get-value (string-append key-prepend "-lon"))))
    (if (or (not lat) (not lon))
        (list
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lat"))
          'text "O")
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lon"))
          'text "0"))
        (list
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lat"))
          'text (number->string lat))
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lon"))
          'text (number->string lon))))))


;; a standard builder for list widgets of entities and a
;; make new button, to add defaults to the list
(define (build-list-widget db table title entity-type edit-activity parent-fn ktv-default-fn)
    (vert-colour
     colour-two
     (horiz
      (mtitle-scale title)
      (button
       (make-id (string-append (symbol->string title) "-add"))
       (mtext-lookup 'add-item-to-list)
       40 (layout 100 'wrap-content 1 'centre 5)
       (lambda ()
         (entity-create!
          db table entity-type
          (ktvlist-merge
           (ktv-default-fn)
           (list (ktv "parent" "varchar" (parent-fn)))))
         (list (update-list-widget db table entity-type edit-activity (parent-fn))))))
     (linear-layout
      (make-id (string-append entity-type "-list"))
      'vertical
      (layout 'fill-parent 'wrap-content 1 'centre 20)
      (list 0 0 0 0)
      (list))))

;; pull db data into list of button widgets
(define (update-list-widget db table entity-type edit-activity parent)
  (let ((search-results
         (if parent
             (db-filter-only db table entity-type
                             (list (list "parent" "varchar" "=" parent))
                             (list (list "name" "varchar")))
             (db-all db table entity-type))))
    (update-widget
     'linear-layout
     (get-id (string-append entity-type "-list"))
     'contents
     (if (null? search-results)
         (list (mtext 'list-empty))
         (map
          (lambda (e)
            (button
             (make-id (string-append "list-button-" (ktv-get e "unique_id")))
             (or (ktv-get e "name") "Unamed item")
             40 (layout 'fill-parent 'wrap-content 1 'centre 5)
             (lambda ()
               (list (start-activity edit-activity 0 (ktv-get e "unique_id"))))))
          search-results)))))

(define (delete-button)
  (mbutton
   'delete
   (lambda ()
     (list
      (alert-dialog
       "delete-check"
       (mtext-lookup 'delete-are-you-sure)
       (lambda (v)
         (cond
          ((eqv? v 1)
           (entity-set-value! "deleted" "int" 1)
           (entity-update-values!)
           (list (finish-activity 1)))
          (else
           (list)))))))))

(define (build-array-from-names db table entity-type)
  (map
   (lambda (e)
     (list (ktv-get e "name")
           (ktv-get e "unique_id")))
   (db-filter-only db table entity-type
                   (list)
                   (list (list "name" "varchar")))))

(define (find-index-from-name-array arr unique-id)
  (define (_ l i)
    (cond
     ((null? l) #f)
     ((equal? unique-id (cadr (car l))) i)
     (else (_ (cdr l) (+ i 1)))))
  (_ arr 0))




(define (simpsons-village db table default-ktvlist)
  (entity-create! db table "village"
                  (ktvlist-merge
                   default-ktvlist
                   (list
                    (ktv "name" "varchar" (string-append "Village-" (number->string (random 1000))))
                    (ktv "block" "varchar" (word-gen))
                    (ktv "district" "varchar" (word-gen))
                    (ktv "car" "int" (random 2))))))

(define (simpsons-household db table parent default-ktvlist)
  (entity-create! db table "household"
                  (ktvlist-merge
                   default-ktvlist
                   (list
                    (ktv "name" "varchar" (string-append "Household-" (number->string (random 1000))))
                    (ktv "num-pots" "int" (random 10))
                    (ktv "parent" "varchar" parent)))))

(define (simpsons-individual db table parent default-ktvlist)
  (let ((n (random 1000)))
  (entity-create! db table "individual"
                  (ktvlist-merge
                   default-ktvlist
                   (append
                    (list (ktv "parent" "varchar" parent))
  (choose
   (list
   (list
    (ktv "name" "varchar"
                (string-append "Abe-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "abe.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Akira-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "akira.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Apu-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "apu.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Barney-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "barney.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Bart-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "bartsimpson.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Billy-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "billy.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Carl-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "carl.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Cletus-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "cletus.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "ComicBookGuy-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "comicbookguy.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Homer-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "homersimpson.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Jasper-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "jasper.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Kent-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "kentbrockman.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Kodos-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "kodos.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Lenny-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "lenny.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Lisa-" (number->string n)))
    (ktv "gender" "varchar" "female")
    (ktv "photo" "file" "lisasimpson.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Marge-" (number->string n)))
    (ktv "gender" "varchar" "female")
    (ktv "photo" "file" "margesimpson.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Martin-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "martinprince.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Milhouse-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "milhouse.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "MrBurns-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "mrburns.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Ned-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "nedflanders.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Nelson-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "nelson.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Otto-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "otto.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Ralph-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "ralphwiggum.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "Santaslittlehelper-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "santaslittlehelper.jpg"))
   (list
    (ktv
     "name" "varchar" (string-append "SideshowBob-" (number->string n)))
    (ktv "gender" "varchar" "male")
    (ktv "photo" "file" "sideshowbob.jpg")))))))))

(define (looper! n fn)
  (when (not (zero? n))
        (fn n)
        (looper! (- n 1) fn)))

(define (build-test! db table village-ktvlist household-ktvlist individual-ktvlist)
  (looper!
   1
   (lambda (i)
     (msg "making village" i)
     (let ((village (simpsons-village db table village-ktvlist)))
       (looper!
        3
        (lambda (i)
          (alog "household")
          (msg "making household" i)
          (let ((household (simpsons-household db table village household-ktvlist)))
            (looper!
             (random 10)
             (lambda (i)
               (msg "making individual" i)
               (simpsons-individual db table household individual-ktvlist))))))))))


(define (mangle-test! db table entities)
  (define (_ n)
    (when (not (zero? n))
          (let ((type (choose entities)))
            (msg type)
            (let ((entities (all-entities db table type)))
              (msg "entities:" entities)
              (when (not (null? entities))
                    (let ((id (choose entities)))
                      (msg "entity id:" id)
                      (let ((ktv-list (get-entity db table id)))
                        (when (not (null? ktv-list))
                              (entity-init! db table type ktv-list)
                              (for-each
                               (lambda (ktv)
                                 (when (and
                                        (not (equal? (ktv-key ktv) "deleted"))
                                        (not (equal? (ktv-key ktv) "unique_id"))
                                        (not (equal? (ktv-key ktv) "parent"))
                                        (eqv? (random 10) 0))
                                       (if (equal? (ktv-type ktv) "varchar")
                                           (entity-set-value! (ktv-key ktv) (ktv-type ktv)
                                                              (string-append
                                                               (get-current 'user-id "noid")
                                                               (random-value-for-type (ktv-type ktv))))
                                           (entity-set-value! (ktv-key ktv) (ktv-type ktv)
                                                              (random-value-for-type (ktv-type ktv))))))
                               ktv-list)
                              (msg "modifying" type id)
                              (entity-update-values!))
                        )))))
          (_ (- n 1))))
  (_ (random 10)))


;;;;;;;;;; m2000 cruft

;; todo, sort these out... use new filter system...

(define (all-entities-sort-normal db table type)
  (let ((s (db-select
            db (string-append
                "select e.entity_id from " table "_entity as e "
                "join " table "_value_varchar "
                "as n on n.entity_id = e.entity_id "
                "where entity_type = ? and n.attribute_id = ? "
                "order by n.value")
            type "name")))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (all-entities-where-ignore-delete db table type ktv)
  (let ((s (db-select
            db (string-append
                "select e.entity_id from " table "_entity as e "
                "join " table "_value_" (ktv-type ktv) " "
                "as a on a.entity_id = e.entity_id and a.attribute_id = ? and a.value = ? "
                "join " table "_value_varchar "
                "as n on n.entity_id = e.entity_id and n.attribute_id = ? "
                "where e.entity_type = ? order by substr(n.value,3)")
            (ktv-key ktv) (ktv-value ktv)
            "name" type)))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))


(define (all-entities-where db table type ktv)
  (let ((s (db-select
            db (string-append
                "select e.entity_id from " table "_entity as e "
                "join " table "_value_" (ktv-type ktv) " "
                "as a on a.entity_id = e.entity_id and a.attribute_id = ? and a.value = ? "
                "join " table "_value_varchar "
                "as n on n.entity_id = e.entity_id and n.attribute_id = ? "
                "left join " table "_value_int "
                "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                "where e.entity_type = ? and (d.value='NULL' or d.value is NULL or d.value = 0) "
                "order by substr(n.value,3)")
            (ktv-key ktv) (ktv-value ktv)
            "name" "deleted" type)))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (all-entities-where2 db table type ktv ktv2)
  (let ((s (db-select
            db (string-append
                "select e.entity_id from " table "_entity as e "
                "join " table "_value_" (ktv-type ktv) " "
                "as a on a.entity_id = e.entity_id and a.attribute_id = ? and a.value = ? "
                "join " table "_value_" (ktv-type ktv2) " "
                "as b on b.entity_id = e.entity_id and b.attribute_id = ? and b.value = ? "
                "join " table "_value_varchar "
                "as n on n.entity_id = e.entity_id and n.attribute_id = ? "
                "left join " table "_value_int "
                "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                "where e.entity_type = ? and (d.value='NULL' or d.value is NULL or d.value = 0) "
                "order by substr(n.value,3)")
            (ktv-key ktv) (ktv-value ktv)
            (ktv-key ktv2) (ktv-value ktv2)
            "name" "deleted" type)))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (all-entities-where2or db table type ktv ktv2 or-value)
  (let ((s (db-select
            db (string-append
                "select e.entity_id from " table "_entity as e "
                "join " table "_value_" (ktv-type ktv) " "
                "as a on a.entity_id = e.entity_id and a.attribute_id = ? and a.value = ? "
                "join " table "_value_" (ktv-type ktv2) " "
                "as b on b.entity_id = e.entity_id and b.attribute_id = ? and (b.value = ? or b.value = ?) "
                "join " table "_value_varchar "
                "as n on n.entity_id = e.entity_id and n.attribute_id = ? "
                "left join " table "_value_int "
                "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                "where e.entity_type = ? and (d.value='NULL' or d.value is NULL or d.value = 0) "
                "order by substr(n.value,3)")
            (ktv-key ktv) (ktv-value ktv)
            (ktv-key ktv2) (ktv-value ktv2) or-value
            "name" "deleted" type)))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (all-entities-where-newer db table type ktv ktv2)
  (let ((s (db-select
            db (string-append
                "select e.entity_id,d.value,b.value from " table "_entity as e "
                "join " table "_value_" (ktv-type ktv) " "
                "as a on a.entity_id = e.entity_id and a.attribute_id = ? and a.value = ?"
                "join " table "_value_" (ktv-type ktv2) " "
                "as b on b.entity_id = e.entity_id and b.attribute_id = ? and (b.value > DateTime(?) and b.value != ?) "
                "join " table "_value_varchar "
                "as n on n.entity_id = e.entity_id and n.attribute_id = ? "
                "left join " table "_value_int "
                "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                "where e.entity_type = ? and (d.value='NULL' or d.value is NULL or d.value = 0) "
                "order by substr(n.value,3)"
                )
            (ktv-key ktv) (ktv-value ktv)
            (ktv-key ktv2) (ktv-value ktv2) "Unknown"
            "name" "deleted"
            type)))
    (msg "where newer" (ktv-value ktv2) s)
    (msg "date select" (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (all-entities-where-older db table type ktv ktv2)
  (let ((s (db-select
            db (string-append
                "select e.entity_id from " table "_entity as e "
                "join " table "_value_" (ktv-type ktv) " "
                "as a on a.entity_id = e.entity_id and a.attribute_id = ? and a.value = ?"
                "join " table "_value_" (ktv-type ktv2) " "
                "as b on b.entity_id = e.entity_id and b.attribute_id = ? and (b.value < DateTime(?) and b.value != ?) "
                "join " table "_value_varchar "
                "as n on n.entity_id = e.entity_id and n.attribute_id = ? "
                "left join " table "_value_int "
                "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                "where e.entity_type = ? and (d.value='NULL' or d.value is NULL or d.value = 0) "
                "order by substr(n.value,3)")
            (ktv-key ktv) (ktv-value ktv)
            (ktv-key ktv2) (ktv-value ktv2) "Unknown"
            "name" "deleted" type)))
    (msg "date select" (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (update-entities-where2 db table type ktv ktv2)
  (let ((s (db-select
            db (string-append
                "select e.entity_id from " table "_entity as e "
                "join " table "_value_" (ktv-type ktv)
                " as a on a.entity_id = e.entity_id "
                "join " table "_value_" (ktv-type ktv2)
                " as b on b.entity_id = e.entity_id "
                "where e.entity_type = ? and a.attribute_id = ? and b.attribute_id =? and a.value = ? and b.value = ?")
            type
            (ktv-key ktv) (ktv-key ktv2)
            (ktv-value ktv) (ktv-value ktv2))))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))



(define (db-all-sort-normal db table type)
  (prof-start "db-all")
  (let ((r (map
   (lambda (i)
     (get-entity db table i))
   (all-entities-sort-normal db table type))))
    (prof-end "db-all")
    r))

(define (db-all-in-date-range db table type start end)
  (prof-start "db-all")
  (let ((r (map
   (lambda (i)
     (get-entity db table i))
   (all-entities-in-date-range db table type start end))))
    (prof-end "db-all")
    r))

(define (db-all-where-ignore-delete db table type ktv)
  (prof-start "db-all-where")
  (let ((r (map
            (lambda (i)
              (get-entity db table i))
            (all-entities-where-ignore-delete db table type ktv))))
    (prof-end "db-all-where")
    r))

(define (db-all-where db table type ktv)
  (prof-start "db-all-where")
  (let ((r (map
            (lambda (i)
              (get-entity db table i))
            (all-entities-where db table type ktv))))
    (prof-end "db-all-where")
    r))

(define (db-all-where2 db table type ktv ktv2)
  (prof-start "db-all-where2")
  (let ((r (map
            (lambda (i)
              (get-entity db table i))
            (all-entities-where2 db table type ktv ktv2))))
    (prof-end "db-all-where2")
    r))

(define (db-all-where2or db table type ktv ktv2 or-value)
  (prof-start "db-all-where2or")
  (let ((r (map
            (lambda (i)
              (get-entity db table i))
            (all-entities-where2or db table type ktv ktv2 or-value))))
    (prof-end "db-all-where2or")
    r))

(define (db-all-newer db table type ktv ktv2)
  (prof-start "db-all-where newer")
  (let ((r (map
            (lambda (i)
              (get-entity db table i))
            (all-entities-where-newer db table type ktv ktv2))))
    (prof-end "db-all-where newer")
    r))

(define (db-all-older db table type ktv ktv2)
  (prof-start "db-all-where older")
  (let ((r (map
            (lambda (i)
              (get-entity db table i))
            (all-entities-where-older db table type ktv ktv2))))
    (prof-end "db-all-where older")
    r))
