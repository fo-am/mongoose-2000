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


;; colours
(msg "starting up....")

(define entity-types
  (list
   "pack"
   "mongoose"
   ))


(define trans-col (list 0 0 0 0))
(define colour-one (list 0 0 255 100))
(define colour-two (list  127 127 255 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent database

(define db "/sdcard/mongoose/local-mongoose.db")
(db-open db)

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

(define (mspinner-scale id list fn)
  (spinner (make-id id) (map cadr list) (layout 'fill-parent 'wrap-content 1 'centre 0) fn))

(define (mtext id text)
  (text-view (make-id id) text 20 (layout 'fill-parent 'wrap-content 1 'centre 0)))

(define (mtitle id text)
  (text-view (make-id id) text 40 (layout 'fill-parent 'wrap-content 1 'centre 0)))

(define (medit-text id text type fn)
  (vert
   (mtext (string-append id "-title") text)
   (edit-text (make-id id) "" 20 type (layout 'fill-parent 'wrap-content 1 'centre 0) fn)))

(define (medit-text-value id text value type fn)
  (linear-layout
   0 'horizontal
   (layout 'fill-parent 'wrap-content -1 'centre 2)
   (list 0 0 0 10)
  (list
   (text-view (make-id (string-append id "-title")) text 20 (layout 'fill-parent 'wrap-content 1 'right 0))
   (edit-text (make-id id) value 20 type (layout 'fill-parent 'wrap-content 1 'centre 0) fn))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review

(define (review-build-contents uid entity)
  (append
   (foldl
    (lambda (ktv r)
      (append
       r (cond
          ((equal? (ktv-type ktv) "varchar")
           ;; normal varchar
           (list (medit-text-value (string-append uid (ktv-key ktv))
                                   (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                   (ktv-value ktv) "normal"
                                   (lambda (v)
                                     (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
          ((equal? (ktv-type ktv) "file")
           ;; normal varchar
           (list (medit-text-value (string-append uid (ktv-key ktv))
                                   (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                   (ktv-value ktv) "normal"
                                   (lambda (v)
                                     (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
          ((equal? (ktv-type ktv) "int")
           (list (medit-text-value (string-append uid (ktv-key ktv))
                                   (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                   (number->string (ktv-value ktv)) "numeric"
                                   (lambda (v)
                                     (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
          ((equal? (ktv-type ktv) "real")
           (list (medit-text-value (string-append uid (ktv-key ktv))
                                   (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                   ;; get around previous bug, should remove
                                   (if (number? (ktv-value ktv))
                                       (number->string (ktv-value ktv))
                                       (ktv-value ktv)) "numeric"
                                       (lambda (v)
                                     (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
          (else (mtext "" (string-append (ktv-type ktv) " not handled!!")) '()))))
    '()
    entity)
   (list
    (horiz
     (mbutton "review-item-cancel" "Cancel" (lambda () (list (finish-activity 0))))
     (mbutton (string-append uid "-save") "Save"
              (lambda ()
                (list
                 (alert-dialog
                  "review-ok"
                  (string-append "Are you sure?")
                  (lambda (v)
                    (cond
                     ((eqv? v 1)
                      (entity-update-values!)
                      (list))
                     (else (list))))))))))))


(define (review-item-build)
  (let ((uid (entity-get-value "unique_id")))
    (list
     (update-widget
      'linear-layout
      (get-id "review-item-container")
      'contents
      (review-build-contents
       uid (get-current 'entity-values '()))))))

(define (review-update-list entity-type)
  (list
   (update-widget
    'linear-layout (get-id "review-list") 'contents
    (map
     (lambda (e)
       (let* ((uid (ktv-get e "unique_id")))
         (msg e)
         (mbutton
          (string-append "review-" uid)
          (ktv-get e "name")
          (lambda ()
            ;; special version to include nulls
            (entity-init! db "sync" entity-type (get-entity-all-by-unique db "sync" uid))
            (list (start-activity "review-item" 0 ""))))))
     (db-filter-only-inc-deleted db "sync" entity-type
                                 (list)
                                 (list (list "name" "varchar")))))))


(define-activity-list

  (activity
   "main"
   (vert
    (horiz
     (text-view 0 "Mongoose db admin" 40 (layout 'fill-parent 'wrap-content 1 'left 0))
     (mspinner-scale "entity-type" (map (lambda (v) (list "" v)) entity-types)
                     (lambda (v)
                       (review-update-list
                        (list-ref entity-types v)))))


    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 1 'left 0)
     (list
      (linear-layout
       (make-id "review-list")
       'vertical
       (layout 'fill-parent 'fill-parent 1 'left 0)
       (list 0 0 0 0)
       (list))
      )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "review-item"
   (vert
    (text-view (make-id "title") "Edit item" 40 fillwrap)
    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 1 'left 0)
     (list
      (linear-layout
       (make-id "review-item-container")
       'vertical
       (layout 'fill-parent 'wrap-content 1 'left 0)
       (list 0 0 0 0)
       (list))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (review-item-build))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  )


;(build-test! db "sync" village-ktvlist household-ktvlist individual-ktvlist)
