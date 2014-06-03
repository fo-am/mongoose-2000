#lang racket

;; MongooseWeb Copyright (C) 2013 Dave Griffiths
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

(require (planet jaymccarthy/sqlite:5:1/sqlite))
(require
 "utils.ss"
 "sql.ss"
 "../../eavdb/ktv.ss"
 "../../eavdb/ktv-list.ss"
 "../../eavdb/entity-values.ss"
 "../../eavdb/entity-insert.ss"
 "../../eavdb/entity-get.ss"
 "../../eavdb/entity-update.ss"
 "../../eavdb/entity-sync.ss"
 "../../eavdb/entity-filter.ss"
 "../../eavdb/eavdb.ss")

(provide (all-defined-out))


(define (request-args->ktvlist data)
  (map
   (lambda (i)
     (let ((kv (string-split (symbol->string (car i)) '(#\:))))
       (list (car kv) (cadr kv) (cdr i))))
   data))

(define (sync-update db table entity-type unique-id dirty version data)
  (let ((entity-id (entity-id-from-unique db table unique-id))
        (ktvlist (request-args->ktvlist data)))
    (msg "sync-update")
    (update-to-version db table entity-id version ktvlist)
    (list "updated" unique-id)))

(define (sync-insert db table entity-type unique-id dirty version data)
  (let ((ktvlist (request-args->ktvlist data)))
    (msg "inserting new")
    (insert-entity-wholesale db table entity-type unique-id dirty version ktvlist)
    (list "inserted" unique-id)))

(define (send-version db table entity-type unique-id current-version)
  (let ((entity-id (entity-id-from-unique db table unique-id)))
    (list
     "new version"
     (list table entity-type entity-id unique-id current-version)
     (get-entity db table entity-id))))

(define (merge-n-bump current-version db table entity-type unique-id dirty version data)
  (let ((entity-id (entity-id-from-unique db table unique-id)))
    (msg "merge start:" (get-entity-version db table entity-id))
    (let ((r (sync-update db table entity-type unique-id dirty version data)))
      (msg "merge post:" (get-entity-version db table entity-id))
      ;; must be one newer than highest in the system
      (update-entity-version db table entity-id (+ current-version 1))
      (msg "merge over:" (get-entity-version db table entity-id))
      r)))

(define (check-for-sync db table entity-type unique-id dirty version data)
  (let ((current-version (entity-version-from-unique db table unique-id)))
    (if (not (null? current-version))
(begin	(msg "versions" version "vs previous " current-version)

        ;; if it exists
        (cond

	 ;; dirty path - basically merge it whatever...

         ;; need to update existing data, newer version from android
         ((and (eq? dirty 1) (> version current-version) )
	  (msg "NEWER - merging...")
	  ;; bump the version as this is a new entity post-merge
	  (merge-n-bump version db table entity-type unique-id dirty version data))

         ;; dirty but matches, should be ok (timeout causes this)
         ((and (eq? dirty 1) (eq? version current-version))
	  (msg "MATCHES, merging...")
	  ;;(list "match" unique-id))
	  ;; bump the version number so others get merged version
	  (merge-n-bump current-version db table entity-type unique-id dirty version data))

         ;; it's changed, but has an old or same version = conflict!!??
	 ;; still merge, but complicated...
         ((and (eq? dirty 1) (< version current-version))
	  (msg "CONFLICT, merging")
          (list "CONFLICT" unique-id)
	  ;; bump the version number so others get merged version
	  (merge-n-bump current-version db table entity-type unique-id dirty version data))

	 ;; not dirty path (avoid doing stuff here as it's probably a bug)

         ;; android version is newer than existing but not changed??
         ((and (eq? dirty 0) (> version current-version))
	  (msg "MISMATCH")
          (list "MISMATCH" unique-id))

         ;; everything matches - no change
         ((and (eq? dirty 0) (eq? version current-version))
	  (msg "NOT DIRTY, WHY SENT? (eq)")
          (list "no change" unique-id))

         ;; need to send update
         ((and (eq? dirty 0) (< version current-version))
	  (msg "NOT DIRTY, WHY SENT? (older)")
          (list "no change" unique-id))

         (else
	  (msg "WAT?")
          (list "WAT?" unique-id))))

        ;; doesnt exist yet, so insert it
        (sync-insert db table entity-type unique-id dirty version data))))

(define (entity-versions db table)
  (let ((s (db-select
	    db (string-append "select unique_id, version from " table "_entity;"))))
    (if (null? s)
	'()
	(map
	 (lambda (i)
	   (list (vector-ref i 0) (vector-ref i 1)))
	 (cdr s)))))

(define (send-entity db table unique-id)
  (let* ((entity-id (entity-id-from-unique db table unique-id))
         (entity (db-select
                  db (string-append "select entity_type, unique_id, version from "
                                    table "_entity where entity_id = ?")
                  entity-id)))
    (if (not (null? entity))
        (list
         (vector->list (cadr entity))
         (get-entity-plain db table entity-id))
        (list "entity not found" unique-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define db (open-db "test.db"))

;;(add-entity
;; db "mongoose"
;; (list
;;  (ktv "code" "varchar" "brendon")
;;  (ktv "gender" "varchar" "male")
;;  (ktv "pack-id" "int" 1)
;;  (ktv "weight" "real" 10.4)))


(define (random-string len)
  (if (zero? len)
      "" (string-append (choose (list "g" "t" "a" "c"))
                        (random-string (- len 1)))))

(define (random-ktv)
  (ktv (random-string 2) "varchar" (random-string 4096)))

(define (random-entity size)
  (if (zero? size)
      '() (cons (random-ktv) (random-entity (- size 1)))))

(define (insert-random-entity db)
  (msg "building")
  (let ((e (random-entity 40)))
    (msg "inserting")
    (insert-entity
     db (random-string 2) e)))

(define (build db n)
  (when (not (zero? n))
        (msg "adding entity" n)
        (insert-random-entity db)
        (build db (- n 1))))

(define (test)
  (let ((db (db-open "unit.db" setup)))
    (build db 99999999)
    ))

;(test)
