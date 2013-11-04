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
(require "eavdb.ss")
(require "utils.ss")
(provide (all-defined-out))


(define (request-args->ktvlist data)
  (map
   (lambda (i)
     (msg i)
     (let ((kv (string-split (symbol->string (car i)) '(#\:))))
       (list
        (car kv) (cadr kv) (cdr i))))
   data))

(define (sync-update db table entity-type unique-id dirty version data)
  (let ((entity-id (entity-id-from-unique db table unique-id))
        (ktvlist (dbg (request-args->ktvlist data))))
    (update-to-version db table entity-id version ktvlist)
    (list "updated" unique-id)))

(define (sync-insert db table entity-type unique-id dirty version data)
  (let ((ktvlist (dbg (request-args->ktvlist data))))
    (insert-entity-wholesale db table entity-type unique-id dirty version ktvlist)
    (list "inserted" unique-id)))

(define (send-version db table entity-type unique-id current-version)
  (let ((entity-id (entity-id-from-unique db table unique-id)))
    (list
     "new version"
     (list table entity-type entity-id unique-id current-version)
     (get-entity db table entity-id))))

(define (check-for-sync db table entity-type unique-id dirty version data)
  (let ((current-version (entity-version-from-unique db table unique-id)))
    (if (not (null? current-version))
        ;; if it exists
        (cond
         ;; everything matches - no change
         ((and (eq? dirty 0) (eq? version current-version))
          (list "no change" unique-id))

         ;; dirty but matches, should be ok (timeout causes this)
         ((and (eq? dirty 1) (eq? version current-version))
          (list "match" unique-id))

         ;; need to update existing data, newer version from android
         ((and (eq? dirty 1) (> version current-version) )
          (sync-update db table entity-type unique-id dirty version data))

         ;; need to send update
         ((and (eq? dirty 0) (< version current-version))
          (send-version db table entity-type unique-id current-version))

         ;; it's changed, but has an old or same version = conflict!!??
         ((and (eq? dirty 1) (<= version current-version))
          (list "CONFLICT" unique-id))

         ;; android version is newer but not changed??
         ((and (eq? dirty 0) (> version current-version))
          (list "MISMATCH" unique-id))

         (else
          (list "WAT?" unique-id)))

        ;; doesnt exist yet, so insert it
        (sync-insert db table entity-type unique-id dirty version data))))

(define (entity-versions db table)
  (map
   (lambda (i)
     (list (vector-ref i 0) (vector-ref i 1)))
   (cdr (db-select
         db (string-append "select unique_id, version from " table "_entity;")))))

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

(define (choose l) (list-ref l (random (length l))))

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
  (let ((db (db-open "unit.db")))
    (build db 99999999)
    ))

;(test)
