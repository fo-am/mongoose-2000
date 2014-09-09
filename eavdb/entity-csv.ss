;; Naked on Pluto Copyright (C) 2010 Aymeric Mansoux, Marloes de Valk, Dave Griffiths
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

#lang scheme

(require
 "../web/scripts/utils.ss"
 "../web/scripts/sql.ss"
 "ktv.ss"
 "ktv-list.ss"
 "entity-values.ss"
 "entity-get.ss"
 "eavdb.ss")

(provide (all-defined-out))

;; prettifications stuff - also used for review on the app

(define (string-remove-whitespace str)
  (define (_ i)
    (cond
     ((>= i (string-length str)) "")
     ((char-whitespace? (string-ref str i))
      (_ (+ i 1)))
     (else (string-append (string (string-ref str i))
                          (_ (+ i 1))))))
  (_ 0))

(define (ktv-key-is-id? ktv)
  (or
   (equal? (ktv-key ktv) "pack")
   (and (equal? (ktv-key ktv) "present")
        (equal? (ktv-type ktv) "varchar"))
   (equal? (ktv-key ktv) "pregnant")
   (equal? (ktv-key ktv) "baby-seen")
   (equal? (ktv-key ktv) "baby-byelim")
   (equal? (substring (ktv-key ktv) 0 3) "id-")))

;; search for a comma in a list of ids
(define (ktv-value-is-list? ktv)
  (foldl
   (lambda (c r)
     (if (or r (eqv? c #\,)) #t r))
   #f
   (string->list (ktv-value ktv))))

(define (uid->name db uid)
  (let* ((entity-id (entity-id-from-unique db "sync" uid)))
    (if (null? entity-id)
	uid
	(ktv-get (get-entity-only db "sync" entity-id
				  (list (list "name" "varchar")))
		 "name"))))

(define (uid-list->names db str)
  (let ((ids (string-split str (list #\,))))
    (foldl
     (lambda (id r)
       (if (equal? r "")
	   (uid->name db id)
	   (string-append r ", " (uid->name db id))))
     ""
     ids)))

(define (convert-id db name)
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

(define (convert-id-list db str)
  (let ((names (string-split str (list #\,))))
    (foldl
     (lambda (name r)
       (if (string? r)
           (let ((id (convert-id db name)))
             (if id
                 (if (equal? r "") id (string-append r "," id))
                 #f))
           #f))
     "" names)))


(define (csv-titles db table entity-type)
  (foldl
   (lambda (kt r)
     (if (equal? r "") (string-append "\"" (ktv-key kt) "\"")
         (string-append r ", \"" (ktv-key kt) "\"")))
   "id "
   (get-attribute-ids/types db table entity-type)))


; basic csv
(define (csv db table entity-type)
  (let ((s (db-select
         db (string-append
             "select entity_id, unique_id from "
             table "_entity where entity_type = ?") entity-type)))
    (msg "CSV ------------------------------>" entity-type)
    (if (null? s)
	;; nothing here, just return titles
	(csv-titles db table entity-type)
	(foldl
	 (lambda (res r)
	   (let ((entity (get-entity-for-csv db table (vector-ref res 0))))
	     (string-append
	      r "\n"
	      (foldl
	       (lambda (ktv r)
		 (cond
		  ((equal? (ktv-key ktv) "unique_id") r)
		  ((null? (ktv-value ktv))
		   (msg "value not found in csv for " (ktv-key ktv))
		   (string-append r ", NULL"))
		  ;; dereferences lists of ids
		  (else
		   (string-append r ", \"" (stringify-value-url ktv) "\""))))
	       (vector-ref res 1) ;; unique_id
	       entity))))
	 (csv-titles db table entity-type)
	 (cdr s)))))

;; convert uids to names
(define (csv-pretty db table entity-type)
  (let ((s (db-select
         db (string-append
             "select entity_id, unique_id from "
             table "_entity where entity_type = ?") entity-type)))
    (msg "CSV ------------------------------>" entity-type)
    (if (null? s)
	;; nothing here, just return titles
	(csv-titles db table entity-type)
	(foldl
	 (lambda (res r)
	   (let ((entity (get-entity-for-csv db table (vector-ref res 0))))
	     (string-append
	      r "\n"
	      (foldl
	       (lambda (ktv r)
		 (cond
		  ((ktv-key-is-id? ktv)
		   (let ((replacement
			  (if (ktv-value-is-list? ktv)
			      (uid-list->names db (ktv-value ktv))
			      (uid->name db (ktv-value ktv)))))
		     (if replacement
			 (string-append r ", \"" replacement "\"")
			 ;; ditch the entity and return error
			 (string-append r ", \"" (ktv-value ktv) "\""))))
		  
		  ((null? (ktv-value ktv))
		   (msg "value not found in csv for " (ktv-key ktv))
		   (string-append r ", NULL"))
		  ;; dereferences lists of ids
		  (else
		   (string-append r ", \"" (stringify-value-url ktv) "\""))))
	       (vector-ref res 1) ;; unique_id
	       entity))))
	 (csv-titles db table entity-type)
	 (cdr s)))))


(define (csv-convert col)
  (if (number? col) (number->string col)
      (if (string? col) col
          (begin
            (msg "csvify found:" col) "oops"))))

;; convert list of lists into comma seperated columns
;; and newline seperated rows
(define (csvify l)
  (foldl
   (lambda (row r)
     (let ((row-text
            (foldl
             (lambda (col r)
               (let ((converted (csv-convert col)))
                 (if (equal? r "")
                     converted
                     (string-append r ", " converted))))
             "" row)))
       (msg row-text)
       (string-append r row-text "\n")))
   "" l))


(define (ktv-filter ktv-list key)
  (filter
   (lambda (ktv)
     (not (equal? (ktv-key ktv) key)))
   ktv-list))

(define (ktv-filter-many ktv-list key-list)
  (foldl
   (lambda (key r)
     (ktv-filter r key))
   ktv-list
   key-list))
