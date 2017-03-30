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
 "entity-get.ss")

(provide (all-defined-out))

(define (csv-titles db table entity-type)
  (foldl
   (lambda (kt r)
     (if (equal? r "") (string-append "\"" (ktv-key kt) "\"")
         (string-append r ", \"" (ktv-key kt) "\"")))
   "id "
   (get-attribute-ids/types db table entity-type)))

(define (csv db table entity-type)
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
            ((and
              (> (string-length (ktv-key ktv)) 8)
              (equal? (substring (ktv-key ktv) 0 8) "id-list-"))
             (string-append r ", \"" (get-entity-names db "sync" (string-split (ktv-value ktv) '(#\,))) "\""))
            ;; look for unique ids and dereference them
            ((and
              (> (string-length (ktv-key ktv)) 3)
              (equal? (substring (ktv-key ktv) 0 3) "id-")
	      (not (equal? (ktv-value ktv) "none")))
	     (let ((name (get-entity-name db "sync" (ktv-value ktv))))
	       (if (null? name)
		   "\"nobody\""
		   (string-append r ", \"" name "\""))))
            (else
             (string-append r ", \"" (stringify-value-url ktv) "\""))))
         (vector-ref res 1) ;; unique_id
         entity))))
   (csv-titles db table entity-type)
   (cdr (db-select
         db (string-append
             "select entity_id, unique_id from "
             table "_entity where entity_type = ?") entity-type))))
