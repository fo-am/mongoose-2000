;; Mongoose 2000 Copyright (C) 2018 FoAM Kernow
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

(define (init-litter pack-id)
  ;; retreve (and increment) the pack's id info
  (entity-init! db "sync" "pack" (dbg (get-entity-by-unique db "sync" pack-id)))
  (msg (entity-get-value "unique_id"))

  (let ((letter (entity-get-value "litter-code-letter"))
	(number (entity-get-value "litter-code-number"))
	(parent (entity-get-value "unique_id")))
    (let ((name (cond 
		 ((or (not letter) (not number)) "No ID")
		 (else
		  (entity-set-value! "litter-code-number" "int" (+ number 1))
		  (entity-update-values!) ;; hmm, cancel won't work
		  (set-current! 'pack (es-ktv-list)) ;; need to remove this pack stuff...
		  (string-append letter (number->string number))))))
      (list
       (ktv "name" "varchar" name)
       (ktv "parent" "varchar" parent) 
       (ktv "date" "varchar" (date->string (date-time)))))))

(define (db-current-litter)
  (db-filter
   db "sync" "litter"
   (list
    (list "parent" "varchar" "=" (ktv-get (get-current 'pack '()) "unique_id"))
    (list "date" "varchar" "d<" 30))))


