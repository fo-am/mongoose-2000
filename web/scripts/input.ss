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
(require "utils.ss")
(require "eavdb.ss")
(provide (all-defined-out))
(require (planet neil/csv:1:=7) net/url)

(define make-mongoose-csv-reader
  (make-csv-reader-maker
   '((separator-chars              #\tab)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (all-rows url make-reader)
  (define next-row (make-reader (open-input-file url)))
  (define (loop)
    (define row (next-row))
    (if (empty? row)
        '()
        (cons row (loop))))
  (loop))



(define (insert-mongooses db table l)
  (map
   (lambda (i)
     (let ((pack (car (db-all-where db table "pack" (list "name" (list-ref i 2)))))
           (date (string-split (list-ref i 3) '(#\/))))
       (msg i)
       (insert-entity db table "mongoose" "sys"
                      (list
                       (ktv "name" "varchar" (list-ref i 0))
                       (ktv "gender" "varchar"
                            (if (equal? (list-ref i 1) "F") "Female" "Male"))
                       (ktv "pack-id" "varchar" (ktv-get pack "unique_id"))
                       (ktv "litter-code" "varchar" (if (eq? (length i) 5) (list-ref i 4) ""))
                       (ktv "chip-code" "varchar" "")
                       (ktv "dob" "varchar" (string-append
                                             (list-ref date 2) "-"
                                             (list-ref date 1) "-"
                                             (list-ref date 0)))
                       ))))
   l))

(define (insert-csv db table path)
  (let ((data (cdr (all-rows path make-mongoose-csv-reader))))
    (insert-mongooses db table data)))

(define (insert-packs db table l)
  (map
   (lambda (i)
     (msg "insert pack" i)
     (insert-entity db table "pack" "sys"
                    (list
                     (ktv "name" "varchar" i))))
   l))


(define (write-db db table path)
  (insert-packs db table (list "11" "14" "15" "17" "18" "1B" "1H" "2" "4B" "4E" "7A"))
  (insert-csv db table path))
