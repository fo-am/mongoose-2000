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
(provide (all-defined-out))

;; tinyscheme
;(define db-select db-exec)

;; racket
(define db-exec exec/ignore)
(define db-select select)
(define db-insert insert)
(define (db-status db) (errmsg db))
(define (time) (list (random) (random))) ; ahem

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-open db-name setup-fn)
  (cond
    ((file-exists? (string->path db-name))
     (display "open existing db")(newline)
     (open (string->path db-name)))
    (else
     (display "making new db")(newline)
     (let ((db (open (string->path db-name))))
       ;; todo, dynamically create these tables
       (setup-fn db "sync")
       (setup-fn db "stream")
       db))))


;; helper to return first instance from a select
(define (select-first db str . args)
  (let ((s (apply db-select (append (list db str) args))))
    (if (or (null? s) (eq? s #t))
        '()
        (vector-ref (cadr s) 0))))

;; get a unique hash for this user (used for all the unique-ids)
(define (get-unique user)
  (let ((t (time)))
    (string-append
     user "-" (number->string (car t)) ":" (number->string (cadr t)))))


;; tests...

(define (sql-test db)
  (db-exec db "create table unittest ( id integer primary key autoincrement, name varchar(256), num int, r real )")

  (define id (db-insert db "insert into unittest values (null, ?, ?, ?)" "hello" 23 1.1))
  (asserteq "sql autoinc" (+ id 1) (db-insert db "insert into unittest values (null, ?, ?, ?)" "hello2" 26 2.3))

  (let ((q (db-select db "select * from unittest")))
    (assert "sql length" (> (length q) 2)))

  (let ((q (db-select db "select * from unittest where id = ?" id)))
    (asserteq "sql select one" (length q) 2)
    (assert "sql select two" (vector? (car q)))
    (asserteq "sql select 3" (vector-ref (cadr q) 2) 23)
    (assert "sql select 4" (feq (vector-ref (cadr q) 3) 1.1)))

  (db-exec db "update unittest set name=? where id = ?" "bob" id)

  (let ((q (db-select db "select * from unittest where id = ?" id)))
    (asserteq "sql update" (vector-ref (cadr q) 1) "bob"))

  (db-exec db "update unittest set name=? where id = ?" "Robert'); DROP TABLE unittest;--" id)

  (let ((q (db-select db "select * from unittest where id = ?" id)))
    (asserteq "bobby tables sql injection" (vector-ref (cadr q) 1) "Robert'); DROP TABLE unittest;--"))

  (asserteq "select first" (select-first db "select name from unittest where id = ?" (+ id 1))
            "hello2")

  )
