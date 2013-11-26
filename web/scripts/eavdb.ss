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


;; create eav tables (add types as required)
(define (setup db table)
  (db-exec db (string-append "create table " table "_entity ( entity_id integer primary key autoincrement, entity_type varchar(256), unique_id varchar(256), dirty integer, version integer)"))
  (db-exec db (string-append "create table " table "_attribute ( id integer primary key autoincrement, attribute_id varchar(256), entity_type varchar(256), attribute_type varchar(256))"))
  (db-exec db (string-append "create table " table "_value_varchar ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096), dirty integer)"))
  (db-exec db (string-append "create table " table "_value_int ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value integer, dirty integer)"))
  (db-exec db (string-append "create table " table "_value_real ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value real, dirty integer)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; basic key/type/value structure
(define (ktv key type value) (list key type value))
(define ktv-key car)
(define ktv-type cadr)
(define ktv-value caddr)

;; stringify based on type (for url)
(define (stringify-value ktv)
  (cond
   ((null? (ktv-value ktv)) "NULL")
   ((equal? (ktv-type ktv) "varchar") (string-append "'" (ktv-value ktv) "'"))
   (else
    (if (not (string? (ktv-value ktv)))
        (number->string (ktv-value ktv))
        (ktv-value ktv)))))

;; stringify based on type (for url)
(define (stringify-value-url ktv)
  (cond
   ((null? (ktv-value ktv)) "NULL")
   ((equal? (ktv-type ktv) "varchar") (ktv-value ktv))
   (else
    (if (not (string? (ktv-value ktv)))
        (number->string (ktv-value ktv))
        (ktv-value ktv)))))


;; helper to return first instance from a select
(define (select-first db str . args)
  (let ((s (apply db-select (append (list db str) args))))
    (if (or (null? s) (eq? s #t))
        '()
        (vector-ref (cadr s) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; putting data in

;; get the type from the attribute table with an entity/key
(define (get-attribute-type db table entity-type key)
  (let ((sql (string-append
              "select attribute_type from " table
              "_attribute where entity_type = ? and attribute_id = ?")))
    (select-first db sql entity-type key)))

;; search for a type and add it if it doesn't exist
(define (find/add-attribute-type db table entity-type key type)
  (let ((t (get-attribute-type db table entity-type key)))
    ;; add and return passed in type if not exist
    (cond
      ((null? t)
       (msg "adding new attribute for" entity-type " called " key " of type " type)
       (db-insert
        db (string-append "insert into " table "_attribute values (null, ?, ?, ?)")
        key entity-type type)
       type)
      (else
       (cond
         ((equal? type t) t)
         (else
          (msg "type has changed for" entity-type key "from" t "to" type "???")
          ;; wont work
          ;; what do we do?
          ;; some kind of coercion for existing data???
          type))))))

;; low level insert of a ktv
(define (insert-value db table entity-id ktv)
  ;; use type to dispatch insert to correct value table
  (db-insert db (string-append "insert into " table "_value_" (ktv-type ktv)
                               " values (null, ?, ?, ?, 0)")
             entity-id (ktv-key ktv) (ktv-value ktv)))

(define (get-unique user)
  (let ((t (time)))
    (string-append
     user "-" (number->string (car t)) ":" (number->string (cadr t)))))

;; insert an entire entity
(define (insert-entity db table entity-type user ktvlist)
  (insert-entity-wholesale db table entity-type (get-unique user) 1 0 ktvlist))

;; insert an entire entity
(define (insert-entity/get-unique db table entity-type user ktvlist)
  (let ((uid (get-unique user)))
    (insert-entity-wholesale db table entity-type uid 1 0 ktvlist)
    uid))

(define sema (make-semaphore 1))

;; all the parameters - for syncing purposes
(define (insert-entity-wholesale db table entity-type unique-id dirty version ktvlist)
  (semaphore-wait sema)
  (db-exec db "begin transaction")
  (let ((id (db-insert
             db (string-append
                 "insert into " table "_entity values (null, ?, ?, ?, ?)")
             entity-type unique-id dirty version)))

    ;; create the attributes if they are new, and validate them if they exist
    (for-each
     (lambda (ktv)
       (find/add-attribute-type db table entity-type (ktv-key ktv) (ktv-type ktv)))
     ktvlist)

    ;; add all the keys
    (for-each
     (lambda (ktv)
       (insert-value db table id ktv))
     ktvlist)

    (db-exec db "end transaction")
    (semaphore-post sema)

    id))

;; update the value given an entity type, a attribute type and it's key (= attriute_id)
;; creates the value if it doesn't already exist, updates it otherwise
(define (update-value db table entity-id ktv)
  (if (null? (select-first
              db (string-append
                  "select * from " table "_value_" (ktv-type ktv) " where entity_id = ? and attribute_id = ?")
              entity-id (ktv-key ktv)))
      (insert-value db table entity-id ktv)
      (db-exec
       db (string-append "update " table "_value_" (ktv-type ktv)
                         " set value=?  where entity_id = ? and attribute_id = ?")
       (ktv-value ktv) entity-id (ktv-key ktv))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getting data out

(define (entity-exists? db table unique-id)
  (not (null? (select-first
               db (string-append
                   "select * from " table "_entity where unique_id = ?")
               unique-id))))

(define (get-entity-type db table entity-id)
  (select-first
   db (string-append
       "select entity_type from " table "_entity where entity_id = ?")
       entity-id))

(define (get-all-entity-types db table)
  (cdr (db-select db (string-append "select distinct entity_type from " table "_entity;"))))

;; get all the (current) attributes for an entity type
(define (get-attribute-ids/types db table entity-type)
  (let ((s (db-select
            db (string-append
                "select * from " table "_attribute where entity_type = ?")
                entity-type)))
    (if (null? s) '()
        (map
         (lambda (row)
           (list (vector-ref row 1)    ;; id
                 (vector-ref row 3)))  ;; type
         (cdr s)))))

;; get the value given an entity type, a attribute type and it's key (= attriute_id)
(define (get-value db table entity-id kt)
  (select-first
   db (string-append "select value from " table "_value_" (ktv-type kt)
                     " where entity_id = ? and attribute_id = ?")
   entity-id (ktv-key kt)))

;; get an entire entity, as a list of key/value pairs
(define (get-entity-plain db table entity-id)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
      ((null? entity-type) (msg "entity" entity-id "not found!") '())
      (else
       (map
        (lambda (kt)
          (list (ktv-key kt) (ktv-type kt) (get-value db table entity-id kt)))
        (get-attribute-ids/types db table entity-type))))))

;; get an entire entity, as a list of key/value pairs (includes entity id)
(define (get-entity db table entity-id)
  (let ((unique-id (get-unique-id db table entity-id)))
    (cons
     (list "unique_id" "varchar" unique-id)
     (get-entity-plain db table entity-id))))

(define (all-entities db table type)
  (let ((s (db-select
            db (string-append "select entity_id from " table "_entity where entity_type = ?")
            type)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (validate db)
  ;; check attribute for duplicate entity-id/attribute-ids
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(define (ktv-get ktv-list key)
  (cond
   ((null? ktv-list) #f)
   ((equal? (ktv-key (car ktv-list)) key)
    (ktv-value (car ktv-list)))
   (else (ktv-get (cdr ktv-list) key))))

(define (ktv-set ktv-list ktv)
  (cond
   ((null? ktv-list) (list ktv))
   ((equal? (ktv-key (car ktv-list)) (ktv-key ktv))
    (cons ktv (cdr ktv-list)))
   (else (cons (car ktv-list) (ktv-set (cdr ktv-list) ktv)))))

(define (db-all db table type)
  (map
   (lambda (i)
     (get-entity db table i))
   (all-entities db table type)))

(define (db-all-where db table type clause)
  (foldl
   (lambda (i r)
     (let ((e (get-entity db table i)))
       (if (equal? (ktv-get e (car clause)) (cadr clause))
           (cons e r) r)))
   '()
   (all-entities db table type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; updating data

;; update an entire entity (version incl), via a (possibly partial) list of key/value pairs
(define (update-to-version db table entity-id version ktvlist)
  (update-entity-values db table entity-id ktvlist)
  (update-entity-version db table entity-id version))

;; auto update version
(define (update-entity db table entity-id ktvlist)
  (update-entity-changed db table entity-id)
  (update-entity-values db table entity-id ktvlist))

;; update an entity, via a (possibly partial) list of key/value pairs
(define (update-entity-values db table entity-id ktvlist)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
     ((null? entity-type) (msg "entity" entity-id "not found!") '())
     (else
      ;; update main entity type
      (for-each
       (lambda (ktv)
         (when (not (equal? (ktv-key ktv) "unique_id"))
               (find/add-attribute-type db table entity-type (ktv-key ktv) (ktv-type ktv))))
       ktvlist)
      (for-each
       (lambda (ktv)
         (update-value db table entity-id ktv))
       ktvlist)))))

;; update or create an entire entity if it doesn't exist
;; will return the new entity id if it's created
(define (update/insert-entity db table entity-type user entity-id ktvlist)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
     ((null? entity-type)
      (insert-entity db table entity-type user ktvlist))
     (else
      (update-entity db table entity-id ktvlist)
      #f))))

(define (insert-entity-if-not-exists db table entity-type user entity-id ktvlist)
  (let ((found (get-entity-type db table entity-id)))
    (if (null? found)
        (insert-entity db table entity-type user ktvlist)
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; versioning

(define (get-entity-version db table entity-id)
  (select-first
   db (string-append "select version from " table "_entity where entity_id = ?")
   entity-id))

(define (get-entity-dirty db table entity-id)
  (select-first
   db (string-append "select dirty from " table "_entity where entity_id = ?")
   entity-id))

(define (update-entity-changed db table entity-id)
  (db-exec
   db (string-append
       "update " table "_entity set dirty=?, version=? where entity_id = ?")
   1 (+ 1 (get-entity-version db table entity-id)) entity-id))

(define (update-entity-version db table entity-id version)
  (db-exec
   db (string-append
       "update " table "_entity set dirty=?, version=? where entity_id = ?")
   1 version entity-id))

(define (update-entity-clean db table unique-id)
  (db-exec
   db (string-append "update " table "_entity set dirty=? where unique_id = ?")
   0 unique-id))

(define (get-dirty-stats db table)
  (list
   (select-first
    db (string-append "select count(entity_id) from " table "_entity where dirty=1"))
   (select-first
    db (string-append "select count(entity_id) from " table "_entity;"))))

(define (dirty-entities db table)
  (let ((de (db-select
             db (string-append
                 "select entity_id, entity_type, unique_id, dirty, version from " table "_entity where dirty=1;"))))
    (if (null? de)
        '()
        (map
         (lambda (i)
           (list
            ;; build according to url ([table] entity-type unique-id dirty version)
            (cdr (vector->list i))
            ;; data entries (todo - only dirty values!)
            (get-entity-plain db table (vector-ref i 0))))
         (cdr de)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syncing

(define (stringify-list l)
  (foldl
   (lambda (i r)
     (string-append r " " i))
   "" l))

(define (stringify-ktvlist ktvlist)
  (foldl
   (lambda (i r)
     (string-append r " " (ktv-key i) ":" (stringify-value i)))
   ""
   ktvlist))

(define (build-sync-debug db table)
  (foldl
   (lambda (i r)
     (string-append
      r "\n" (vector-ref i 0) " " (vector-ref i 1) " "
      (stringify-ktvlist (get-entity db table (vector-ref i 0)))))
   ""
   (cdr (db-select
         db (string-append "select * from " table "_entity where dirty=1;")))))


(define (build-sync db table)
  (map
   (lambda (i)
     (list
      (vector->list i)
      (get-entity db table (vector-ref i 0))))
   (cdr (db-select
         db (string-append "select * from " table "_entity where dirty=1;")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doing things with unique ids

(define (entity-id-from-unique db table unique-id)
  (select-first
   db (string-append "select entity_id from " table "_entity where unique_id = ?")
   unique-id))

(define (entity-version-from-unique db table unique-id)
  (select-first
   db (string-append "select version from " table "_entity where unique_id = ?")
   unique-id))


(define (get-unique-id db table entity-id)
  (select-first
   db (string-append
       "select unique_id from " table "_entity where entity_id = ?")
       entity-id))

(define (get-entity-id db table unique-id)
  (select-first
   db (string-append
       "select entity_id from " table "_entity where unique_id = ?")
   unique-id))

(define (get-entity-name db table unique-id)
  (let ((e (get-entity-id db table unique-id)))
    (if (null? e)
	unique-id
	(let ((r (ktv-get (get-entity db table e) "name")))
	  (if (null? r)
	      (begin ;(msg "no name for" unique-id "found")
		unique-id)
	      r)))))

(define (get-entity-names db table id-list)
  (foldl
   (lambda (id r)
     (if (equal? r "")
         (get-entity-name db table id)
         (string-append r ", " (get-entity-name db table id))))
   ""
   id-list))

(define (csv-titles db table entity-type)
  (foldl
   (lambda (kt r)
     (if (equal? r "") (string-append "\"" (ktv-key kt) "\"")
         (string-append r ", \"" (ktv-key kt) "\"")))
   "\"id\""
   (get-attribute-ids/types db table entity-type)))

(define (csv db table entity-type)
  (foldl
   (lambda (res r)
     (let ((entity (get-entity db table (vector-ref res 0))))
       (string-append
        r "\n"
        (foldl
         (lambda (ktv r)
           (cond
            ((equal? (ktv-key ktv) "unique_id") r)
            ((null? (ktv-value ktv))
             ;;(msg "value not found in csv for " (ktv-key ktv))
             r)
            ;; dereferences lists of ids
            ((and
              (> (string-length (ktv-key ktv)) 8)
              (equal? (substring (ktv-key ktv) 0 8) "id-list-"))
             (string-append r ", \"" (get-entity-names db "sync" (string-split (ktv-value ktv) '(#\,))) "\""))
	    ;; look for unique ids and dereference them
            ((and
              (> (string-length (ktv-key ktv)) 3)
              (equal? (substring (ktv-key ktv) 0 3) "id-"))
             (string-append r ", \"" (get-entity-name db "sync" (ktv-value ktv)) "\""))
            (else
             (string-append r ", \"" (stringify-value-url ktv) "\""))))
         (string-append "\"" (vector-ref res 1) "\"") ;; unique_id
         entity))))
   (csv-titles db table entity-type)
   (let ((r (db-select
	     db (string-append
		 "select entity_id, unique_id from "
		 table "_entity where entity_type = ?") entity-type)))
     (if (null? r) r (cdr r)))))


(define (db-open db-name)
  (cond
    ((file-exists? (string->path db-name))
     (display "open existing db")(newline)
     (open (string->path db-name)))
    (else
     (display "making new db")(newline)
     (let ((db (open (string->path db-name))))
       ;; todo, dynamically create these tables
       (setup db "sync")
       (setup db "stream")
       db))))





(define (unit-tests)
  ;; db
(msg "testing db")
(define db "unit-test.db")
(set! db (db-open db))

(define (feq a b)
  (< (abs (- a b)) 0.001))

;;(msg (db-status db))

;; test low level sql
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

;; test the entity attribute value system
(define table "eavunittest")
(setup db table)

(asserteq "ktv one" (stringify-value (ktv "one" "varchar" "two")) "'two'")
(asserteq "ktv 2" (stringify-value (ktv "one" "int" 3)) "3")
(asserteq "ktv 3" (stringify-value-url (ktv "one" "varchar" "two")) "two")
(asserteq "ktv 4" (stringify-value-url (ktv "one" "int" 3)) "3")

(asserteq "select first" (select-first db "select name from unittest where id = ?" (+ id 1))
          "hello2")

(define e (insert-entity db table "thing" "me" (list (ktv "param1" "varchar" "bob")
                                                     (ktv "param2" "int" 30)
                                                     (ktv "param3" "real" 3.141))))

(asserteq "eav ent type" (get-entity-type db table e) "thing")

(let ((e (get-entity db table e)))
  (asserteq "entity get 1" (ktv-get e "param1") "bob")
  (asserteq "entity get 2" (ktv-get e "param2") 30)
  (assert "entity get 3" (feq (ktv-get e "param3") 3.141)))

(update-value db table e (ktv "param1" "varchar" "fred"))

(let ((e (get-entity db table e)))
  (asserteq "update value 1" (ktv-get e "param1") "fred")
  (asserteq "update value 2" (ktv-get e "param2") 30))

(assert "all-entities" (> (length (all-entities db table "thing")) 0))

(update-entity db table e (list (ktv "param1" "varchar" "wotzit")
                                (ktv "param2" "int" 1)))

(let ((e (get-entity db table e)))
  (asserteq "update-entity 1" (ktv-get e "param1") "wotzit")
  (asserteq "update-entity 2" (ktv-get e "param2") 1))

(update-entity db table e (list (ktv "param3" "real" 3.3)))

(let ((e (get-entity db table e)))
  (msg e)
  (asserteq "update-entity 3" (ktv-get e "param1") "wotzit")
  (asserteq "update-entity 4" (ktv-get e "param2") 1)
  (assert "update-entity 5" (feq (ktv-get e "param3") 3.3)))

(define e2 (insert-entity db table "thing" "me"
                          (list (ktv "param1" "varchar" "bob")
                                (ktv "param2" "int" 30)
                                (ktv "param3" "real" 3.141)
                                (ktv "param4" "int" 0))))

(let ((e (get-entity db table e2)))
  (msg e)
  (asserteq "new entity 1" (ktv-get e "param1") "bob")
  (asserteq "new entity 2" (ktv-get e "param2") 30)
  (assert "new entity 3" (feq (ktv-get e "param3") 3.141))
  (asserteq "new entity 3" (ktv-get e "param4") 0))

;; test the versioning
(asserteq "dirty flag" (get-entity-dirty db table e) 1)
(asserteq "dirty flag2" (get-entity-dirty db table e2) 1)
(let ((uid (get-unique-id db table e2)))
  (update-entity-clean db table uid))
(asserteq "dirty flag post clean" (get-entity-dirty db table e2) 0)
(asserteq "versioning" (get-entity-version db table e) 2)
(asserteq "dirty flag3" (get-entity-dirty db table e) 1)
(assert "dirty" (> (length (dbg (dirty-entities db table))) 0))

(for-each
 (lambda (e)
   (update-entity-clean
    db table
    (list-ref (car e) 1)))
 (dirty-entities db table))

(asserteq "cleaning" (length (dirty-entities db table)) 0)

(msg (csv db table "thing"))

(msg (db-status db))
)

;(unit-tests)
