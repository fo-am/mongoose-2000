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

;; sql (in)sanity

;; android/racket stuff
(define exec/ignore db-exec)
(define db-select db-exec)

;; create eav tables (add types as required)
(define (setup db table)
  (exec/ignore db (string-append "create table " table "_entity ( entity_id integer primary key autoincrement, entity_type varchar(256), unique_id varchar(256), dirty integer)"))
  (exec/ignore db (string-append "create table " table "_attribute ( id integer primary key autoincrement, attribute_id varchar(256), entity_type varchar(256), attribute_type varchar(256))"))
  (exec/ignore db (string-append "create table " table "_value_varchar ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096))"))
  (exec/ignore db (string-append "create table " table "_value_int ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value integer)"))
  (exec/ignore db (string-append "create table " table "_value_real ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value real)")))

(define (sqls str)
  ;; todo sanitise str
  str)

;; basic key/type/value structure
(define (ktv key type value) (list key type value))
(define ktv-key car)
(define ktv-type cadr)
(define ktv-value caddr)

;; stringify based on type
(define (stringify-value ktv)
  (cond
    ((equal? (ktv-type ktv) "varchar") (string-append "'" (ktv-value ktv) "'"))
    (else (number->string (ktv-value ktv)))))

;; helper to return first instance from a select
(define (select-first db str)
  (let ((s (db-select db str)))
    (if (or (null? s) (eq? s #t))
        '()
        (vector-ref (cadr s) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; putting data in

;; get the type from the attribute table with an entity/key
(define (get-attribute-type db table entity-type key)
  (let ((sql (string-append
              "select attribute_type from " table "_attribute where entity_type = '"
              (sqls entity-type)
              "' and attribute_id = '"
              (sqls key) "'")))
    (select-first db sql)))

;; search for a type and add it if it doesn't exist
(define (find/add-attribute-type db table entity-type key type)
  (let ((t (get-attribute-type db table entity-type key)))
    ;; add and return passed in type if not exist
    (cond
      ((null? t)
       (msg "adding new attribute for" entity-type " called " key " of type " type)
       (db-insert
        db (string-append
            "insert into " table "_attribute values (null, '"
            (sqls key) "', '" (sqls entity-type) "', '" (sqls type) "')"))
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
  (db-insert db (string-append "insert into " table "_value_" (sqls (ktv-type ktv))
                               " values (null, " (number->string entity-id) ", '" (sqls (ktv-key ktv)) "', "
                               (stringify-value ktv) ")")))


(define (get-unique user)
  (let ((t (time)))
    (string-append
     user "-" (number->string (car t)) ":" (number->string (cadr t)))))

;; insert an entire entity
(define (insert-entity db table entity-type user ktvlist)
  (msg table entity-type ktvlist)
  (let ((id (db-insert
             db (string-append
                 "insert into " table "_entity values (null, '" (sqls entity-type) "', '" (get-unique user) "', 1)"))))
    ;; create the attributes if they are new, and validate them if they exist
    (for-each
     (lambda (ktv)
       (find/add-attribute-type db table entity-type (ktv-key ktv) (ktv-type ktv)))
     ktvlist)
    ;; add all the keys
    (for-each
     (lambda (ktv)
       (msg (ktv-key ktv))
       (insert-value db table id ktv))
     ktvlist)))

;; update the value given an entity type, a attribute type and it's key (= attriute_id)
(define (update-value db table entity-id ktv)
  (msg "update-value" table entity-id ktv)
  (db-exec
   db (string-append "update " table "_value_" (sqls (ktv-type ktv))
                     " set value='" (ktv-value ktv) "'"
                     " where entity_id = " (number->string entity-id)
                     " and attribute_id = '" (sqls (ktv-key ktv)) "'"))
  (msg (db-status db)))

(define (update-entity-dirty db table entity-id v)
  (db-exec
   db (string-append "update " table "_entity "
                     "set dirty='" (number->string v) "'"
                     " where entity_id = " (number->string entity-id) ";")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getting data out

(define (get-entity-type db table entity-id)
  (select-first
   db (string-append
       "select entity_type from " table "_entity where entity_id = "
       (number->string entity-id))))

;; get all the (current) attributes for an entity type
(define (get-attribute-ids/types db table entity-type)
  (let ((s (db-select
            db (string-append
                "select * from " table "_attribute where entity_type = '"
                (sqls entity-type) "'"))))
    (if (null? s) '()
        (map
         (lambda (row)
           (list (vector-ref row 1)    ;; id
                 (vector-ref row 3)))  ;; type
         (cdr s)))))

;; get the value given an entity type, a attribute type and it's key (= attriute_id)
(define (get-value db table entity-id kt)
  (select-first
   db (string-append "select value from " table "_value_" (sqls (ktv-type kt))
                     " where entity_id = " (number->string entity-id)
                     " and attribute_id = '" (sqls (ktv-key kt)) "'")))

;; get an entire entity, as a list of key/value pairs
(define (get-entity db table entity-id)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
      ((null? entity-type) (msg "entity" entity-id "not found!") '())
      (else
       (cons
        (list "entity_id" "int" entity-id)
        (map
         (lambda (kt)
           (list (ktv-key kt) (ktv-type kt) (get-value db table entity-id kt)))
         (get-attribute-ids/types db table entity-type)))))))


(define (all-entities db table type)
  (let ((s (db-select
            db (string-append "select entity_id from " table "_entity where entity_type = '"
                              (sqls type) "';"))))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (string->number (vector-ref i 0)))
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

;; update an entire entity, via a (possibly partial) list of key/value pairs
(define (update-entity db table entity-id ktvlist)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
      ((null? entity-type) (msg "entity" entity-id "not found!") '())
      (else
       (update-entity-dirty db table entity-id 1)
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



;; todo
;; update (with partial values)
