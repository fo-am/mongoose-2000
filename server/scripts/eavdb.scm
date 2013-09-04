#lang racket
(require (planet jaymccarthy/sqlite:5:1/sqlite))

;; sql sanity

(define (msg . args)
  (for-each 
   (lambda (i) (display i)(display " "))
   args)
  (newline))

(define (dbg i) (msg i) i)

(define (setup db)
  (exec/ignore db "create table entity ( entity_id integer primary key autoincrement, entity_type varchar(256))")
  (exec/ignore db "create table attribute ( id integer primary key autoincrement, attribute_id varchar(256), entity_type varchar(256), attribute_type varchar(256))")
  (exec/ignore db "create table value_varchar ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(1024))")
  (exec/ignore db "create table value_int ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value integer)")
  (exec/ignore db "create table value_real ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value real)"))

(define (sqls str)
  ;; todo sanitise str
  str)

(define (ktv key type value) (list key type value))
(define ktv-key car)
(define ktv-type cadr)
(define ktv-value caddr)

(define (stringify-value ktv)
  (cond
    ((equal? (ktv-type ktv) "varchar") (string-append "'" (ktv-value ktv) "'"))
    (else (number->string (ktv-value ktv)))))

(define (select-first db str)
  (let ((s (select db str)))
    (if (null? s)
        s
        (vector-ref (cadr s) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; putting data in

(define (get-attribute-type db entity-type key)
  (select-first
   db 
   (string-append
    "select attribute_type from attribute where entity_type = '"
    (sqls entity-type)
    "' and attribute_id = '"
    (sqls key) "'")))

(define (find/add-attribute-type db entity-type key type)
  (let ((t (get-attribute-type db entity-type key)))
    ;; add and return passed in type if not exist 
    (cond 
      ((null? t)
       (msg "adding new attribute for" entity-type " called " key " of type " type)
       (insert 
        db (string-append
            "insert into attribute values (null, '" 
            (sqls key) "', '" (sqls entity-type) "', '" (sqls type) "')"))
       type)
      (else
       (cond 
         ((equal? type t) t)
         (else
          (msg "type has changed for" entity-type key "from" t "to" type "???")
          ;; wont work
          type))))))

(define (insert-value db entity-id ktv)
  ;; use type to dispatch insert to correct value table
  (insert 
   db (string-append "insert into value_" (sqls (ktv-type ktv)) 
                     " values (null, " (number->string entity-id) ", '" (sqls (ktv-key ktv)) "', "
                     (stringify-value ktv) ")")))

(define (add-entity db entity-type ktvlist)
  (let ((id (insert 
             db (string-append
                 "insert into entity values (null, '" (sqls entity-type) "')"))))
    
    ;; create the attributes if they are new, and validate them if they exist
    (for-each
     (lambda (ktv)
       (find/add-attribute-type db entity-type (ktv-key ktv) (ktv-type ktv)))
    ktvlist)

    (for-each 
     (lambda (ktv)
       (insert-value db id ktv))
     ktvlist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getting data out

(define (get-entity-type db entity-id)
  (select-first 
   db (string-append
       "select entity_type from entity where entity_id = " (number->string entity-id))))

(define (get-attribute-ids/types db entity-type)
  (let ((s (select
            db (string-append
                "select * from attribute where entity_type = '" (sqls entity-type) "'"))))
    (if (null? s) '()
        (map 
         (lambda (row)
           (list (vector-ref row 1)    ;; id
                 (vector-ref row 3)))  ;; type
         (cdr s)))))

(define (get-value db entity-id kt)
  (select-first
   db (string-append "select value from value_" (sqls (ktv-type kt)) 
                     " where entity_id = " (number->string entity-id) 
                     " and attribute_id = '" (sqls (ktv-key kt)) "'")))

(define (get-entity db entity-id)
  (let* ((entity-type (get-entity-type db entity-id)))
    (cond
      ((null? entity-type) (msg "entity" entity-id "not found!") '())
      (else
       (map
        (lambda (kt)
          (list (ktv-key kt) (get-value db entity-id kt)))
        (get-attribute-ids/types db entity-type))))))

  
(define (validate db)
  ;; check attribute for duplicate entity-id/attribute-ids
  0)

(define (open-db db-name)
  (cond 
    ((file-exists? (string->path db-name))
     (display "open existing db")(newline)
     (open (string->path db-name)))
    (else
     (display "making new db")(newline)
     (let ((db (open (string->path db-name))))
       (setup db)
       db))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define db (open-db "test.db"))

;;(add-entity 
;; db "mongoose" 
;; (list 
;;  (ktv "code" "varchar" "brendon") 
;;  (ktv "gender" "varchar" "male")
;;  (ktv "pack-id" "int" 1)
;;  (ktv "weight" "real" 10.4)))

(get-entity db 3)

;; eav 

