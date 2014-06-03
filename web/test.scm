#!/usr/bin/env racket
#lang racket

(require
 "scripts/utils.ss"
 "scripts/request.ss"
 "scripts/logger.ss"
 "scripts/json.ss"
 "scripts/sql.ss"
 "scripts/sql.ss"
 "../eavdb/ktv.ss"
 "../eavdb/ktv-list.ss"
 "../eavdb/entity-values.ss"
 "../eavdb/entity-insert.ss"
 "../eavdb/entity-get.ss"
 "../eavdb/entity-update.ss"
 "../eavdb/entity-sync.ss"
 "../eavdb/entity-filter.ss"
 "../eavdb/entity-csv.ss"
 "../eavdb/eavdb.ss"
 "scripts/txt.ss"
 "scripts/server-sync.ss")


(open-log "unit-test-log.txt")


(define (unit-tests)
  ;; db
  (msg "testing db")
  (define db "unit-test.db")
  (with-handlers
   ((exn:fail? (lambda (e) (msg e))))
   (delete-file db))
  (set! db (db-open db setup))


;;(msg (db-status db))

;; test low level sql

(sql-test db)
(ktv-test)

;; test the entity attribute value system
(define table "eavunittest")
(setup db table)

(entity-update-test db table)
(entity-sync-test db table)

(msg (csv db table "thing"))

(msg (db-status db))

(msg "test over...")
)

(unit-tests)
