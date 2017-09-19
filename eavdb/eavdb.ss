#lang racket

;; Starwisp Copyright (C) 2014 Dave Griffiths
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

;; common code - require and provide ignored on tinyscheme

(require (planet jaymccarthy/sqlite:5:1/sqlite))

(require
 "../web/scripts/utils.ss"
 "../web/scripts/sql.ss"
 "ktv.ss"
 "ktv-list.ss"
 "entity-values.ss"
 "entity-insert.ss"
 "entity-get.ss"
 "entity-update.ss"
 "entity-sync.ss"
 "entity-filter.ss")

(provide (all-defined-out))


(define (upgrade-table db name)
  (db-exec db (string-append "alter table " name " add version integer"))
  (db-exec db (string-append "alter table " name " add sent integer default 0")))

;; create eav tables (add types as required)
;; aggregating version updates - should clean all this up
;;
;; remember that this is run on both pi and android - on the tablets we don't really know
;; how old the initial versions of the databases are, they might go right back to the beginning
;; so need to be super conservative and alter tables to add new stuff
(define (setup db table)
  (msg "db setup")
  (db-exec db (string-append "create table " table "_entity ( entity_id integer primary key autoincrement, entity_type varchar(256), unique_id varchar(256), dirty integer, version integer, sent integer default 0)"))
  ;; throws benign errors if it exists - we catch and ignore em...
  (db-exec db (string-append "alter table " table "_entity add sent integer default 0"))
  (db-exec db (string-append "create index if not exists index_" table "_entity on " table "_entity (unique_id)"))


  (db-exec db (string-append "create table " table "_attribute ( id integer primary key autoincrement, attribute_id varchar(256), entity_type varchar(256), attribute_type varchar(256), sent integer default 0)"))
  (db-exec db (string-append "alter table " table "_attribute add sent integer default 0"))
  (db-exec db (string-append "create index if not exists index_" table "_attribute on " table "_attribute (entity_type)"))



  (db-exec db (string-append "create table " table "_value_varchar ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096), dirty integer, version integer, sent integer default 0)"))
  (upgrade-table db (string-append table "_value_varchar"))
  (db-exec db (string-append "create index if not exists index_" table "_value_varchar on " table "_value_varchar (entity_id,attribute_id)"))

  (db-exec db (string-append "create table " table "_value_int ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value integer, dirty integer, version integer, sent integer default 0)"))
  (upgrade-table db (string-append table "_value_int"))
  (db-exec db (string-append "create index if not exists index_" table "_value_int on " table "_value_int (entity_id,attribute_id)"))

  (db-exec db (string-append "create table " table "_value_real ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value real, dirty integer, version integer, sent integer default 0)"))
  (upgrade-table db (string-append table "_value_real"))
  (db-exec db (string-append "create index if not exists index_" table "_value_real on " table "_value_real (entity_id,attribute_id)"))

  (db-exec db (string-append "create table " table "_value_file ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096), dirty integer, version integer, sent integer default 0)"))
  (upgrade-table db (string-append table "_value_file"))
  (db-exec db (string-append "create index if not exists index_" table "_value_file on " table "_value_file (entity_id,attribute_id)"))

  )


(define (validate db)
  ;; check attribute for duplicate entity-id/attribute-ids
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(define (db-all db table type)
  (map
   (lambda (i)
     (get-entity db table i))
   (all-entities db table type)))

(define (db-with-parent db table type parent)
  (map
   (lambda (i)
     (get-entity db table i))
   (all-entities-with-parent db table type parent)))

(define (db-filter db table type filter)
  (map
   (lambda (i)
     (get-entity db table i))
   (filter-entities db table type filter)))

(define (db-filter-inc-deleted db table type filter)
  (map
   (lambda (i)
     (get-entity db table i))
   (filter-entities-inc-deleted db table type filter)))

;; only return (eg. name and photo)
(define (db-filter-only db table type filter kt-list)
  (map
   (lambda (i)
     (get-entity-only db table i kt-list))
   (filter-entities db table type filter)))

;; only return (eg. name and photo)
(define (db-filter-only-inc-deleted db table type filter kt-list)
  (map
   (lambda (i)
     (get-entity-only db table i kt-list))
   (filter-entities-inc-deleted db table type filter)))

(define (run-unit-tests)
  (msg "running eavdb tests...")
  (define last-test-db (open (string->path "unit-test.db")))
  ;; clear out last test
  (db-exec last-test-db "drop table sync_entity;")
  (db-exec last-test-db "drop table sync_attribute;")
  (db-exec last-test-db "drop table sync_value_varchar;")
  (db-exec last-test-db "drop table sync_value_int;")
  (db-exec last-test-db "drop table sync_value_real;")
  (db-exec last-test-db "drop table sync_value_file;")

  (db-exec last-test-db "drop table stream_entity;")
  (db-exec last-test-db "drop table stream_attribute;")
  (db-exec last-test-db "drop table stream_value_varchar;")
  (db-exec last-test-db "drop table stream_value_int;")
  (db-exec last-test-db "drop table stream_value_real;")
  (db-exec last-test-db "drop table stream_value_file;")
  ;; reopen to run setup
  (define test-db (db-open "unit-test.db" setup))
  (ktv-test)
  (entity-update-test test-db "sync")
  (entity-sync-test test-db "sync")
  (msg "finished running eavdb tests..."))

(run-unit-tests)
  
