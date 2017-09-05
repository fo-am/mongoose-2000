#!/usr/bin/env racket
#lang racket
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

(require racket/system
;;         racket/foreign
         racket/cmdline
         web-server/servlet
         web-server/servlet-env
         web-server/http/response-structs
         racket/match
         "scripts/utils.ss"
         "scripts/request.ss"
         "scripts/logger.ss"
         "scripts/json.ss"
         "scripts/sql.ss"
         "../eavdb/entity-get.ss"
         "../eavdb/entity-sync.ss"
         "../eavdb/entity-csv.ss"
         "../eavdb/eavdb.ss"
         "scripts/txt.ss"
         "scripts/server-sync.ss"
         "scripts/input.ss"
	 )

; a utility to change the process owner,
; assuming mzscheme is called by root.
;;(unsafe!)
;;(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))

(define db-name "client/htdocs/mongoose.db")
(define db (db-open db-name setup))
(open-log "log.txt")

;(write-db db "sync" "/var/www/mongoose-web/web/input.csv")


;(msg (csv db "sync" "individual"))

(define sema (make-semaphore 1))

(define (syncro-new fn)
   (msg "s-start")
   (semaphore-wait sema)
   (let ((r (fn)))
     (msg "s-end")
     (semaphore-post sema)
     r))

(define (syncro fn)
  (fn))

(define registered-requests
  (list

   (register
    (req 'upload '())
    (lambda (req)
      (syncro
       (lambda ()
	 (msg "upload")
	 (match (bindings-assq #"binary" (request-bindings/raw req))
		((struct binding:file (id filename headers content))
		 (with-output-to-file
		     (string-append "files/" (bytes->string/utf-8 filename)) #:exists 'replace
		     (lambda ()
		       (write-bytes content)))))
	 (pluto-response (scheme->txt '("ok")))))))

   ;; http://localhost:8888/mongoose?fn=sync&table=sync&entity-type=mongoose&unique-id=dave1234&dirty=1&version=0&next:varchar=%22foo%22&blah:int=20

   ;; all dirty entities are sent to this function from the android in
   ;; general - we shouldn't care about version numbers from this
   ;; point locally they are dirty, and that should be it?
   ;;
   ;; * perhaps they are very old changes from a tablet that hasn't
   ;; been updated?
   ;;
   ;; * is this the place to flag problems?
   ;;
   ;; * sometimes this is not called for dirty entities - in the case
   ;; of a full db update thing
   (register
    (req 'sync '(table entity-type unique-id dirty version))
    (lambda (req table entity-type unique-id dirty version . data)
      (syncro
       (lambda ()
	 (msg "sync")
	 (pluto-response
	  (dbg (scheme->txt
	   (check-for-sync
	    db
	    table
	    entity-type
	    unique-id
	    (string->number dirty)
	    (string->number version) (dbg data)))))))))

   ;; returns a table of all entities and their corresponding versions
   (register
    (req 'entity-versions '(table))
    (lambda (req table)
      (syncro
       (lambda ()
	 (msg "entity-versions")
	 (pluto-response
	  (scheme->txt
	   (entity-versions db table)))))))

   ;; returns the entity - the android requests these based on the version numbers
   ;; (request all ones that are newer than it's stored version)
   (register
    (req 'entity '(table unique-id))
    (lambda (req table unique-id)
      (syncro
       (lambda ()
	 (msg "entity")
	 (pluto-response
	  (scheme->txt
	   (send-entity db table unique-id)))))))

   (register
    (req 'entity-types '(table))
    (lambda (req table)
      (syncro
       (lambda ()
	 (msg "entity-types")
	 (pluto-response
	  (scheme->txt
	   (get-all-entity-types db table)))))))

   (register
    (req 'entity-csv '(table type))
    (lambda (req table type)
      (syncro
       (lambda ()
	 (msg "entity-csv")
	 (let ((r (csv-pretty db table type)))
	   (msg "--------------------------------------- csv request for" type "[" r "]")
	   (pluto-response
	    r))))))

   (register
    (req 'file-list '())
    (lambda (req)
      (syncro
       (lambda ()
         (msg "file-list")
         (pluto-response
          (dbg (scheme->txt
           (map path->string (directory-list "files/")))))))))


   ))

(define (start request)
  (let ((values (url-query (request-uri request))))
    (if (not (null? values))   ; do we have some parameters?
        (let ((name (assq 'fn values)))
	  (msg "request incoming:" name)
          (if name           ; is this a well formed request?
	      (request-dispatch
	       registered-requests
	       (req (string->symbol (cdr name))
		    (filter
		     (lambda (v)
		       (not (eq? (car v) 'fn)))
		     values))
	       request)
	      (pluto-response "could't find a function name")))
        (pluto-response "malformed thingy"))))

(printf "server is running...~n")

; Here we become the user 'nobody'.
; This is a security rule that *only works* if nobody owns no other processes
; than mzscheme. Otherwise better create another dedicated unprivileged user.
; Note: 'nobody' must own the state directory and its files.

;(setuid 65534)

;;

(serve/servlet
 start
 ;; port number is read from command line as argument
 ;; ie: ./server.scm 8080
 #:listen-ip "192.168.2.1"
 #:port (string->number (command-line #:args (port) port))
 #:command-line? #t
 #:servlet-path "/mongoose"
 #:server-root-path
 (build-path "client"))
