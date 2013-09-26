#!/usr//bin/env mzscheme
#lang scheme/base
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

(require scheme/system
         scheme/foreign
         scheme/cmdline
         web-server/servlet
         web-server/servlet-env
         web-server/http/response-structs
         "scripts/request.ss"
         "scripts/logger.ss"
         "scripts/json.ss"
         "scripts/eavdb.ss")

(require (planet jaymccarthy/sqlite:5:1/sqlite))

; a utility to change the process owner,
; assuming mzscheme is called by root.
;;(unsafe!)
;;(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))

(define db-name "mongoose.db")
(define db #f)


(open-log "log.txt")



(define (pluto-response txt)
  (response/full
   200                ; code
   #"Okay"            ; message
   (current-seconds)  ; seconds
   #"text/javascript" ; mime type
   '()                ; headers
   (list (string->bytes/utf-8 txt)))) ; body

(define registered-requests
  (list
   (register
    (req 'ping '())
    (lambda ()
      (pluto-response (scheme->json '("hello")))))))


(define (start request)
  (let ((values (url-query (request-uri request))))
    (display "a request has arrived!")(newline)
    (log (format "~a" values))
    (if (not (null? values)) ; do we have some parameters?
        (let ((name (assq 'function_name values)))
          (when name ; is this a well formed request?
                (request-dispatch
                 registered-requests
                 (req (string->symbol (cdr name))
                      (filter
                       (lambda (v)
                         (not (eq? (car v) 'function_name)))
                       values)))))
        "hello")))

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
