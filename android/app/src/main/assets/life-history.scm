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

(define list-pack-lifehist
  (list
   (list 'endgrp "End of pack")
   (list 'lgrp "Lost Pack")
   (list 'fgrp "Found Pack")))

(define list-litter-lifehist
  (list
   (list 'unsuccessful "Unsuccessful")
   (list 'short-lived "Short lived")
   (list 'successful "Successful")))

(define list-individual-lifehist
  (list
   (list 'adied "Assumed died")
   (list 'died "Died")
   (list 'lseen "Last seen")
   (list 'fseen "First seen")
   (list 'stev "Start eviction")
   (list 'endev "End eviction")
   (list 'leave "Leave")
   (list 'return "Return")
   (list 'imm "Immigrate")
   (list 'emm "Emmigrate")))

(define list-female-lifehist
  (list
   (list 'fpreg "First pregnant")
   (list 'abort "Abort")
   (list 'birth "Birth")))

(define (lifehist-types type)
  (cond
   ((eq? type 'pack) list-pack-lifehist)
   ((eq? type 'litter) list-litter-lifehist)
   ((eq? type 'male) list-individual-lifehist)
   (else (append list-individual-lifehist list-female-lifehist))))

(define (lifehist-text type)
  (cond
   ((eq? type 'pack) "pack")
   ((eq? type 'litter) "litter")
   ((eq? type 'male) "male")
   (else "female")))

;; for updating mongooses to the right gender
(define (update-lifehist gender)
  (list
   (update-widget 
    'spinner (get-id "mongooselifehist-type") 'array
    (symbol-list-to-names
     (lifehist-types (if (equal? gender "female") 'female 'male))))
   (update-widget 'text-view (get-id "mongooselifehist-title") 'text
		  (string-append "New life history event for this " gender))))
  
(define (build-lifehist type)
  (let ((typeid (symbol->string type)))
    (vert-colour 
     lh-bgcol
     (text-view (make-id (string-append typeid "lifehist-title")) (string-append "New life history event for this " (lifehist-text type)) 30 fillwrap)
     (horiz
      (vert
       (horiz
	(mtext 0 "Date:")
      (mtext "lifehist-date-view" (date->string (date-time))))
     (mbutton-large 
      (string-append typeid "lifehist-date") "Set date" 
      (lambda ()
	(list (date-picker-dialog
	       (string-append typeid "lifehist-date")
	       (lambda (day month year)
		 (let ((datestring (date->string (list year (+ month 1) day))))
		   (set-current! 'lifehist-date datestring)
		   (list
		    (update-widget
		     'text-view (get-id (string-append typeid "lifehist-date-view")) 
		     'text datestring)))))))))
    (vert
     (mtext 0 "Code")
     (mspinner (string-append typeid "lifehist-type") 
	       ;; if type is mongoose, then redirect to a real list for now...
	       (lifehist-types (if (eq? type 'mongoose) 'male type))
	       (lambda (v) 
		 (set-current! 
		  'lifehist-code 
		  (spinner-choice 
		   (lifehist-types 
		    (if (eq? type 'mongoose) 
			(ktv-get (get-current 'individual ()) "gender")
			type)) v))
		 '())))
    (vert
     (mtext 0 "")
     (mcolour-button-large 
      (string-append typeid "lifehist-record") "Record"
      lh-col
      (lambda ()
	(list
	 (alert-dialog
	  (string-append typeid "lifehist-check")
	  "Recording life history event: are you sure?"
	  (lambda (v)
	    (cond
	     ((eqv? v 1)
	      ;; using entity-create! so as not to disturb the current
	      ;; pack/litter/individual being currently edited in 
	      ;; memory via the rest of the interface
	      (entity-create! 
	       db "stream" "lifehist-event" 
	       (list
		(ktv "date" "varchar" (get-current 'lifehist-date (date-time->string (date-time))))
		(ktv "type" "varchar" (symbol->string type))
		(ktv "code" "varchar" (get-current 'lifehist-code "none"))
		(ktv "entity-uid" "varchar" 
		     (cond 
		      ((eq? type 'pack) (ktv-get (get-current 'pack ()) "unique_id"))
		      ((eq? type 'litter) (ktv-get (get-current 'litter ()) "unique_id"))
		      (else (ktv-get (get-current 'individual ()) "unique_id"))))
		(ktv "entity-name" "varchar"
		     (cond 
		      ((eq? type 'pack) (ktv-get (get-current 'pack ()) "name"))
		      ((eq? type 'litter) (ktv-get (get-current 'litter ()) "name"))
		      (else (ktv-get (get-current 'individual ()) "name"))))))
	      '())
	     (else
	      (list)))))))))))))


