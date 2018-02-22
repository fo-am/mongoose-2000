(msg "life-history happened")

(define list-pack-lifehist
  (list
   (list 'endgrp "End of pack")
   (list 'lgrp "Last seen")
   (list 'fgrp "First seen")))

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

(define (init-lifehist db)
  (entity-init! db "stream" "lifehist-event" 
		(list
		 (ktv "date" "varchar" (date-time->string (date-time)))
		 (ktv "type" "varchar" "none")
		 (ktv "code" "varchar" "none")
		 (ktv "entity-uid" "varchar" "none")
		 (ktv "entity-name" "varchar" "none"))))
  

(define (build-lifehist type)
  (linear-layout
   (make-id "") 'vertical fillwrap lh-bgcol
   (list
    (text-view 0 (string-append "New life history event for this " (lifehist-text type)) 30 fillwrap)
    (horiz
     (vert
      (horiz
       (mtext 0 "Date:")
       (mtext "lifehist-date-view" (date->string (date-time))))
      (mbutton-large "lifehist-date" "Set date" 
	       (lambda ()
		 (list (date-picker-dialog
			"lifehist-date"
			(lambda (day month year)
			  (let ((datestring (date-time->string (list year (+ month 1) day))))			
			    (set-current! 'entity-type "lifehist-event")
			    (entity-set-value! "date" "varchar" datestring)
			    (list
			     (update-widget
			      'text-view
			      (get-id "lifehist-date-view") 'text datestring)))))))))
     (vert
      (mtext 0 "Code")
      (mspinner "lifehist-type" 
		(lifehist-types type)
		(lambda (v) 
		  (set-current! 'entity-type "lifehist-event")
		  (entity-set-value! 
		   "code" "varchar" 
		   (spinner-choice (lifehist-types type) v))
		  '())))
     (vert
      (mtext 0 "")
      (mcolour-button-large 
       "lifehist-record" "Record"
       lh-col
       (lambda ()
	 (set-current! 'entity-type "lifehist-event")
	 (entity-set-value! "entity-uid" "varchar" (get-current 'focal-id ""))
	 (list
	  (alert-dialog
	   "lifehist-check"
	   "Recording life history event: are you sure?"
	   (lambda (v)
	     (cond
	      ((eqv? v 1)
	       (entity-record-values!)
	       '())
	      (else
	       (list)))))))))))))



