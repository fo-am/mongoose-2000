#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; basic key/type/value structure
;; used for all data internally, and maps to the eavdb types

(require "../web/scripts/utils.ss")
(provide (all-defined-out))

(define (ktv key type value) (list key type value))
(define ktv-key car)
(define ktv-type cadr)
(define ktv-value caddr)

(define (ktv-eq? a b)
  (and
   (equal? (ktv-key a) (ktv-key b))
   (equal? (ktv-type a) (ktv-type b))
   (cond
    ((or
      (equal? (ktv-type a) "int")
      (equal? (ktv-type a) "real"))
     (eqv? (ktv-value a) (ktv-value b)))
    ((or
      (equal? (ktv-type a) "varchar")
      (equal? (ktv-type a) "file"))
     (equal? (ktv-value a) (ktv-value b)))
    (else
     (msg "unsupported ktv type in ktv-eq?: " (ktv-type a))
     #f))))

;; this is just used for the csv building
(define (null-value-for-type type)
  (cond
   ((equal? type "varchar") "not set")
   ((equal? type "int") 0)
   ((equal? type "real") 0)
   ((equal? type "file") "not set")))

;; regression testing
(define vowel (map symbol->string (list 'a 'e 'i 'o 'u)))
(define consonant (map symbol->string (list 'b 'c 'd 'f 'g 'h 'j 'k 'l 'm 'n 'p 'q 'r 's 't 'v 'w 'x 'y 'z)))

(define (word-gen)
  (define (_ s vowel-prob)
    (cond
     ((zero? s) '())
     ((< (rndf) vowel-prob)
      (cons (choose vowel) (_ (- s 1) (/ vowel-prob 2))))
     (else
      (cons (choose consonant) (_ (- s 1) (* vowel-prob 2))))))
  (apply string-append (_ (+ 3 (random 8)) 0.5)))

(define (random-value-for-type type)
  (cond
   ((equal? type "varchar") (word-gen))
   ((equal? type "int") (random 100))
   ((equal? type "real") (rndf))
   ((equal? type "file") (word-gen))))

;; stringify based on type (for url)
(define (stringify-value ktv)
  (cond
   ((null? (ktv-value ktv)) "NULL")
   ((equal? (ktv-type ktv) "varchar") (string-append "'" (ktv-value ktv) "'"))
   (else
    (if (not (string? (ktv-value ktv)))
        (number->string (ktv-value ktv))
        (ktv-value ktv)))))

;; filter uri chars
(define (filter-uri-chars s)
  (list->string
   (filter
    (lambda (v)
      (not (or
            (eqv? v #\newline)
            (eqv? v #\!)
            (eqv? v #\*)
            (eqv? v #\')
            (eqv? v #\()
            (eqv? v #\))
            (eqv? v #\;)
;;            (eqv? v #\:)
            (eqv? v #\@)
            (eqv? v #\&)
            (eqv? v #\=)
            (eqv? v #\+)
            (eqv? v #\$)
;;            (eqv? v #\,)
            (eqv? v #\/)
            (eqv? v #\?)
            (eqv? v #\#)
            (eqv? v #\[)
            (eqv? v #\]))))
    (string->list s))))

(msg "TESTING FILTER URI CHARS")
(msg (filter-uri-chars "1234"))
(msg (filter-uri-chars "12&34"))
(msg (filter-uri-chars "1\
234"))
(msg (filter-uri-chars "12[[]]34"))

;; stringify based on type (for url)
(define (stringify-value-url ktv)
  (filter-uri-chars
   (cond
    ((null? (ktv-value ktv)) "NULL")
    ((equal? (ktv-type ktv) "varchar") (ktv-value ktv))
    (else
     (if (not (string? (ktv-value ktv)))
         (number->string (ktv-value ktv))
         (ktv-value ktv))))))

;; tests...

(define (ktv-test)
  (asserteq "ktv one" (stringify-value (ktv "one" "varchar" "two")) "'two'")
  (asserteq "ktv 2" (stringify-value (ktv "one" "int" 3)) "3")
  (asserteq "ktv 3" (stringify-value-url (ktv "one" "varchar" "two")) "two")
  (asserteq "ktv 4" (stringify-value-url (ktv "one" "int" 3)) "3"))
