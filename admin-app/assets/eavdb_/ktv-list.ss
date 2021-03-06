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

#lang scheme
(require "ktv.ss")

(provide (all-defined-out))

(define (ktv-get ktv-list key)
  (cond
   ((null? ktv-list) #f)
   ((equal? (ktv-key (car ktv-list)) key)
    (ktv-value (car ktv-list)))
   (else (ktv-get (cdr ktv-list) key))))

(define (ktv-get-type ktv-list key)
  (cond
   ((null? ktv-list) #f)
   ((equal? (ktv-key (car ktv-list)) key)
    (ktv-type (car ktv-list)))
   (else (ktv-get-type (cdr ktv-list) key))))

(define (ktv-set ktv-list ktv)
  (cond
   ((null? ktv-list) (list ktv))
   ((equal? (ktv-key (car ktv-list)) (ktv-key ktv))
    (cons ktv (cdr ktv-list)))
   (else (cons (car ktv-list) (ktv-set (cdr ktv-list) ktv)))))
