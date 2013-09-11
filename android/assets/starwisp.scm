;; Starwisp Copyright (C) 2013 Dave Griffiths
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-activity-list
  (activity
   "splash"
   (vert
    (text-view (make-id "splash-title") "Mongoose 2000" 40 fillwrap)
    (text-view (make-id "splash-about") "Advanced mongoose technology" 20 fillwrap)
    (spacer 20)
    (button (make-id "f2") "Get started!" 20 fillwrap
            (lambda () (list (start-activity-goto "main" 2 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "main"
   (vert
    (text-view (make-id "title") "Mongoose 2000" 40 fillwrap)
    (spacer 10)
    (button (make-id "main-sync") "Sync Data" 20 fillwrap (lambda () (list)))
    (button (make-id "main-sync") "Experiment" 20 fillwrap (lambda () (list)))
    (button (make-id "main-sync") "Manage Packs" 20 fillwrap (lambda () (list)))
    (button (make-id "main-sync") "Tag Location" 20 fillwrap (lambda () (list)))
    (button (make-id "main-sync") "Send Database" 20 fillwrap (lambda () (list)))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (list
      (update-widget 'linear-layout (get-id "main-field-list") 'contents
                     (build-field-buttons)))))



  )
