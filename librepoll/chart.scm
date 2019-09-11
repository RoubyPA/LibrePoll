;;; LibrePoll -- Free opinion poll service.
;;;
;;; Copyright (C) 2019 Pierre-Antoine Rouby <contact@parouby.fr>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (librepoll chart)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 format) #:prefix icef:)
  #:use-module (sxml simple)
  #:export (make-pie-chart))

(define PI 3.141592653589793)

(define (percentage->coordinate percent)
  "Percentage PERCENT to coordinate '(x y)."
  (let* ((var (* 2 PI percent))
         (x (cos var))
         (y (sin var)))
    `(,x ,y)))

(define (svg-line x1 y1 percent color)
  (match (percentage->coordinate percent)
    ((x y)
     `((line (@ (x1 ,x1) (y1 ,y1)
                (x2 ,(+ x1 x)) (y2 ,(+ y1 y))
                (stroke-width "0.05")
                (fill "transparent")
                (stroke ,color)))))))

(define (svg-path x1 y1 x2 y2 c p)
  `((path (@ (d ,(icef:format #f "M ~5f ~5f A 1 1 0 ~a 1 ~5f ~5f L 0 0"
                              (number->string x1)
                              (number->string y1)
                              (if (> p 0.5) 1 0)
                              (number->string x2)
                              (number->string y2)))
             (id ,(format "svg_path_~a" c))))))

(define (make-paths x1 y1 p accp c acc)
  (match p
    (() acc)
    ((a . rest)
     (match (percentage->coordinate (+ a accp))
       ((x2 y2)
        (let ((acc (cons (svg-path x1 y1 x2 y2 c a) acc)))
          (make-paths x2 y2 rest (+ a accp)
                      (+ 1 c) acc)))))))
(define make-circle
  `((circle (@ (cx "0")
               (cy "0")
               (r  "1")
               (id "svg_path_0")))))

(define (make-pie-chart percent-lst)
  "Make sxml svg pie chart from list of precentage PRENCET-LST between 0 and
1."
  `((svg (@ (width "200px")
            (height "200px")
            (viewBox "-1 -1 2 2")
            (style "transform: rotate(-0.25turn)")
            (xmlns "http://www.w3.org/2000/svg")
            (xmlns "http://www.w3.org/1999/xlink"))
         ,(if (= (length percent-lst) 1)
              make-circle
              (make-paths 1 0 percent-lst 0 0 '())))))
