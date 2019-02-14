;;-----------------------------------------------------------------------------
;;
;; k-means implementation that is recursive and allows partitioned input data
;;
;; Copyright 2019 Jörgen Brandt <joergen@cuneiform-lang.org>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;-----------------------------------------------------------------------------

#lang typed/racket/base

(provide gen-random-sample
         plot-gen-sample
         plot-sample
         plot-history
         plot-partition)

(require (only-in "main.rkt"

                  Point
                  
                  gen-cluster-data
                  partition)
         
         (only-in plot

                  lines
                  plot
                  points
                  renderer2d)
         
         (only-in math/distributions
                  
                  sample
                  uniform-dist)

         (only-in racket/list
                  last))

(define PLOT-MIN : Real 0)
(define PLOT-MAX : Real 10)
(define CLUSTER-CENTER-MIN : Real 1)
(define CLUSTER-CENTER-MAX : Real 9)
(define STDDEV-MIN : Nonnegative-Real 0.1)
(define STDDEV-MAX : Nonnegative-Real 2)
(define PLOT-SIZE : Exact-Positive-Integer 450)


(: gen-random-sample (Exact-Nonnegative-Integer
                      Exact-Nonnegative-Integer -> (Listof (Listof Point))))
(define (gen-random-sample k n)

  (define (make-cluster i)

    (define center-point : Point
      (sample (uniform-dist CLUSTER-CENTER-MIN CLUSTER-CENTER-MAX) 2))

    (define stddev : Positive-Real
      (assert (sample (uniform-dist STDDEV-MIN STDDEV-MAX)) positive?))

    (gen-cluster-data center-point stddev n))

  
  (build-list k make-cluster))




(: plot-gen-sample ((Listof (Listof Point)) -> Any))
(define (plot-gen-sample partition-lst)

  (define k : Exact-Nonnegative-Integer
    (length partition-lst))

  (: make-points (Exact-Nonnegative-Integer -> renderer2d))
  (define (make-points i)
    (points (list-ref partition-lst i)
            #:color (add1 i)))

  (define points-lst : (Listof renderer2d)
    (build-list k make-points))

  (plot points-lst
        #:x-min PLOT-MIN
        #:x-max PLOT-MAX
        #:y-min PLOT-MIN
        #:y-max PLOT-MAX
        #:width PLOT-SIZE
        #:height PLOT-SIZE
        #:x-label #f
        #:y-label #f))

(: plot-sample ((Listof Point) -> Any))
(define (plot-sample point-lst)

  (define r : renderer2d
    (points point-lst))

  (plot r
        #:x-min PLOT-MIN
        #:x-max PLOT-MAX
        #:y-min PLOT-MIN
        #:y-max PLOT-MAX
        #:width PLOT-SIZE
        #:height PLOT-SIZE
        #:x-label #f
        #:y-label #f))


(: plot-partition ((Listof Point) (Listof Point) -> Any))
(define (plot-partition point-lst cluster-center-lst)

  (define k : Exact-Nonnegative-Integer
    (length cluster-center-lst))

  (define partition-lst : (Listof (Listof Point))
    (partition point-lst cluster-center-lst))
  
  (: make-points (Exact-Nonnegative-Integer -> renderer2d))
  (define (make-points i)
    (points (list-ref partition-lst i)
            #:color (add1 i)))
  
  (define points-lst : (Listof renderer2d)
    (build-list k make-points))

  (plot points-lst
        #:x-min PLOT-MIN
        #:x-max PLOT-MAX
        #:y-min PLOT-MIN
        #:y-max PLOT-MAX
        #:width PLOT-SIZE
        #:height PLOT-SIZE
        #:x-label #f
        #:y-label #f))


(: plot-history ((Listof Point) (Listof (Listof Point)) -> Any))
(define (plot-history point-lst history)

  (: pivot ((Listof (Listof Point)) -> (Listof (Listof Point))))
  (define (pivot history)

    (when (null? history)
      (raise-argument-error
       'history
       "History must not be empty."
       history))
    
    (if (member '() history)
        '()
        (let ([hd : (Listof Point) (map (λ ([x : (Listof Point)]) (car x)) history)]
              [tl : (Listof (Listof Point)) (map (λ ([x : (Listof Point)]) (cdr x)) history)])
          (cons hd (pivot tl)))))

  (define h : (Listof (Listof Point))
    (pivot history))
  
  (define k : Exact-Nonnegative-Integer
    (length h))

  (: make-lines (Exact-Nonnegative-Integer -> renderer2d))
  (define (make-lines i)
    (lines (list-ref h i)
           #:color (add1 i)))

  (define lines-lst : (Listof renderer2d)
    (build-list k make-lines))

  (: make-points (Exact-Nonnegative-Integer -> renderer2d))
  (define (make-points i)
    (points (list (list-ref (last history) i))
            #:sym #\+
            #:color (add1 i)))

  (define points-lst : (Listof renderer2d)
    (build-list k make-points))

  (define p : renderer2d
    (points point-lst
            #:color 'lightgray))

  (plot (cons p (append points-lst lines-lst))
        #:x-min PLOT-MIN
        #:x-max PLOT-MAX
        #:y-min PLOT-MIN
        #:y-max PLOT-MAX
        #:width PLOT-SIZE
        #:height PLOT-SIZE
        #:x-label #f
        #:y-label #f))