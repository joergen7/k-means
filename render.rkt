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

(provide render-gen-sample
         render-sample
         render-history
         render-partition)

(require (only-in "main.rkt"

                  Point

                  partition)
         
         (only-in plot

                  lines
                  points
                  renderer2d)
         


         (only-in racket/list
                  last))




(: render-gen-sample ((Listof (Listof Point)) -> (Listof renderer2d)))
(define (render-gen-sample partition-lst)

  (define k : Exact-Nonnegative-Integer
    (length partition-lst))

  (: make-points (Exact-Nonnegative-Integer -> renderer2d))
  (define (make-points i)
    (points (list-ref partition-lst i)
            #:color (add1 i)))

  (build-list k make-points))

(: render-sample ((Listof Point) -> renderer2d))
(define (render-sample point-lst)
  (points point-lst))


(: render-partition ((Listof Point) (Listof Point) -> (Listof renderer2d)))
(define (render-partition point-lst cluster-center-lst)

  (define k : Exact-Nonnegative-Integer
    (length cluster-center-lst))

  (define partition-lst : (Listof (Listof Point))
    (partition point-lst cluster-center-lst))
  
  (: make-points (Exact-Nonnegative-Integer -> renderer2d))
  (define (make-points i)
    (points (list-ref partition-lst i)
            #:color (add1 i)))
  
  (build-list k make-points))


(: render-history ((Listof Point) (Listof (Listof Point)) -> (Listof renderer2d)))
(define (render-history point-lst history)

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

  (cons p (append points-lst lines-lst)))
