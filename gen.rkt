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
         gen-init-cc-lst)

(require (only-in "main.rkt"

                  Point
                  
                  gen-cluster-data)
         
         (only-in math/distributions
                  
                  sample
                  uniform-dist))

(define CLUSTER-CENTER-MIN : Real 1)
(define CLUSTER-CENTER-MAX : Real 9)
(define STDDEV-MIN : Nonnegative-Real 0.1)
(define STDDEV-MAX : Nonnegative-Real 2)

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

(: gen-init-cc-lst (Exact-Nonnegative-Integer -> (Listof Point)))
(define (gen-init-cc-lst k)
  (apply append (gen-random-sample 4 1)))