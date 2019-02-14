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


(require (only-in "main.rkt"

                  Point
                  
                  pick-k
                  run-k-means
                  split)
                  
         (only-in "gen.rkt"
                  
                  gen-random-sample)

         (only-in "render.rkt"
                  
                  render-gen-sample
                  render-sample
                  render-history
                  render-partition)

         (only-in racket/list

                  last
                  shuffle)

         (only-in plot
                  plot))


;;----------------------------------------------------------

(define K-GEN : Exact-Nonnegative-Integer 4)
(define N-GEN : Exact-Nonnegative-Integer 10000)
(define K-EST : Exact-Nonnegative-Integer 4)
(define N-PART : Exact-Positive-Integer 32)

; generate sample with k clusters and n points each
(define gen-data : (Listof (Listof Point))
  (gen-random-sample K-GEN N-GEN))

; plot the generated sample
(displayln "generated sample:")
(plot (render-gen-sample gen-data)
      #:x-min      0
      #:x-max     10
      #:y-min      0
      #:y-max     10
      #:width    450
      #:height   450
      #:x-label  #f
      #:y-label  #f)


; mix everything together
(define data : (Listof Point)
  (shuffle (apply append gen-data)))

; plot input data
(displayln "input data:")
(plot (render-sample data)
      #:x-min      0
      #:x-max     10
      #:y-min      0
      #:y-max     10
      #:width    450
      #:height   450
      #:x-label  #f
      #:y-label  #f)


; pick initial cluster centers
(define initial-cc-lst : (Listof Point)
  (pick-k data K-EST))

; split the sample into n partitions
(define split-lst : (Listof (Listof Point))
  (split data N-PART))

; run k-means
(define history : (Listof (Listof Point))
  (run-k-means split-lst (list initial-cc-lst)))

; plot cluster center history
(displayln "history:")
(plot (render-history data history)
      #:x-min      0
      #:x-max     10
      #:y-min      0
      #:y-max     10
      #:width    450
      #:height   450
      #:x-label  #f
      #:y-label  #f)

; plot resulting cluster partitioning
(displayln "partitioning:")
(plot (render-partition data (last history))
      #:x-min      0
      #:x-max     10
      #:y-min      0
      #:y-max     10
      #:width    450
      #:height   450
      #:x-label  #f
      #:y-label  #f)







  



