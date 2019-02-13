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

(provide ; types and type predicates
         Cluster-Pair
         cluster-pair?
         Point
         point?

         ; functions
         classify
         cluster-center
         gen-cluster-data
         partition
         pick-k
         run-k-means
         split
         weighted-mean)

(require (only-in math/statistics
                  mean)
         (only-in racket/list
                  shuffle
                  list-set
                  make-list
                  take)
         (only-in math/distributions
                  sample
                  normal-dist
                  uniform-dist))

(require/typed racket/list
               [index-of ((Listof Any) Any -> (U Exact-Nonnegative-Integer False))])

(define-type Point (Listof Real))
(define-predicate point? Point)

(define-type Cluster-Pair (Pairof (Listof (U Point False)) Exact-Nonnegative-Integer))
(define-predicate cluster-pair? Cluster-Pair)


;; split subdivides a list of points into n sub-lists.

(: split ((Listof Point) Exact-Positive-Integer -> (Listof (Listof Point))))
(define (split point-lst n)

  (if (= n 1)
      (list point-lst)
      (let* ([l : Index (quotient (length point-lst) n)]
             [hd : (Listof Point) (take point-lst l)]
             [tl : (Listof Point) (list-tail point-lst l)])
        (cons hd (split tl (assert (sub1 n) positive?))))))


;; cluster-center computes the center point of a given point partition.
;;
;; If the point partition is empty, then the function returns false.

(: cluster-center ((Listof Point) -> (U Point False)))
(define (cluster-center partition)

  (: cc ((Listof Point) -> Point))
  (define (cc partition)
    (if (member '() partition)
        '()
        (let ([hd : Point          (map (λ ([p : Point]) (car p)) partition)]
              [tl : (Listof Point) (map (λ ([p : Point]) (cdr p)) partition)])
          (cons (mean hd) (cc tl)))))

  (if (null? partition)
      #f
      (cc partition)))

(: classify (Point (Listof Point) -> Exact-Nonnegative-Integer))
(define (classify point cluster-center-lst)

  (when (null? cluster-center-lst)
    (raise-argument-error 'cluster-center-lst
                          "Cluster center list must not be empty."
                          cluster-center-lst))

  (define d-lst : (Listof Real)
    (for/list ([cluster-center : Point (in-list cluster-center-lst)])

      (define difference : (Listof Real)
        (for/list ([a : Real (in-list point)]
                   [b : Real (in-list cluster-center)])
          (- b a)))

      
      (apply + (map (λ ([x : Real]) (* x x)) difference))))

  (assert (index-of d-lst (apply min d-lst))))

(: partition ((Listof Point) (Listof Point) -> (Listof (Listof Point))))
(define (partition point-lst cluster-center-lst)

  (when (null? cluster-center-lst)
    (raise-argument-error 'cluster-center-lst
                          "Cluster center list must not be empty."
                          cluster-center-lst))

  (define k : Exact-Nonnegative-Integer
    (length cluster-center-lst))

  (for/fold ([acc : (Listof (Listof Point)) (make-list k '())])
            ([point : Point (in-list point-lst)])

    (define cluster-id : Exact-Nonnegative-Integer
      (classify point cluster-center-lst))

    (define cluster : (Listof Point)
      (list-ref acc cluster-id))

    (list-set acc cluster-id (cons point cluster))))


(: pick-k ((Listof Point) Exact-Nonnegative-Integer -> (Listof Point)))
(define (pick-k point-lst k)

  (when (> k (length point-lst))
    (raise-argument-error 'point-lst
                          "Argument k must not exceed sample size."
                          k))
  
  (if (= k 0)
      '()
      (let ([lst : (Listof Point) (shuffle point-lst)])
        (cons (car lst)
              (pick-k (cdr lst) (sub1 k))))))

(: weighted-mean ((Listof Cluster-Pair) -> (Listof Point)))
(define (weighted-mean pair-lst)

  (when (null? pair-lst)
    (raise-argument-error 'pair-lst
                          "Cluster pair list must not be empty."
                          pair-lst))

  (define count-lst : (Listof Exact-Nonnegative-Integer)
    (map (λ ([x : Cluster-Pair]) (cdr x))
         pair-lst))

  (define cc-lst-lst : (Listof (Listof (U Point False)))
    (map (λ ([x : Cluster-Pair]) (car x)) pair-lst))

  (: proc ((Listof (Listof (U Point False))) -> (Listof Point)))
  (define (proc cc-lst-lst)
  
    (if (member '() cc-lst-lst)
        '()
        (let ([hd : (Listof (U Point False))          (map (λ ([x : (Listof (U Point False))]) (car x)) cc-lst-lst)]
              [tl : (Listof (Listof (U Point False))) (map (λ ([x : (Listof (U Point False))]) (cdr x)) cc-lst-lst)])

          (: raw-cc (U Point False))
          (: total Exact-Nonnegative-Integer)
          (define-values (raw-cc total)
            (for/fold ([acc : (U Point False)           #f]
                       [t   : Exact-Nonnegative-Integer 0])
                      ([p   : (U Point False)           (in-list hd)]
                       [c   : Exact-Nonnegative-Integer (in-list count-lst)])

              (if p
                  (let ([acc1 : Point (for/list ([x1 : Real (in-list (if acc acc (make-list (length p) 0)))]
                                                 [x2 : Real (in-list p)])
                                        (+ x1 (* x2 c)))]
                        [t1   : Exact-Nonnegative-Integer (+ t c)])
                    (values acc1 t1))
                  (values acc t))))

          (define cc : Point
            (for/list ([x : Real (in-list (assert raw-cc))])
              (/ (exact->inexact x)
                 (exact->inexact total))))

          (cons cc (proc tl)))))

  (proc cc-lst-lst))

            

(: gen-cluster-data (Point
                     Positive-Real
                     Exact-Nonnegative-Integer -> (Listof Point)))
(define (gen-cluster-data center-point stddev n)

  (define l : Exact-Nonnegative-Integer
    (length center-point))

  (: make-point (Exact-Nonnegative-Integer -> Point))
  (define (make-point i)

    (: make-element (Exact-Nonnegative-Integer -> Real))
    (define (make-element j)
      
      (define u : Real
        (list-ref center-point j))

      (sample (normal-dist u stddev)))

    (build-list l make-element))
  
  (build-list n make-point))


(: step ((Listof (Listof Point)) (Listof Point) -> (Listof Point)))
(define (step split-lst cluster-center-lst)

  (define z-lst : (Listof Cluster-Pair)
    (for/list ([splt : (Listof Point) (in-list split-lst)])
      
      (define l : Exact-Nonnegative-Integer
        (length splt))

      (define partition-lst : (Listof (Listof Point))
        (partition splt cluster-center-lst))

      (define cc-lst : (Listof (U Point False))
        (for/list ([partition : (Listof Point) partition-lst])
          (cluster-center partition)))

      (cons cc-lst l)))

  (weighted-mean z-lst))


(: run-k-means ((Listof (Listof Point))
                (Listof (Listof Point)) -> (Listof (Listof Point))))
(define (run-k-means split-lst history)

  (when (null? history)
    (raise-argument-error 'history
                          "History must not be empty."
                          history))

  (define cc-lst : (Listof Point)
    (car history))

  (define new-cc-lst : (Listof Point)
    (step split-lst cc-lst))

  (if (equal? cc-lst new-cc-lst)
      (reverse history)
      (run-k-means split-lst (cons new-cc-lst history))))







(module+ test

  (require (only-in typed/rackunit
                    check-equal?))

  (check-equal? (split '() 1)
                '(())
                "Splitting the empty list in one split should yield one empty split.")

  (check-equal? (split '() 2)
                '(() ())
                "Splitting the empty list in two splits should yield two empty splits.")

  (check-equal? (split '() 3)
                '(() () ())
                "Splitting the empty list in three splits should yield three empty splits.")

  (check-equal? (split '((1 2)) 1)
                '(((1 2)))
                "Splitting a list of size one in one split should yield one split of size one.")

  (check-equal? (split '((1 2)) 2)
                '(() ((1 2)))
                "Splitting a list of size one in two splits should yield one empty split and one split of size one.")

  (check-equal? (split '((1 2)) 3)
                '(() () ((1 2)))
                "Splitting a list of size one in three splits should yield two empty splits and one split of size one.")

  (check-equal? (split '((1 2) (3 4)) 1)
                '(((1 2) (3 4)))
                "Splitting a list of size two in one split should yield one split of size two.")

  (check-equal? (split '((1 2) (3 4)) 2)
                '(((1 2)) ((3 4)))
                "Splitting a list of size two in two splits should yield two splits of size one.")

  (check-equal? (split '((1 2) (3 4)) 3)
                '(() ((1 2)) ((3 4)))
                "Splitting a list of size two in three splits should yield one empty split and two splits of size one.")
  

  (check-equal? (split '((1 2) (3 4) (5 6)) 1)
                '(((1 2) (3 4) (5 6)))
                "Splitting a list of size three in one split should yield one split of size three.")

  (check-equal? (split '((1 2) (3 4) (5 6)) 2)
                '(((1 2)) ((3 4) (5 6)))
                "Splitting a list of size three in two splits should yield one split of size one and one split of size two.")

  (check-equal? (split '((1 2) (3 4) (5 6)) 3)
                '(((1 2)) ((3 4)) ((5 6)))
                "Splitting a list of size three in three splits should yield three splits of size one.")

  (check-equal? (split '((1 2) (3 4) (5 6) (7 8)) 1)
                '(((1 2) (3 4) (5 6) (7 8)))
                "Splitting a list of size four in one split should yield one split of size four.")

  (check-equal? (split '((1 2) (3 4) (5 6) (7 8)) 2)
                '(((1 2) (3 4)) ((5 6) (7 8)))
                "Splitting a list of size four in two splits should yield two splits of size two.")
  
  (check-equal? (split '((1 2) (3 4) (5 6) (7 8)) 3)
                '(((1 2)) ((3 4)) ((5 6) (7 8)))
                "Splitting a list of size four in three splits should yield two splits of size one and one split of size two.")
  
  (let ([data (gen-cluster-data '(0 0) 1 999)])
      (check-equal? (apply append (split data 10))
                    data
                    "Splitting a list and appending the splits should yield the original list."))

  (let ([data (gen-cluster-data '(0 0) 1 1000)])
    (check-equal? (apply append (split data 10))
                   data
                   "Splitting a list and appending the splits should yield the original list."))

  (let ([data (gen-cluster-data '(0 0) 1 1001)])
    (check-equal? (apply append (split data 10))
                  data
                  "Splitting a list and appending the splits should yield the original list."))

  (check-equal? (cluster-center '((1 2)))
                '(1 2)
                "The cluster center of a partition of size one is its only point.")

  (check-equal? (cluster-center '((1 2) (3 4)))
                '(2 3)
                "The cluster center of a partition of size two is the mean of both points")

  (check-equal? (cluster-center '((1 2) (3 4) (5 6)))
                '(3 4)
                "The cluster center of a partition of size three is the mean of all points")

  (check-equal? (partition '() '((1 2))) '(()))
  (check-equal? (partition '((1 2)) '((1 2))) '(((1 2))))
  (check-equal? (partition '((1 2) (3 4)) '((1 2))) '(((3 4) (1 2))))
  (check-equal? (partition '((1 2) (3 4) (5 6)) '((1 2))) '(((5 6) (3 4) (1 2))))

  (check-equal? (partition '() '((1 2) (3 4))) '(() ()))
  (check-equal? (partition '((1 2)) '((1 2) (3 4))) '(((1 2)) ()))
  (check-equal? (partition '((1 2) (3 4)) '((1 2) (3 4))) '(((1 2)) ((3 4))))
  (check-equal? (partition '((1 2) (3 4) (5 6)) '((1 2) (3 4))) '(((1 2)) ((5 6) (3 4))))

  (check-equal? (partition '() '((1 2) (3 4) (5 6))) '(() () ()))
  (check-equal? (partition '((1 2)) '((1 2) (3 4) (5 6))) '(((1 2)) () ()))
  (check-equal? (partition '((1 2) (3 4)) '((1 2) (3 4) (5 6))) '(((1 2)) ((3 4)) ()))
  (check-equal? (partition '((1 2) (3 4) (5 6)) '((1 2) (3 4) (5 6))) '(((1 2)) ((3 4)) ((5 6))))

  (check-equal? (weighted-mean '((() . 1))) '())
  (check-equal? (weighted-mean '((() . 2))) '())
  (check-equal? (weighted-mean '((() . 3))) '())
  
  (check-equal? (weighted-mean '((((1 2)) . 1))) '((1.0 2.0)))
  (check-equal? (weighted-mean '((((1 2)) . 2))) '((1.0 2.0)))
  (check-equal? (weighted-mean '((((1 2)) . 3))) '((1.0 2.0)))

  (check-equal? (weighted-mean '((((1 2) (3 4)) . 1))) '((1.0 2.0) (3.0 4.0)))
  (check-equal? (weighted-mean '((((1 2) (3 4)) . 2))) '((1.0 2.0) (3.0 4.0)))
  (check-equal? (weighted-mean '((((1 2) (3 4)) . 3))) '((1.0 2.0) (3.0 4.0)))

  (check-equal? (weighted-mean '((((1 2)) . 1) (((3 4)) . 3))) '((2.5 3.5)))
  (check-equal? (weighted-mean '((((1 2)) . 2) (((3 4)) . 6))) '((2.5 3.5)))
  (check-equal? (weighted-mean '((((1 2)) . 3) (((3 4)) . 9))) '((2.5 3.5)))


  )
