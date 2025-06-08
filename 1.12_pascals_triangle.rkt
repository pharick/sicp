#lang racket

(require sicp)
(require rackunit)

(define (solution r c)
  (if (or (= r 1) (= c 1) (= r c))
       1
       (+ (solution (- r 1) (- c 1))
          (solution (- r 1) c))))
      

(check-equal? (solution 1 1) 1)
(check-equal? (solution 3 2) 2)
(check-equal? (solution 4 3) 3)
(check-equal? (solution 5 2) 4)
(check-equal? (solution 5 3) 6)
