#lang racket

(require sicp)
(require rackunit)

(define (cont-frac n d k)
  (define (iter n d k res)
    (if (= k 0)
        res
        (iter n d (- k 1) (/ (n k) (+ (d k) res)))))
  (iter n d k 0))

(define (test k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(check-equal? (round (* 1000 (test 100))) 618.0)
