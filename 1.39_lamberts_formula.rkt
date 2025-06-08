#lang racket

(require sicp)
(require rackunit)

(define (cont-frac n d k)
  (define (iter k res)
    (if (= k 0)
        res
        (iter (- k 1) (/ (n k) (+ (d k) res)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

(define pi 3.141592653589793)

(check-equal? (tan-cf (/ pi 4) 100) 1.0)
(check-equal? (tan-cf 0 10) 0)
(check-equal? (round (* 100 (tan-cf (/ pi 3) 100))) 173.0)
