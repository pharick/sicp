#lang racket

(require sicp)
(require rackunit)

(define (cont-frac n d k)
  (define (iter n d k res)
    (if (= k 0)
        res
        (iter n d (- k 1) (/ (n k) (+ (d k) res)))))
  (iter n d k 0))

(define (e k)
  (+ 2 (cont-frac (lambda (i) 1)
                  (lambda (i) (if (= (modulo i 3) 2)
                                  (* 2 (+ 1 (quotient i 3)))
                                  1.0))
                  k)))

(check-equal? (round (* 1000000000000 (e 100))) 2718281828459.0) 
(check-equal? (round (* 100000 (e 10)))  271828.0)
