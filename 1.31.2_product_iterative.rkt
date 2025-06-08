#lang racket

(require sicp)
(require rackunit)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (wallis n)
  (define (nom-term k)
    (+ 4 (* 2 (quotient k 2))))
  (define (den-term k)
    (+ 3 (* 2 (quotient k 2))))
  (exact->inexact (* 4 (/ (* 2 (product nom-term 0 inc (- n 2)))
                          (product den-term 0 inc (- n 1))))))

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

(check-equal? (product square 1 inc 3) 36)
(check-equal? (product identity 3 inc 5) 60)
(check-equal? (factorial 5) 120)
