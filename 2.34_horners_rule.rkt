#lang racket

(require sicp)
(require rackunit)

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(check-equal? (horner-eval 1 (list 1 3 0 5 0 1)) 10)
(check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)
(check-equal? (horner-eval 2 (list 1 3 1 5 0 1)) 83)
