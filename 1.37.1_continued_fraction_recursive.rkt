#lang racket

(require sicp)
(require rackunit)

(define (cont-frac n d k)
  (define (rec n d i)
    (if (> i k) 0
        (/ (n i)
           (+ (d i) (rec n d (+ i 1))))))
  (rec n d 1))

(define (test k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(check-equal? (round (* 1000 (test 100))) 618.0)
