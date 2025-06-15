#lang racket

(require sicp)
(require rackunit)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define l (list 1 2 3))
(define nil '())

(check-equal? (subsets l) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
