#lang racket

(require sicp)
(require rackunit)

(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3))))))) 

(check-equal? (f-recursive 2) 2)
(check-equal? (f-recursive -42) -42)
(check-equal? (f-recursive 4) 11)
(check-equal? (f-recursive 3) 4)

(define (f-iterative n)
  (define (f n a b c)
    (if (= n 0)
        a
        (f (- n 1)
           b
           c
           (+ c (* 2 b) (* 3 a)))))
  (if (< n 0) n (f n 0 1 2)))

(check-equal? (f-iterative 2) 2)
(check-equal? (f-iterative -42) -42)
(check-equal? (f-iterative 4) 11)
(check-equal? (f-iterative 3) 4)
