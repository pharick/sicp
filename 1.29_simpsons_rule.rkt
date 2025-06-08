#lang racket

(require sicp)
(require rackunit)

(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (coeff k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (iter f a b k sum)
    (if (<= k n)
        (iter f
              a
              b
              (+ k 1)
              (+ sum
                 (*
                  (coeff k)
                  (f (+ a (* k h))))))
        sum))
  (if (not (even? n))
      (error "n should be even")
      (* (/ h 3) (iter f a b 0 0.0))))

(check-equal? (round (* 100 (simpson cube 0 1 100))) 25.0)
(check-equal? (round (* 100 (simpson cube 0 1 1000))) 25.0)
(check-equal? (floor (* 1000 (simpson cube 0 1 100))) 249.0)
(check-equal? (floor (* 1000 (simpson cube 0 1 1000))) 250.0)
