#lang racket

(require sicp)
(require rackunit)

(define (filtered-accumulate combiner null-value term a next b pred)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (prime-squares-sum a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (relatively-prime-product n)
  (define (n-relatively-prime? i)
    (relatively-prime? i n))
  (filtered-accumulate * 1 identity 1 inc (- n 1) n-relatively-prime?))

(define (relatively-prime? i n)
  (= (gcd i n) 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prime? n)
  (define (has-divisor? d)
    (cond ((> (* d d) n) #f)
          ((= (remainder n d) 0) #t)
          (else (has-divisor? (+ d 1)))))
  (cond ((< n 2) #f)
        (else (not (has-divisor? 2)))))

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

(check-equal? (filtered-accumulate * 1 square 1 inc 3 odd?) 9)
(check-equal? (filtered-accumulate * 1 identity 3 inc 5 odd?) 15)
(check-equal? (filtered-accumulate + 0 identity 1 inc 10 odd?) 25)
