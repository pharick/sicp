#lang racket

(require sicp)
(require rackunit)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((half-exp (expmod base (/ exp 2) m)))
           (if (= half-exp 0)  ; Signal already detected
               0
               (let ((square (remainder (* half-exp half-exp) m)))
                 (if (nontrivial-square-root? half-exp square m)
                     0  ; Signal nontrivial square root found
                     square)))))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (nontrivial-square-root? x square n)
  (and (= square 1)           ; x^2 ≡ 1 (mod n)
       (not (= x 1))          ; x ≠ 1
       (not (= x (- n 1)))))  ; x ≠ n-1

(check-equal? (miller-rabin-test 3) #t)
(check-equal? (miller-rabin-test 4) #f)
(check-equal? (miller-rabin-test 561) #f)
(check-equal? (miller-rabin-test 1105) #f)
(check-equal? (miller-rabin-test 1729) #f)
(check-equal? (miller-rabin-test 2465) #f)
(check-equal? (miller-rabin-test 2821) #f)
(check-equal? (miller-rabin-test 6601) #f)
(check-equal? (miller-rabin-test 19999) #f)
(check-equal? (miller-rabin-test 1999) #t)
