#lang racket

(require sicp)
(require rackunit)

;; -- percentage tolerance interval

(define (make-center-percent c p)
  (make-center-width c (* (/ c 100) p)))

(define (percent i)
  (* 100 (/ (width i) (center i))))

;; -- center-width interval

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; -- endpoints interval

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; -- tests --

(define center-test 4)

(define interval (make-interval 3 5))

(define center-interval (make-center-percent center-test 25))

(define center-test2 100)

(define interval2 (make-interval 90 110))

(define center-interval2 (make-center-percent center-test2 10))

(check-equal? (center interval) center-test)
(check-equal? interval center-interval)
(check-equal? (center interval2) center-test2)
(check-equal? interval2 center-interval2)
(check-equal? (percent interval) 25)
(check-equal? (percent center-interval2) 10)
