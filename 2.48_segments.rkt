#lang racket

(require sicp)
(require rackunit)

;; -- segments --

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; -- vectors --

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; -- tests --

(define vect1 (make-vect 1.0 2.5))
(define vect2 (make-vect 0.0 5.0))

(define segment (make-segment vect1 vect2))

(check-equal? (start-segment segment) vect1)
(check-equal? (end-segment segment) vect2)
