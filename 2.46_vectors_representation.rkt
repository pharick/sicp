#lang racket

(require sicp)
(require rackunit)

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

(define vec1 (make-vect 1.0 2.0))
(define vec2 (make-vect 0.5 0.5))

(check-equal? (xcor-vect vec1) 1.0)
(check-equal? (ycor-vect vec2) 0.5)
(check-equal? (add-vect vec1 vec2) (make-vect 1.5 2.5))
(check-equal? (sub-vect vec1 vec2) (make-vect 0.5 1.5))
(check-equal? (scale-vect 2 vec2) (make-vect 1.0 1.0))
