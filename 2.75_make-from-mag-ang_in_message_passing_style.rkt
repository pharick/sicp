#lang racket

(require rackunit)

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (get-real-part val)
  (apply-generic 'real-part val))

(define (get-imag-part val)
  (apply-generic 'imag-part val))

(define (get-magnitude val)
  (apply-generic 'magnitude val))

(define (get-angle val)
  (apply-generic 'angle val))

(define (square x)
  (* x x))

;; -- tests --

(define magnitude 2)
(define angle 90.0)

(define test-value (make-from-mag-ang magnitude angle))

(check-equal? (get-magnitude test-value) magnitude)
(check-equal? (get-angle test-value) angle)
(check-equal? (get-real-part test-value) (* magnitude (cos angle)))
(check-equal? (get-imag-part test-value) (* magnitude (sin angle)))
