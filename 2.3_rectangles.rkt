#lang racket

(require sicp)
(require rackunit)

; -- rectangle --

(define (make-rectangle top-left-point w h)
  (cons top-left-point (make-dimensions2d w h)))

(define (rectangle-top-left-point r)
  (car r))

(define (rectangle-dimensions r)
  (cdr r))

(define (rectangle-width r)
  (w-dimensions2d (rectangle-dimensions r)))

(define (rectangle-height r)
  (h-dimensions2d (rectangle-dimensions r)))

(define (rectangle-top-right-point r)
  (let ((trx (x-point (rectangle-top-left-point r)))
        (try (y-point (rectangle-top-left-point r)))
        (w (rectangle-width r)))
    (make-point (+ trx w) try)))

(define (rectangle-bottom-left-point r)
  (let ((trx (x-point (rectangle-top-left-point r)))
        (try (y-point (rectangle-top-left-point r)))
        (h (rectangle-height r)))
    (make-point trx (+ try h))))

(define (rectangle-bottom-right-point r)
  (let ((trx (x-point (rectangle-top-left-point r)))
        (try (y-point (rectangle-top-left-point r)))
        (w (rectangle-width r))
        (h (rectangle-height r)))
    (make-point (+ trx w) (+ try h))))

(define (rectangle-square r)
  (let ((w (rectangle-width r))
        (h (rectangle-height r)))
    (* w h)))

(define (rectangle-perimeter r)
  (let ((w (rectangle-width r))
        (h (rectangle-height r)))
    (+ (* w 2) (* h 2))))

; -- dimensions2d --

(define (make-dimensions2d w h)
  (cons w h))

(define (w-dimensions2d d)
  (car d))

(define (h-dimensions2d d)
  (cdr d))

; -- point --

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

; -- tests --

(define start-point (make-point 0 0))

(define rectangle (make-rectangle start-point 2 3))

(check-equal? (rectangle-square rectangle) 6)
(check-equal? (rectangle-perimeter rectangle) 10)
