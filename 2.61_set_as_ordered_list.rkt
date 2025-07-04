#lang racket

(require rackunit)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (if (null? set)
      (cons x set)
      (let ((head (car set))
            (tail (cdr set)))
        (cond ((= x head) set)
              ((> x head) (cons head (adjoin-set x tail)))
              (else (cons x set))))))
  

;; -- tests --

(define x (adjoin-set 2 '()))
(define y (adjoin-set 0 x))
(define z (adjoin-set 1 y))
(define q (adjoin-set 1 z))

(check-equal? z q)
(check-equal? (adjoin-set 1 (adjoin-set 0 (adjoin-set 2 '())))
              (adjoin-set 1 (adjoin-set 2 (adjoin-set 0 '()))))
