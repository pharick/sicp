#lang racket

(require rackunit)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (union-set (adjoin-set (car set2) set1) (cdr set2))))

(define first '(1 2 3 5 3))
(define second '(3 4 5 6 5))

(check-equal? (union-set first second) '(5 6 5 4 3 1 2 3 5 3))
(check-equal? (intersection-set first second) '(3 5 3))
(check-equal? (intersection-set second first) '(3 5 5))
