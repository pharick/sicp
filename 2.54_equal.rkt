#lang racket

(require sicp)
(require rackunit)

(define (equal-proc? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        (else #f)))

(check-equal? (equal? '(this is a list) '(this is a list)) #t)
(check-equal? (equal? '(this is a list) '(this (is a) list)) #f)

(check-equal? (equal-proc? '(this is a list) '(this is a list)) #t)
(check-equal? (equal-proc? '(this is a list) '(this (is a) list)) #f)
