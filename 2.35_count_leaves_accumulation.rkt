#lang racket

(require sicp)
(require rackunit)

(define (count-leaves t)
  (accumulate + 0 (map (lambda (n) (if (pair? n)
                                       (count-leaves n)
                                       1))
                       t)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; --- tests ---

(define x (cons (list 1 2) (list 3 4)))

(check-equal? (count-leaves (list 1 2 (list 3 (list 4 5)))) 5)
(check-equal? (count-leaves (list x x)) 8)
