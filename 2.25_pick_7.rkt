#lang racket

(require sicp)

(define lst1 (list 1 3 (list 5 7) 9))

(define (pick7-1 lst)
  (car (cdr (car (cdr (cdr lst))))))

(define lst2 (list ( list 7)))

(define (pick7-2 lst)
  (car (car lst)))

(define lst3 '(1 (2 (3 (4 (5 (6 7)))))))

(define (pick7-3 lst)
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst)))))))))))))
