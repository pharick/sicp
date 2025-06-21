#lang racket

(require sicp)
(require rackunit)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; --- tests ---

(define m (list (list 1 2)
                (list 3 4)))

(define m2 '((1 2 3 4)
             (4 5 6 6)
             (6 7 8 9)))

(define m2-transpose '((1 4 6)
                       (2 5 7)
                       (3 6 8)
                       (4 6 9)))

(define v (list 1 2))

(define x1 (list 2 3))

(define x2 (list 3 2))

(check-equal? (dot-product v x1) 8)
(check-equal? (dot-product v x2) 7)
(check-equal? (matrix-*-vector m v) (list 5 11))

(check-equal? (transpose m) (list (list 1 3)
                                  (list 2 4)))

(check-equal? (transpose m2) m2-transpose)

(check-equal? (matrix-*-matrix m m) '((7 10)
                                      (15 22)))
