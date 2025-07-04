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

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 ((< x2 x1) (cons x2 (union-set set1 (cdr set2))))
                 (else (cons x1 (union-set (cdr set1) (cdr set2)))))))))

(define x '(1 3 5 8))
(define y '(0 1 2 4 9))

(check-equal? (union-set x y) '(0 1 2 3 4 5 8 9))
(check-equal? (union-set x y) (union-set y x))
(check-equal? (union-set '() y) y)
(check-equal? (union-set x '()) x)
(check-equal? (union-set x x) x)
