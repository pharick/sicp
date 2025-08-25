#lang racket

(require rackunit
         compatibility/mlist
         racket/set)

(define (cycle? x)
  (let ([visited (mutable-seteq)])
    (let loop ([node x])
      (cond
        [(not (mpair? node)) #f]
        [(set-member? visited node) #t]
        [else
         (set-add! visited node)
         (or (loop (mcar node))
             (loop (mcdr node)))]))))

;; -- tests --

(define (cdr x) (mcdr x))

(define (set-cdr! rest pair) (set-mcdr! rest pair)) 

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (mlist 'a 'b 'c)))

(check-equal? (cycle? z) #t)
(check-equal? (cycle? (mlist 1 2 3)) #f)
