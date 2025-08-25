#lang racket

(require compatibility/mlist
         rackunit)

(define (cycle? x)
  (let loop ([slow x] [fast x])
    (cond
      [(or (not (mpair? slow))
           (not (mpair? fast))
           (not (mpair? (mcdr fast))))
       #f]
      [else
       (let ((nslow (mcdr slow))
             (nfast (mcdr (mcdr fast))))
         (if (eq? nslow nfast)
             #t
             (loop nslow nfast)))])))

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
