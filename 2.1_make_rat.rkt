#lang racket

(require sicp)
(require rackunit)

(define (make-rat n d)
  (let* ((g (gcd n d))
         (rn (abs (/ n g)))
         (rd (abs (/ d g))))
    (if (>= (* n d) 0)
        (cons rn rd)
        (cons (- rn) rd))))

(check-equal? (make-rat 1 2) '(1 . 2))
(check-equal? (make-rat (- 1) 2) '(-1 . 2))
(check-equal? (make-rat (- 1) (- 2)) '(1 . 2))
(check-equal? (make-rat 1 (- 2)) '(-1 . 2))
(check-equal? (make-rat 2 4) '(1 . 2))
