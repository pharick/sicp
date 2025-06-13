#lang racket

(require sicp)
(require rackunit)

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond
      ; both intervals >= 0
      ((and (>= lx 0) (>= ly 0))
       (make-interval (* lx ly) (* ux uy)))

      ; x >= 0, y <= 0
      ((and (>= lx 0) (<= uy 0))
       (make-interval (* ux ly) (* lx uy)))

      ; x >= 0, y spans zero
      ((and (>= lx 0) (< ly 0) (> uy 0))
       (make-interval (* ux ly) (* ux uy)))

      ; x <= 0, y >= 0
      ((and (<= ux 0) (>= ly 0))
       (make-interval (* lx uy) (* ux ly)))

      ; both intervals <= 0
      ((and (<= ux 0) (<= uy 0))
       (make-interval (* ux uy) (* lx ly)))

      ; x <= 0, y spans zero
      ((and (<= ux 0) (< ly 0) (> uy 0))
       (make-interval (* lx uy) (* lx ly)))

      ; x spans zero, y >= 0
      ((and (< lx 0) (> ux 0) (>= ly 0))
       (make-interval (* lx uy) (* ux uy)))

      ; x spans 0, y <= 0
      ((and (< lx 0) (> ux 0) (<= uy 0))
       (make-interval (* ux ly) (* lx ly)))

      ; both intervals span zero - general case
      (else
       (let ((p1 (* lx ly))
             (p2 (* lx uy))
             (p3 (* ux ly))
             (p4 (* ux uy)))
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4)))))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))


(define interval (make-interval -5 15))
(define interval2 (make-interval 5 10))
(define interval3 (make-interval 6 8))

(define result1 (mul-interval interval interval2))
(define result2 (mul-interval interval2 interval))
(define result3 (mul-interval interval2 interval3))

(check-equal? result1 result2)
(check-equal? result3 '(30 . 80))
