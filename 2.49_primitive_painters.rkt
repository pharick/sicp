#lang racket

(require sicp-pict)

(define outline
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define cross
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond
  (segments->painter
   (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
         (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.2) (make-vect 0.2 0.8))
    (make-segment (make-vect 0.2 0.8) (make-vect 0.4 0.2))
    (make-segment (make-vect 0.4 0.2) (make-vect 0.6 0.8))
    (make-segment (make-vect 0.6 0.8) (make-vect 0.8 0.2))
    (make-segment (make-vect 0.8 0.2) (make-vect 1.0 0.8)))))
