#lang racket

(require racket/mpair)

;; -- queue --

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with on empty queue")
          (car front-ptr)))
    (define (insert! item)
      (if (empty?)
          (begin
            (set! front-ptr (mcons item '()))
            (set! rear-ptr front-ptr))
          (begin
            (set-mcdr! rear-ptr (mcons item '()))
            (set! rear-ptr (mcdr rear-ptr)))))
    (define (delete!)
      (if (empty?)
          (error "DELETE! called with on empty queue")
          (set! front-ptr (mcdr front-ptr))))

    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
            ((eq? m 'front) (front))
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) (delete!))
            (else
             (error "MAKE-QUEUE Invalid command" m))))
    dispatch))
