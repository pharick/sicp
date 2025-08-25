#lang racket

;; -- mutable pair --

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond
      [(eq? m 'car) x]
      [(eq? m 'cdr) y]
      [(eq? m 'set-car!) set-x!]
      [(eq? m 'set-cdr!) set-y!]
      [(eq? m 'print) (format "(~a . ~a)" x y)]
      (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z v)
  ((z 'set-car!) v)
  z)

(define (set-cdr! z v)
  ((z 'set-cdr!) v)
  z)

(define (print-pair p)
  (display (p 'print)))

;; -- queue --

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue]))

(define (print-queue q)
  (display "<queue: (")
  (let loop ([node (front-ptr q)] [first? #t])
    (cond
      [(null? node)
       (display ")>")
       (newline)]
      [else
       (unless first? (display " "))
       (write (car node))                   ; prints atoms/lists readably
       (loop (cdr node) #f)])))
    
;; -- test --

(define q1 (make-queue))

(print-queue (insert-queue! q1 'a))

(print-queue (insert-queue! q1 'b))

(print-queue (delete-queue! q1))

(print-queue (delete-queue! q1))
