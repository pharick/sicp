#lang racket

(require racket/mpair)
(require rackunit)

;; -- node --

(define (make-deque-node value)
  (mcons value (mcons '() '())))

(define (deque-node-value node)
  (mcar node))

(define (deque-node-prev node)
  (mcar (mcdr node)))

(define (deque-node-next node)
  (mcdr (mcdr node)))

(define (deque-node-set-prev! node v)
  (set-mcar! (mcdr node) v))

(define (deque-node-set-next! node v)
  (set-mcdr! (mcdr node) v))

;; -- deque --

(define (make-deque)
  (mcons '() '()))

(define (deque-front-node deque)
  (mcar deque))

(define (deque-rear-node deque)
  (mcdr deque))

(define (deque-set-front-node! deque v)
  (set-mcar! deque v))

(define (deque-set-rear-node! deque v)
  (set-mcdr! deque v))

(define (empty-deque? deque)
  (null? (deque-front-node deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty queue" deque)
      (deque-node-value (deque-front-node deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty queue" deque)
      (deque-node-value (deque-rear-node deque))))

(define (front-insert-deque! deque value)
  (let ((node (make-deque-node value)))
    (if (empty-deque? deque)
        (begin
          (deque-set-front-node! deque node)
          (deque-set-rear-node! deque node))
        (begin
          (deque-node-set-next! node (deque-front-node deque))
          (deque-node-set-prev! (deque-front-node deque) node)
          (deque-set-front-node! deque node)))))

(define (rear-insert-deque! deque value)
  (let ((node (make-deque-node value)))
    (if (empty-deque? deque)
        (begin
          (deque-set-front-node! deque node)
          (deque-set-rear-node! deque node))
        (begin
          (deque-node-set-prev! node (deque-rear-node deque))
          (deque-node-set-next! (deque-rear-node deque) node)
          (deque-set-rear-node! deque node)))))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "DELETE! called with an empty queue" deque)
      (deque-set-front-node! deque (deque-node-next (deque-front-node deque)))))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "DELETE! called with an empty queue" deque)
      (deque-set-rear-node! deque (deque-node-prev (deque-rear-node deque)))))

;; -- tests --

(define (cons x pair) (mcons x pair))

(define (car x) (mcar x))

(define (cdr x) (mcdr x))

(define (set-car! head pair) (set-mcar! head pair))

(define (set-cdr! tail pair) (set-mcdr! tail pair))

(define dq (make-deque))

(check-true (empty-deque? dq))

(front-insert-deque! dq 'a)
(check-false (empty-deque? dq))
(check-equal? (front-deque dq) 'a)
(check-equal? (rear-deque dq) 'a)

(rear-insert-deque! dq 'b)
(check-equal? (rear-deque dq) 'b)

(front-delete-deque! dq)
(check-equal? (front-deque dq) 'b)

(front-insert-deque! dq 'a)
(check-equal? (front-deque dq) 'a)

(rear-delete-deque! dq)
(check-equal? (rear-deque dq) 'a)
