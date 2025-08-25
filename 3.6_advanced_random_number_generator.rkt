#lang racket

(require rackunit)

(define rand-init 0)
(define (rand-update x) (modulo (+ (* 1103515245 x) 12345) (expt 2 31)))

(define rand
  (let ([x rand-init])
    (lambda (command)
      (cond [(eq? command 'generate)
             (begin
               (set! x (rand-update x))
               x)]
            [(eq? command 'reset)
             (lambda (init) (set! x init))]
            [else (error "unknown command -- RAND" command)]))))

;; -- test --

(define random-value (rand 'generate))
(define test-value-1 (rand 'generate))
(define test-value-2 (rand 'generate))
(define test-value-3 (rand 'generate))

((rand 'reset) random-value)

(check-equal? test-value-1 (rand 'generate))
(check-equal? test-value-2 (rand 'generate))
(check-equal? test-value-3 (rand 'generate))
