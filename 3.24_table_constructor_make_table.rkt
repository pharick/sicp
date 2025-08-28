#lang racket

(require racket/mpair)
(require rackunit)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (car (car records)))
           (car records))
          (else
           (assoc key (cdr records)))))
  
  (let ((local-table (cons '*table* '())))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (cons key-1
                                  (cons (cons key-2 value) '()))
                      (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else
             (error "Unknown operation -- TABLE" m))))
    dispatch))

;; -- tests --

;(define (set-cdr! tail pair) (set-mcdr! tail pair))

(define table-equal (make-table equal?))
(define get-equal (table-equal 'lookup-proc))
(define put-equal (table-equal 'insert-proc!))

(put-equal 1 2 42)
(check-equal? (get-equal 1 2) 42)
(check-false (get-equal 1 2.0000001))

(define (same-key-epsilon? epsilon)
  (lambda (x y) 
    (if (< (abs (- x y)) epsilon) #t
        #f)))

(define table-epsilon (make-table (same-key-epsilon? 0.1)))
(define get-epsilon (table-epsilon 'lookup-proc))
(define put-epsilon (table-epsilon 'insert-proc!))

(put-epsilon 1 2 42)
(check-equal? (get-epsilon 1 2) 42)
(check-equal? (get-epsilon 1 2.001) 42)
(check-equal? (get-epsilon 1.01 2.01001) 42)
(check-false (get-epsilon 1.2 2.1))
