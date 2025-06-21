#lang racket

(require sicp)
(require rackunit)

(define (make-triple-sum n s)
  (filter (lambda (triplet)
            (= (accumulate + 0 triplet) s))
          (generate-triplets n)))

(define (generate-triplets n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval a b)
  (define (iter a b lst)
    (if (> a b)
        lst
        (iter a (- b 1) (cons b lst))))
  (iter a b '()))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(check-equal? (make-triple-sum 4 6) '((3 2 1)))
(check-equal? (make-triple-sum 3 6) '((3 2 1)))
(check-equal? (length (make-triple-sum 9 9)) 3)
