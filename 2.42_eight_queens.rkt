#lang racket

(require rackunit)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k positions)
  (let ((row-k (car positions))
        (rest (cdr positions)))
    (define (check diag-dist others)
      (cond ((empty? others) #t)
            ((or (= row-k (car others))
                 (= (abs (- row-k (car others))) diag-dist))
             #f)
            (else (check (+ diag-dist 1) (cdr others)))))
    (check 1 rest)))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (enumerate-interval a b)
  (define (iter a b lst)
    (if (> a b)
        lst
        (iter a (- b 1) (cons b lst))))
  (iter a b '()))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; --- tests ---

(define queens4 (queens 4))

(check-equal? (queens 1) '((1)))
(check-equal? (queens 2) '())
(check-equal? (queens 3) '())
(check-true (or (equal? queens4 '((3 1 4 2) (2 4 1 3)))
                (equal? queens4 '((2 4 1 3) (3 1 4 2)))))