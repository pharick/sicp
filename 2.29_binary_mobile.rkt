#lang racket

(require sicp)
(require rackunit)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (mobile-balanced? mobile)
  (if (not (pair? mobile))
      #t
      (let ((left-torque (* (branch-length (left-branch mobile))
                            (total-weight (branch-structure (left-branch mobile)))))
            (right-torque (* (branch-length (right-branch mobile))
                             (total-weight (branch-structure (right-branch mobile))))))
        (and (= left-torque right-torque)
             (mobile-balanced? (branch-structure (left-branch mobile)))
             (mobile-balanced? (branch-structure (right-branch mobile)))))))

;; -- tests --

(define nested-left-branch (make-branch 4 5))

(define nested-right-branch (make-branch nested-left-branch (make-branch 6 7)))

(define left (make-branch 1 2))

(define right (make-branch 3 nested-right-branch))

(define mobile (make-mobile left right))

(define mobile2 (make-mobile (make-branch 5 2)
                             (make-branch 1 10)))

(check-equal? (total-weight mobile) 14)
(check-equal? (total-weight mobile2) 12)
(check-equal? (left-branch mobile) left)
(check-equal? (right-branch mobile) right)
(check-equal? (branch-length left) 1)
(check-equal? (branch-structure right) '((4 5) (6 7)))
(check-equal? (mobile-balanced? mobile) #f)
(check-equal? (mobile-balanced? mobile2) #t)
