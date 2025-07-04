#lang racket

(require rackunit)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; -- sum --

(define (make-sum . terms)
  (let ((consts (filter number? terms))
        (non-consts (filter (complement number?) terms)))
    (let* ((consts-sum (apply + 0 consts))
           (res-terms (if (= consts-sum 0)
                          non-consts
                          (cons consts-sum non-consts))))
      (cond ((null? res-terms) 0)
            (else (make-infix '+ res-terms))))))

(define (sum? x)
  (and (pair? x) (not (null? (cdr x))) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

;; -- product

(define (make-product . terms)
  (let ((consts (filter number? terms))
        (non-consts (filter (complement number?) terms)))
    (let* ((consts-product (apply * 1 consts))
           (res-terms (cond ((= consts-product 1) non-consts)
                            ((= consts-product 0) '())
                            (else (cons consts-product non-consts)))))
      (cond ((null? res-terms) 0)
            (else (make-infix '* res-terms))))))

(define (product? x)
  (and (pair? x) (not (null? (cdr x))) (eq? (cadr x) '*)))

(define (multiplier s) (car s))

(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

;; -- helpers --

(define (make-infix op terms)
  (define (recurse terms)
    (cond ((null? terms) '())
          ((null? (cdr terms)) (list (car terms)))
          (else (cons (car terms) (cons op (recurse (cdr terms)))))))
  (let ((infix (recurse terms)))
    (cond ((null? infix) (error "No operators in infix form"))
          ((null? (cdr infix)) (car infix))
          (else infix))))

(define (complement f)
  (lambda (x) (not (f x))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; -- tests --

(check-equal? (deriv '(x * 2) 'x) 2)
(check-equal? (deriv '(x * y) 'x) 'y)
(check-equal? (deriv '(2 * x + y) 'x) 2)
(check-equal? (deriv '((2 * x) + (3 * x) + 1) 'x) 5)
