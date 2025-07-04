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
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (- (exponent exp) 1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; -- exponentiation --

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

;; -- sum --

(define (make-sum . ops)
  (let ((symbols (filter (lambda (x) (not (number? x))) ops))
        (number (foldr + 0 (filter number? ops))))
    (cond ((null? symbols) number)
          ((and (= number 0) (null? (cdr symbols))) (car symbols))
          ((= number 0) (cons '+ symbols))
          ((null? (cdr symbols)) (list '+ number (car symbols)))
          (else (cons '+ (cons number symbols))))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

;; -- product --

(define (make-product . ops)
  (let ((symbols (filter (lambda (x) (not (number? x))) ops))
        (number (foldr * 1 (filter number? ops))))
    (cond ((= number 0) 0)
          ((and (= number 1) (null? (cdr symbols))) (car symbols))
          ((= number 1) (cons '* symbols))
          ((null? (cdr symbols)) (list '* number (car symbols)))
          (else (cons '* (cons number symbols))))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;; -- predicates --

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; -- tests --

(check-equal? (deriv '(* x y) 'x) 'y)
(check-equal? (deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
