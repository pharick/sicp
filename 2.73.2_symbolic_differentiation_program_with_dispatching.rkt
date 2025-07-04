#lang racket

(require compatibility/mlist)
(require rackunit)

;; -- dispatching table --

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; -- derivative --

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; -- predicates --

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; -- sum derivative package --

(define (install-sum-package)
  (put 'deriv '+
       (lambda (operands var)
         (let ((exp (cons '+ operands)))
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var))))))

;; -- product derivative package --

(define (install-product-package)
  (put 'deriv '*
       (lambda (operands var)
         (let ((exp (cons '* operands)))
           (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp)))))))

;; -- exponentiation derivative package --

(define (install-exponentiation-package)
  (put 'deriv '**
       (lambda (operands var)
         (if (not (= (length operands) 2))
             (error "exponentiation should have two operands" operands)
             (let ((exp (cons '** operands)))
               (make-product
                (exponent exp)
                (make-product
                 (make-exponentiation (base exp) (- (exponent exp) 1))
                 (deriv (base exp) var))))))))

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
    (cond ((null? symbols) number)
          ((= number 0) 0)
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

;; -- exponentiation --

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

;; -- tests --

(install-sum-package)
(install-product-package)
(install-exponentiation-package)

(check-equal? (deriv '(+ x 3) 'x) 1)
(check-equal? (deriv '(+ 3 x) 'x) 1)
(check-equal? (deriv '(* 5 x) 'x) 5)
(check-equal? (deriv '(* x 3) 'x) 3)
(check-equal? (deriv '(** x 2) 'x) '(* 2 x))
