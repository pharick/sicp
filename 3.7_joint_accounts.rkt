#lang racket

(require rackunit)

(define (make-account balance correct-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (cond ((not (eq? password correct-password))
           (error "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint acc pass joint-pass)
  (lambda (jpass m)
    (if (not (eq? jpass joint-pass))
        (error "Incorrect joint password -- MAKE-JOINT" jpass)
        (acc pass m))))

(define peter-acc (make-account 100 'secret-password))

(define paul-acc
  (make-joint peter-acc 'secret-password 'rosebud))

(check-equal? ((peter-acc 'secret-password 'deposit) 10) 110)
(check-equal? ((paul-acc 'rosebud 'deposit) 10) 120)
(check-equal? ((peter-acc 'secret-password 'withdraw) 20) 100)
(check-equal? ((paul-acc 'rosebud 'withdraw) 85) 15)
