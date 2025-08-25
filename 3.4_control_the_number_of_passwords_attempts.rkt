#lang racket

(require rackunit)
(require racket/exn)

(define (make-account balance correct-password)
  (let ((attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      (error "Cops called"))
    (define (dispatch password m)
      (if (not (eq? password correct-password))
          (begin (set! attempts (+ attempts 1))
                 (if (> attempts 7)
                     (call-the-cops)
                     (error "Wrong password")))
          (begin (set! attempts 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT"
                                    m))))))
    dispatch))

(define acc (make-account 100 'secret-password))

(define wrong-message "Wrong password")

(define cops-message "Cops called")

(define (attempt)
  (with-handlers ([exn:fail?
                   (lambda (e) (exn->string e))])
    ((acc 'some-other-password 'deposit) 50)))

(check-equal? ((acc 'secret-password 'withdraw) 40) 60)
(check-equal? ((acc 'secret-password 'deposit) 40) 100)
; substring used because checking system has extra output
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 11) cops-message)
