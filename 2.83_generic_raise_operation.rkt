#lang racket

;; -- dispatching table --

(define-values (get put get-coercion put-coercion)
  (let ((table          (make-hash))
        (coercion-table (make-hash)))

    ;; lookup a method
    (define (get op type-tags)
      (define subtable (hash-ref table op #f))
      (and subtable (hash-ref subtable type-tags #f)))

    ;; install a method
    (define (put op type-tags proc)
      (define subtable
        (or (hash-ref table op #f)
            (let ((h (make-hash)))
              (hash-set! table op h)
              h)))
      (hash-set! subtable type-tags proc)
      'ok)

    ;; lookup a coercion
    (define (get-coercion from-type to-type)
      (define m (hash-ref coercion-table from-type #f))
      (and m (hash-ref m to-type #f)))

    ;; install a coercion
    (define (put-coercion from-type to-type proc)
      (define m
        (or (hash-ref coercion-table from-type #f)
            (let ((h (make-hash)))
              (hash-set! coercion-table from-type h)
              h)))
      (hash-set! m to-type proc)
      'ok)

    (values get put get-coercion put-coercion)))

;; -- type tags --

(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "bad tagged data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

;; -- apply generic --

(define (apply-generic op . args)
  (define (check-coercion-to to-type type-tags)
    (if (null? type-tags)
        #t
        (let ((from-type (car type-tags)))
          (cond ((eq? (car type-tags) to-type)
                 (check-coercion-to to-type (cdr type-tags)))
                ((not (get-coercion (car type-tags) to-type)) #f)
                (else (check-coercion-to to-type (cdr type-tags)))))))
  (define (find-coercion-to type-tags-iter type-tags)
    (if (null? type-tags-iter)
        '()
        (let ((type-to (car type-tags-iter)))
          (if (check-coercion-to type-to type-tags)
              type-to
              (find-coercion-to (cdr type-tags-iter) type-tags)))))
  (define (make-coercion x to-type)
    (let ((type (type-tag x)))
      (if (eq? to-type type)
          x
          ((get-coercion (type-tag x) to-type) (contents x)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            ((> (length args) 1)
             (let ((to-type (find-coercion-to type-tags type-tags)))
               (if (null? to-type)
                   (error "No method for these types"
                          (list op type-tags))
                   (apply apply-generic op (map
                                            (lambda (x) (make-coercion x to-type))
                                            args)))))
            (else (error "No method for these types"
                         (list op type-tags)))))))

;; -- generic arithmetic procedures --

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ? x y))

(define (=zero? x) (apply-generic '=zero? x))

(define (raise x) (apply-generic 'raise x))

;; -- scheme number package --

(define (install-scheme-number-package)
  ;; internal procedures
  (define (=zero? x)
    (= x 0))
  (define (raise x)
    ((get-coercion 'scheme-number 'rational) x))
  ;; interface
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) =zero?)
  (put 'raise '(scheme-number) raise)
  'done)

;; -- rational number package

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (= 0 (numer x)))
  (define (raise x)
    ((get-coercion 'rational 'complex) x))
  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational) raise)
  ;; coercions
  (define (scheme-number->rational x)
    (tag (make-rat x 1)))
  (put-coercion 'scheme-number 'rational
                scheme-number->rational)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; -- complex numbers rectangular package --

(define (install-complex-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; -- complex numbers polar package --

(define (install-complex-polar-package)
  ;; internal procedures
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))
  ;; interface
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; -- complex number package --

(define (install-complex-number-package)
  ;; imported procedures from rectangular and polar packages
  (install-complex-rectangular-package)
  (install-complex-polar-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (or (and (= (real-part z1) (real-part z2))
             (= (imag-part z1) (imag-part z2)))
        (and (= (magnitude z1) (magnitude z2))
             (= (angle z1) (angle z2)))))
  (define (=zero? z)
    (and (= 0 (real-part z))
         (= 0 (imag-part z))))
  ;; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  ;; coercions
  (define (rational->complex x)
    (make-complex-from-real-imag x 0))
  (put-coercion 'rational 'complex rational->complex)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

;; -- helpers --

(define (square x)
  (* x x))

;; -- install packages --

(install-scheme-number-package)
(install-rational-package)
(install-complex-number-package)
