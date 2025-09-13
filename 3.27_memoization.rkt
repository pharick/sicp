#lang racket

(require racket/mpair)

;; -- node --

(define (make-node key value)
  (mlist key value 'r '() '()))

(define (node-key node)
  (mcar node))

(define (node-value node)
  (mcar (mcdr node)))

(define (node-color node)
  (mcar (mcdr (mcdr node))))

(define (node-left node)
  (mcar (mcdr (mcdr (mcdr node)))))

(define (node-right node)
  (mcar (mcdr (mcdr (mcdr (mcdr node))))))

(define (set-value! node v)
  (set-mcar! (mcdr node) v))

(define (set-color! node v)
  (set-mcar! (mcdr (mcdr node)) v))

(define (set-left! node v)
  (set-mcar! (mcdr (mcdr (mcdr node))) v))

(define (set-right! node v)
  (set-mcar! (mcdr (mcdr (mcdr (mcdr node)))) v))

;; -- rb tree --

(define (node-red? node)
  (and (not (null? node))
       (eq? (node-color node) 'r)))

(define (node-black? node)
  (or (null? node)
      (eq? (node-color node) 'b)))

(define (rotate-left n)
  (let* ((r (node-right n))
         (_ (set-right! n (node-left r)))
         (_ (set-left! r n))
         (_ (set-color! r (node-color n)))
         (_ (set-color! n 'r)))
    r))

(define (rotate-right n)
  (let* ((l (node-left n))
         (_ (set-left! n (node-right l)))
         (_ (set-right! l n))
         (_ (set-color! l (node-color n)))
         (_ (set-color! n 'r)))
    l))

(define (color-flip n)
  (when (not (null? n))
    (let ((l (node-left n))
          (r (node-right n)))
      (when (not (null? l)) (set-color! l (if (node-red? l) 'b 'r)))
      (when (not (null? r)) (set-color! r (if (node-red? r) 'b 'r)))
      (set-color! n (if (node-red? n) 'b 'r))))
  n)

;; -- table --

(define (make-table cmp)
  (let ((root '()))

    (define (lookup key)
      (define (loop node)
        (if (null? node)
            #f
            (let ((cmp-res (cmp key (node-key node))))
              (cond ((< cmp-res 0)
                     (loop (node-left node)))
                    ((> cmp-res 0)
                     (loop (node-right node)))
                    (else
                     (node-value node))))))
      (loop root))

    (define (insert! key value)
      (define (balance n)
        (let ((n (if (and (node-red? (node-right n))
                          (not (node-red? (node-left n))))
                     (rotate-left n)
                     n)))
          (let ((n (if (and (node-red? (node-left n))
                            (node-red? (node-left (node-left n))))
                       (rotate-right n)
                       n)))
            (begin
              (when (and (node-red? (node-left n))
                         (node-red? (node-right n)))
                (color-flip n))
              n))))
      
      (define (ins node)
        (if (null? node)
            (make-node key value)
            (let ((cmp-res (cmp key (node-key node))))
              (cond ((< cmp-res 0)
                     (set-left! node (ins (node-left node)))
                     (balance node))
                    ((> cmp-res 0)
                     (set-right! node (ins (node-right node)))
                     (balance node))
                    (else
                     (set-value! node value)
                     node)))))
      (set! root (ins root))
      (when (not (null? root)) (set-color! root 'b))
      'ok)

    (define (print)
      (define (loop node indent)
        (unless (null? node)
          (loop (node-right node) (+ indent 4))
          (printf "~a~a(~a)~n"
                  (make-string indent #\space)
                  (node-key node)
                  (node-color node))
          (loop (node-left node) (+ indent 4))))
      (loop root 0))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else
             (error "Unknown operation -- TABLE" m))))

    dispatch))

(define (lookup table key)
  ((table 'lookup-proc) key))

(define (insert! table key value)
  ((table 'insert-proc!) key value))

(define (print table)
  ((table 'print-proc)))

;; -- memoization --

(define (memoize f)
  (let ((table (make-table (lambda (x y) (- x y)))))
    (lambda (x)
      (let ((previously-computed-result (lookup table x)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! table x result)
              result))))))

;; -- fibonacci --

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else
                    (+ (memo-fib (- n 1))
                       (memo-fib (- n 2))))))))

;; -- tests --


(require rackunit)

;; base cases
(check-equal? (memo-fib 0) 0)
(check-equal? (memo-fib 1) 1)

;; small sequence
(check-equal?
 (for/list ([n (in-range 0 11)]) (memo-fib n))
 '(0 1 1 2 3 5 8 13 21 34 55))

;; a larger value to ensure recursion + caching work
(check-equal? (memo-fib 20) 6765)
(check-equal? (memo-fib 30) 832040)

;; repeated calls should be consistent (cached results)
(check-equal? (memo-fib 30) 832040)
(check-equal? (memo-fib 20) 6765)
