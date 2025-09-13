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

;; -- tests --

(require rackunit)

;; comparators
(define (num-cmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))
(define (str-cmp a b) (cond [(string<? a b) -1] [(string>? a b) 1] [else 0]))

;; -------- numeric keys --------
(let ([t (make-table num-cmp)])
  ;; empty
  (check-false (lookup t 123))

  ;; single insert + lookup
  (insert! t 10 'a)
  (check-equal? (lookup t 10) 'a)

  ;; branch left/right
  (insert! t 5 'b)
  (insert! t 15 'c)
  (check-equal? (lookup t 5)  'b)
  (check-equal? (lookup t 15) 'c)

  ;; overwrite existing key
  (insert! t 10 'A)
  (check-equal? (lookup t 10) 'A)

  ;; more inserts (mixed order)
  (for ([k '(8 3 12 1 4 9 13 2 7 6 11)])
    (insert! t k k))

  ;; spot checks
  (for ([kv '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (6 . 6) (7 . 7)
                      (8 . 8) (9 . 9) (11 . 11) (12 . 12) (13 . 13))])
    (check-equal? (lookup t (car kv)) (cdr kv)))

  ;; repeated updates on same key
  (insert! t 42 'x)
  (insert! t 42 'y)
  (insert! t 42 'z)
  (check-equal? (lookup t 42) 'z)

  ;; missing key
  (check-false (lookup t 999))

  (print t))

;; -------- string keys --------
(let ([t (make-table str-cmp)])
  (check-false (lookup t "missing"))

  (insert! t "cat" 1)
  (insert! t "dog" 2)
  (insert! t "ant" 3)

  (check-equal? (lookup t "ant") 3)
  (check-equal? (lookup t "cat") 1)
  (check-equal? (lookup t "dog") 2)

  ;; update
  (insert! t "cat" 99)
  (check-equal? (lookup t "cat") 99)

  (print t))
