#lang racket

(require racket/mpair)

;; -- node --

(define (make-node key value)
  (mcons key (mcons value '())))

(define (node-key node)
  (mcar node))

(define (node-value node)
  (mcar (mcdr node)))

(define (node-subtable node)
  (mcdr (mcdr node)))

(define (set-node-value! node value)
  (set-mcar! (mcdr node) value))

(define (add-subnode! node new-node)
  (set-mcdr! (mcdr node)
             (mcons new-node (node-subtable node))))

;; -- table --

(define (make-table)

  (define (assoc key nodes)
    (cond ((null? nodes) #f)
          ((equal? key (node-key (mcar nodes)))
           (mcar nodes)) ; return node
          (else
           (assoc key (mcdr nodes)))))

  (let ((table (make-node '*table* '())))

    (define (lookup keys)
      (define (loop keys node)
        (cond ((not node) #f)
              ((null? keys) (node-value node))
              (else
               (loop (mcdr keys)
                     (assoc (mcar keys) (node-subtable node))))))
      (loop keys table))

    (define (insert! keys value)
      (define (loop keys node)
        (if (null? keys)
            (begin
              (set-node-value! node value)
              'ok)
            (let ((key (mcar keys)))
              (let ((next-node (assoc key (node-subtable node))))
                (if next-node
                    (loop (mcdr keys) next-node)
                    (let ((new-node (make-node key '())))
                      (begin
                        (add-subnode! node new-node)
                        (loop (mcdr keys) new-node))))))))
      (loop keys table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else
             (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup table keys)
  ((table 'lookup-proc) keys))

(define (insert! table keys value)
  ((table 'insert-proc!) keys value))

;; -- tests --

(require rackunit)

(define t (make-table))

(insert! t (mlist 'a) 5)
(insert! t (mlist 'a 'b) 10)
(insert! t (mlist 'a 'b 'c) 42)

(check-equal? (lookup t (mlist 'a)) 5)
(check-equal? (lookup t (mlist 'a 'b)) 10)
(check-false (lookup t (mlist 'b)))
(check-equal? (lookup t (mlist 'a 'b 'c)) 42)
