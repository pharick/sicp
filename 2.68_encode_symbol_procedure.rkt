#lang racket

(require rackunit)

;; -- leaf --

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;; -- code tree --

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; -- decode huffman code --

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; -- make leaf set --

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; -- encode message --

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-impl symbol tree)
    (cond ((not (element-of-set? symbol (symbols tree))) #f)
          ((leaf? tree) '())
          (else
           (let ((left-try (encode-symbol-impl symbol (left-branch tree)))
                 (right-try (encode-symbol-impl symbol (right-branch tree))))
             (cond ((and (not (equal? left-try #f))
                         (not (equal? right-try #f)))
                    #f)
                   ((not (equal? left-try #f))
                    (cons 0 left-try))
                   ((not (equal? right-try #f))
                    (cons 1 right-try))
                   (else #f))))))
  (let ((result (encode-symbol-impl symbol tree)))
    (if (equal? result #f)
        (error "tree does not contain symbol or ambiguous" symbol)
        result)))

;; -- test --

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define test-message '(A D A B B C A))

(define encoded-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(check-equal? (encode test-message sample-tree) encoded-message)
(check-equal? (encode '(A B C D) sample-tree) '(0 1 0 1 1 1 1 1 0))
