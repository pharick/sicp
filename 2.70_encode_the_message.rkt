#lang racket

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

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; -- generate huffman encoding tree --

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) (error "empty pair list -- SUCCESSIVE-MERGE"))
        ((null? (cdr leaf-set)) (car leaf-set))
        (else
         (let ((leaf1 (car leaf-set))
               (leaf2 (cadr leaf-set)))
           (successive-merge (adjoin-set (make-code-tree leaf1 leaf2)
                                         (cddr leaf-set)))))))

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

;; -- tests --

(define tree (generate-huffman-tree '((A 2)
                                      (BOOM 1)
                                      (GET 2)
                                      (JOB 2)
                                      (NA 16)
                                      (SHA 3)
                                      (YIP 9)
                                      (WAH 1))))

(length (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP)
                tree))
