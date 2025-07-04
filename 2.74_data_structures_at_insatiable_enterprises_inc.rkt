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

;; -- type tag --

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged data -- TYPE-TAG" datum)))

;; -- division1 records

(define Ben (list (cons 'name "Bitdiddle Ben") (cons 'address "Slumerville") (cons 'position "computer wizard") (cons 'salary "60000")))
(define Alyssa (list (cons 'name "Hacker Alyssa P") (cons 'address "Cambridge") (cons 'position "computer programmer") (cons 'salary "40000")))

(define ins-div1-records (list Ben Alyssa))

(define typed-ins1
  (attach-tag 'ins1-record ins-div1-records))

(define (install-div1-package)
  (define (get-key record key)
    (if (null? record)
        #f
        (let ((pair (car record)))
          (if (eq? (car pair) key)
              (cdr pair)
              (get-key (cdr record) key)))))
  (define (get-record records need-name)
    (if (null? records)
        '()
        (let ((record (car records)))
          (let ((name (get-key record 'name)))
            (if (string=? name need-name)
                (attach-tag 'ins1-record record)
                (get-record (cdr records) need-name))))))
  (define (get-salary record)
    (get-key record 'salary))
  (put 'get-record 'ins1-record get-record)
  (put 'get-salary 'ins1-record get-salary))

;; -- division2 records

(define Fect (list "Fect Cy D" (list "computer programmer" "Cambridge"  35000)))
(define Tweakit (list "Tweakit Lem E" (list "computer technician" "Boston" 25000)))

(define ins-div2-records (list Fect Tweakit))

(define typed-ins2
  (attach-tag 'ins2-record ins-div2-records))

(define (install-div2-package)
  (define (get-record records need-name)
    (if (null? records)
        '()
        (let ((record (car records)))
          (let ((name (car record)))
            (if (string=? name need-name)
                (attach-tag 'ins2-record record)
                (get-record (cdr records) need-name))))))
  (define (get-salary record)
    (let ((info (cadr record)))
      (caddr info)))
  (put 'get-record 'ins2-record get-record)
  (put 'get-salary 'ins2-record get-salary))

;; -- generic selectors --

(define (get-record records name)
  ((get 'get-record (type-tag records)) (cdr records) name))

(define (get-salary tagged-record)
  ((get 'get-salary (type-tag tagged-record)) (cdr tagged-record)))

(define (find-employee-record div-records-list name)
  (if (null? div-records-list)
      '()
      (let ((div-records (car div-records-list)))
        (let ((record (get-record div-records name)))
          (if (not (null? record))
              (cdr record)
              (find-employee-record (cdr div-records-list) name))))))

;; -- tests --

(install-div1-package)
(install-div2-package)

(check-equal? (find-employee-record (list typed-ins1 typed-ins2) "Bitdiddle Ben") Ben)
(check-equal? (find-employee-record (list typed-ins1 typed-ins2) "Tweakit Lem E") Tweakit)
(check-equal? (cdr (get-record typed-ins1 "Hacker Alyssa P")) Alyssa)
(check-equal? (get-record typed-ins2 "Hacker Alyssa P") '())
(check-equal? (cdr (get-record typed-ins2 "Fect Cy D")) Fect)
(check-equal? (get-record typed-ins1 "Fect Cy D") '())
(check-equal? (get-salary (get-record typed-ins1 "Bitdiddle Ben")) "60000")
(check-equal? (get-salary (get-record typed-ins2 "Fect Cy D")) 35000)
