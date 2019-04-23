#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (let loop ((s1 set1)
             (s2 set2))
    (cond ((null? s2) s1)
          (else 
           (loop (adjoin-set (car s2) s1) (cdr s2))))))
    
(intersection-set '(1 4 3 10 5 6) '(4 5 6 7 8 9))
(union-set '(1 2 3 4 5 6) '(4 5 6 7 8 9))