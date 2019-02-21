#lang racket

(define (balanced? m)
  (= (cond ((not (pair?(branch-structure (left-branch m)))) (* (branch-structure (left-branch m)) (branch-length (left-branch m))))
           (else (* (total-weight (branch-structure (left-branch m))) (branch-length (left-branch m)))))
           (cond ((not (pair?(branch-structure (right-branch m)))) (* (branch-structure (right-branch m)) (branch-length (right-branch m))))
                 (else (* (total-weight (branch-structure (right-branch m))) (branch-length (right-branch m)))))))

(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else (+ (total-weight (branch-structure (left-branch m))) (total-weight (branch-structure (right-branch m)))))))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define b1_1 (make-branch 1 3))
(define b2_1 (make-branch 1 1))
(define b22 (make-mobile b1_1 b2_1))
(define b1 (make-branch 10 5))
(define b2 (make-branch 12.5 b22))
(define m1 (make-mobile b1 b2))

(total-weight m1)

(balanced? m1)