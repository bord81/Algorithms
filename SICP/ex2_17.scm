#lang racket

(define (last-pair l)
  (define (l-p-int l el)
    (if (null? l)
        el
        (l-p-int (cdr l) (car l))))
  (l-p-int l '()))

(last-pair (list 23 72 149 34))