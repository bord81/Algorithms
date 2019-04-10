#lang racket

(define (my-equal? a b)
  (cond ((or (not (list? a)) (not (list? b))) (eq? a b))
         ((and (pair? a) (pair? b))
               (and (my-equal? (car a) (car b)) (my-equal? (cdr a) (cdr b))))))

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

(my-equal? '(this is a list) '(this is a list))

(my-equal? '(this is a list) '(this (is a) list))