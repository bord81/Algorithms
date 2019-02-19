#lang racket

(define (fringe t)
  (define l (list))
  (define (iter-leaves x)
    (cond
      ((and (not (pair? x)) (not (null? x))) (set! l (cons x l)))
      ((not (null? x)) (begin (iter-leaves (car x))
                              (iter-leaves (cdr x))))))
  (iter-leaves t)
  (reverse l))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))