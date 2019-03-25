#lang planet neil/sicp

(define (make-segment v1 v2)
 (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))