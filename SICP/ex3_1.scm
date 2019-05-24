#lang racket

(define (make-accumulator init-bal)
  (lambda (to-add)
    (set! init-bal (+ init-bal to-add))
          init-bal))

(define A (make-accumulator 5))
(A 10)
(A 10)