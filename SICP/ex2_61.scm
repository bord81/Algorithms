#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (define (ins-sorted e s)
    (cond ((null? s) (cons e '()))
          ((> e (car s)) (cons (car s) (ins-sorted e (cdr s))))
          ((< e (car s)) (cons e s))))
  (if (element-of-set? x set)
      set
      (ins-sorted x set)))

(adjoin-set '0 '(1 2 3 4 6 7 9))
(adjoin-set '5 '(1 2 3 4 6 7 9))
(adjoin-set '11 '(1 2 3 4 6 7 9))
(adjoin-set '9 '(1 2 3 4 6 7 9))