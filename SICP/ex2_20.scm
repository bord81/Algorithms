#lang racket

(define (same-parity p . l)
  (let loop ((x (remainder p 2))
             (arg l)
             (res (cons p (list))))
    (cond ((null? arg) (reverse res))
          ((= x (remainder (car arg) 2)) (loop x (cdr arg) (cons (car arg) res)))
          (else (loop x (cdr arg) res)))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
